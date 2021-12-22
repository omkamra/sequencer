(ns omkamra.sequencer
  (:refer-clojure :exclude [compile])
  (:require [omkamra.sequencer.protocols.Target :as Target]
            [omkamra.sequencer.protocols.TargetFactory :as TargetFactory])
  (:import (java.util.concurrent TimeUnit)
           (java.util.concurrent.locks LockSupport))
  (:require [clojure.stacktrace :refer [print-cause-trace]]))

(def nanosleep-default-precision (.toNanos TimeUnit/MILLISECONDS 2))
(def parknanos-default-precision 100)

(def nanosleep-current-precision (atom nanosleep-default-precision))
(def parknanos-current-precision (atom parknanos-default-precision))

(defn nanosleep
  "Sleeps for ns nanoseconds. Attempts to be precise, even if this
  requires busy waiting."
  [^long ns]
  (let [end (+ (System/nanoTime) ns)]
    (loop [time-left ns]
      (when (pos? time-left)
        (let [sleep-start (System/nanoTime)]
          (let [^long precision @nanosleep-current-precision]
            (if (> time-left precision)
              (let [sleep-dur (- time-left precision)]
                (.sleep TimeUnit/NANOSECONDS sleep-dur)
                (let [elapsed (- (System/nanoTime) sleep-start)
                      diff (Math/abs (- elapsed sleep-dur))]
                  (when (or (>= diff (* precision 2))
                            (<= diff (/ precision 2)))
                    (reset! nanosleep-current-precision diff))))
              (let [^long precision @parknanos-current-precision]
                (if (> time-left precision)
                  (let [sleep-dur (- time-left precision)]
                    (LockSupport/parkNanos sleep-dur)
                    (let [elapsed (- (System/nanoTime) sleep-start)
                          diff (Math/abs (- elapsed sleep-dur))]
                      (when (or (>= diff (* precision 2))
                                (<= diff (/ precision 2)))
                        (reset! parknanos-current-precision diff))))
                  (Thread/yield))))))
        (recur (- end (System/nanoTime)))))))

(defn beats->ms
  [beats bpm]
  (let [beats-per-second (/ bpm 60)
        seconds-per-beat (/ 1.0 beats-per-second)
        ms-per-beat (* 1000 seconds-per-beat)]
    (* beats ms-per-beat)))

(defn beats->ns
  [beats bpm]
  (-> (beats->ms beats bpm)
      (* 1000 1000)))

(defn beats->ticks
  [beats tpb]
  (* beats tpb))

(defn ticks->ms
  [ticks tpb bpm]
  (let [beats-per-tick (/ 1.0 tpb)]
    (beats->ms (* ticks beats-per-tick) bpm)))

(defn ticks->ns
  [ticks tpb bpm]
  (-> (ticks->ms ticks tpb bpm)
      (* 1000 1000)))

(defn switch!
  "Sets the value of atom to new-val. Returns the previous value."
  [atom new-val]
  (let [old-val @atom
        success? (compare-and-set! atom old-val new-val)]
    (if success?
      old-val
      (recur atom new-val))))

(defn align-position
  [position alignment]
  (if (zero? alignment)
    position
    (let [m (mod position alignment)]
      (if (zero? m)
        position
        (- (+ position alignment) m)))))

(defn conjv
  [v x]
  (conj (or v []) x))

(defn merge-pattern-queue
  "Merges each pattern in pq into the timeline starting at start-pos."
  [timeline start-pos pq]
  (reduce
   (fn [timeline {:keys [snap events delay] :as pattern}]
     (let [snapped-start-pos (align-position start-pos snap)]
       (reduce
        (fn [timeline [offset callback :as event]]
          ;; avoid scheduling callbacks to start-pos as that's
          ;; already in the past
          (let [absolute-pos (+ snapped-start-pos (int offset) delay)
                adjusted-pos (max absolute-pos (inc start-pos))]
            (update timeline adjusted-pos conjv callback)))
        timeline events)))
   timeline pq))

;; targets

(defonce target-factories (atom #{}))

(defn register-target-factory
  [factory]
  (swap! target-factories conj factory))

(defn find-target-factory
  [descriptor]
  (let [ok? (fn [factory] (TargetFactory/understands-descriptor? factory descriptor))
        ok-factories (filter ok? @target-factories)]
    (when (empty? ok-factories)
      (throw (ex-info "unable to find target factory which understands descriptor"
                      {:descriptor descriptor})))
    (when (> (count ok-factories) 1)
      (throw (ex-info "descriptor is understood by several target factories"
                      {:descriptor descriptor})))
    (first ok-factories)))

(def make-target*
  (memoize
   (fn [factory sanitized-descriptor]
     (TargetFactory/make-target factory sanitized-descriptor))))

(defn make-target
  "Finds the target factory which understands descriptor and uses it
  to create the corresponding target. Caches the result, so successive
  calls with the same descriptor return the same target. Uses the
  descriptor's sanitized (canonical) form as the cache key."
  [descriptor]
  (let [factory (find-target-factory descriptor)
        descriptor (TargetFactory/sanitize-descriptor factory descriptor)]
    (make-target* factory descriptor)))

(defonce
  ^{:doc "Set of targets shared by all sequencers. These are automatically started when the first sequencer starts and stopped when the last active sequencer exits."}
  registered-targets (atom #{}))

(defonce
  ^{:doc "Aliases which can be used to identify targets in :bind expressions."}
  target-aliases (atom {}))

(defn register-target
  ([target]
   (swap! registered-targets conj target))
  ([target alias]
   (assert (qualified-keyword? alias) "target alias must be a qualified keyword")
   (swap! registered-targets conj target)
   (swap! target-aliases assoc alias target)))

(defn unregister-target
  [target]
  (swap! registered-targets disj target)
  (swap! target-aliases
         (fn [aliases]
           (into {}
                 (remove (fn [[k v]] (= v target)))
                 aliases))))

(def
  ^:dynamic
  ^{:doc "The pattern compiler turns to this target if it finds a pattern form, pattern expression, binding key or bind expression that it does not understand. This target can also supply default bindings for :bind forms."}
  *compile-target* nil)

(defn resolve-target
  [target]
  (if (satisfies? Target/protocol target)
    target
    (get @target-aliases target)))

;; patterns

(def seed-pattern
  {:events []
   :snap 0
   :delay 0
   :offset 0})

(defn add-callback
  [{:keys [offset] :as pattern} callback]
  (if callback
    (update pattern :events conj [offset callback])
    pattern))

(defn add-callback-after
  [{:keys [offset] :as pattern} delay callback]
  (if (and delay callback)
    (update pattern :events conj [(+ offset delay) callback])
    pattern))

(defmacro pfn
  "Same as fn but marks the resulting function as a pattern transformer."
  {:style/indent 1}
  [& args]
  `(vary-meta (fn ~@args) assoc :pfn? true))

(defn pfn?
  [x]
  (and (fn? x) (:pfn? (meta x))))

(defn pattern-expr?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defmulti compile-pattern-expr first)

(defn compile-pattern-form
  [x]
  (if (pfn? x)
    x
    (recur (cond
             (pattern-expr? x) (compile-pattern-expr x)
             (var? x) (compile-pattern-expr [:var x])
             (fn? x) (compile-pattern-expr [:call x])
             (sequential? x) (compile-pattern-expr (cons :seq x))
             (set? x) (compile-pattern-expr (cons :mix x))
             (nil? x) (compile-pattern-expr [:nop])
             :else (or (and *compile-target*
                            (Target/compile-pattern-form *compile-target* x))
                       (throw (ex-info "unable to compile pattern form" {:form x})))))))

(def compile compile-pattern-form)

(defmethod compile-pattern-expr :default
  [expr]
  (or (and *compile-target*
           (Target/compile-pattern-expr *compile-target* expr))
      (throw (ex-info "unable to compile pattern expression" {:expr expr}))))

(defmethod compile-pattern-expr :nop
  [[_]]
  (pfn [pattern bindings]
    pattern))

(defmethod compile-pattern-expr :clear
  [[_]]
  (pfn [pattern bindings]
    seed-pattern))

(defmethod compile-pattern-expr :call
  [[_ callback & args]]
  (let [callback (if args
                   #(apply callback args)
                   callback)]
    (pfn [pattern bindings]
      (add-callback pattern callback))))

(defmethod compile-pattern-expr :snap
  [[_ beats]]
  (pfn [pattern bindings]
    (let [{:keys [sequencer]} bindings
          {:keys [tpb]} sequencer]
      (assoc pattern :snap (beats->ticks beats tpb)))))

(defmethod compile-pattern-expr :delay
  [[_ pf delay]]
  (pfn [pattern bindings]
    (-> pattern pf (assoc :delay delay))))

(defmethod compile-pattern-expr :wait
  [[_ steps]]
  (if (zero? steps)
    (compile-pattern-expr [:nop])
    (pfn [pattern bindings]
      (let [{:keys [step sequencer]} bindings
            {:keys [tpb]} sequencer
            ticks (beats->ticks (* steps step) tpb)]
        (if (pos? ticks)
          (update pattern :offset + ticks)
          (let [alignment (- ticks)]
            (update pattern :offset align-position alignment)))))))

(defmethod compile-pattern-expr :var
  [[_ v]]
  (pfn [pattern bindings]
    (let [pf (compile-pattern-form (var-get v))]
      (pf pattern bindings))))

(defn collect-bindings
  [items]
  (loop [items items
         bindings {}
         body []]
    (if (seq items)
      (let [head (first items)]
        (if (map? head)
          (recur (next items)
                 (merge bindings head)
                 body)
          (recur (next items)
                 bindings
                 (conj body head))))
      [bindings body])))

(defmethod compile-pattern-expr :seq
  [[_ & body]]
  (if (nil? body)
    (compile-pattern-expr [:nop])
    (let [[bindings body] (collect-bindings body)]
      (if (seq bindings)
        (compile-pattern-expr `[:bind ~bindings [:seq ~@body]])
        (if (nil? (next body))
          (compile-pattern-form (first body))
          (let [pfs (mapv compile-pattern-form body)]
            (pfn [pattern bindings]
              (reduce (fn [pattern pf]
                        (pf pattern bindings))
                      pattern pfs))))))))

(defmethod compile-pattern-expr :mix
  [[_ & body]]
  (let [[bindings body] (collect-bindings body)]
    (if (seq bindings)
      (compile-pattern-expr `[:bind ~bindings [:mix ~@body]])
      (let [pfs (mapv compile-pattern-form body)]
        (pfn [pattern bindings]
          (let [{:keys [offset]} pattern]
            (reduce (fn [pattern pf]
                      (-> (pf pattern bindings)
                          (assoc :offset offset)))
                    pattern pfs)))))))

(defmethod compile-pattern-expr :mix1
  [[_ & body]]
  (let [[bindings body] (collect-bindings body)]
    (if (seq bindings)
      (compile-pattern-expr `[:bind ~bindings [:mix1 ~@body]])
      (let [[leader & rest] body
            leader-pf (compile-pattern-form leader)
            rest-pf (compile-pattern-expr (cons :mix rest))]
        (pfn [pattern bindings]
          (-> pattern
              (rest-pf bindings)
              (leader-pf bindings)))))))

(declare play)

(defmethod compile-pattern-expr :play
  [[_ & body]]
  (let [pf (compile-pattern-expr [:seq [:delay 1] (cons :seq body)])]
    (pfn [{:keys [offset] :as pattern}
          {:keys [sequencer] :as bindings}]
      ;; @position-2: callback executed, future
      ;;   invokes (play sequencer ...) in a separate thread which
      ;;   applies pf to the initial pattern and bindings, then
      ;;   adds the resulting pattern P to the pattern queue
      ;; @position-1: P is merged into the timeline
      ;; @position-0: events in P are ready to be executed
      ;;
      ;; merge-pattern-queue ensures that the actually scheduled
      ;; position of any event is not in the past (this may result in
      ;; a one tick delay for events with an offset of zero)
      (let [sched-offset (- offset 2)
            callback #(future (play sequencer pf bindings))]
        (update pattern :events conj [sched-offset callback])))))

(declare bpm!)

(defmethod compile-pattern-expr :bpm
  [[_ new-bpm]]
  (pfn [pattern {:keys [sequencer] :as bindings}]
    (add-callback pattern #(bpm! sequencer new-bpm))))

;; bind

(defn resolve-binding
  [k v]
  (if (and (= k :target) (keyword? v))
    (or (resolve-target v)
        (throw (ex-info "cannot resolve target" {:target-alias v})))
    (or (and *compile-target*
             (Target/resolve-binding *compile-target* k v))
        v)))

(defn bind-expr?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defmulti compile-bind-expr
  (fn [k bind-expr]
    (first bind-expr)))

(defn compile-bind-spec
  "Takes a key and a spec, returns a function which when given a map of
  bindings, returns a value calculated from the bindings as described
  by spec."
  [k spec]
  (if (bind-expr? spec)
    (compile-bind-expr k spec)
    (constantly (resolve-binding k spec))))

(defmethod compile-bind-expr :default
  [k expr]
  (or (and *compile-target*
           (Target/compile-bind-expr *compile-target* k expr))
      (throw (ex-info "unable to compile bind expression" {:k k :expr expr}))))

(defmethod compile-bind-expr :binding-of
  [k [_ arg]]
  (fn [bindings]
    (get bindings arg)))

(defmacro compile-binop-bind-expr
  [name op]
  `(defmethod compile-bind-expr ~name
     [~'k [~'_ ~'x ~'y]]
     (if ~'y
       (let [~'x (compile-bind-spec ~'k ~'x)
             ~'y (compile-bind-spec ~'k ~'y)]
         (fn [~'bindings] (~op (~'x ~'bindings) (~'y ~'bindings))))
       (compile-bind-expr ~'k [~name [:binding-of ~'k] ~'x]))))

(compile-binop-bind-expr :add +)
(compile-binop-bind-expr :sub -)
(compile-binop-bind-expr :mul *)
(compile-binop-bind-expr :div /)

(defn bind-specs->update-fn
  "Takes a map of binding specifications and creates a function which
  when given a map of bindings, updates them according to the specs."
  [bind-specs]
  (reduce-kv
   (fn [f k v]
     (let [calculate-bind-value (compile-bind-spec k v)]
       (comp #(assoc % k (calculate-bind-value %)) f)))
   identity bind-specs))

(defn get-default-bindings
  []
  (or (and *compile-target*
           (Target/get-default-bindings *compile-target*))
      {}))

(defmethod compile-pattern-expr :bind
  [[_ bind-specs & body]]
  (if (empty? bind-specs)
    (compile-pattern-expr (cons :seq body))
    (let [target (resolve-target (:target bind-specs))]
      (binding [*compile-target* (or target *compile-target*)]
        (let [pf (compile-pattern-expr (cons :seq body))
              default-bindings (get-default-bindings)
              update-bindings (bind-specs->update-fn bind-specs)]
          (pfn [pattern bindings]
            (pf pattern (-> default-bindings
                            (merge bindings)
                            update-bindings))))))))

;; sequencer

(defonce active-sequencers (atom #{}))

(defn register-sequencer
  [sequencer]
  (let [oldval @active-sequencers
        newval (conj oldval sequencer)
        success? (compare-and-set! active-sequencers oldval newval)]
    (if success?
      (when (empty? oldval)
        (doseq [target @registered-targets]
          (Target/start target)))
      (recur sequencer))))

(defn unregister-sequencer
  [sequencer]
  (let [oldval @active-sequencers
        newval (disj oldval sequencer)
        success? (compare-and-set! active-sequencers oldval newval)]
    (if success?
      (when (and (seq oldval) (empty? newval))
        (doseq [target @registered-targets]
          (Target/stop target)))
      (recur sequencer))))

(defrecord Sequencer [config bpm tpb
                      timeline position pattern-queue
                      playing player-thread]

  Target/protocol

  (start [this]
    (if @playing
      :already-playing
      (do
        (register-sequencer this)
        (reset! playing true)
        (reset! player-thread
                (future
                  (try
                    (loop [pos @position
                           tl @timeline]
                      (if @playing
                        (let [tick-start (System/nanoTime)]
                          (when-let [callbacks (get tl pos)]
                            (doseq [cb callbacks]
                              (cb)))
                          (let [pq (switch! pattern-queue [])]
                            (swap! timeline merge-pattern-queue pos pq))
                          (let [tick-duration (ticks->ns 1 tpb @bpm)
                                elapsed (- (System/nanoTime) tick-start)
                                remaining (- tick-duration elapsed)]
                            (nanosleep remaining))
                          (recur (swap! position inc)
                                 (swap! timeline dissoc pos)))
                        :stopped))
                    (catch Exception e
                      (print-cause-trace e)
                      :crashed))))
        :started)))

  (stop [this]
    (do
      (reset! playing false)
      (let [result @@player-thread]
        (unregister-sequencer this)
        result)))

  (restart [this]
    (Target/stop this)
    (Target/start this)))

(defn clear!
  [sequencer]
  (reset! (:timeline sequencer) {})
  (reset! (:pattern-queue sequencer) [])
  :cleared)

(defn play
  ([sequencer pf bindings]
   (Target/start sequencer)
   (let [init-pattern seed-pattern
         init-bindings (merge {:step 1}
                              bindings
                              {:sequencer sequencer})
         pf (compile-pattern-form pf)
         pattern (pf init-pattern init-bindings)]
     (swap! (:pattern-queue sequencer) conj pattern)
     :queued))
  ([sequencer pf]
   (play sequencer pf {})))

(defn bpm!
  [sequencer new-bpm]
  (reset! (:bpm sequencer) new-bpm))

(defn start
  [sequencer]
  (Target/start sequencer))

(defn stop
  [sequencer]
  (Target/stop sequencer))

(defn restart
  [sequencer]
  (Target/restart sequencer))

(defn status
  [sequencer]
  {:bpm @(:bpm sequencer)
   :tpb (:tpb sequencer)
   :playing @(:playing sequencer)
   :timeline @(:timeline sequencer)
   :position @(:position sequencer)
   :pattern-queue @(:pattern-queue sequencer)})

(defn create
  ([config]
   (map->Sequencer {:config config
                    :bpm (atom (get config :bpm 120))
                    :tpb (get config :tpb 96)
                    :playing (atom false)
                    :timeline (atom {})
                    :position (atom 0)
                    :pattern-queue (atom [])
                    :player-thread (atom nil)}))
  ([]
   (create {})))
