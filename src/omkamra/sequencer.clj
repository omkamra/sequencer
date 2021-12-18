(ns omkamra.sequencer
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
   (fn [timeline {:keys [snap events] :as pattern}]
     (let [snapped-start-pos (align-position start-pos snap)]
       (reduce
        (fn [timeline [position callback :as event]]
          ;; avoid scheduling callbacks to start-pos as that's
          ;; already in the past
          (let [absolute-pos (+ snapped-start-pos (int position))
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
  (let [ok? (fn [factory] (TargetFactory/understands-target-descriptor? factory descriptor))
        ok-factories (filter ok? @target-factories)]
    (when (empty? ok-factories)
      (throw (ex-info "unable to find target factory which understands descriptor"
                      {:descriptor descriptor})))
    (when (> (count ok-factories) 1)
      (throw (ex-info "descriptor is understood by several target factories"
                      {:descriptor descriptor})))
    (first ok-factories)))

(defonce
  ^{:doc "Set of targets shared by all sequencers. These are automatically started when a sequencer starts playing and stopped when the last sequencer stops playing."}
  registered-targets (atom #{}))

(defn register-target
  [target]
  (swap! registered-targets conj target))

(defn unregister-target
  [target]
  (swap! registered-targets disj target))

(defmacro define-target
  [name descriptor]
  `(def ~name (make-target ~descriptor)))

(defmacro define-and-register-target
  [name descriptor]
  `(do
     (def ~name (make-target ~descriptor))
     (register-target ~name)))

(def ^:dynamic *compile-target* nil)

;; patterns

(def init-pattern
  {:position 0
   :snap 0
   :events []})

(defn add-callback
  [{:keys [position] :as pattern} callback]
  (if callback
    (update pattern :events conj [position callback])
    pattern))

(defn add-callback-after
  [{:keys [position] :as pattern} delay callback]
  (if (and delay callback)
    (update pattern :events conj [(+ position delay) callback])
    pattern))

(defmacro pfn
  "Same as fn but marks the resulting function as a pattern function."
  {:style/indent 1}
  [& args]
  `(vary-meta (fn ~@args) assoc :pfn? true))

(defn pfn?
  [x]
  (and (fn? x) (:pfn? (meta x))))

(defn pattern?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defmulti compile-pattern first)

(defn compile-form
  [x]
  (if (pfn? x)
    x
    (recur (cond
             (pattern? x) (compile-pattern x)
             (var? x) (compile-pattern [:var x])
             (fn? x) (compile-pattern [:call x])
             (sequential? x) (compile-pattern (cons :seq x))
             (set? x) (compile-pattern (cons :mix x))
             (nil? x) (compile-pattern [:nop])
             :else (if *compile-target*
                     (Target/compile-form *compile-target* x)
                     (throw (ex-info "unable to compile form" {:form x})))))))

(defmethod compile-pattern :default
  [pattern]
  (if *compile-target*
    (Target/compile-pattern *compile-target* pattern)
    (throw (ex-info "unable to compile pattern" {:pattern pattern}))))

(defmethod compile-pattern :nop
  [[_]]
  (pfn [pattern bindings]
    pattern))

(defmethod compile-pattern :clear
  [[_]]
  (pfn [pattern bindings]
    init-pattern))

(defmethod compile-pattern :call
  [[_ callback]]
  (pfn [pattern bindings]
    (add-callback pattern callback)))

(defmethod compile-pattern :snap
  [[_ beats]]
  (pfn [pattern bindings]
    (let [{:keys [sequencer]} bindings
          tpb (:tpb sequencer)]
      (assoc pattern :snap (beats->ticks beats tpb)))))

(defmethod compile-pattern :wait
  [[_ steps]]
  (if (zero? steps)
    (compile-pattern [:nop])
    (pfn [pattern bindings]
      (let [{:keys [step sequencer]} bindings
            tpb (:tpb sequencer)
            ticks (beats->ticks (* steps step) tpb)]
        (if (pos? ticks)
          (update pattern :position + ticks)
          (let [alignment (- ticks)]
            (update pattern :position align-position alignment)))))))

(defmethod compile-pattern :var
  [[_ v]]
  (pfn [pattern bindings]
    (let [pf (var-get v)]
      (assert (pfn? pf))
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

(defmethod compile-pattern :seq
  [[_ & body]]
  (if (nil? body)
    (compile-pattern [:nop])
    (let [[bindings body] (collect-bindings body)]
      (if (seq bindings)
        (compile-pattern `[:bind ~bindings [:seq ~@body]])
        (if (nil? (next body))
          (compile-form (first body))
          (let [pfs (mapv compile-form body)]
            (pfn [pattern bindings]
              (reduce (fn [pattern pf]
                        (pf pattern bindings))
                      pattern pfs))))))))

(defmethod compile-pattern :mix
  [[_ & body]]
  (let [[bindings body] (collect-bindings body)]
    (if (seq bindings)
      (compile-pattern `[:bind ~bindings [:mix ~@body]])
      (let [pfs (mapv compile-form body)]
        (pfn [pattern bindings]
          (let [{:keys [position]} pattern]
            (reduce (fn [pattern pf]
                      (-> (pf pattern bindings)
                          (assoc :position position)))
                    pattern pfs)))))))

(defmethod compile-pattern :mix1
  [[_ & body]]
  (let [[bindings body] (collect-bindings body)]
    (if (seq bindings)
      (compile-pattern `[:bind ~bindings [:mix1 ~@body]])
      (let [[leader & rest] body
            leader-pf (compile-form leader)
            rest-pf (compile-pattern (cons :mix rest))]
        (pfn [pattern bindings]
          (-> pattern
              (rest-pf bindings)
              (leader-pf bindings)))))))

(declare play)

(defmethod compile-pattern :play
  [[_ & body]]
  (let [pf (compile-pattern (cons :seq body))]
    (pfn [{:keys [position] :as pattern}
          {:keys [sequencer] :as bindings}]
      ;; @position-2: callback executed, future
      ;;   invokes (play sequencer ...) in a separate thread which
      ;;   applies pf to the initial pattern and bindings, then adds
      ;;   the resulting pattern P to the pattern queue
      ;; @position-1: P is merged into the timeline
      ;; @position-0: events in P are ready to be executed
      ;;
      ;; merge-pattern-queue ensures that the actually scheduled
      ;; position of any event is not in the past (this may result in
      ;; a one tick delay for events with a relative position of zero)
      (let [sched-pos (- position 2)
            callback #(future (play sequencer pf bindings))]
        (update pattern :events conj [sched-pos callback])))))

(declare bpm!)

(defmethod compile-pattern :bpm
  [[_ new-bpm]]
  (pfn [pattern {:keys [sequencer] :as bindings}]
    (add-callback pattern #(bpm! sequencer new-bpm))))

;; bind

(defn resolve-binding
  [k v]
  (or (and *compile-target* (Target/resolve-binding *compile-target* k v))
      v))

(defn bind-expr?
  [x]
  (and (vector? x)
       (keyword? (first x))))

(defmulti compile-bind-expr
  (fn [k expr]
    (first expr)))

(defn compile-binding-spec
  "Takes a key and a spec, returns a function which when given a map of
  bindings, returns a value calculated from the bindings as described
  by spec."
  [k spec]
  (if (bind-expr? spec)
    (compile-bind-expr k spec)
    (constantly (resolve-binding k spec))))

(defmethod compile-bind-expr :default
  [k expr]
  (if *compile-target*
    (Target/compile-bind-expr *compile-target* k expr)
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
       (let [~'x (compile-binding-spec ~'k ~'x)
             ~'y (compile-binding-spec ~'k ~'y)]
         (fn [~'bindings] (~op (~'x ~'bindings) (~'y ~'bindings))))
       (compile-bind-expr ~'k [~name [:binding-of ~'k] ~'x]))))

(compile-binop-bind-expr :add +)
(compile-binop-bind-expr :sub -)
(compile-binop-bind-expr :mul *)
(compile-binop-bind-expr :div /)

(defn binding-specs->update-fn
  "Takes a map of binding specifications and creates a function which
  when given a map of bindings, updates them according to the specs."
  [binding-specs]
  (reduce-kv
   (fn [f k v]
     (let [calculate-bind-value (compile-binding-spec k v)]
       (comp #(assoc % k (calculate-bind-value %)) f)))
   identity binding-specs))

(defn get-default-bindings
  []
  (or (and *compile-target* (Target/get-default-bindings *compile-target*))
      {}))

(defmethod compile-pattern :bind
  [[_ binding-specs & body]]
  (if (empty? binding-specs)
    (compile-pattern (cons :seq body))
    (binding [*compile-target* (or (:target binding-specs)
                                   *compile-target*)]
      (let [pf (compile-pattern (cons :seq body))
            default-bindings (get-default-bindings)
            update-bindings (binding-specs->update-fn binding-specs)]
        (pfn [pattern bindings]
          (pf pattern (-> default-bindings
                          (merge bindings)
                          update-bindings)))))))

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
   (let [init-pattern init-pattern
         init-bindings (merge {:step 1}
                              bindings
                              {:sequencer sequencer})
         pf (compile-form pf)
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
