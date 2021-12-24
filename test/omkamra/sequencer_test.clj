(ns omkamra.sequencer-test
  (:use [clojure.test])
  (:require [omkamra.sequencer :as s]))

(deftest beats->ms
  (is (= 1e3 (s/beats->ms 1 60))))

(deftest beats->ns
  (is (= 1e9 (s/beats->ns 1 60))))

(deftest beats->ticks
  (is (= (* 2 96) (s/beats->ticks 2 96))))

(deftest ticks->ms
  (is (= 250.0 (s/ticks->ms 48 96 120))))

(deftest ticks->ns
  (is (= 250.0e6 (s/ticks->ns 48 96 120))))

(deftest switch!
  (let [a (atom 5)
        b (s/switch! a 6)]
    (is (= 5 b))
    (is (= 6 @a))))

(deftest align-position
  (is (= 8 (s/align-position 5 8)))
  (is (= 16 (s/align-position 10 8)))
  (is (= 11 (s/align-position 11 0)))
  (is (= 11 (s/align-position 11 1)))
  (is (= 5 (s/align-position 5 5))))

(deftest conjv
  (is (= [1 2 3] (s/conjv [1 2] 3)))
  (is (= [1] (s/conjv nil 1))))

(deftest merge-pattern-queue
  (let [p {:events [[0 :a] [1 :b] [2 :c]]
           :snap 0
           :delay 0}]
    (is (= {1 [:a :b] 2 [:c]}
           (s/merge-pattern-queue {} 0 [p]))
        "events scheduled to start-pos are delayed by one tick"))
  (let [p {:events [[1 :a] [2 :b] [3 :c]]
           :snap 0
           :delay 0}]
    (is (= {1 [:a] 2 [:b] 3 [:c]}
           (s/merge-pattern-queue {} 0 [p]))
        "events scheduled after start-pos are not delayed"))
  (let [p {:events [[1 :a] [2 :b] [1 :d] [3 :c] [2 :f] [2 :g] [1 :h]]
           :snap 0
           :delay 0}]
    (is (= {1 [:a :d :h] 2 [:b :f :g] 3 [:c]}
           (s/merge-pattern-queue {} 0 [p]))
        "events scheduled to the same position are scheduled one after the other"))
  (let [p {:events [[1 :a] [2 :b] [1 :d] [3 :c] [2 :f] [2 :g] [1 :h]]
           :snap 16
           :delay 0}]
    (is (= {17 [:a :d :h] 18 [:b :f :g] 19 [:c]}
           (s/merge-pattern-queue {} 7 [p]))
        "snap"))
  (let [p {:events [[0 :a] [1 :b] [0 :d] [2 :c] [1 :f] [1 :g] [0 :h]]
           :snap 0
           :delay 3}]
    (is (= {3 [:a :d :h] 4 [:b :f :g] 5 [:c]}
           (s/merge-pattern-queue {} 0 [p]))
        "delay")))

(deftest dissoc-value
  (is (= {:a 3 :b 5 :d 16} (s/dissoc-value {:a 3 :b 5 :c 8 :d 16} 8))))

(deftest add-callback
  (is (= (merge s/seed-pattern {:offset 8 :events [[8 :cb]]})
         (s/add-callback (assoc s/seed-pattern :offset 8) :cb))))

(deftest add-callback-after
  (is (= (merge s/seed-pattern {:offset 8 :events [[20 :cb]]})
         (s/add-callback-after (assoc s/seed-pattern :offset 8) 12 :cb))))

(deftest pfn
  (is (s/pfn? (s/pfn [pattern bindings] pattern))))

(deftest pf-nop
  (let [pf (s/compile [:nop])
        pattern s/seed-pattern
        bindings {}]
    (is (= pattern (pf pattern bindings)))))

(deftest pf-clear
  (let [pf (s/compile [:clear])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 3
                       :offset 100)
        bindings {}]
    (is (= s/seed-pattern (pf pattern bindings)))))

(deftest pf-call
  (let [pf (s/compile [:call #(+ 8 13)])
        pattern (assoc s/seed-pattern :offset 11)
        bindings {}
        out (pf pattern bindings)]
    (is (vector? (:events out)))
    (is (= 1 (count (:events out))))
    (let [v (first (:events out))]
      (is (vector? v))
      (is (= 2 (count v)))
      (let [[offset callback] v]
        (is (= 11 offset))
        (is (= 21 (callback))))))
  (let [pf (s/compile [:call #(+ %1 %2) 8 13])
        pattern (assoc s/seed-pattern :offset 11)
        bindings {}
        out (pf pattern bindings)]
    (is (vector? (:events out)))
    (is (= 1 (count (:events out))))
    (let [v (first (:events out))]
      (is (vector? v))
      (is (= 2 (count v)))
      (let [[offset callback] v]
        (is (= 11 offset))
        (is (= 21 (callback)))))))

(deftest pf-snap
  (let [pf (s/compile [:snap 8])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 3
                       :offset 100)
        bindings {:sequencer {:tpb 96}}]
    (is (= (assoc pattern :snap (* 8 96)) (pf pattern bindings)))))

(deftest pf-delay
  (let [pf (s/compile [:delay 3])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 100)
        bindings {}]
    (is (= (assoc pattern :delay 3) (pf pattern bindings)))))

(deftest pf-wait
  (let [pf (s/compile [:wait 1])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 100)
        bindings {:sequencer {:tpb 96} :step 3}]
    (is (= (assoc pattern :offset (+ 100 (* 3 96))) (pf pattern bindings))))
  (let [pf (s/compile [:wait -4])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 100)
        bindings {:sequencer {:tpb 96} :step 3}]
    (is (= (assoc pattern :offset 1152) (pf pattern bindings))))
  (let [pf (s/compile [:wait -4])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 1200)
        bindings {:sequencer {:tpb 96} :step 3}]
    (is (= (assoc pattern :offset 2304) (pf pattern bindings)))))

(def v1
  (s/pfn [pattern bindings]
    (assoc pattern :snap (:foo bindings))))

(defn v2 [snap]
  (s/pfn [pattern bindings]
    (assoc pattern :snap snap)))

(def v3
  [:bind {:step [:binding-of :foo]}
   [:wait 1]])

(def v4
  [:bind {:foo 4}
   [:var #'v2 11]
   #'v3])

(deftest pf-var
  (is (= {:snap 5} (v1 {:snap 0} {:foo 5})))
  (let [pf (s/compile [:var #'v1])]
    (is (= {:snap 5} (pf {:snap 0} {:foo 5}))))
  (let [pf (s/compile [:var #'v2 7])]
    (is (= {:snap 7} (pf {:snap 0} {:foo 5}))))
  (let [pf (s/compile [:var #'v3])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 100)
        bindings {:sequencer {:tpb 96} :step 3 :foo 5}]
    (is (= (assoc pattern :offset (+ 100 (* 5 96)))
           (pf pattern bindings))))
  (let [pf (s/compile [:var #'v4])
        pattern (assoc s/seed-pattern
                       :events [[0 :a] [10 :b]]
                       :snap 16
                       :delay 1
                       :offset 100)
        bindings {:sequencer {:tpb 96} :step 3 :foo 5}]
    (is (= (assoc pattern
                  :offset (+ 100 (* 4 96))
                  :snap 11)
           (pf pattern bindings)))))
