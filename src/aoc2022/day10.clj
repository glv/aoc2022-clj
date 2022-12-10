(ns aoc2022.day10
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn cycle-seq
  ([lines] (cycle-seq lines 1 1 false))
  ([[insn & rest :as insns] cycle x mid?]
   (lazy-seq
    (if insn
      (condp #(str/starts-with? %2 %1) insn
        "noop" (cons [cycle x]
                     (cycle-seq rest (inc cycle) x false))

        "addx" (if mid?
                 (let [v (Integer/parseInt
                          (second (re-matches #"^addx (.+) *$" insn)))]
                   (cons [cycle x]
                         (cycle-seq rest (inc cycle) (+ x v) false)))
                 (cons [cycle x]
                       (cycle-seq insns (inc cycle) x true)))
        )
      (cons [cycle x] nil)))))

(defn render-display-buf [cycles]
  (letfn [(sprite-visible? [pixel pos]
            (let [pixel-pos (mod pixel 40)]
              (and (>= pixel-pos (dec pos))
                   (<= pixel-pos (inc pos)))))]
    (loop [[[cycle-num pos :as cycle] & cycles] cycles buf []]
      (if cycle
        (recur cycles (conj buf (if (sprite-visible? (dec cycle-num) pos)
                                  \#
                                  \.)))

        (str/join buf)))))

(defn make-display-rows [display]
  (->> display
       (partition 40)
       (map str/join)
       (str/join "\n")))

(defn run [args]
  (let [cycles (cycle-seq input-lines)
        signal-strengths (->> cycles
                              (drop 19)
                              (partition-all 40)
                              (map first)
                              (map (partial apply *)))
        display (make-display-rows (render-display-buf cycles))]
    (println "star 1:" (reduce + signal-strengths))
    (println "star 2:")
    (println display)))
