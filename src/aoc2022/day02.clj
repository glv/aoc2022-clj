(ns aoc2022.day02
  (:require [clojure.string :as str]))

(defn input-lines []
  (line-seq (java.io.BufferedReader. *in*)))

(defonce score-map {"A" 1
                    "B" 2
                    "C" 3
                    "X" 1
                    "Y" 2
                    "Z" 3})

(defonce goal-map {"X" :l
                   "Y" :d
                   "Z" :w})

(defonce game-map {0 :d
                   1 :w
                   2 :l})

(defonce play-map {:d 0
                   :w 1
                   :l 2})

(defonce game-point-map {:l 0
                         :d 3
                         :w 6})

(defn round-score [[elf me]]
  (let [my-play (score-map me)
        elf-play (score-map elf)
        result (game-map (mod (- my-play elf-play) 3))
        game-score (game-point-map result)]
    (+ my-play game-score)))

(defn corrected-round-score [[elf goal]]
  (let [elf-play (score-map elf)
        result (goal-map goal)
        my-play (-> result
                    play-map
                    (+ (dec elf-play))
                    (mod 3)
                    (inc))
        game-score (game-point-map result)]
    (+ my-play game-score)))

(defn run [args]
  (let [strategies (map #(str/split % #" +") (input-lines))
        scores (map round-score strategies)
        corrected-scores (map corrected-round-score strategies)]
    (println "star 1:" (reduce + scores))
    (println "star 2:" (reduce + corrected-scores))))
