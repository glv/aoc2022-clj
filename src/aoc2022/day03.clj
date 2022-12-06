(ns aoc2022.day03
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn priority-map [startc endc base]
  (->> (range (int startc) (inc (int endc)))
       (map #(vector (char %)
                     (+ (- % (int startc))
                        base)))
       (into {})))

(defonce priorities
  (merge (priority-map \a \z 1) (priority-map \A \Z 27)))

(defn rucksack-compartments [r]
  (let [compartment-size (count r)]
    (split-at (/ compartment-size 2) r)))

(defn common-item [c1 c2]
  (first (set/intersection (set c1) (set c2))))

(defn find-badge [[r1 r2 r3]]
  (first (set/intersection (set r1) (set r2) (set r3))))

(defn run [args]
  (let [rucksacks input-lines
        common-priorities (->> rucksacks
                               (map rucksack-compartments)
                               (map #(apply common-item %))
                               (map priorities))
        badge-priorities (->> (partition-all 3 rucksacks)
                              (map find-badge)
                              (map priorities))]
    (println "star 1:" (reduce + common-priorities))
    (println "star 2:" (reduce + badge-priorities))))
