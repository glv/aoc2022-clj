(ns aoc2022.day04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn input-lines []
  (line-seq (java.io.BufferedReader. *in*)))

(defn parse-range [elf-assignment]
  (map #(Integer/parseInt %) (str/split elf-assignment #"-")))

(defn parse-ranges [pair-assignment]
  (map parse-range
       (str/split pair-assignment #",")))

(defn subsumes? [[e1s e1e] [e2s e2e]]
  (and (<= e1s e2s)
       (>= e1e e2e)))

(defn full-overlap? [[e1 e2]]
  (or (subsumes? e1 e2)
      (subsumes? e2 e1)))

(defn overlap? [[e1 e2]]
  (let [[e1s e1e] e1
        [e2s e2e] e2]
    (or (and (<= e1s e2s) (>= e1e e2s))
        (and (<= e1s e2e) (>= e1e e2e))
        (full-overlap? [e1 e2]))))

(defn run [args]
  (let [pairs (->> (input-lines)
                   (map parse-ranges))
        redundant-pairs (filter full-overlap? pairs)
        overlapping-pairs (filter overlap? pairs)]
    (println "star 1:" (count redundant-pairs))
    (println "star 2:" (count overlapping-pairs))))
