(ns aoc2022.day13
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn non-delim-group? [l]
  (or (> (count l) 1)
      (not= "" (first l))))

(defn read-packet-pair [lines]
  (map read-string lines))

(defn mapall [f & colls]
  (let [step (fn step [cs]
               (lazy-seq
                (let [ss (map seq cs)]
                  (when (some identity ss)
                    (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step colls))))

(defn packet-compare [p1 p2]
  (cond
    (every? number? [p1 p2]) (compare p1 p2)
    (number? p1) (packet-compare [p1] p2)
    (number? p2) (packet-compare p1 [p2])
    :else (loop [[[v1 v2 :as vs] & rest] (mapall vector p1 p2)]
            (let [c (cond
                      (every? nil? vs) :equal

                      (nil? v1) -1

                      (nil? v2) 1

                      :else (packet-compare v1 v2)
                      )]
              (if (= c 0)
                (recur rest)
                (if (= c :equal)
                  0
                  c))))))

(defn proper-pair? [i [p1 p2]]
  (when (<= (packet-compare p1 p2) 0)
    (inc i))
  )

(defn run [args]
  (let [packet-pairs (->> input-lines
                          (partition-by str/blank?)
                          (filter non-delim-group?)
                          (map read-packet-pair))
        proper-pair-indexes (keep-indexed proper-pair? packet-pairs)
        dividers [[[2]] [[6]]]
        divider-indexes (->> packet-pairs
                             (apply concat dividers)
                             (sort packet-compare)
                             (keep-indexed (fn [i p] (when (some #(= p %) dividers)
                                                       (inc i)))))]
    (println "star 1:" (reduce + proper-pair-indexes))
    (println "star 2:" (reduce * divider-indexes))))
