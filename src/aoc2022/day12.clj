(ns aoc2022.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn run [args]
  (let [shortest-path (->> input-lines
                           build-grid
                           dijkstra)]
    (println "star 1:" (count shortest-path))
    (println "star 2:")))
