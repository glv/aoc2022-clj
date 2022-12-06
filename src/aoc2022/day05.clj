(ns aoc2022.day05
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn input-lines []
  (line-seq (java.io.BufferedReader. *in*)))

(defn add-crate [stacks [stack crate]]
  (if (= crate \ )
    stacks
    (update-in stacks [(dec stack)] conj crate)))

(defn parse-level [move]
  (->> move
       (partition-all 4)
       (map (partial drop 1))
       (map first)))

(defn add-level [stacks level-line]
  (let [crates (parse-level level-line)
        indexed-crates (map #(vector %1 %2) (map inc (range)) crates)]
    (reduce add-crate stacks indexed-crates)))

(defn build-stacks [stack-lines]
  (let [levels (drop-last stack-lines)
        stacks-line (last stack-lines)
        num-stacks (count (filter (partial not= \ ) stacks-line))
        empty-stacks (apply vector (repeatedly num-stacks list))]
    (reduce add-level empty-stacks (reverse levels))))

(defn move-crate [stacks from to]
  (let [crate (first (nth stacks (dec from)))]
    (-> stacks
        (update-in [(dec to)] conj crate)
        (update-in [(dec from)] #(drop 1 %)))))

(defn play-move-1 [stacks [count from to]]
  (loop [i count stacks stacks]
    (if (= i 0)
      stacks
      (recur (dec i) (move-crate stacks from to)))))

(defn play-move-2 [stacks [count from to]]
  (let [crates (take count (nth stacks (dec from)))]
    (-> stacks
        (update-in [(dec to)] #(concat crates %))
        (update-in [(dec from)] #(drop count %)))))

(defn play-moves [stacks move-lines move-func]
  (reduce move-func stacks move-lines))

(defn parse-move [move-line]
  (map #(Integer/parseInt %)
       (drop 1 (re-matches #"^move (\d+) from (\d+) to (\d+)$" move-line))))

(defn run [args]
  (let [[start-lines rest] (split-with (comp not str/blank?) (input-lines))
        moves (map parse-move (drop 1 rest))
        stacks (build-stacks start-lines)
        updated-stacks-1 (play-moves stacks moves play-move-1)
        updated-stacks-2 (play-moves stacks moves play-move-2)]
    (println "star 1:" (str/join (map first updated-stacks-1)))
    (println "star 2:" (str/join (map first updated-stacks-2)))))
