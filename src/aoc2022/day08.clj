(ns aoc2022.day08
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn grid-line [line]
  (into [] (map #(- (int %) (int \0)) line)))

(defn build-grid [lines]
  (let [first-line (first lines)
        width (count first-line)]
    (into [] (map grid-line lines))))

(defn hvisible?
  "Checks whether the tree at (x,y) is visible from the left or right."
  [grid [x y :as coord]]
  (let [row (nth grid y)
        [l rest] (split-at x row)
        [elem r] (split-at 1 rest)
        elem (first elem)]
    (when (or (every? #(< % elem) l)
              (every? #(< % elem) r))
      coord)))

(defn hvis [grid [x y :as coord]]
  (let [row (nth grid y)
        [l rest] (split-at x row)
        l (reverse l)
        [elem r] (split-at 1 rest)
        elem (first elem)
        lscore (min (count l) (inc (count (take-while #(< % elem) l))))
        rscore (min (count r) (inc (count (take-while #(< % elem) r))))]
    [lscore rscore]))

(defn visibility-score [grid pgrid coord]
  (reduce * (concat (hvis grid coord)
                    (hvis pgrid (reverse coord)))))

(defn run [args]
  (let [grid (build-grid input-lines)
        pgrid (apply mapv vector grid) ; pivot
        w (count (first grid))
        h (count grid)
        coords (for [x (range w) y (range h)] [x  y])
        hvisibles (->> coords
                       (map (partial hvisible? grid))
                       (filter identity)
                       (into #{}))
        visibles (->> coords
                      (map reverse)
                      (map (partial hvisible? pgrid))
                      (filter identity)
                      (map reverse)
                      (into hvisibles))
        scores (map (partial visibility-score grid pgrid) coords)]
    (println "star 1:" (count visibles))
    (println "star 2:" (first (reverse (sort scores))))))
