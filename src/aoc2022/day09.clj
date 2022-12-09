(ns aoc2022.day09
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn move-head [[x y] dir]
  (condp = dir
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn shift-second
  "Shift one coordinate in the right direction"
  [sc dc]
  (cond
    (pos? dc) (inc sc)
    (neg? dc) (dec sc)
    :else sc))

(defn move-next [[fx fy] [sx sy :as second]]
  ;; h-t, going around the compass points starting with north.
  ;;
  ;; 1 away           2 away
  ;; H N:  [ 0  1]    [ 0  2]  N
  ;;                  [ 1  2]  NE
  ;; H NE: [ 1  1]    [ 2  2]  NE
  ;;                  [ 2  1]  NE
  ;; H E:  [ 1  0]    [ 2  0]  E
  ;;                  [ 2 -1]  SE
  ;; H SE: [ 1 -1]    [ 2 -2]  SE
  ;;                  [ 1 -2]  SE
  ;; H S:  [ 0 -1]    [ 0 -2]  S
  ;;                  [-1 -2]  SW
  ;; H SW: [-1 -1]    [-2 -2]  SW
  ;;                  [-2 -1]  SW
  ;; H W:  [-1  0]    [-2  0]  W
  ;;                  [-2  1]  NW
  ;; H NW: [-1  1]    [-2  2]  NW
  ;;                  [-1  2]  NW
  ;;
  ;; So: if any are 2, move.
  ;; If dx is positive, +1 to tx
  ;; If dx is negative, -1 to tx
  ;; If dy is positive, +1 to ty
  ;; If dy is negative, -1 to ty
  (let [[dx dy] [(- fx sx) (- fy sy)]]
    (if (and (<= (abs dx) 1) (<= (abs dy) 1))
      second
      [(shift-second sx dx) (shift-second sy dy)])))

(defn move-rest [head others]
  (loop [f head
         moved []
         [s & others] others]

    (if (nil? s)
      (apply list moved)
      (let [new-s (move-next f s)
            new-moved (conj moved new-s)]
        (recur new-s new-moved others)))))

;; This is where the big change will have to happen
(defn move-rope-single [{:keys [knots tail-positions]} dir]
  (let [[head & others] knots
        new-head (move-head head dir)
        new-others (move-rest new-head others)
        new-tail-positions (conj tail-positions (last new-others))]
    {:knots (conj new-others new-head) :tail-positions new-tail-positions}))

(defn move-rope [state move]
  (let [[dir count-str] (str/split move #" +")
        count (Integer/parseInt count-str)]
    (reduce (fn [state _] (move-rope-single state dir)) state (range count))))

(defn process-input [knots lines]
  (let [initial-state {:knots knots :tail-positions #{}}
        final-state (reduce move-rope initial-state lines)]
    (:tail-positions final-state)))

(defn run [args]
  (let [tail-positions-1 (process-input '([0 0] [0 0]) input-lines)
        tail-positions-2 (process-input (repeat 10 [0 0]) input-lines)]
    (println "star 1:" (count tail-positions-1))
    (println "star 2:" (count tail-positions-2))))
