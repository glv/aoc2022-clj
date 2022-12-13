(ns aoc2022.day12
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn cell-index [{size :size} [col row]]
  (let [[cols rows] size]
    (+ (* row cols) col)))

(defn grid-cell [grid [col row]]
  (get (:cells grid) (cell-index grid [col row])))

(defn neighbor [grid cell direction]
  (let [[col row] (:coord cell)
        [mcol mrow] (map dec (:size grid))]
    (case direction
      :north (when (> row 0) [col (dec row)])
      :west  (when (> col 0) [(dec col) row])
      :south (when (< row mrow) [col (inc row)])
      :east  (when (< col mcol) [(inc col) row]))))

(defn neighbors
  ([grid cell] (neighbors grid cell [:north :south :east :west]))
  ([grid cell directions]
   (filter identity (map #(neighbor grid cell %) directions))))

(defn build-cells [lines cols rows]
  (let [chars (str/join lines)]
    (loop [cells []
           index 0
           start nil
           end nil
           [char & chars] chars]
      (if char
        (let [x (mod index cols)
              y (int (/ index cols))
              start? (= char \S)
              end? (= char \E)
              height (cond
                       start? \a
                       end? \z
                       :else char)
              cell {:coord [x y]
                    :height height}
              start (cond start start start? [x y] :else nil)
              end (cond end end end? [x y] :else nil)]
          (recur (conj cells cell) (inc index) start end chars))

        [cells start end]))))

(defn build-grid [lines]
  (let [rows (count lines)
        cols (count (first lines))
        [cells start end] (build-cells lines cols rows)]
    {:size [cols rows] :cells cells :start start :end end}))

(defn process-frontier [grid frontier path-length]
  (letfn [(eligible-neighbors [grid cell]
            (let [min-height (dec (int (:height cell)))
                  neighbor-coords (neighbors grid cell)
                  neighbor-cells (map (partial grid-cell grid) neighbor-coords)]
              (filter #(>= (int (:height %)) min-height) neighbor-cells)))]
    (let [next-frontier (->> frontier
                             (mapcat (partial eligible-neighbors grid))
                             (into #{})
                             (filter #(nil? (:step-num %))))]
      next-frontier)))

(defn dijkstra [{:keys [cells end] :as grid} found-fn]
  (loop [grid (assoc-in grid [:cells (cell-index grid end) :step-num] 0)
         path-length 1
         [frontier & frontiers] [[(grid-cell grid end)]]]
    (if (some found-fn frontier)  ; #(= start (:coord %))
      (dec path-length)

      (let [next-frontier (process-frontier grid frontier path-length)]
        (recur (reduce #(assoc-in %1 [:cells (cell-index grid (:coord %2)) :step-num] path-length)
                       grid
                       next-frontier)
               (inc path-length)
               (conj frontiers next-frontier))))))

(defn run [args]
  (let [grid (build-grid input-lines)
        shortest-path-from-start (dijkstra grid #(= (:start grid) (:coord %)))
        shortest-path-from-any-a (dijkstra grid #(= \a (:height %)))]
    (println "star 1:" shortest-path-from-start)
    (println "star 2:" shortest-path-from-any-a))
  )
