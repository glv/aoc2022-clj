(ns aoc2022.day14
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defmacro dbg
  "Print to *out* the value of an expr, labeled with the expression itself.
  Can be called in four ways:
  * (dbg expr) -- prints 'expr: value'
  * (dbg 2 expr) -- prints '  expr: value' (the int is the level of indentation)
  * (dbg \"msg\" expr) -- prints 'msg: value' (overrides the default label)
  * (dbg 2 \"msg\" expr) -- prints '  msg: value' (both indent and label override)"
  [& args]
  (let [[lvl msg val] (condp = (count args)
                        1 [0 (first args) (first args)]
                        2 (if (number? (first args))
                            [(first args) (second args) (second args)]
                            [0 (first args) (second args)])
                        3 args)]
    `(println ~(str (str/join (repeat lvl " ")) msg ":") ~val)))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn cell-index [{:keys [xmin ymin cols cells]} [x y]]
  (let [x (- x xmin)
        y (- y ymin)]
    (+ (* y cols) x)))

(defn grid-cell [{cells :cells :as grid} [x y]]
  (get cells (cell-index grid [x y])))

(defn off-grid? [{:keys [xmin xmax ymax]} [x y]]
  (or (< x xmin)
      (> x xmax)
      (> y ymax)))

(defn print-grid [{:keys [xmin xmax cells] :as grid}]
  (let [row-len (inc (- xmax xmin))
        label-xs (into #{} (concat [xmin]
                                   (filter #(= 0 (mod % 10))
                                           (range (inc xmin) xmax))
                                   [xmax]))]
    (doseq [m [100 10 1]]
      (println (apply str "    " (for [x (range xmin (inc xmax))]
                                   (if (contains? label-xs x)
                                     (mod (int (/ x m)) 10)
                                     " ")
                                   )))
      )
    (doseq [[i row] (map vector (range) (->> cells
                                             (partition row-len)
                                             (map #(map :v %))
                                             (map str/join)))]
      (println (format "%3d" i) row))))

(defn coords-from-line [line]
  (let [point-strs (str/split line #" -> ")]
    (->> point-strs
         (map #(str/split % #","))
         (map #(map read-string %))
         (map vec))))

(defn build-grid [{:keys [xmin xmax ymin ymax] :as state}]
  (let [cols (inc (- xmax xmin))
        rows (inc (- ymax ymin))
        cells (vec (repeat (* cols rows) {:v \.}))]
    (assoc state
           :cols cols :rows rows :cells cells
           :sand-units 0 :abyss-reached false)))

(defn expand-dims [{:keys [xmin xmax ymin ymax] :as state} line]
  (letfn [(newdim [minormax pluck-fn dim coords]
            (apply minormax (->> coords
                                 (map pluck-fn)
                                 (cons dim)
                                 (filter identity))))]
    (let [coords (coords-from-line line)
          xmin (newdim min first xmin coords)
          xmax (newdim max first xmax coords)
          ymin (newdim min second ymin coords)
          ymax (newdim max second ymax coords)]
      {:xmin xmin :xmax xmax :ymin ymin :ymax ymax})))

(defn find-dims [lines]
  (let [state {:xmin nil :xmax nil :ymin 0 :ymax nil}]
    (reduce expand-dims state lines)))

(defn segments-from-line [line]
  (let [coords (coords-from-line line)]
    (map vector coords (rest coords))))

(defn parse-segments [lines]
    (mapcat segments-from-line lines))

(defn add-segment [grid segment]
  (let [[start end] (sort segment)
        advance-fn (if (= (first start) (first end))
                     #(vector (first %) (inc (second %)))
                     #(vector (inc (first %)) (second %)))]
    (loop [grid grid coord start]
      (let [new-grid (assoc-in grid [:cells (cell-index grid coord) :v] \#)]
        (if (= end coord)
          new-grid
          (recur new-grid (advance-fn coord)))))))

(defn add-paths [{cells :cells :as grid} lines]
  (let [grid (assoc-in grid
                       [:cells (cell-index grid [500 0]) :v]
                       \+)
        segments (parse-segments lines)
        ]
    (reduce add-segment grid segments)))

(defn trickle-sand-unit [grid]
  (letfn [(candidate-status [grid [x y :as coord]]
            (if (off-grid? grid coord)
              :abyss
              (condp = (:v (grid-cell grid coord))
                \. :free
                \+ :blocked
                \# :blocked
                \o :blocked)))
          (next-sand-pos [grid [x y :as sand-pos]]
            (let [candidates [[x (inc y)]
                              [(dec x) (inc y)]
                              [(inc x) (inc y)]]
                  statuses (map (partial candidate-status grid) candidates)]
              (or (->> statuses
                       (map vector candidates)
                       (filter #(not= :blocked (second %)))
                       first)
                  [nil :blocked])))]
    (loop [grid grid sand-pos [500 0]]
      (let [[next-pos status] (next-sand-pos grid sand-pos)]
        (condp = status
          :blocked (-> grid
                       (assoc-in [:cells (cell-index grid sand-pos) :v] \o)
                       (update :sand-units inc)
                       (assoc :full-up (= sand-pos [500 0])))
          :abyss (assoc grid :abyss-reached true)
          :free (recur grid next-pos))))))

(defn trickle-sand [grid]
  (->> grid
       (iterate trickle-sand-unit)
       (drop-while #(and (not (:abyss-reached %))
                         (not (:full-up %))))
       first))

(defn add-floor [{:keys [cells cols rows ymax] :as grid}]
  (let [new-cells (vec (concat cells
                               (repeat cols {:v \.})
                               (repeat cols {:v \#})))]
    (assoc grid
           :cells new-cells
           :rows (+ 2 rows)
           :ymax (+ 2 ymax)
           :floored true
           :full-up false)))

(defn run [args]
  (let [{:keys [xmin xmax ymin ymax] :as dims} (find-dims input-lines)
        grid1 (-> dims
                  build-grid
                  (add-paths input-lines)
                  trickle-sand)
        rows (- ymax ymin)
        new-xmin (- xmin (+ 5 rows))
        new-xmax (+ xmax (+ 5 rows))
        grid2 (-> dims
                  (assoc :xmin new-xmin :xmax new-xmax)
                  build-grid
                  (add-paths input-lines)
                  add-floor
                  trickle-sand)]
    (print-grid grid1)
    (println "star 1:" (:sand-units grid1))
    (println "star 2:" (:sand-units grid2))))
