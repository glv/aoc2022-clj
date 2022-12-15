(ns aoc2022.day15
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defmacro dbg
  "Print to *out* the value of an expr, labeled with the expression itself.
Can be called in four ways:
* (dbg expr) -- prints 'expr: value'
* (dbg 2 expr) -- prints '  expr: value' (the int is the level of indentation)
* (dbg \"msg\" expr) -- prints 'msg: value' (overrides the default label)
* (dbg 2 \"msg\" expr) -- prints '  msg: value' (both indent and label override)
Returns the value of expr."
  [& args]
  (let [[lvl msg val] (condp = (count args)
                        1 [0 (first args) (first args)]
                        2 (if (number? (first args))
                            [(first args) (second args) (second args)]
                            [0 (first args) (second args)])
                        3 args)]
    `(let [val# ~val]
       (println ~(str (str/join (repeat lvl " ")) msg ":") val#)
       val#)))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(def line-pattern
  #"^Sensor at x=(-?\d+), y=(-?\d+): .* at x=(-?\d+), y=(-?\d+) *$")

(defn build-sensor [line]
  (let [coord-strings (rest (re-matches line-pattern line))
        [sx sy bx by] (map #(Integer/parseInt %) coord-strings)
        distance (+ (abs (- sx bx)) (abs (- sy by)))]
    {:pos [sx sy]
     :beacon [bx by]
     :distance distance
     :vertical-range [(- sy distance) (+ sy distance)]}))

(defn build-sensor-map [lines]
  (map build-sensor lines))

(defn impossible-range [row-num {:keys [distance pos] :as sensor}]
  (let [[sx sy] pos
        v-distance (abs (- sy row-num))
        h-distance (- distance v-distance)]
    [(- sx h-distance) (+ sx h-distance)]))

(defn merge-ranges [ranges]
  (letfn [(overlap? [r1 r2]
            (let [[r1s r1e] r1
                  [r2s r2e] r2]
              (or (and (<= r1s r2s) (>= r1e r2s))
                  (and (<= r1s r2e) (>= r1e r2e))
                  (and (<= r1s r2s) (>= r1e r2e))
                  (and (<= r2s r1s) (>= r2e r1e)))))
          (adjacent? [[ls le] [rs re]]
            (= rs (inc le)))
          (merge-ranges [r1 r2]
            (let [[r1s r1e] r1
                  [r2s r2e] r2]
              [(min r1s r2s)
               (max r1e r2e)]))
          (merge-overlapping [disjoint-ranges range]
            (loop [result [] range range [dr & rest] disjoint-ranges]
              (if (nil? dr)
                (conj result range)

                (cond
                  (overlap? range dr) (recur result (merge-ranges dr range) rest)
                  (adjacent? range dr) (recur result [(first range) (second dr)] rest)
                  (adjacent? dr range) (recur result [(first dr) (second range)] rest)
                  :else (recur (conj result dr) range rest)))))]
    (reduce merge-overlapping [] ranges)))

(defn count-positions [disjoint-ranges]
  (let [reversed-ranges (map reverse disjoint-ranges)
        range-spans (map #(apply - %) reversed-ranges)]
    (reduce + range-spans)))

(defn impossible-positions [row-num sensors]
  (let [is-candidate? (fn [sensor]
                        (and (>= row-num (first (:vertical-range sensor)))
                             (<= row-num (second (:vertical-range sensor)))))]
  (->> sensors
       (filter is-candidate?)
       (map (partial impossible-range row-num))
       (merge-ranges))))

(defn invert-ranges [lower upper ranges]
  (let [range-pairs (map vector ranges (rest ranges))
        in-between (fn [[[s1 e1] [s2 e2]]]
                     [(inc e1) (dec s2)])
        initial-range [lower (dec (ffirst ranges))]
        trailing-range [(inc (last (last ranges))) upper]]
    (filter #(<= (first %) (second %))
            (concat [initial-range]
                    (map in-between range-pairs)
                    [trailing-range]))))

(defn possible-in-range [lower upper impossible-ranges]
  (let [adjusted-impossible-ranges
        (->> impossible-ranges
             sort
             (filter #(> (second %) lower))   ; not below bounds
             (filter #(< (first %) upper))    ; not above bounds
             (map #(if (< (first %) lower)    ; trim if overlapping lower
                     [lower (second %)]
                     %))
             (map #(if (> (second %) upper)   ; trim if overlapping upper
                     [(first %) upper]
                     %)))]

    (invert-ranges lower upper adjusted-impossible-ranges)))

(defn find-possible-positions [lower upper sensors]
  (let [possible-in-row (fn [row]
                          (->> sensors
                               (impossible-positions row)
                               (possible-in-range lower upper)
                               (mapcat #(range (first %) (inc (second %))))
                               (map #(vector % row))))]
    (mapcat possible-in-row (range lower (inc upper)))))

(defn run [args]
  (let [sensors (vec (build-sensor-map input-lines))
        row-num (if (< (count sensors) 15)
                  10
                  2000000)
        lower 0
        upper (if (< (count sensors) 15)
                20
                4000000)
        impossible-ranges (impossible-positions row-num sensors)
        possible-positions (find-possible-positions lower upper sensors)]
    (println "star 1:" (count-positions impossible-ranges))
    (if (> (count possible-positions) 1)
      (println "Error: found" (count possible-positions) "possible positions")
      (let [[x y] (first possible-positions)
            tuning-frequency (+ y (* x 4000000))]
        (println "star 2:" tuning-frequency)))))
