(ns aoc2022.day01
  (:require [clojure.string :as str])
  )

(defn input-lines []
  (line-seq (java.io.BufferedReader. *in*)))

(defn non-delim-group? [l]
  (or (> (count l) 1)
      (not= "" (first l))))

(defn strs-to-ints [l]
  (map #(Integer/parseInt %) l))

(defn run [args]
  (let [elf-groups (->> (input-lines)
                        (partition-by str/blank?)
                        (filter non-delim-group?)
                        (map strs-to-ints))
        elf-sums (map #(reduce + %) elf-groups)]
    (println (apply max elf-sums)) ; first star
    (println (apply + (take 3 (reverse (sort elf-sums))))) ; second star
    ))
