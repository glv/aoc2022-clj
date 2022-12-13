(ns aoc2022.day00
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defmacro dbg [& args]
  (let [[lvl msg val] (condp = (count args)
                        1 [0 (first args) (first args)]
                        2 (if (number? (first args))
                            [(first args) (second args) (second args)]
                            [0 (first args) (second args)])
                        3 args)]
    `(println ~(str (str/join (repeat lvl " ")) msg ":") ~val)))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn run [args]
  (let [a input-lines]
    (println "star 1:")
    (println "star 2:")))
