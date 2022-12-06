(ns aoc2022.day06
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn find-marker [line marker-len]
  (let [[firstn rest] (split-at (dec marker-len) line)]
    (loop [pos marker-len
           buf (reverse firstn)
           [char & rest] rest]
      (let [buf (conj buf char)
            uniques (count (set buf))]
        (if (= uniques marker-len)
          pos
          (recur (inc pos), (butlast buf), rest))))))

(defn find-packet-marker [line]
  (find-marker line 4))

(defn find-message-marker [line]
  (find-marker line 14))

;; The challenge is to process just a single line of input, but they
;; supplied 5 different sample inputs, so I'm going to process all lines in
;; the file and print the result of each as a sequence, to help with
;; debugging. When running the real challenge input, the sequence will only
;; have one result.
(defn ^:private collapse-result [result]
  (if (= 1 (count result))
    (first result)
    result))

(defn run [args]
  (let [packet-positions (map find-packet-marker input-lines)
        message-positions (map find-message-marker input-lines)
        packet-result (collapse-result packet-positions)
        message-result (collapse-result message-positions)]
    (println "star 1:" packet-result)
    (println "star 2:" message-result)))
