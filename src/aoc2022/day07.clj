(ns aoc2022.day07
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn build-dir-tree [lines]
  (loop [root {}
         dir-stack [] ; for use with get-in, assoc-in, update-in
         [cmd & lines] lines]
    (if (nil? cmd)
      root
      (condp #(str/starts-with? %2 %1) cmd
        "$ cd /" (recur root [] lines)
        "$ cd .." (recur root (vec (butlast dir-stack)) lines)
        "$ cd " (let [dir-name (nth (re-matches #"^\$ cd (.+)$" cmd) 1)
                      new-dir-stack (conj dir-stack dir-name)
                      curr-dir (get-in root dir-stack)
                      new-root (if (contains? curr-dir dir-name)
                                 root
                                 (assoc-in root new-dir-stack {}))]
                  (recur new-root new-dir-stack lines))
        "$ ls" (recur root dir-stack lines)
        "dir " (let [dir-name (nth (re-matches #"^dir (.+) *$" cmd) 1)
                     new-root (assoc-in root (conj dir-stack dir-name) {})]
                 (recur new-root dir-stack lines))
        (let [[size-str file-name] (rest (re-matches #"^(\d+) (.+) *$" cmd))
              size (Integer/parseInt size-str)
              new-root (assoc-in root (conj dir-stack file-name) size)]
          (recur new-root dir-stack lines))
        ))))

(defn run [args]
  (let [dir-tree (build-dir-tree input-lines)
        _ (println "dir-tree:" dir-tree)]
    (println "star 1:")
    (println "star 2:")))
