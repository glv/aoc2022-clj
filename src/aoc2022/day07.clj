(ns aoc2022.day07
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn make-cd-partitioner
  "Returns a new function for use with partition-by that starts a new
partition every time a `cd` command is seen."
  []
  (let [id (atom 0)]
    (fn [line]
      (when (str/starts-with? line "$ cd ")
        (swap! id inc))
      @id)))

(defn find-dir-groups [lines]
  (partition-by (make-cd-partitioner) lines))

(defn build-dir-tree [lines]
  (loop [root {}
         dir-stack [] ; for use with get-in, assoc-in, update-in
         [cmd & lines] lines]
    (println "root:" root)
    (println "dir-stack:" dir-stack)
    (println "cmd:" cmd)
    (println "lines:" lines)
    (if (nil? cmd)
      root
      (condp #(str/starts-with? %2 %1) cmd
        "$ cd /" (recur root [] lines) ; done
        "$ cd .." (recur root (vec (butlast dir-stack)) lines) ; done
        "$ cd " (let [dir-name (nth (re-matches #"^\$ cd (.+)$" cmd) 1)
                      new-dir-stack (conj dir-stack dir-name)
                      curr-dir (get-in root dir-stack)
                      new-root (if (contains? curr-dir dir-name)
                                 root
                                 (assoc-in root new-dir-stack {}))]
                  (recur new-root new-dir-stack lines)) ;done
        "$ ls" (recur root dir-stack lines) ; done
        "dir " (let [dir-name (nth (re-matches #"^dir (.+) *$" cmd) 1)
                     new-root (assoc-in root (conj dir-stack dir-name) {})]
                 (recur new-root dir-stack lines)) ; done
        (let [[size-str file-name] (rest (re-matches #"^(\d+) (.+) *$" cmd))
              size (Integer/parseInt size-str)
              new-root (assoc-in root (conj dir-stack file-name) size)]
          (recur new-root dir-stack lines))
        ))))

(defn run [args]
  (let [dir-groups (find-dir-groups input-lines)
        dir-tree (build-dir-tree input-lines)
        _ (println "dir groups:" dir-tree)]
    (println "star 1:")
    (println "star 2:")))
