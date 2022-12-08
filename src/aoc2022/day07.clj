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

(defn file-sizes [dir]
  (reduce + (filter number? (vals dir))))

(defn subdir-name [cur sub]
  (if (= cur "/")
    (str cur sub)
    (str cur "/" sub)))

(defn dir-sizes [name dir level]
  (let [files (file-sizes dir)
        dir-keys (filter #(map? (get dir %)) (keys dir))
        dirs (mapcat #(dir-sizes (subdir-name name %) (get dir %) (inc level)) dir-keys)
        sum-of-subdirs (reduce + 0 (map second (filter #(= (inc level) (first %)) dirs)))]
    (conj dirs [level (+ files sum-of-subdirs) name])))

(def fs-size 70000000)
(def space-needed 30000000)

(defn run [args]
  (let [dir-tree (build-dir-tree input-lines)
        dirs-and-sizes (dir-sizes "/" dir-tree 0)
        sizes (map second dirs-and-sizes)
        candidates (filter #(<= % 100000) sizes)

        used-space (->> dirs-and-sizes
                        (filter #(= 0 (first %)))
                        first
                        second)
        free-space (- fs-size used-space)
        must-free (- space-needed free-space)
        size-of-dir-to-delete (->> sizes
                                   sort
                                   (filter #(>= % must-free))
                                   first)]
    (println "star 1:" (reduce + candidates))
    (println "star 2:" size-of-dir-to-delete)))
