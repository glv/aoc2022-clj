(ns aoc2022.day07
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn transduce-dir-tree [lines]
  (letfn [(ident-reducer
            ([a] a)
            ([a b] a))
          (process-line []
            (fn [xf]
              (let [dir-stack (atom [])]
                (letfn [(cdroot [root _cmd]
                          (reset! dir-stack [])
                          root)
                        (cdup [root _cmd]
                          (swap! dir-stack (comp vec butlast))
                          root)
                        (cd [root cmd]
                          (let [dir-name (second (re-matches #"^\$ cd (.+)$" cmd))]
                            (swap! dir-stack #(conj % dir-name))
                            root))
                        (ls [root cmd] root)
                        (dir [root cmd]
                          (let [dir-name (second (re-matches #"^dir (.+) *$" cmd))]
                            (assoc-in root (conj @dir-stack dir-name) {})))
                        (file [root cmd]
                          (let [[size-str file-name] (rest (re-matches #"^(\d+) (.+) *$" cmd))
                                size (Integer/parseInt size-str)]
                            (assoc-in root (conj @dir-stack file-name) size)))]
                  (fn
                    ([] (xf))
                    ([root] (xf root))
                    ([root cmd]
                     (let [new-root (condp #(str/starts-with? %2 %1) cmd
                                      "$ cd /" :>> (partial cdroot root)
                                      "$ cd .." :>> (partial cdup root)
                                      "$ cd " (cd root cmd)
                                      "$ ls" :>> (partial ls root)
                                      "dir"  (dir root cmd)
                                      (file root cmd)
                                      )]
                       (xf new-root @dir-stack))))))))]
    (transduce (process-line) ident-reducer {} lines)))

(defn reduce-dir-tree [lines]
  (letfn [(process-line [[root dir-stack :as state] cmd]
            (condp #(str/starts-with? %2 %1) cmd

              "$ cd /" [root []]

              "$ cd .." [root (vec (butlast dir-stack))]

              "$ cd " (let [dir-name (nth (re-matches #"^\$ cd (.+)$" cmd) 1)
                            new-dir-stack (conj dir-stack dir-name)]
                        [root new-dir-stack])

              "$ ls" [root dir-stack]

              "dir " (let [dir-name (nth (re-matches #"^dir (.+) *$" cmd) 1)
                           new-root (assoc-in root (conj dir-stack dir-name) {})]
                       [new-root dir-stack])

              (let [[size-str file-name] (rest (re-matches #"^(\d+) (.+) *$" cmd))
                    size (Integer/parseInt size-str)
                    new-root (assoc-in root (conj dir-stack file-name) size)]
                [new-root dir-stack])
              ))]
    (first (reduce process-line [{} []] lines))))

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
                      new-dir-stack (conj dir-stack dir-name)]
                  (recur root new-dir-stack lines))

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
  (let [;dir-tree (build-dir-tree input-lines)
        dir-tree (reduce-dir-tree input-lines)
        ;dir-tree (transduce-dir-tree input-lines)
        _ (println "dir-tree:" dir-tree)
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
