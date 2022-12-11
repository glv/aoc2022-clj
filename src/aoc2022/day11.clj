(ns aoc2022.day11
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-lines
  (line-seq (java.io.BufferedReader. *in*)))

(defn non-delim-group? [l]
  (or (> (count l) 1)
      (not= "" (first l))))

(def monkeypat
  #"(?x)
    Monkey\ (?<num>\d+):.*items:\ (?<items>.+?)%
    \ \ Operation:\ new\ =\ old\ (?<op>.)\ (?<operand>.*?)%
    \ \ Test:\ divisible\ by\ (?<divisor>\d*)%
    \ \ \ \ If\ true:\ throw\ to\ monkey\ (?<truemonkey>\d+)%
    \ \ \ \ If\ false:\ throw\ to\ monkey\ (?<falsemonkey>\d+).*$"
  )

(defn monkeyfn [parts]
  (let [thismonkey (:num parts)
        opfn (condp = (:op parts) "+" + "*" *)
        operand (if (= "old" (:operand parts))
                  :old
                  (Integer/parseInt (:operand parts)))
        divisor (:divisor parts)
        truemonkey (:truemonkey parts)
        falsemonkey (:falsemonkey parts)]
    (fn [state monkey cycle-length]
      (loop [state state [worry & items] (:items monkey)]
        (if worry
          (let [worry (if (= operand :old)
                        (opfn worry worry)
                        (opfn worry operand))
                worry (if cycle-length
                        (mod worry cycle-length)
                        (int (/ worry 3)))
                tomonkey (if (= 0 (mod worry divisor))
                           truemonkey
                           falsemonkey)]
            (recur (-> state
                       (update-in [tomonkey :items] #(conj % worry))
                       (update-in [thismonkey :num-inspected] inc))
                   items))

          (assoc-in state [thismonkey :items] []))))))

(defn define-monkey [lines]
  (let [matcher (re-matcher monkeypat (str/join "%" lines))
        parts (when (.matches matcher)
                {:num (Integer/parseInt (.group matcher "num"))
                 :items (map #(Integer/parseInt %)
                             (str/split (.group matcher "items") #", "))
                 :op (.group matcher "op")
                 :operand (.group matcher "operand")
                 :divisor (Integer/parseInt (.group matcher "divisor"))
                 :truemonkey (Integer/parseInt (.group matcher "truemonkey"))
                 :falsemonkey (Integer/parseInt (.group matcher "falsemonkey"))})
        items (vec (:items parts))]
    {:items items :divisor (:divisor parts) :num-inspected 0 :fn (monkeyfn parts)}))

(defn do-monkey [cycle-length state index]
  (let [monkey (get state index)]
    ((:fn monkey) state monkey cycle-length)))

(defn do-round [monkeys cycle-length]
  (reduce (partial do-monkey cycle-length)
          monkeys
          (range (count monkeys))))

(defn print-monkeys [monkeys]
  (doseq [[monkey i] (map vector monkeys (range))]
    (println (str "monkey " (inc i) ":") (select-keys monkey [:items :num-inspected]))))

(defn monkey-business [monkeys cycle-length num-rounds]
  (apply * (->> monkeys
                (iterate #(do-round % cycle-length))
                rest
                (take num-rounds)
                last
                (map :num-inspected)
                sort
                reverse
                (take 2))))

(defn run [args]
  (let [monkeys (->> input-lines
                           (partition-by str/blank?)
                           (filter non-delim-group?)
                           (map define-monkey)
                           vec)
        cycle-length (apply * (map :divisor monkeys))]
    (println "star 1:" (monkey-business monkeys nil 20))
    (println "star 2:" (monkey-business monkeys cycle-length 10000))))
