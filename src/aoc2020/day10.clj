(ns aoc2020.day10
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.edn :as edn])
  (:gen-class))

(defn load-data [filename]
  (let [data (map edn/read-string (line-seq (reader (resource filename))))]
    (vec (sort (conj data 0 (+ (apply max data) 3))))))

(defn count-diff [diff adapters]
  (count (filter #(= diff %) (map (fn [[a b]] (- b a)) (partition 2 1 adapters)))))

(defn part1 []
  (let [adapters (load-data "day10.txt")]
    (* (count-diff 1 adapters)
       (count-diff 3 adapters))))

(defn count-combinations [[adapter & tail] visited]
  (if-let [result (get @visited tail)]
    result
    (let [result
          (if (nil? tail)
            1
            (let [options (take-while #(<= (- % adapter) 3) tail)]
              (reduce + (for [option options
                              :let [tail (drop-while #(< % option) tail)]]
                          (count-combinations tail visited)))))]
      (swap! visited assoc tail result)
      result)))

(defn part2 []
  (let [adapters (load-data "day10.txt")
        visited (atom {})]
    (count-combinations adapters visited)))