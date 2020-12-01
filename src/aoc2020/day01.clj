(ns aoc2020.day01
  (:require [clojure.edn :as edn])
  (:require [clojure.java.io :refer [reader resource]])
  (:gen-class))

(defn load-data [filename]
  (mapv edn/read-string (line-seq (reader (resource filename)))))

(defn search-part-1 [data n]
  (let [data-size (count data)]
    (first (for [i (range data-size)
                 j (range i data-size)
                 :let [a (nth data i)
                       b (nth data j)]
                 :when (= n (+ a b))]
             (* a b)))))

(defn part1 []
  (let [data (load-data "day01.txt")]
    (search-part-1 data 2020)))

(defn search-part-2 [data n]
  (let [data-size (count data)]
    (first (for [i (range data-size)
                 j (range (inc i) data-size)
                 k (range (inc j) data-size)
                 :let [a (nth data i)
                       b (nth data j)
                       c (nth data k)]
                 :when (= n (+ a b c))]
             (* a b c)))))

(defn part2 []
  (let [data (load-data "day01.txt")]
    (search-part-2 data 2020)))