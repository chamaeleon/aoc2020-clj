(ns aoc2020.day03
  (:require [clojure.java.io :refer [reader resource]])
  (:gen-class))

(defn load-data [filename]
  (let [lines (line-seq (reader (resource filename)))]
    (mapv #(mapv identity %) lines)))

(defn count-trees [data width [col-mult row-mult]]
  (->> (for [n (range (count data))
             :let [row (* row-mult n)
                   col (* col-mult n)]
             :while (< row (count data))]
         (get-in data [row (rem col width)]))
       (filter #(= % \#))
       (count)))

(defn part1 []
  (let [data (load-data "day03.txt")
        width (count (nth data 0))]
    (count-trees data width [3 1])))

(defn part2 []
  (let [data (load-data "day03.txt")
        width (count (nth data 0))
        paths [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (apply * (map #(count-trees data width %) paths))))