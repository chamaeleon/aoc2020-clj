(ns aoc2020.day09
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.edn :as edn])
  (:require [clojure.math.combinatorics :as c])
  (:gen-class))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (mapv edn/read-string data)))

(defn make-groups [n data]
  (let [groups (partition (inc n) 1 data)]
    (mapv (fn [g] [(take n g) (nth g n)]) groups)))

(defn check-group [[candidates sum]]
  (let [pairs (c/combinations candidates 2)]
    (some (fn [[a b]] (= sum (+ a b))) pairs)))

(defn part1 []
  (let [data (load-data "day09.txt")
        groups (make-groups 25 data)]
    (->> (filter #(not (check-group %)) groups)
         (first)
         (second))))

(defn search-sum-seq [result data]
  (loop [numbers []
         sum 0
         [n & tail :as data] data]
    (cond (nil? data) numbers
          (= (+ sum n) result) (conj numbers n)
          (< (+ sum n) result) (recur (conj numbers n) (+ sum n) tail)
          :else (recur (subvec numbers 1)
                       (- sum (first numbers))
                       data))))

(defn part2 []
  (let [data (load-data "day09.txt")
        result (part1)
        sequence (search-sum-seq result data)]
    (+ (apply min sequence) (apply max sequence))))