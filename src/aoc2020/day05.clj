(ns aoc2020.day05
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn load-data [filename]
  (line-seq (reader (resource filename))))

(defn calculate-seat [seat]
  (let [seat (str/replace seat #"[FL]" "0")
        seat (str/replace seat #"[BR]" "1")]
    (Integer/parseInt seat 2)))

(defn part1 []
  (let [data (load-data "day05.txt")]
    (apply max (map calculate-seat data))))

(defn part2 []
  (let [data (load-data "day05.txt")
        seats (sort (map calculate-seat data))]
    (->> (partition 2 1 seats)
         (filter (fn [[seat1 seat2]] (> (- seat2 seat1) 1)))
         (first)
         (first)
         (inc))))