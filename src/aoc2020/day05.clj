(ns aoc2020.day05
  (:require [clojure.java.io :refer [reader resource]])
  (:gen-class))

(def conversion {\F 0, \B 1, \L 0, \R 1})

(defn load-data [filename]
  (line-seq (reader (resource filename))))

(defn calculate-seat [seat]
  (let [front-back (apply str (map conversion (filter #{\F \B} seat)))
        left-right (apply str (map conversion (filter #{\L \R} seat)))
        row (Integer/parseInt front-back 2)
        col (Integer/parseInt left-right 2)]
    (+ (* row 8) col)))

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