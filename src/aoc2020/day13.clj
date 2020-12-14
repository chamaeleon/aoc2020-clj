(ns aoc2020.day13
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))
        estimate (Long/parseLong (first data))
        buses (map #(if (not= "x" %) (Long/parseLong %) %) (str/split (second data) #","))]
    [estimate buses]))

(defn next-arrival-time [estimate bus]
  (let [n (inc (quot estimate bus))]
    (- (* n bus) estimate)))

(defn part1 []
  (let [[estimate buses] (load-data "day13.txt")
        known-buses (filter number? buses)
        next-arrival-times (->> known-buses
                            (map-indexed (fn [idx bus] [(next-arrival-time estimate bus) idx]))
                            (sort-by first))
        best (first next-arrival-times)]
    (* (first best) (nth known-buses (second best)))))

(defn update-for-bus [t buses-product bus offset]
  (if (= 0 (rem (+ t offset) bus)) t
      (recur (+ t buses-product) buses-product bus offset)))

(defn find-t [buses]
  (loop [[[bus offset] & tail] buses
         buses-product 1
         t (ffirst buses)]
    (if (nil? bus) t
        (recur tail (* buses-product bus) (update-for-bus t buses-product bus offset)))))

(defn part2 []
  (let [[_ buses] (load-data "day13.txt")
        buses (filter #(number? (first %)) (map-indexed (fn [idx bus] [bus idx]) buses))]
    (find-t buses)))