(ns aoc2020.day07
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.edn :as edn])
  (:gen-class))

(defn parse-line [line]
  (let [k (re-find #"^\w+ \w+" line)
        no-other (re-find #"no other bags" line)
        content (map #(rest %) (re-seq #"(\d+) (\w+ \w+)" line))]
    (if no-other
      [k nil]
      [k (reduce (fn [m [v k]] (assoc m k (edn/read-string v))) {} content)])))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (->> (map parse-line data)
         (reduce (fn [m [k v]] (assoc m k v)) {}))))

(defn count-combinations [count bag-map entry]
  (if (bag-map entry)
    (reduce + (for [bag (bag-map entry)]
                (if (= (first bag) "shiny gold")
                  (count-combinations (inc count) bag-map (first bag))
                  (count-combinations count bag-map (first bag)))))
    count))

(defn part1 []
  (let [data (load-data "day07.txt")]
    (->> (map (fn [bag] [bag (count-combinations 0 data bag)]) (keys data))
         (filter (fn [[_ n]] (> n 0)))
         (count))))

(defn count-bags [count bag-map bag]
  (if-let [contained-bags (get bag-map bag)]
    (inc (reduce + (for [[contained-bag n] contained-bags]
                     (* n (count-bags count bag-map contained-bag)))))
    1))

(defn part2 []
  (let [data (load-data "day07.txt")]
    (dec (count-bags 0 data "shiny gold"))))