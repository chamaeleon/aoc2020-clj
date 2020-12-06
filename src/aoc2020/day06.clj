(ns aoc2020.day06
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:gen-class))

(defn load-data-part1 [filename]
  (let [data (-> (slurp (reader (resource filename)))
                 (str/split #"\r?\n\r?\n"))]
    (map #(into #{} (str/replace % #"\s" "")) data)))

(defn part1 []
  (let [data (load-data-part1 "day06.txt")]
    (reduce + (map count data))))

(defn load-data-part2 [filename]
  (let [data (-> (slurp (reader (resource filename)))
                 (str/split #"\r?\n\r?\n"))
        data (map #(str/split % #"\r?\n") data)
        data (map (fn [groups] (map #(into #{} %) groups)) data)]
    data))

(defn part2 []
  (let [data (load-data-part2 "day06.txt")]
    (->> (map #(apply set/intersection %) data)
         (map count)
         (reduce +))))