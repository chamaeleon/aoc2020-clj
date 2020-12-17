(ns aoc2020.day15
  (:require [clojure.java.io :refer [resource]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn load-data [filename]
  (let [data (slurp (resource filename))]
    (map #(Long/parseLong %) (str/split data #","))))

(defn make-state [spoken turn last-number]
  {:spoken spoken
   :turn turn
   :last-number last-number})

(defn next-number [state]
  (if-let [[turn-recent turn-previous] (get (:spoken state) (:last-number state))]
    (if (nil? turn-previous)
      0
      (- turn-recent turn-previous))
    0))

(defn speak [state n]
  (-> state
      (update-in [:spoken n] (fn [turns] (take 2 (conj turns (inc (:turn state))))))
      (update :turn inc)
      (assoc :last-number n)))

(defn find-answer [n]
  (let [data (load-data "day15.txt")
        state (reduce (fn [state n] (speak state n))
                      (make-state (hash-map) 0 0) data)]
    (loop [state state]
      (if (= n (:turn state)) (:last-number state)
          (recur (speak state (next-number state)))))))

(defn part1 []
  (find-answer 2020))

(defn part2 []
  (find-answer 30000000))