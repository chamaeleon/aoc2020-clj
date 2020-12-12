(ns aoc2020.utils
  (:gen-class))

(defn get-cell [grid x y]
  (get (get grid y) x))

(defn set-cell [grid x y v]
  (assoc grid y (assoc (get grid y) x v)))