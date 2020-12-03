(ns aoc2020.day02
  (:require [clojure.edn :as edn])
  (:require [clojure.java.io :refer [reader resource]])
  (:gen-class))

(def policy-re #"^([0-9]+)-([0-9]+) ([a-z]): ([a-z]*)$")

(defn parse-policy [policy]
  (when-let [[n1 n2 letter password] (rest (re-find policy-re policy))]
    [(edn/read-string n1) (edn/read-string n2) (first letter) password]))

(defn load-data [filename]
  (mapv parse-policy (line-seq (reader (resource filename)))))

(defn validate-part1 [[min-count max-count letter password]]
  (let [freq (frequencies password)
        count (get freq letter 0)]
    (and (<= min-count count)
         (<= count max-count))))

(defn part1 []
  (let [passwords (load-data "day02.txt")]
    (count (filter validate-part1 passwords))))

(defn validate-part2 [[pos1 pos2 letter password]]
  (let [ch1 (nth password (dec pos1))
        ch2 (nth password (dec pos2))]
    (not= (= letter ch1) (= letter ch2))))

(defn part2 []
  (let [passwords (load-data "day02.txt")]
    (count (filter validate-part2 passwords))))