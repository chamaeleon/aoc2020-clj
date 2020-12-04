(ns aoc2020.day04
  (:require [clojure.java.io :refer [resource reader]])
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn])
  (:gen-class))

(def all-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid})
(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn make-passport [entry]
  (let [fields (map #(rest (re-find #"^([^:]+):(.+)$" %)) entry)]
    (zipmap (map keyword (map first fields)) (map second fields))))

(defn load-data [filename]
  (let [content (slurp (reader (resource filename)))]
    (->> (str/split content #"\r?\n\r?\n")
         (map #(str/split % #"\r?\n| +"))
         (map make-passport))))

(defn valid-passport-part1 [passport]
  (= (disj (set (keys passport)) :cid) (disj all-keys :cid)))

(defn part1 []
  (let [data (load-data "day04.txt")]
    (count (filter valid-passport-part1 data))))

(defn valid-year [byr low high]
  (let [byr (edn/read-string byr)]
    (and (<= low byr high))))

(defn valid-height [hgt]
  (let [[height system] (rest (re-find #"^([0-9]+)(cm|in)$" hgt))
        height (edn/read-string height)]
    (or (and (= system "cm") (<= 150 height 193))
        (and (= system "in") (<= 59 height 76)))))

(defn valid-hair-color [hcl]
  (re-find #"^#[0-9a-f]{6}$" hcl))

(defn valid-eye-color [ecl]
  (contains? eye-colors ecl))

(defn valid-passport-id [pid]
  (re-find #"^[0-9]{9}$" pid))

(defn valid-passport-part2 [passport]
  (and (valid-passport-part1 passport)
       (valid-year (:byr passport) 1920 2002)
       (valid-year (:iyr passport) 2010 2020)
       (valid-year (:eyr passport) 2020 2030)
       (valid-height (:hgt passport))
       (valid-hair-color (:hcl passport))
       (valid-eye-color (:ecl passport))
       (valid-passport-id (:pid passport))))

(defn part2 []
  (let [data (load-data "day04.txt")]
    (count (filter valid-passport-part2 data))))