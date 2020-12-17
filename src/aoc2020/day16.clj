(ns aoc2020.day16
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:gen-class))

(def field-re #"([^:]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)")

(defn parse-field [field]
  (let [[_ name range1 range2 range3 range4] (re-find field-re field)]
    [name
     [(Long/parseLong range1) (Long/parseLong range2)]
     [(Long/parseLong range3) (Long/parseLong range4)]]))

(defn parse-ticket [ticket]
  (map #(Long/parseLong %) (str/split ticket #",")))

(defn load-data [filename]
  (let [[fields my-ticket nearby-tickets]
        (str/split (slurp (reader (resource filename))) #"\r?\n\r?\n")]
    [(map parse-field (str/split fields #"\r?\n"))
     (parse-ticket (second (str/split my-ticket #"\r?\n")))
     (map parse-ticket (rest (str/split nearby-tickets #"\r?\n")))]))

(defn validate-against-field [[_ [range1 range2] [range3 range4]] n]
  (or (<= range1 n range2)
      (<= range3 n range4)))

(defn validate-against-fields [fields n]
  (some #(validate-against-field % n) fields))

(defn filter-invalid-numbers [fields ticket]
  (filter #(not (validate-against-fields fields %)) ticket))

(defn part1 []
  (let [[fields _ tickets] (load-data "day16.txt")]
    (->> (map #(filter-invalid-numbers fields %) tickets)
         (flatten)
         (apply +))))

(defn validate-all-ticket-fields [fields ticket]
  (every? #(validate-against-fields fields %) ticket))

(defn field-set [[_ [range1 range2] [range3 range4]]]
  (-> (sorted-set)
      (into (range range1 (inc range2)))
      (into (range range3 (inc range4)))))

(defn make-field-ranges [fields]
  (reduce (fn [m [name _ :as field]] (assoc m name (field-set field))) {} fields))

(defn update-ticket-sets [m ticket]
  (reduce (fn [m [i n]]
            (update m i #(if (nil? %) #{n} (conj % n))))
          m
          (map-indexed (fn [idx n] [idx n]) ticket)))

(defn valid-ticket-fields [tickets field]
  (->> (map first (filter #(= (second %)
                              (set/intersection (second %) field))
                          tickets))
       (into #{})))

(defn map-valid-ticket-fields [tickets fields]
  (->> (map (fn [[name field]] [name (valid-ticket-fields tickets field)])
            fields)
       (sort-by #(count (second %)))))

(defn remove-claimed-columns [[_ claimed] fields]
  (map (fn [[name columns]]
         [name (set/difference columns claimed)])
       fields))

(defn map-field-name-to-column [mapping [field & tail]]
  (if (nil? field) mapping
      (recur (conj mapping field) (remove-claimed-columns field tail))))

(defn part2 []
  (let [[fields my-ticket tickets] (load-data "day16.txt")
        field-ranges (make-field-ranges fields)
        valid-tickets (filter #(validate-all-ticket-fields fields %) tickets)
        ticket-sets (reduce update-ticket-sets {} valid-tickets)
        ]
    (->> (map-valid-ticket-fields ticket-sets field-ranges)
         (map-field-name-to-column [])
         (filter #(re-find #"^departure" (first %)))
         (map #(some identity (second %)))
         (map #(nth my-ticket %))
         (apply *))))