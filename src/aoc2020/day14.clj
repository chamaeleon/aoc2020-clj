(ns aoc2020.day14
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:require [clojure.math.combinatorics :as c])
  (:gen-class))

(def mask-re #"^mask = ([X01]*)$")
(def mem-re #"^mem\[([0-9]+)\] = ([0-9]+)$")

(defn generate-floating-masks [mask]
  (let [floating-count (count (filter #(= \X %) mask))
        floating-bits (c/selections [0 1] floating-count)
        floating-pos (->> (map-indexed (fn [idx bit] [idx bit]) (reverse mask))
                          (filter #(= \X (second %)))
                          (map first))]
    (map #(mapv (fn [a b] [a b]) floating-pos %) floating-bits)))

(defn parse-instruction [instruction]
  (if-let [mask (second (re-find mask-re instruction))]
    [:mask {:and-mask (Long/parseLong (str/replace mask #"X" "1") 2)
            :or-mask (Long/parseLong (str/replace mask #"X" "0") 2)
            :floating (generate-floating-masks mask)}]
    (if-let [mem (rest (re-find mem-re instruction))]
      [:mem {:address (Long/parseLong (first mem))
             :value (Long/parseLong (second mem))}]
      (throw (Exception. (format "Invalid instruction [%s]" instruction))))))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (doall (map parse-instruction data))))

(defrecord State [and-mask or-mask mem-1 mem-2])

(defn make-state []
  (->State (dec (bit-shift-left 1 36)) 0 {} {}))

(defn generate-addresses [floating-bits-seq address]
  (->> (for [floating-bits floating-bits-seq]
         (reduce (fn [addr [bit-pos bit-value]]
                   (if (= 1 bit-value)
                     (bit-set addr bit-pos)
                     (bit-clear addr bit-pos)))
                 address
                 floating-bits))
       (flatten)
       (into #{})))

(defn instruction-dispatch [_ [instruction _]] instruction)

(defmulti execute #'instruction-dispatch)

(defmethod execute :mask [state [_ masks]]
  (-> state
      (assoc :and-mask (:and-mask masks))
      (assoc :or-mask (:or-mask masks))
      (assoc :floating (:floating masks))))

(defmethod execute :mem [state [_ mem]]
  (let [address-1 (:address mem)
        value-1 (-> (:value mem)
                    (bit-and (:and-mask state))
                    (bit-or (:or-mask state)))
        address-2 (bit-or address-1 (:and-mask state))
        value-2 (:value mem)
        addresses (generate-addresses (:floating state) address-2)]
    (as-> state state
      (assoc-in state [:mem-1 address-1] value-1)
      (reduce (fn [state address]
                (assoc-in state [:mem-2 address] value-2))
              state
              addresses))))

(defn part1 []
  (let [instructions (load-data "day14.txt")]
    (reduce + (vals (:mem-1 (reduce execute (make-state) instructions))))))

(defn part2 []
  (let [instructions (load-data "day14.txt")]
    (reduce + (vals (:mem-2 (reduce execute (make-state) instructions))))))