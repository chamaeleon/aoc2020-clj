(ns aoc2020.day08
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.edn :as edn])
  (:gen-class))

(def instructions {"nop" :nop, "acc" :acc, "jmp" :jmp})

(defn parse-instruction [instruction]
  (let [[_ instruction data] (re-find #"^(\w+) (.+)" instruction)]
    [(get instructions instruction)
     (edn/read-string data)]))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (mapv parse-instruction data)))

(defmulti execute (fn [instruction _ _] (first instruction)))

(defmethod execute :nop [[_ _] offset accum]
  [(inc offset) accum])

(defmethod execute :acc [[_ arg] offset accum]
  [(inc offset) (+ accum arg)])

(defmethod execute :jmp [[_ arg] offset accum]
  [(+ offset arg) accum])

(defmethod execute :default [[_ _] offset]
  (throw (format "Unimplemented instruction at offset %d!" offset)))

(defn run-until-loop [visited instructions offset accum]
  (if (or (>= offset (count instructions))
          (contains? visited offset))
    [accum offset]
    (let [instruction (get instructions offset)
          [next-offset accum] (execute instruction offset accum)]
      (recur (conj visited offset) instructions next-offset accum))))

(defn part1 []
  (let [instructions (load-data "day08.txt")]
    (first (run-until-loop #{} instructions 0 0))))

(def fix {:nop :jmp, :jmp :nop})

(defn fix-instruction [[instruction arg]]
  [(get fix instruction instruction) arg])

(defn instruction-fix-seq [instructions]
  (for [offset (range (count instructions))
        :let [instruction (get instructions offset)]
        :when (get fix (first instruction))]
    (assoc instructions offset (fix-instruction instruction))))

(defn part2 []
  (let [instructions (load-data "day08.txt")
        candidates (instruction-fix-seq instructions)
        results (map #(run-until-loop #{} % 0 0) candidates)]
    (->> (filter (fn [[_ offset]] (>= offset (count instructions))) results)
         (first)
         (first))))