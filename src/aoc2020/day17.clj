(ns aoc2020.day17
  (:require [clojure.java.io :refer [reader resource]])
  (:require [clojure.string :as str])
  (:gen-class))

(defn load-data [filename]
  (let [data (->> (line-seq (reader (resource filename)))
                  (mapv #(mapv identity (str/split % #""))))]
    (->> (for [y (range (count data))
               x (range (count (nth data y)))
               :when (= (get-in data [y x]) "#")]
           [x y 0 0])
         (into #{}))))

(defn count-neighbors [cells dimensions [x y z w :as cell]]
  [cell
   (contains? cells cell)
   (->> (for [dx (range -1 2)
              dy (range -1 2)
              dz (range -1 2)
              dw (if (= dimensions 3) (range 1) (range -1 2))
              :let [nx (+ x dx)
                    ny (+ y dy)
                    nz (+ z dz)
                    nw (+ w dw)]
              :when (and (not= 0 dx dy dz dw)
                         (get cells [nx ny nz nw]))]
          [nz ny nz nw])
        (count))])

(defn step-candidates [cells dimensions]
  (reduce (fn [candidates [x y z w]]
            (->> (for [dx (range -1 2)
                       dy (range -1 2)
                       dz (range -1 2)
                       dw (if (= 3 dimensions) (range 1) (range -1 2))
                       :let [nx (+ x dx)
                             ny (+ y dy)
                             nz (+ z dz)
                             nw (+ w dw)]]
                   [nx ny nz nw])
                 (into candidates)))
          #{}
          cells))

(defn process-cells [candidates]
  (reduce (fn [next-generation [coords active count]]
            (if (or (= count 3)
                    (and active (= count 2)))
              (conj next-generation coords)
              next-generation))
          #{}
          candidates))

(defn find-solution [dimensions]
  (let [cells (load-data "day17.txt")]
    (loop [cells cells
           generation 0]
      (if (= generation 6)
        (count cells)
        (let [candidates (map #(count-neighbors cells dimensions %)
                              (step-candidates cells dimensions))]
          (recur (process-cells candidates) (inc generation)))))))

(defn part1 []
  (find-solution 3))

(defn part2 []
  (find-solution 4))