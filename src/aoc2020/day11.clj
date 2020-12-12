(ns aoc2020.day11
  (:require [clojure.java.io :refer [reader resource]])
  (:require [aoc2020.utils :refer [get-cell set-cell]])
  (:gen-class))

(def cell-type {\. :floor, \L :empty, \# :occupied})
(def print-lookup {:floor \., :empty \L, :occupied \#})

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (mapv (fn [line] (mapv cell-type (seq line))) data)))

(defn get-dimensions [grid]
  [(count (first grid)) (count grid)])

(defn count-occupied [grid]
  (count (filter #(= :occupied %) (flatten grid))))

(defn get-adjacent-occupied-count [grid x y dist]
  (->> (for [ys (range -1 2)
             xs (range -1 2)
             :when (and (or (not= xs 0) (not= ys 0)))]
         (->> (for [dist (range 1 (inc dist))
                    :let [y (+ y (* dist ys))
                          x (+ x (* dist xs))
                          v (get-cell grid x y)]
                    :while v]
                v)
              (filter #(not= :floor %))))
       (filter (fn [[pos & _]] (= :occupied pos)))
       (count)))

(defn generate-updates [grid threshold dist]
  (let [[xsize ysize] (get-dimensions grid)]
    (->> (for [y (range ysize)
               x (range xsize)]
           (let [current (get-cell grid x y)]
             (if (= current :floor)
               :floor
               (let [occupied-count (get-adjacent-occupied-count grid x y dist)]
                 (cond (and (= 0 occupied-count) (= :empty current)) :occupied
                       (and (>= occupied-count threshold) (= :occupied current)) :empty
                       :else current))))))))

(defn apply-updates [grid updates]
  (mapv vec (partition (count (first grid)) updates)))

(defn update-until-equal [grid threshold dist]
  (loop [grid grid]
    (let [new-grid (apply-updates grid (generate-updates grid threshold dist))]
      (if (= grid new-grid) new-grid
          (recur new-grid)))))

(defn part1 []
  (let [grid (load-data "day11.txt")]
    (count-occupied (update-until-equal grid 4 1))))

(defn part2 []
  (let [grid (load-data "day11.txt")
        dist (apply max (get-dimensions grid))]
    (count-occupied (update-until-equal grid 5 dist))))