(ns aoc2020.core
  (:require [clojure.java.io :refer [resource]])
  (:require [aoc2020 day01 day02 day03 day04 day05 day06 day07 day08])
  (:require [aoc2020 day09 day10 day11 day12 day13 day14 day15 day16])
  (:require [aoc2020 day17 day18])
  (:gen-class))

(def problems
  [aoc2020.day01/part1 aoc2020.day01/part2
   aoc2020.day02/part1 aoc2020.day02/part2
   aoc2020.day03/part1 aoc2020.day03/part2
   aoc2020.day04/part1 aoc2020.day04/part2
   aoc2020.day05/part1 aoc2020.day05/part2
   aoc2020.day06/part1 aoc2020.day06/part2
   aoc2020.day07/part1 aoc2020.day07/part2
   aoc2020.day08/part1 aoc2020.day08/part2
   aoc2020.day09/part1 aoc2020.day09/part2
   aoc2020.day10/part1 aoc2020.day10/part2
   aoc2020.day11/part1 aoc2020.day11/part2
   aoc2020.day12/part1 aoc2020.day12/part2
   aoc2020.day13/part1 aoc2020.day13/part2
   aoc2020.day14/part1 aoc2020.day14/part2
   aoc2020.day15/part1 aoc2020.day15/part2
   aoc2020.day16/part1 aoc2020.day16/part2
   aoc2020.day17/part1 aoc2020.day17/part2
   aoc2020.day18/part1 aoc2020.day18/part2])

(def counter (atom 0))

(defn prefix [n]
  (format "Day %2d%s"
          (inc (quot n 2))
          (if (= (rem n 2) 0) "a" "b")))

(defn run-aoc-fn [aoc-fn]
  (let [start-ms (inst-ms (java.util.Date.))
        result (.toString (aoc-fn))
        end-ms (inst-ms (java.util.Date.))]
    (format "%s %15s [%dms]" (prefix @counter) result (- end-ms start-ms))))

(defn verify-data-file-presence []
  (let [filenames (map #(format "day%02d.txt" %) (range 1 (inc (quot (count problems) 2))))]
    (every? identity (map resource filenames))))

(defn run []
  (doseq [problem problems]
    (println (run-aoc-fn problem))
    (swap! counter inc)))

(defn -main []
  (if (verify-data-file-presence)
    (run)
    (println "Ensure all data files are placed in the resources folder")))