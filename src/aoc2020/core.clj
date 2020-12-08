(ns aoc2020.core
  (:require [clojure.java.io :refer [resource]])
  (:require [aoc2020 day01 day02 day03 day04 day05 day06 day07 day08])
  (:gen-class))

(def problems
  [aoc2020.day01/part1 aoc2020.day01/part2
   aoc2020.day02/part1 aoc2020.day02/part2
   aoc2020.day03/part1 aoc2020.day03/part2
   aoc2020.day04/part1 aoc2020.day04/part2
   aoc2020.day05/part1 aoc2020.day05/part2
   aoc2020.day06/part1 aoc2020.day06/part2
   aoc2020.day07/part1 aoc2020.day07/part2
   aoc2020.day08/part1 aoc2020.day08/part2])

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