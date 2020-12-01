(ns aoc2020.core
  (:require [aoc2020 day01])
  (:gen-class))

(def problems
  [aoc2020.day01/part1 aoc2020.day01/part2])

(def counter (atom 0))

(defn prefix [n]
  (format "Day %2d%s"
          (inc (quot n 2))
          (if (= (rem n 2) 0) "a" "b")))

(defn run-aoc-fn [aoc-fn]
  (let [start-ms (inst-ms (java.util.Date.))
        result (.toString (aoc-fn))
        end-ms (inst-ms (java.util.Date.))]
    (format "%s %15s [%dms]" (prefix @counter) result (- end-ms start-ms)))
  )

(defn run []
  (doseq [problem problems]
    (println (run-aoc-fn problem))
    (swap! counter inc)))

(defn -main []
  (run))