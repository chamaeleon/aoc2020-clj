(ns aoc2020.day12
  (:require [clojure.java.io :refer [reader resource]])
  (:gen-class))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    (->> (map #(rest (re-find #"(\w)([0-9]+)" %)) data)
         (map (fn [[cmd n]] [(keyword cmd)
                             (mod (Long/parseLong n) 360)])))))

(defrecord State [pos dir])

(def multiplier {0 [1 0], 90 [0 1], 180 [-1 0], 270 [0 -1]})

(defn forward [[x y] dir n]
  (let [[xm ym] (get multiplier dir)]
    [(+ x (* xm n)) (+ y (* ym n))]))

(defmulti move (fn [[cmd _] _] cmd))

(defmethod move :L [[_ n] state]
  (update state :dir (fn [dir] (rem (+ dir n) 360))))

(defmethod move :R [[_ n] state]
  (update state :dir (fn [dir] (mod (- dir n) 360))))

(defmethod move :N [[_ n] state]
  (update state :pos (fn [[x y]] [x (+ y n)])))

(defmethod move :S [[_ n] state]
  (update state :pos (fn [[x y]] [x (- y n)])))

(defmethod move :E [[_ n] state]
  (update state :pos (fn [[x y]] [(+ x n) y])))

(defmethod move :W [[_ n] state]
  (update state :pos (fn [[x y]] [(- x n) y])))

(defmethod move :F [[_ n] state]
  (update state :pos (fn [pos] (forward pos (:dir state) n))))

(defn manhattan [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn part1 []
  (let [movement (load-data "day12.txt")]
    (->> (reduce (fn [state cmd] (move cmd state))
                 (->State [0 0] 0)
                 movement)
         :pos
         (manhattan))))

(defn rotate [[x y] n]
  (if (= n 0) [x y]
      (recur [(- y) x] (- n 90))))

(defmulti waypoint (fn [[cmd _] _] cmd))

(defmethod waypoint :L [[_ n] state]
  (update state :dir (fn [dir] (rotate dir n))))

(defmethod waypoint :R [[_ n] state]
  (update state :dir (fn [dir] (rotate dir (- 360 n)))))

(defmethod waypoint :N [[_ n] state]
  (update state :dir (fn [[x y]] [x (+ y n)])))

(defmethod waypoint :S [[_ n] state]
  (update state :dir (fn [[x y]] [x (- y n)])))

(defmethod waypoint :E [[_ n] state]
  (update state :dir (fn [[x y]] [(+ x n) y])))

(defmethod waypoint :W [[_ n] state]
  (update state :dir (fn [[x y]] [(- x n) y])))

(defmethod waypoint :F [[_ n] state]
  (let [[dx dy] (:dir state)]
    (update state :pos (fn [[x y]] [(+ x (* dx n))
                                    (+ y (* dy n))]))))

(defn part2 []
  (let [movement (load-data "day12.txt")]
    (->> (reduce (fn [state cmd] (waypoint cmd state))
                 (->State [0 0] [10 1])
                 movement)
         :pos
         (manhattan))))