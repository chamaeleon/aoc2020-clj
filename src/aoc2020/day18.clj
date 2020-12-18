(ns aoc2020.day18
  (:require [clojure.java.io :refer [reader resource]])
  (:require [instaparse.core :as insta])
  (:gen-class))

(defn load-data [filename]
  (let [data (line-seq (reader (resource filename)))]
    data))

(def parser-part-1
  (insta/parser
   "expr      = add-mul
    <add-mul> = term | add | mul
    add       = add-mul <'+'> term
    mul       = add-mul <'*'> term
    <term>    = num | <'('> add-mul <')'>
    num       = #'[0-9]+'"
   :auto-whitespace :standard))

(def parser-part-2
  (insta/parser
   "expr       = mul-expr
    <mul-expr> = add-expr | mul
    mul        = mul-expr <'*'> add-expr
    <add-expr> = term | add
    add        = add-expr <'+'> term
    <term>     = num | <'('> mul-expr <')'>
    num        = #'[0-9]+'
    <ws>       = #'\\s*'"
   :auto-whitespace :standard))

(defn evaluate-expr [[tag & args]]
  (cond (= tag :expr) (evaluate-expr (first args))
        (= tag :mul) (* (evaluate-expr (first args))
                        (evaluate-expr (second args)))
        (= tag :add) (+ (evaluate-expr (first args))
                        (evaluate-expr (second args)))
        (= tag :num) (Long/parseLong (first args))))

(defn part1 []
  (let [expressions (load-data "day18.txt")]
    (->> (map parser-part-1 expressions)
         (map evaluate-expr)
         (apply +))))

(defn part2 []
  (let [expressions (load-data "day18.txt")]
    (->> (map parser-part-2 expressions)
         (map evaluate-expr)
         (apply +))))