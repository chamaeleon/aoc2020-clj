(defproject aoc2020 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2020 in Clojure"
  :url "https://github.com/chamaeleon/aoc2020-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [instaparse "1.4.10"]]
  :jvm-opts ["-Xmx12g"]
  :main ^:skip-aot aoc2020.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
