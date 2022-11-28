(defproject org.clojars.shchipts/dice-simulator "1.0.0"
  :description "DICE-like simulation model"
  :url "https://github.com/shchipts/dice-simulator"
  :scm {:name "git"
        :url "https://github.com/shchipts/dice-simulator"}
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.iiasa/utilities-clj "1.1.0"]]
  :plugins [[lein-codox "0.9.5"]]
  :codox {:output-path "docs"}
  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]
  :main nil)
