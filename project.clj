(defproject org.clojars.shchipts/dice-simulator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/shchipts/dice-simulator"
  :scm {:name "git"
        :url "https://github.com/shchipts/dice-simulator"}
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.iiasa/utilities-clj "1.1.0"]]
  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]
  :main nil)
