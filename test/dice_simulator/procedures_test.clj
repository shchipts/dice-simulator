;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.procedures-test
  (:require [clojure.test :refer :all]
            [dice-simulator.procedures :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest post-peak-decarbonization-constraints
  (testing "constraints on speed of decarbonization after peak warming"
    (; Act
     let [tree (emissions-tree 0.5
                               {:industrial-emissions 1.0
                                :reduction-rate 0.5}
                               {:time-step 5
                                :cobb-douglas {:labor [1 1 1]
                                               :tfp [1 1 1]
                                               :capital-elasticity 1}
                                :depreciation-rate 0
                                :carbon-intensity [1 1 1]}
                               {:volume
                                {:industrial-emissions
                                 {:produced {:maximum [2 2]}}}
                                :decarbonization
                                {:post-peak {:reduction-rate
                                             {:maximum 0.8
                                              :growth-rate 1.5}}
                                 :net-zero-timing 0}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [2 2]
              :gross [4 4 4 4]
              :abated [2 3 2 3]
              :layer-size [1 2]
              :heads [1 1 2]})))))

(deftest pre-peak-decarbonization-constraints
  (testing "constraints on speed of decarbonization before peak warming"
    (; Act
     let [tree (emissions-tree 0.1
                               {:industrial-emissions 1.0
                                :reduction-rate 0.5}
                               {:time-step 5
                                :cobb-douglas {:labor [1 1 1]
                                               :tfp [1 1 1]
                                               :capital-elasticity 1}
                                :depreciation-rate 0
                                :carbon-intensity [1 1 1]}
                               {:volume
                                {:industrial-emissions
                                 {:produced {:maximum [2 2]}}}
                                :decarbonization
                                {:pre-peak {:reduction {:growth 0.2}
                                            :reduction-rate {:maximum 0.65}}
                                 :net-zero-timing 3}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 4]
              :gross [20 20 20 20 20 20 20]
              :abated [10 11 12 10 11 12 13]
              :layer-size [1 2 3 2]
              :heads [1 1 2 1 2 3 2 3]})))))

(deftest investment-constraints
  (testing "constraints on emissions produced by world economy"
    (; Act
     let [tree (emissions-tree 0.5
                               {:industrial-emissions 3.0
                                :reduction-rate 0.25}
                               {:time-step 5
                                :cobb-douglas {:labor [4.2 5.1 5.4]
                                               :tfp [1.6 1.7 1.7]
                                               :capital-elasticity 0.3}
                                :depreciation-rate 0.1
                                :carbon-intensity [0.8 0.75 0.72]}
                               {:volume
                                {:industrial-emissions
                                 {:produced {:maximum [3.9 3.4]}}}
                                :decarbonization
                                {:post-peak {:reduction-rate
                                             {:maximum 0.25 :growth-rate 1.2}}
                                 :net-zero-timing 0}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [1 3]
              :gross [7 6 6 5]
              :abated [1 0 1 0]
              :layer-size [1 1 1]
              :heads [1 1 1]})))))

(deftest cummulative-emissions-constraint
  (testing "check cumulative emissions constraints"
    (; Arrange
     let [init {:industrial-emissions 1.0 :reduction-rate 0}
          pars {:time-step 5
                :cobb-douglas {:labor [1 1 1 1 1 1 1 1]
                               :tfp [1 1 1 1 1 1 1 1]
                               :capital-elasticity 1}
                :depreciation-rate 0
                :carbon-intensity [1 1 1 1 1 1 1 1]}
          conf (fn [net-zero-timing growth cummax]
                 {:volume
                  {:industrial-emissions
                   {:produced {:maximum [1 1 1 1 1 1 1 1]}
                    :cumulative-emissions {:maximum cummax}}}
                  :decarbonization
                  {:pre-peak {:reduction {:growth growth}
                              :reduction-rate {:maximum 0}}
                   :post-peak {:reduction-rate
                               {:maximum 0 :growth-rate 0}}
                   :net-zero-timing net-zero-timing}})]

      (; Act
       let [tree1 (emissions-tree 0.5 init pars (conf 4 0.2 23.99))
            tree2 (emissions-tree 0.5 init pars (conf 4 0.2 21.99))
            tree3 (emissions-tree 0.5 init pars (conf 3 0.2 18.99))
            tree4 (emissions-tree 0.5 init pars (conf 7 0.1 31.99))
            tree5 (emissions-tree 0.5 init pars (conf 4 0.1 19.49))
            tree6 (emissions-tree 0.5 init pars (conf 7 0.8 16.99))
            tree7 (emissions-tree 0.5 init pars (conf 5 0.8 15.99))]

        ; Assert
        (is (= (first (drop 5 tree1))
               {:level-size [1 1 1 1 0 0]
                :gross [2 2 2 2]
                :abated [0 0 0 0]
                :layer-size [1 1 1]
                :heads [1 2 3]}))
        (is (= (first (drop 5 tree2))
               {:level-size [1 1 1 0 0 0]
                :gross [2 2 2]
                :abated [0 0 0]
                :layer-size [1 1]
                :heads [1 2]}))
        (is (= (first (drop 5 tree3))
               {:level-size [1 1 0 0 0 0]
                :gross [2 2]
                :abated [0 0]
                :layer-size [1]
                :heads [1]}))
        (is (= (first (drop 6 tree4))
               {:level-size [1 1 1 0 0 0 0]
                :gross [2 2 2]
                :abated [0 0 0]
                :layer-size [1 1]
                :heads [1 2]}))
        (is (= (first (drop 2 tree5))
               {:level-size [1 0 0]
                :gross [2]
                :abated [0]
                :layer-size []
                :heads []}))
        (is (= (first (drop 2 tree6))
               {:level-size [1 1 0]
                :gross [2 2]
                :abated [0 0]
                :layer-size [1]
                :heads [1]}))
        (is (= (first (drop 1 tree7))
               {:level-size [1 0]
                :gross [2]
                :abated [0]
                :layer-size []
                :heads []}))))))

(deftest emissions-paths-test
  (testing "emissions tree traversal with cumulative emissions constraint and
optional inverted U-shaped emissions curve constraint"
    (; Act
     let [paths1 (emissions-paths {:level-size [2 1 2 3]
                                   :gross [3 2 4 5 5 2 2 2]
                                   :abated [0 0 2 0 4 0 1 2]
                                   :layer-size [2 1 1 2 1 1]
                                   :heads [1 2 3 3 4 5 4 5]}
                                  2
                                  {:industrial-emissions 1.0}
                                  {:time-step 5}
                                  {:industrial-emissions
                                   {:cumulative-emissions {:maximum 84.99}}}
                                  true)
          paths2 (emissions-paths {:level-size [3 2 2 2]
                                   :gross [3 2 1 4 4 5 5 2 2]
                                   :abated [0 0 0 0 2 0 4 0 1]
                                   :layer-size [1 3 2 2 2 2]
                                   :heads [1 1 2 3 4 5 4 5 6 7 6 7]}
                                  1
                                  {:industrial-emissions 2.0}
                                  {:time-step 1}
                                  {:industrial-emissions
                                   {:cumulative-emissions {:maximum 20}}}
                                  true)
          paths3 (emissions-paths {:level-size [3 2 2 2]
                                   :gross [3 2 1 4 4 5 5 2 2]
                                   :abated [0 0 0 0 2 0 4 0 1]
                                   :layer-size [1 3 2 2 2 2]
                                   :heads [1 1 2 3 4 5 4 5 6 7 6 7]}
                                  1
                                  {:industrial-emissions 2.0}
                                  {:time-step 1}
                                  {:industrial-emissions
                                   {:cumulative-emissions {:maximum 11}}})]

      ; Assert
      (is (= paths1
             '([1 3 5 8]
               [2 3 5 8])))

      (is (= paths2
             '([1 4 6 8]
               [2 5 6 8]
               [1 4 6 9]
               [2 5 6 9]
               [1 4 7 9]
               [1 5 7 9]
               [2 5 7 9])))

      (is (= paths3
             '([1 5 7 8]
               [2 5 7 8]
               [3 5 7 8]
               [3 5 6 9]
               [1 4 7 9]
               [1 5 7 9]
               [2 5 7 9]
               [3 5 7 9]))))))

(deftest emissions-growth-constraint
  (testing "constraint on feasible produced emissions growth"
    (; Act
     let [tree (emissions-tree 1
                               {:industrial-emissions 1
                                :reduction-rate 0}
                               {:time-step 5
                                :cobb-douglas {:labor [1 1 1]
                                               :tfp [1 1 1]
                                               :capital-elasticity 1}
                                :depreciation-rate 0
                                :carbon-intensity [1 1 1]}
                               {:volume
                                {:industrial-emissions
                                 {:produced {:maximum [10 4]
                                             :growth 2}}}
                                :decarbonization
                                {:post-peak {:reduction-rate
                                             {:maximum 0
                                              :growth-rate 1}}
                                 :net-zero-timing 0}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 4]
              :gross [3 2 1 4 3 2 1]
              :abated [0 0 0 0 0 0 0]
              :layer-size [2 3 2 1]
              :heads [1 2 1 2 3 2 3 3]})))))


;;; test grouping


(deftest emissions-tree-test
  (testing "Emissions tree:\n"
    (post-peak-decarbonization-constraints)
    (pre-peak-decarbonization-constraints)
    (investment-constraints)
    (cummulative-emissions-constraint)
    (emissions-growth-constraint)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace"
  []
  (emissions-tree-test)
  (emissions-paths-test))
