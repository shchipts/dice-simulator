;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.procedures2-test
  (:require [clojure.test :refer :all]
            [dice-simulator.procedures2 :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest no-abatement
  (testing "baseline constraint on emissions produced by world economy"
    (; Act
     let [tree (emissions-tree 1
                               {:industrial-emissions 2
                                :reduction-rate 0}
                               {:time-step 5
                                :cobb-douglas {:labor [1.2 1.5 50]
                                               :tfp [1 2 2.2]
                                               :capital-elasticity 0.5}
                                :depreciation-rate 0.4
                                :carbon-intensity [0.7 0.5 0.3]}
                               {:decarbonization2
                                {:produced {:maximum [2 2]}
                                 :net-emitted {:growth 1}}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 5]
              :gross [1 2 2 1 2 1 2 2]
              :abated [0 1 0 1 2 0 1 0]
              :edges {1 [] 2 [] 3 []
                      4 [1]
                      5 [1 2]
                      6 [1]
                      7 [1 2 3]
                      8 [1 3]}})))))

(deftest feasible-emissions-growth
  (testing "constraint on feasible growth of emissions produced
by world economy"
    (; Act
     let [tree (emissions-tree 1
                               {:industrial-emissions 2
                                :reduction-rate 0}
                               {:time-step 5
                                :cobb-douglas {:labor [1.2 1.5 50]
                                               :tfp [1 2 2.2]
                                               :capital-elasticity 0.5}
                                :depreciation-rate 0.4
                                :carbon-intensity [0.7 0.5 0.3]}
                               {:decarbonization2
                                {:produced {:maximum [2 2] :growth 0.5}
                                 :net-emitted {:growth 1}}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 5]
              :gross [1 2 2 1 2 1 2 2]
              :abated [0 1 0 1 2 0 1 0]
              :edges {1 [] 2 [] 3 []
                      4 [1]
                      5 [2]
                      6 [1]
                      7 [2 3]
                      8 [3]}})))))

(deftest deep-mitigation
  (testing "constraint on deep mitigation pathway"
    (; Act
     let [tree (emissions-tree 1
                               {:industrial-emissions 3
                                :reduction-rate 0}
                               {:time-step 5
                                :cobb-douglas {:labor [1 1 1]
                                               :tfp [1 1 1]
                                               :capital-elasticity 1}
                                :depreciation-rate 0
                                :carbon-intensity [1 1 1]}
                               {:decarbonization2
                                {:produced {:maximum [5 5] :growth 100}
                                 :gross-emitted {:minimum [4 4]}
                                 :net-emitted {:growth 1}}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 3]
              :gross [4 5 5 4 5 5]
              :abated [0 1 0 0 1 0]
              :edges {1 [] 2 [] 3 []
                      4 [1] 5 [1 2 3] 6 [1 3]}})))))

(deftest cdr-deployment
  (testing "constraint on deep mitigation pathway with massive CDR deployment"
    (; Act
     let [tree (emissions-tree 1
                               {:industrial-emissions 3
                                :reduction-rate 0}
                               {:time-step 5
                                :cobb-douglas {:labor [1 1 1]
                                               :tfp [1 1 1]
                                               :capital-elasticity 1}
                                :depreciation-rate 0
                                :carbon-intensity [1 1 1]}
                               {:decarbonization2
                                {:produced {:maximum [5 5] :growth 100}
                                 :gross-emitted {:minimum [5 5]}
                                 :cdr-deployed {:maximum [1 2]}
                                 :net-emitted {:growth 1}}})]

      ; Assert
      (is (= (first (drop 1 tree))
             {:level-size [3 5]
              :gross [4 5 5 4 5 4 5 5]
              :abated [0 1 0 1 2 0 1 0]
              :edges {1 [] 2 [] 3 []
                      4 [1]
                      5 [1 2]
                      6 [1]
                      7 [1 2 3]
                      8 [1 3]}})))))


;;; test grouping


(deftest emissions-tree-test
  (testing "Emissions tree:\n"
    (no-abatement)
    (feasible-emissions-growth)
    (deep-mitigation)
    (cdr-deployment)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace"
  []
  (emissions-tree-test))
