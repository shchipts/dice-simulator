;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.procedures-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.procedures :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest upper-bound
  (testing "upper bound for emissions pathways"
    (; Act
     let [sims1
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum nil
            :max-cummulative-emissions 8.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           (fn [_] (identity true))
           1.0)
          sims2
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum nil
            :max-cummulative-emissions 5.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           (fn [_] (identity true))
           1.0)]

      ; Assert
      (is (= (count sims1) 5))
      (is (real= (nth sims1 0)
                 [2.003 0]))
      (is (real= (nth sims1 1)
                 [2.003 1]))
      (is (real= (nth sims1 2)
                 [2.003 2]))
      (is (real= (nth sims1 3)
                 [2.003 3]))
      (is (real= (nth sims1 4)
                 [2.003 4]))

      (is (= (count sims2) 3))
      (is (real= (nth sims2 0)
                 [2.003 0]))
      (is (real= (nth sims2 1)
                 [2.003 1]))
      (is (real= (nth sims2 2)
                 [2.003 2])))))

(deftest lower-bound
  (testing "lower bound for emissions pathways"
    (; Act
     let [sims
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum '(2.003 1.5)
            :max-cummulative-emissions 8.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           (fn [_] (identity true))
           1.0)]

      ; Assert
      (is (= (count sims) 3))
      (is (real= (nth sims 0)
                 [2.003 2]))
      (is (real= (nth sims 1)
                 [2.003 3]))
      (is (real= (nth sims 2)
                 [2.003 4])))))

(deftest feasibility-constraint
  (testing "feasible emissions pathways"
    (; Act
     let [sims1
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :max-cummulative-emissions 8.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           #(real<= (second %) 2)
           1.0)
          sims2
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :max-cummulative-emissions 5.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           #(real<= (second %) 2)
           1.0)
          sims3
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum '(2.003 1.5)
            :max-cummulative-emissions 8.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           #(real<= (second %) 2)
           1.0)
          sims4
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum '(2.003 0.5)
            :max-cummulative-emissions 5.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           #(real<= (second %) 1)
           1.0)
          sims5
          (non-negative-emissions
           {:n-steps 1
            :time-step 2
            :depreciation-rate 0.5
            :init {:industrial-emissions 2.003 :capital-stock 1.0}
            :non-negative-emissions-minimum '(2.003 0.5)
            :max-cummulative-emissions 5.0
            :carbon-intensity '(1.5 1.0)
            :cobb-douglas {:labor '(1.0 1.0)
                           :tfp '(2.0 1.0)
                           :capital-elasticity 1}
            :climate-module identity}
           (fn [_] (identity false))
           1.0)]

      ; Assert
      (is (= (count sims1) 3))
      (is (real= (nth sims1 0)
                 [2.003 0]))
      (is (real= (nth sims1 1)
                 [2.003 1]))
      (is (real= (nth sims1 2)
                 [2.003 2]))

      (is (= (count sims2) 3))
      (is (real= (nth sims2 0)
                 [2.003 0]))
      (is (real= (nth sims2 1)
                 [2.003 1]))
      (is (real= (nth sims2 2)
                 [2.003 2]))

      (is (= (count sims3) 1))
      (is (real= (nth sims3 0)
                 [2.003 2]))

      (is (= (count sims4) 1))
      (is (real= (nth sims4 0)
                 [2.003 1]))

      (is (= sims5 [])))))

(deftest multi-period-pathways
  (testing "simulations of multi-period emissions"
    (; Arrange
     let [base1 {:n-steps 2
                 :time-step 2
                 :depreciation-rate 0.5
                 :init {:industrial-emissions 2.003 :capital-stock 0.2}
                 :max-cummulative-emissions 10.0
                 :carbon-intensity '(6.0 5.0 1.0)
                 :cobb-douglas {:labor '(1.0 1.0 1.0)
                                :tfp '(1.0 1.5 4.0)
                                :capital-elasticity 1}
                 :climate-module identity}
          base2 {:n-steps 4
                 :time-step 2
                 :depreciation-rate 0.5
                 :init {:industrial-emissions 2.003 :capital-stock 0.2}
                 :max-cummulative-emissions 12.0
                 :carbon-intensity '(6.0 5.0 2.0 1.0 0.5)
                 :cobb-douglas {:labor '(1.0 1.0 1.0 1.0 1.0)
                                :tfp '(1.0 1.0 1.0 1.0 1.0)
                                :capital-elasticity 1}
                 :climate-module identity}]

      (; Act
       let [sims1
            (non-negative-emissions
             (assoc base1
                    :non-negative-emissions-minimum
                    '(2.003 0.5 0.5))
             (fn [e]
               (or (real= e [2.003 2 0.5])
                   (real= e [2.003 1 0.5])
                   (real= e [2.003 2 2])
                   (real= e [2.003 2 1])
                   (real= e [2.003 1 4])
                   (real= e [2.003 1 3])
                   (real= e [2.003 1 2])
                   (real= e [2.003 1 1])))
             1.0)
            sims2
            (non-negative-emissions
             base1
             (fn [e]
               (or (real= e [2.003 3])
                   (real= e [2.003 2])
                   (real= e [2.003 1])
                   (real= e [2.003 0])
                   (real= e [2.003 3 0])
                   (real= e [2.003 2 1])
                   (real= e [2.003 2 0])
                   (real= e [2.003 1 3])
                   (real= e [2.003 1 2])
                   (real= e [2.003 1 1])
                   (real= e [2.003 1 0])))
             1.0)
            sims3
            (non-negative-emissions
             (assoc base2
                    :non-negative-emissions-minimum
                    '(2.003 0.5 0.5))
             (fn [e]
               (or (real= e [2.003 1 0.5])
                   (real= e [2.003 1 1])
                   (real= e [2.003 1 1 2])
                   (real= e [2.003 1 1 1])
                   (real= e [2.003 1 1 0])
                   (real= e [2.003 1 1 2 0])
                   (real= e [2.003 1 1 1 1])
                   (real= e [2.003 1 1 1 0])))
             1.0)
            sims4
            (non-negative-emissions
             (assoc base2
                    :non-negative-emissions-minimum
                    '(2.003 1 1))
             (fn [e]
               (or (real= e [2.003 1 1])
                   (real= e [2.003 1 1 0])))
             1.0)
            sims5
            (non-negative-emissions
             (assoc base2
                    :non-negative-emissions-minimum
                    '(2.003 0.5 0.5))
             (fn [_] (identity false))
             1.0)
            sims6
            (non-negative-emissions
             (assoc base2
                    :non-negative-emissions-minimum
                    '(2.003 0.5 0.5)
                    :max-cummulative-emissions 2.5)
             (fn [_] (identity true))
             1.0)
            sims7
            (non-negative-emissions
             (assoc base2
                    :non-negative-emissions-minimum
                    '(2.003 0.5 0.5))
             (fn [e]
               (or (real= e [2.003 1 0.5])
                   (real= e [2.003 1 1])
                   (real= e [2.003 1 1 0])))
             1.0)]

        ; Assert
        (is (= (count sims1) 6))
        (is (real= (nth sims1 0) [2.003 1 1]))
        (is (real= (nth sims1 1) [2.003 1 2]))
        (is (real= (nth sims1 2) [2.003 1 3]))
        (is (real= (nth sims1 3) [2.003 1 4]))
        (is (real= (nth sims1 4) [2.003 2 1]))
        (is (real= (nth sims1 5) [2.003 2 2]))

        (is (= (count sims2) 8))
        (is (real= (nth sims2 0) [2.003 1 0]))
        (is (real= (nth sims2 1) [2.003 1 1]))
        (is (real= (nth sims2 2) [2.003 1 2]))
        (is (real= (nth sims2 3) [2.003 1 3]))
        (is (real= (nth sims2 4) [2.003 2 0]))
        (is (real= (nth sims2 5) [2.003 2 1]))
        (is (real= (nth sims2 6) [2.003 3 0]))
        (is (real= (nth sims2 7) [2.003 0]))

        (is (= (count sims3) 4))
        (is (real= (nth sims3 0) [2.003 1 1 1 0]))
        (is (real= (nth sims3 1) [2.003 1 1 1 1]))
        (is (real= (nth sims3 2) [2.003 1 1 2 0]))
        (is (real= (nth sims3 3) [2.003 1 1 0]))

        (is (= (count sims4) 1))
        (is (real= (nth sims4 0) [2.003 1 1 0]))

        (is (= (count sims5) 0))
        (is (= (count sims6) 0))

        (is (= (count sims7) 1))
        (is (real= (nth sims7 0) [2.003 1 1 0]))))))

;;; test grouping


(deftest non-negative-emissions-test
  (testing "Simulation of non-negative emission pathways:\n"
    (upper-bound)
    (lower-bound)
    (feasibility-constraint)
    (multi-period-pathways)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace."
  []
  (non-negative-emissions-test))
