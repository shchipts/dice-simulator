;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.binary-search-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.binary-search :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest no-feasible-values
  (testing "min value is not feasible"
    (; Act
     let [out1 (run 1.1 2.3 (fn [_] (identity false)) 0.1)
          out2 (run 1.1
                    2.3
                    #(if (< % 1.999) true false)
                    1)]

     ; Assert
      (is (real= out1
                 []))
      (is (real= out2
                 [])))))

(deftest all-values
  (testing "max value is feasible"
    (; Act
     let [out1 (run 1.1 2.3 (fn [_] (identity true)) 0.1)
          out2 (run 1.1
                    3.3
                    #(if (> % 3.001) false true)
                    1)]

     ; Assert
      (is (real= out1
                 [1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3]))
      (is (real= out2
                 [2.0 3.0])))))

(deftest find-values
  (testing "find feasible values"
    (; Act
     let [out1 (run 2.5
                    6
                    #(if (> % 3.001) false true)
                    0.5)
          out2 (run 2.4
                    6.2
                    #(if (> % 5.501) false true)
                    0.5)
          out3 (run 0
                    6
                    #(if (> % 3.001) false true)
                    0.5)
          out4 (run 2.5
                    6
                    #(if (> % 4.501) false true)
                    0.5)
          out5 (run 2.5
                    6
                    #(if (> % 2.501) false true)
                    0.5)]

     ; Assert
      (is (real= out1
                 [2.5 3]))
      (is (real= out2
                 [2.5 3 3.5 4 4.5 5 5.5]))
      (is (real= out3
                 [0 0.5 1 1.5 2 2.5 3]))
      (is (real= out4
                 [2.5 3 3.5 4 4.5]))
      (is (real= out5
                 [2.5])))))

;;; test grouping


(deftest run-test
  (testing "Binary search:\n"
    (no-feasible-values)
    (all-values)
    (find-values)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace."
  []
  (run-test))
