;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.condition-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.condition :refer :all]))

;;; tests

(deftest limiting-case-ffi-test
  (testing "filter net FFI emissions curves which lie under the limiting case"
    (; Act
     let [out1 (limiting-case-ffi [35.85 65 2030 -19.5 15 50])
          out2 (limiting-case-ffi [35.85 36 2025 -20 5 50])
          out3 (limiting-case-ffi [35.85 65 2020 -20 15 50])
          out4 (limiting-case-ffi [35.85 36 2045 -20 15 50])]

      ; Assert
      (is (true? out1))
      (is (false? out2))
      (is (false? out3))
      (is (false? out4)))))

(deftest net-emissions-baseline
  (testing "filter net FFI emissions curves which lie above SSP baseline"
    (; Act
     let [out1 (baseline-ffi [35.85 36 2060 -19.5 15 50] :SSP1)
          out2 (baseline-ffi [35.85 40 2030 -20 15 50] :SSP1)
          out3 (baseline-ffi [35.85 48 2025 -20 5 50] :SSP5)]

      ; Assert
      (is (true? out1))
      (is (false? out2))
      (is (false? out3)))))

(deftest gross-emissions-baseline
  (testing "filter pars of net FFI emissions and CDR curves which lie
above SSP baseline"
    (; Act
     let [out1 (baseline-ffi [35.85 36 2055 -2.5 50 50] [20 2030 40] :SSP1)
          out2 (baseline-ffi [35.85 36 2055 -2.5 50 50] [20 2030 40] :SSP2)
          out3 (baseline-ffi [35.85 36 2055 -2.5 50 50] [20 2030 40] :SSP3)
          out4 (baseline-ffi [35.85 36 2055 -2.5 50 50] [20 2030 40] :SSP4)
          out5 (baseline-ffi [35.85 36 2055 -2.5 50 50] [20 2030 40] :SSP5)

          out6 (baseline-ffi [35.85 36 2055 -2.5 50 50] [19.5 2040 40] :SSP1)
          out7 (baseline-ffi [35.85 36 2055 -2.5 50 50] [19.5 2040 40] :SSP2)
          out8 (baseline-ffi [35.85 36 2055 -2.5 50 50] [19.5 2040 40] :SSP3)
          out9 (baseline-ffi [35.85 36 2055 -2.5 50 50] [19.5 2040 40] :SSP4)
          out10 (baseline-ffi [35.85 36 2055 -2.5 50 50] [19.5 2040 40] :SSP5)]

      ; Assert
      (is (false? out1))
      (is (false? out2))
      (is (true? out3))
      (is (false? out4))
      (is (false? out5))

      (is (false? out6))
      (is (true? out7))
      (is (true? out8))
      (is (false? out9))
      (is (true? out10)))))

(deftest emissions-quota-test
  (testing "filter net FFI emissions curves which exceed cumulative emissions
quota"
    (; Act
     let [out1 (emissions-quota [35.85 65 2045 -20 15 50] :SSP5 :3.4)
          out2 (emissions-quota [35.85 51.5 2050 -16 23 50] :SSP1 :2.6)
          out3 (emissions-quota [35.85 65 2045 -20 15 50] :SSP2 :4.5)
          out4 (emissions-quota [35.85 51.5 2050 -16 23 50] :SSP1 :1.9)]

      ; Assert
      (is (true? out1))
      (is (true? out2))
      (is (false? out3))
      (is (false? out4)))))

(deftest non-negative-gross-gdp-test
  (testing "filter emissions curves which produce negative gross GDP"
    (; Act
     let [out1 (non-negative-gross-gdp [35.85 38.5 2050 -19.5 16 50]
                                       [0.5 2050 40]
                                       :SSP1
                                       (range 2015 2105 5))
          out2 (non-negative-gross-gdp [35.85 38.5 2050 -19.5 16 50]
                                       [0.5 2050 40]
                                       :SSP1
                                       (range 2015 2080 5))
          out3 (non-negative-gross-gdp [35.85 38.5 2050 -19.5 16 50]
                                       [0.5 2050 40]
                                       :SSP1
                                       (range 2015 2075 5))]

      ; Assert
      (is (false? out1))
      (is (false? out2))
      (is (true? out3)))))


;;; test grouping


(deftest baseline-ffi-test
  (testing "Baseline emissions constraint:\n"
    (net-emissions-baseline)
    (gross-emissions-baseline)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace"
  []
  (limiting-case-ffi-test)
  (baseline-ffi-test)
  (emissions-quota-test)
  (non-negative-gross-gdp-test))
