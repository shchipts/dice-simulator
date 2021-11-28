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
  (testing "filter net FFI emissions curve which lie under the limiting case"
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

(deftest baseline-ffi-test
  (testing "filter net FFI emissions curve which lie above SSP baseline"
    (; Act
     let [out1 (baseline-ffi [35.85 36 2060 -19.5 15 50] :SSP1)
          out2 (baseline-ffi [35.85 40 2030 -20 15 50] :SSP1)
          out3 (baseline-ffi [35.85 48 2025 -20 5 50] :SSP5)]

      ; Assert
      (is (true? out1))
      (is (false? out2))
      (is (false? out3)))))

(deftest emissions-quota-test
  (testing "filter net FFI emissions curve which exceed cumulative emissions
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
