;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.generator-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.generator :refer :all]))

;;; tests

(deftest parameterize-net-emissions-ffi-test
  (testing "parameter tuples for linear-logistic curve of net FFI emissions"
    (; Act
     let [pars (parameterize-net-emissions-ffi
                [1 2]
                [2030 2050 2060]
                [[-10 20 50] [-20 15 60]])]

      ; Assert
      (is (= (:y_ pars)
             [1 1 1 1 1 1
              2 2 2 2 2 2]))
      (is (= (:x1 pars)
             [2030 2030 2050 2050 2060 2060
              2030 2030 2050 2050 2060 2060]))
      (is (= (:K pars)
             [-10 -20 -10 -20 -10 -20
              -10 -20 -10 -20 -10 -20]))
      (is (= (:midpoint-offset pars)
             [20 15 20 15 20 15
              20 15 20 15 20 15]))
      (is (= (:dt pars)
             [50 60 50 60 50 60
              50 60 50 60 50 60])))))

(deftest infill-net-emissions-land-use-test
  (testing "select land use emissions from SSP database"
    (; Act
     let [land-use1 (infill-net-emissions-land-use
                     [35.85 36 2060 -20 20 60]
                     :SSP5)
          land-use2 (infill-net-emissions-land-use
                     [35.85 45 2060 -20 20 60]
                     :SSP1)]

      ; Assert
      (is (= land-use1 :3.4))
      (is (= land-use2 :4.5)))))
