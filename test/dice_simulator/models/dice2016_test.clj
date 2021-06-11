;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.models.dice2016-test
  (:require [clojure.test :refer :all]
            [dice-simulator.models.dice2016 :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest model-test
  (testing "dice2016 model instance"
    (; Act
     let [dice (model 11 5)]

      ; Assert
      (is (= (:n-steps dice) 11))
      (is (= (:time-step dice) 5))
      (is (real= (:depreciation-rate dice) 0.1))
      (is (real= (:industrial-emissions (:init dice)) 35.85))
      (is (real= (:capital-stock (:init dice)) 223))
      (is (real= (:non-negative-emissions-minimum dice) [35.85]))
      (is (real= (:carbon-intensity dice)
                 [0.35032003 0.32468228 0.30103494 0.27921523 0.25907432
                  0.24047608 0.22329595 0.20741990 0.19274355 0.17917124
                  0.16661534 0.15499552]))
      (is (real= (:labor (:cobb-douglas dice))
                 [7.403 7.853091 8.264921 8.638975 8.976557 9.279543
                  9.550180 9.790920 10.004299 10.192839 10.358983 10.505050]))
      (is (real= (:tfp (:cobb-douglas dice))
                 [5.1150000 5.5357143 5.9788909 6.4448083 6.9336926
                  7.4457170 7.9810008 8.5396092 9.1215527 9.7267878
                  10.3552173 11.0066908]))
      (is (real= (:capital-elasticity (:cobb-douglas dice))
                 0.3))
      (is (real= ((:climate-module dice)
                  [35.7447137 39.3835637 42.9314853 46.3496993 49.6059721
                   52.6740699 55.5331375 58.1670563 60.5638178 62.7149366
                   64.6149143 66.2607601])
                 [0.85000000 1.01634518 1.18923663 1.36792985 1.55174653
                  1.74002086 1.93207743 2.12722733 2.32477390 2.52402262
                  2.72429207 2.92492408]))
      (is (real= (:max-cumulative-emissions dice)
                 21996))

      (is (real= ((:unadjusted-abatement-cost dice) 0.037727673)
                 0.04214105455))
      (is (real= ((:unadjusted-abatement-cost dice) 0.040776287)
                 0.05157619442))

      (is (real= ((:damages dice) {:temperature 1.18923663} nil nil)
                 0.003337710))
      (is (real= ((:damages dice) {:temperature 1.36792985} nil nil)
                 0.004416108))

      (is (real= ((:reduction-max (:feasible-decarbonization dice)) 5 1)
                 -5))
      (is (real= ((:reduction-max (:feasible-decarbonization dice)) 12 7)
                 2))
      (is (nil? ((:reduction-max (:feasible-decarbonization dice)) 12 8)))

      (is (real= ((:rate-max (:feasible-decarbonization dice)) 5 1)
                 1))
      (is (real= ((:rate-max (:feasible-decarbonization dice)) 5 7)
                 1))
      (is (real= ((:rate-max (:feasible-decarbonization dice)) 1 8)
                 1.1))
      (is (real= ((:rate-max (:feasible-decarbonization dice)) 1.3 8)
                 1.2)))))
