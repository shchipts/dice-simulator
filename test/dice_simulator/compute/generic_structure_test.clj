;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.generic-structure-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.generic-structure :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest cobb-douglas-output-test
  (testing "Cobb-Douglas output of world economy"
    (; Act
     let [y1 (output {:cobb-douglas
                      {:labor [7.403 7.853 8.265 8.639 8.977 9.28]
                       :tfp [5.115 5.5357 5.9789 6.4448 6.9337 7.4457]
                       :capital-elasticity 0.3}}
                     318.42
                     2)
          y2 (output {:cobb-douglas
                      {:labor [7.403 7.853 8.265 8.639 8.977 9.28]
                       :tfp [5.115 5.5357 5.9789 6.4448 6.9337 7.4457]
                       :capital-elasticity 0.3}}
                     223
                     0)]

      ; Assert
      (is (real= y1 147.77174153142))
      (is (real= y2 105.177421975459)))))

(deftest capital-stock-law-test
  (testing "The law of motion for the capital stock"
    (; Act
     let [k (capital-stock {:time-step 5
                            :depreciation-rate 0.1}
                           267.94
                           32.04)]

      ; Assert
      (is (real= k 318.4158906)))))

(deftest investment-test
  (testing "Investment in the closed economy"
    (; Act
     let [i1 (investment {:cobb-douglas
                          {:labor [7.403 7.853 8.265 8.639 8.977 9.28]}}
                         198.48
                         16.61
                         4)
          i2 (investment {:cobb-douglas
                          {:labor [7.403 7.853 8.265 8.639 8.977 9.28]}}
                         105
                         10.5
                         0)]

      ; Assert
      (is (real= i1 49.37203))
      (is (real= i2 27.2685)))))

(deftest emissions-test
  (testing "Emissions from the produced output after abatement"
    (; Act
     let [e1 (emissions {:cobb-douglas
                         {:labor [7.403 7.85309084767271 8.265 8.639 8.977 9.28]
                          :tfp [5.115 5.5357142857 5.9789 6.4448 6.9337 7.4457]
                          :capital-elasticity 0.3}
                         :carbon-intensity [0.350320027
                                            0.324682279
                                            0.301034941
                                            0.279215233
                                            0.259074324
                                            0.240476081]}
                        0.0322971885006973
                        267.942170750277
                        1)
          e2 (emissions {:cobb-douglas
                         {:labor [7.403 7.853 8.265 8.639 8.977 9.28]
                          :tfp [5.115 5.5357 5.9789 6.4448 6.9337 7.4457]
                          :capital-elasticity 0.3}
                         :carbon-intensity [0.350320027
                                            0.324682279
                                            0.301034941
                                            0.279215233
                                            0.259074324
                                            0.240476081]}
                        0.0298825095077324
                        223
                        0)]

      ; Assert
      (is (real= e1 39.3835637138958))
      (is (real= e2 35.7447136540239)))))

(deftest gdp-test
  (testing "GDP net of abatement and damages"
    (; Arrange
     let [model {:cobb-douglas
                 {:labor [7.403 7.85309085 8.26492066
                          8.63897496 8.97655692 9.27954298]
                  :tfp [5.115 5.53571429 5.97889093
                        6.44480826 6.93369256 7.44571697]
                  :capital-elasticity 0.3}
                 :carbon-intensity [0.350320027 0.324682279 0.301034941
                                    0.279215233 0.259074324 0.240476081]
                 :unadjusted-abatement-cost
                 (fn [x]
                   (cond
                     (real= x 0.25624950) 0.02813310123
                     (real= x 0.24697883) 0.06312380788
                     :else 0))
                 :damages
                 (fn [posterior prior model-instance]
                   (cond
                     (and (real= (:capital-stock posterior) 267.9421708)
                          (real= (:temperature posterior) 1.01634518)
                          (real= (:capital-stock prior) 223)
                          (real= (:temperature prior) 0.85)) 0.0024377798
                     (and (real= (:capital-stock posterior) 504.7555419)
                          (real= (:temperature posterior) 1.74002086)
                          (real= (:capital-stock prior) 436.6775239)
                          (real= (:temperature prior) 1.55174653)) 0.0071453073
                     :else 0))}
          posterior1 {:capital-stock  267.9421708 :temperature 1.01634518}
          prior1 {:capital-stock 223 :temperature 0.85}
          posterior2 {:capital-stock 504.7555419 :temperature 1.74002086}
          prior2 {:capital-stock 436.6775239 :temperature 1.55174653}]

      (; Act
       let [gdp1 (gdp model posterior1 prior1 0.25624950 1)
            gdp2 (gdp model posterior2 prior2 0.24697883 5)]

        ; Assert
        (is (real= gdp1 125.04045325))
        (is (real= gdp2 227.49890267))))))
