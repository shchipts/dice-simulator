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
