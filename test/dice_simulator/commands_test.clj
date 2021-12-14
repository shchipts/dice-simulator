;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.commands-test
  (:require [clojure.test :refer :all]
            [dice-simulator.commands :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest net-emissions-ffi-test
  (testing "net FFI emissions under SSP scenarios"
    (; Act
     let [paths1 (net-emissions-ffi 35.85
                                    [36 38 40]
                                    [2045]
                                    [[-20 15 50]]
                                    (range 2015 2105 5))
          paths2 (net-emissions-ffi 35.85
                                    [40]
                                    [2045]
                                    [[0 55 50]]
                                    (range 2015 2105 5))]

      ; Assert
      (is (= (count (:paths (:SSP1 paths1)))
             1))
      (is (= (:parameters (:SSP1 paths1))
             [[38 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP1 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))

      (is (= (count (:paths (:SSP2 paths1)))
             1))
      (is (= (:parameters (:SSP2 paths1))
             [[38 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP2 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))

      (is (= (count (:paths (:SSP3 paths1)))
             2))
      (is (= (:parameters (:SSP3 paths1))
             [[38 2045 -20 15 50]
              [40 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP3 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))
      (is (real= (second (:paths (:SSP3 paths1)))
                 [35.85000000 37.23333333 38.61666667 40.00000000 41.38333333
                  42.76666667 44.15000000 29.04572941 21.25550097 12.07500000
                  2.89449903 -4.89572941 -10.63680968 -14.44297128 -16.79250000
                  -18.17950554 -18.97680417 -19.42812390]))

      (is (= (count (:paths (:SSP4 paths1)))
             2))
      (is (= (:parameters (:SSP4 paths1))
             [[38 2045 -20 15 50]
              [40 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP4 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))
      (is (real= (second (:paths (:SSP4 paths1)))
                 [35.85000000 37.23333333 38.61666667 40.00000000 41.38333333
                  42.76666667 44.15000000 29.04572941 21.25550097 12.07500000
                  2.89449903 -4.89572941 -10.63680968 -14.44297128 -16.79250000
                  -18.17950554 -18.97680417 -19.42812390]))

      (is (= (count (:paths (:SSP5 paths1)))
             2))
      (is (= (:parameters (:SSP5 paths1))
             [[38 2045 -20 15 50]
              [40 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP5 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))
      (is (real= (second (:paths (:SSP5 paths1)))
                 [35.85000000 37.23333333 38.61666667 40.00000000 41.38333333
                  42.76666667 44.15000000 29.04572941 21.25550097 12.07500000
                  2.89449903 -4.89572941 -10.63680968 -14.44297128 -16.79250000
                  -18.17950554 -18.97680417 -19.42812390]))

      (is (= (count (:paths (:SSP1 paths2)))
             (count (:paths (:SSP2 paths2)))
             (count (:paths (:SSP3 paths2)))
             (count (:paths (:SSP4 paths2)))
             (count (:paths (:SSP5 paths2)))
             0)))))
