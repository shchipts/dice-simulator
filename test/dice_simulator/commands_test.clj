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
             [[2 38 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP1 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))

      (is (= (count (:paths (:SSP2 paths1)))
             1))
      (is (= (:parameters (:SSP2 paths1))
             [[2 38 2045 -20 15 50]]))
      (is (real= (first (:paths (:SSP2 paths1)))
                 [35.85000000 36.56666667 37.28333333 38.00000000 38.71666667
                  39.43333333 40.15000000 25.98753895 18.68306131 10.07500000
                  1.46693869 -5.83753895 -11.22064072 -14.78947346 -16.99250000
                  -18.29302040 -19.04060438 -19.46378258]))

      (is (= (count (:paths (:SSP3 paths1)))
             2))
      (is (= (:parameters (:SSP3 paths1))
             [[2 38 2045 -20 15 50]
              [3 40 2045 -20 15 50]]))
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
             [[2 38 2045 -20 15 50]
              [3 40 2045 -20 15 50]]))
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
             [[2 38 2045 -20 15 50]
              [3 40 2045 -20 15 50]]))
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

(deftest economic-growth-test
  (testing "economic output in SSP scenario"
    (; Arrange
     let [net-emissions [[1 35.85 36 2055 -2.5 50 50]
                         [2 35.85 36 2055 -2.5 50 50]]
          cdr-emissions [[3 20 2030 40]
                         [4 20 2030 40]]
          temperatures (repeat 2 [1.243 1.38 1.503 1.624 1.722 1.812 1.897
                                  1.98 2.062 2.142 2.225 2.308 2.392 2.474
                                  2.555 2.633 2.708 2.774])
          ts (range 2015 2105 5)]

      (; Act
       let [paths1 (economic-growth net-emissions
                                    cdr-emissions
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP1
                                    ts)
            paths2 (economic-growth net-emissions
                                    cdr-emissions
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP2
                                    ts)
            paths3 (economic-growth net-emissions
                                    cdr-emissions
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP3
                                    ts)
            paths4 (economic-growth net-emissions
                                    cdr-emissions
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP4
                                    ts)
            paths5 (economic-growth net-emissions
                                    cdr-emissions
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP5
                                    ts)
            paths6 (economic-growth [[1 35.85 38.5 2050 -19.5 16 50]]
                                    [[1 0.5 2050 40]]
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP1
                                    ts)
            paths7 (economic-growth [[1 35.85 36 2020 -6 43 50]]
                                    [[1 5.5 2050 40]]
                                    temperatures
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP1
                                    ts)
            paths8 (economic-growth [[4059 35.85 41.0 2060 -20.0 15 50]]
                                    [[36 17.5 2050 40]]
                                    [[1.243 1.416 1.557 1.695 1.819 1.942 2.059
                                      2.174 2.288 2.402 2.522 2.602 2.655 2.68
                                      2.678 2.651 2.607 2.553]]
                                    :howard-sterner2017
                                    :dice2016
                                    :SSP5
                                    (range 2015 2105 5))]

        ; Assert
        (is (= (count (:net-emissions paths3))
               (count (:cdr paths3))
               (count (:gross-gdp paths3))
               (count (:damages paths3))
               (count (:costs paths3))
               (count (:consumption paths3))
               4))
        (is (= (nth (vec (:net-emissions paths3)) 0)
               [1 35.85 36 2055 -2.5 50 50]))
        (is (= (nth (vec (:net-emissions paths3)) 1)
               [1 35.85 36 2055 -2.5 50 50]))
        (is (= (nth (vec (:net-emissions paths3)) 2)
               [2 35.85 36 2055 -2.5 50 50]))
        (is (= (nth (vec (:net-emissions paths3)) 3)
               [2 35.85 36 2055 -2.5 50 50]))
        (is (= (nth (vec (:cdr paths3)) 0)
               [3 20 2030 40]))
        (is (= (nth (vec (:cdr paths3)) 1)
               [4 20 2030 40]))
        (is (= (nth (vec (:cdr paths3)) 2)
               [3 20 2030 40]))
        (is (= (nth (vec (:cdr paths3)) 3)
               [4 20 2030 40]))

        (is (real= (nth (vec (:gross-gdp paths3)) 0)
                   [89.53589388 106.6639626 121.5553789 138.469817 153.5602228
                    166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
                    189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
                    182.8620026 172.770857 157.5834267]))
        (is (real= (nth (vec (:gross-gdp paths3)) 1)
                   [89.53589388 106.6639626 121.5553789 138.469817 153.5602228
                    166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
                    189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
                    182.8620026 172.770857 157.5834267]))
        (is (real= (nth (vec (:gross-gdp paths3)) 2)
                   [89.53589388 106.6639626 121.5553789 138.469817 153.5602228
                    166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
                    189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
                    182.8620026 172.770857 157.5834267]))
        (is (real= (nth (vec (:gross-gdp paths3)) 3)
                   [89.53589388 106.6639626 121.5553789 138.469817 153.5602228
                    166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
                    189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
                    182.8620026 172.770857 157.5834267]))

        (is (real= (nth (vec (:damages paths3)) 0)
                   [0.009463992 0.011902474 0.014329634 0.016936928 0.019208256
                    0.021420019 0.023619548 0.025871038 0.028196037 0.030560726
                    0.03311472 0.035771194 0.038564014 0.041391585 0.044282877
                    0.04715933 0.050010501 0.05258875]))
        (is (real= (nth (vec (:damages paths3)) 1)
                   [0.009463992 0.011902474 0.014329634 0.016936928 0.019208256
                    0.021420019 0.023619548 0.025871038 0.028196037 0.030560726
                    0.03311472 0.035771194 0.038564014 0.041391585 0.044282877
                    0.04715933 0.050010501 0.05258875]))
        (is (real= (nth (vec (:damages paths3)) 2)
                   [0.009463992 0.011902474 0.014329634 0.016936928 0.019208256
                    0.021420019 0.023619548 0.025871038 0.028196037 0.030560726
                    0.03311472 0.035771194 0.038564014 0.041391585 0.044282877
                    0.04715933 0.050010501 0.05258875]))
        (is (real= (nth (vec (:damages paths3)) 3)
                   [0.009463992 0.011902474 0.014329634 0.016936928 0.019208256
                    0.021420019 0.023619548 0.025871038 0.028196037 0.030560726
                    0.03311472 0.035771194 0.038564014 0.041391585 0.044282877
                    0.04715933 0.050010501 0.05258875]))

        (is (real= (nth (vec (:costs paths3)) 0)
                   [0.00000000 0.00016054 0.00051641 0.00120127 0.00205364
                    0.00273841 0.00313972 0.00329752 0.00330988 0.00328668
                    0.00321865 0.00315562 0.00311153 0.00312040 0.00322491
                    0.00349408 0.00404014 0.00511083]))
        (is (real= (nth (vec (:costs paths3)) 1)
                   [0.00000000 0.00016054 0.00051641 0.00120127 0.00205364
                    0.00273841 0.00313972 0.00329752 0.00330988 0.00328668
                    0.00321865 0.00315562 0.00311153 0.00312040 0.00322491
                    0.00349408 0.00404014 0.00511083]))
        (is (real= (nth (vec (:costs paths3)) 2)
                   [0.00000000 0.00016054 0.00051641 0.00120127 0.00205364
                    0.00273841 0.00313972 0.00329752 0.00330988 0.00328668
                    0.00321865 0.00315562 0.00311153 0.00312040 0.00322491
                    0.00349408 0.00404014 0.00511083]))
        (is (real= (nth (vec (:costs paths3)) 3)
                   [0.00000000 0.00016054 0.00051641 0.00120127 0.00205364
                    0.00273841 0.00313972 0.00329752 0.00330988 0.00328668
                    0.00321865 0.00315562 0.00311153 0.00312040 0.00322491
                    0.00349408 0.00404014 0.00511083]))

        (is (real= (nth (vec (:net-gdp-capita paths3)) 0)
                   [12.16791186 13.68917542 14.77295408 15.96820787 16.91416082
                    17.52970733 17.70512022 17.61593117 17.45006607 17.12952451
                    16.86980381 16.55161496 16.22029087 15.77777087 15.16414263
                    14.30647275 13.20428466 11.76619088]))
        (is (real= (nth (vec (:net-gdp-capita paths3)) 1)
                   [12.16791186 13.68917542 14.77295408 15.96820787 16.91416082
                    17.52970733 17.70512022 17.61593117 17.45006607 17.12952451
                    16.86980381 16.55161496 16.22029087 15.77777087 15.16414263
                    14.30647275 13.20428466 11.76619088]))
        (is (real= (nth (vec (:net-gdp-capita paths3)) 2)
                   [12.16791186 13.68917542 14.77295408 15.96820787 16.91416082
                    17.52970733 17.70512022 17.61593117 17.45006607 17.12952451
                    16.86980381 16.55161496 16.22029087 15.77777087 15.16414263
                    14.30647275 13.20428466 11.76619088]))
        (is (real= (nth (vec (:net-gdp-capita paths3)) 3)
                   [12.16791186 13.68917542 14.77295408 15.96820787 16.91416082
                    17.52970733 17.70512022 17.61593117 17.45006607 17.12952451
                    16.86980381 16.55161496 16.22029087 15.77777087 15.16414263
                    14.30647275 13.20428466 11.76619088]))

        (is (real= (nth (vec (:consumption paths3)) 0)
                   [9.475929745 11.27952518 12.0079858 12.8100472 13.91224362
                    14.79393293 15.36516271 15.48486907 15.6451508 15.54638309
                    15.49834064 15.38240049 15.27369427 15.10676197 14.75088413
                    14.12535991 13.18494447]))
        (is (real= (nth (vec (:consumption paths3)) 1)
                   [9.475929745 11.27952518 12.0079858 12.8100472 13.91224362
                    14.79393293 15.36516271 15.48486907 15.6451508 15.54638309
                    15.49834064 15.38240049 15.27369427 15.10676197 14.75088413
                    14.12535991 13.18494447]))
        (is (real= (nth (vec (:consumption paths3)) 2)
                   [9.475929745 11.27952518 12.0079858 12.8100472 13.91224362
                    14.79393293 15.36516271 15.48486907 15.6451508 15.54638309
                    15.49834064 15.38240049 15.27369427 15.10676197 14.75088413
                    14.12535991 13.18494447]))
        (is (real= (nth (vec (:consumption paths3)) 3)
                   [9.475929745 11.27952518 12.0079858 12.8100472 13.91224362
                    14.79393293 15.36516271 15.48486907 15.6451508 15.54638309
                    15.49834064 15.38240049 15.27369427 15.10676197 14.75088413
                    14.12535991 13.18494447]))

        (is (= (count (:net-emissions paths1))
               (count (:net-emissions paths2))
               (count (:net-emissions paths4))
               (count (:net-emissions paths5))
               (count (:net-emissions paths6))
               (count (:net-emissions paths7))
               (count (:net-emissions paths8))
               0))))))

(deftest cdr-emissions-test
  (testing "CDR emissions"
    (; Act
     let [paths (cdr-emissions [[1 20 2030 40]
                                [2 19.5 2040 40]]
                               (range 2015 2105 5))]

      ; Assert
      (is (= (:parameters paths)
             [[1 20 2030 40]
              [2 19.5 2040 40]]))

      (is (= (count (:paths paths))
             2))
      (is (real= (first (:paths paths))
                 [0 3.732109937 6.477108411 10 13.52289159 16.26789006
                  18.01989979 19 19.50821472 19.76139084 19.88499764
                  19.94475138 19.97349923 19.98729806 19.99391409 19.99708455
                  19.99860347 19.99933107]))
      (is (real= (second (:paths paths))
                 [0 0.975 1.930597709 3.638807189 6.315180701 9.75 13.1848193
                  15.86119281 17.56940229 18.525 19.02050935 19.26735607
                  19.3878727 19.4461326 19.47416175 19.48761561 19.49406624
                  19.49715743])))))
