;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.generator-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.generator :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest parameterize-net-emissions-ffi-test
  (testing "parameter tuples for linear-logistic curve of net FFI emissions"
    (; Act
     let [pars (parameterize-net-emissions-ffi
                [1 2]
                [2030 2050 2060]
                [[-10 20 50] [-20 15 60]])]

      ; Assert
      (is (= (:id pars)
             [1 2 3 4 5 6 7 8 9 10 11 12]))
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

(deftest damages-test
  (testing "calculate damages from climate change"
    (; Act
     let [damages1 (damages :howard-sterner2017
                            [1.2430000 1.4038090 1.5431011 1.6575053 1.7459433
                             1.8266544 1.8903494 1.9467692 1.9945795 2.0366409
                             2.0775002 2.1127794 2.1417328 2.1621153 2.1723421
                             2.1717380 2.1594992 2.1347266]
                            nil
                            nil
                            nil)
          damages2 (damages :howard-sterner2017
                            [1.243 1.38 1.503 1.624 1.722 1.812 1.897 1.98 2.062
                             2.142 2.225 2.308 2.392 2.474 2.555 2.633 2.708
                             2.774]
                            nil
                            nil
                            nil)
          damages3 (damages :burke2015
                            [1.2430000 1.4038090 1.5431011 1.6575053 1.7459433
                             1.8266544 1.8903494 1.9467692 1.9945795 2.0366409
                             2.0775002 2.1127794 2.1417328 2.1621153 2.1723421
                             2.1717380 2.1594992 2.1347266]
                            [89.53589388 106.6639626 121.5553789 138.469817
                             153.5602228 166.2937531 174.7730037 180.6741384
                             184.9654521 187.4794874 189.8661596 191.4636119
                             192.5362864 192.1001137 189.1970479 182.8620026
                             172.770857 157.5834267]
                            :SSP3
                            (range 2015 2105 5))]

      ; Assert
      (is (real= damages1
                 [0.009463992 0.012354731 0.015169599 0.017697401 0.019784902
                  0.021791560 0.023443577 0.024957304 0.026277108 0.027466329
                  0.028646767 0.029685969 0.030552671 0.031170285 0.031482500
                  0.031464015 0.031090669 0.030341802]))
      (is (real= damages2
                 [0.009463992 0.011902474 0.014329634 0.016936928 0.019208256
                  0.021420019 0.023619548 0.025871038 0.028196037 0.030560726
                  0.03311472 0.035771194 0.038564014 0.041391585 0.044282877
                  0.04715933 0.050010501 0.05258875]))

      (is (real= damages3
                 [0.00946399 0.01498606 0.02210866 0.03053700 0.04001501
                  0.05046079 0.06166733 0.07351090 0.08584262 0.09858591
                  0.11168037 0.12505723 0.13861372 0.15224439 0.16582261
                  0.17922455 0.19231440 0.20497860])))))

(deftest gross-gdp-test
  (testing "calculate gross gdp series"
    (; Arrange
     let [net-emissions [35.85 35.9 35.95 36 36.05 36.1 36.15 36.2 36.25
                         36.057535 35.904557 35.631935 35.150325 34.3125
                         32.89326 30.594137 27.12622 22.420509]
          cdr [0 3.732109937 6.477108411 10 13.52289159 16.26789006 18.01989979
               19 19.50821472 19.76139084 19.88499764 19.94475138 19.97349923
               19.98729806 19.99391409 19.99708455 19.99860347 19.99933107]
          ts (range 2015 2105 5)]

      ; Act
      (let [gdp1 (gross-gdp net-emissions cdr :SSP1 ts)
            gdp2 (gross-gdp net-emissions cdr :SSP2 ts)
            gdp3 (gross-gdp net-emissions cdr :SSP3 ts)
            gdp4 (gross-gdp net-emissions cdr :SSP4 ts)
            gdp5 (gross-gdp net-emissions cdr :SSP5 ts)]

        ; Assert
        (is (real= gdp1
                   [100.5790181 124.1196561 162.0130039 205.0305859 261.5050725
                    316.8535427 374.152292 427.6500221 478.0908023 524.3811473
                    584.7198012 645.9724607 733.8202884 829.117589 910.0697412
                    983.6767184 1010.774488 1006.192068]))
        (is (real= gdp2
                   [97.69423351 122.5739617 147.6701211 175.694137 205.0925417
                    231.4719711 253.0630391 270.239216 286.3821332 298.7974048
                    313.7928712 326.3865834 330.5525801 331.519745 333.9301172
                    329.0530684 321.0274516 301.3048358]))
        (is (real= gdp3
                   [89.53589388 106.6639626 121.5553789 138.469817 153.5602228
                    166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
                    189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
                    182.8620026 172.770857 157.5834267]))
        (is (real= gdp4
                   [92.8515159 111.076423 131.7284991 154.5992746 183.3947891
                    210.1294082 235.8195536 258.5874334 278.9684557 296.8542936
                    316.7281884 335.7606428 360.0828566 383.2462878 398.7007052
                    407.1990601 401.4610209 382.3391463]))
        (is (real= gdp5
                   [96.08938194 115.8751224 142.0862191 169.1714772 204.8251613
                    234.8539452 259.6033022 278.155945 290.949899 299.3757817
                    313.5391703 324.6091689 340.5686226 352.1212535 373.8950478
                    386.960929 395.8107441 388.9487175]))))))

(deftest capital-stock-test
  (testing "calculate capital stock series"
    (; Arrange
     let [gdp [89.5358939 106.6639626 121.5553789 138.4698170 153.5602228
               166.2937531 174.7730037 180.6741384 184.9654521 187.4794874
               189.8661596 191.4636119 192.5362864 192.1001137 189.1970479
               182.8620026 172.7708570 157.5834267]
          ts (range 2015 2105 5)]

      ; Act
      (let [capital1 (capital-stock gdp :SSP1 ts)
            capital2 (capital-stock gdp :SSP2 ts)
            capital3 (capital-stock gdp :SSP3 ts)
            capital4 (capital-stock gdp :SSP4 ts)
            capital5 (capital-stock gdp :SSP5 ts)]

        ; Assert
        (is (real= capital1
                   [226.73336450201262 268.1895796649148 251.63106634867685
                    256.36815501609635 239.58474364069448 222.0325474143312
                    196.6080715988674 171.6086784008327 150.78884420414605
                    131.21628087629148 116.23983914547381 102.9819858097869
                    92.77687084222106 82.32883264584963 71.73977848542005
                    59.508295741599646 47.16267406872911 33.884386467681225]))
        (is (real= capital2
                   [230.4997498 271.0043641 277.7746972 300.5375447 311.6130431
                    311.6363845 290.1127725 263.3679252 233.4252633 204.5735502
                    177.5715501 154.6466367 133.5519783 114.2622026 94.09775525
                    74.26821716 54.84473154 36.98749693]))
        (is (real= capital3
                   [171.57393927526059 199.4182545009106 210.50016275521764
                    236.36355889242245 274.01803716600796 295.17659700403374
                    300.92717251286786 290.09644008788064 277.3953911775567
                    256.44317597848004 235.13068689744915 213.21580483794932
                    190.89478127530225 166.56100879606018 137.38325370327297
                    105.67898652630969 73.39077175990889 44.53336820775048]))
        (is (real= capital4
                   [196.7824246568506 231.7375197589706 230.72399419289002
                    246.02850362664756 252.85514696781567 249.3821095158175
                    235.3748705513938 214.4017402739077 196.19466843125352
                    175.6116252520374 159.9121547906742 144.63427368033817
                    132.84926226179627 119.395967910925 104.17769202806357
                    85.62812317084658 65.70929516967465 44.94105311130746]))
        (is (real= capital5
                   [307.725014 362.3960547 347.1713046 356.9882661 330.7595839
                    309.1364062 276.065843 246.0266481 217.1729677 191.7483076
                    169.2488432 150.5187677 133.96354 118.6176715 102.5552375
                    85.83332127 68.53722793 50.97973782]))))))

(deftest costs-test
  (testing "calculate abatement cost"
    (; Arrange
     let [net-emissions [35.85 35.9 35.95 36 36.05 36.1 36.15 36.2 36.25
                         36.057535 35.904557 35.631935 35.150325 34.3125
                         32.89326 30.594137 27.12622 22.420509]
          cdr [0 3.732109937 6.477108411 10 13.52289159 16.26789006 18.01989979
               19 19.50821472 19.76139084 19.88499764 19.94475138 19.97349923
               19.98729806 19.99391409 19.99708455 19.99860347 19.99933107]
          ts (range 2015 2105 5)]

      ; Act
      (let [cost1 (costs :dice2016 net-emissions cdr :SSP1 ts)
            cost2 (costs :dice2016 net-emissions cdr :SSP2 ts)
            cost3 (costs :dice2016 net-emissions cdr :SSP3 ts)
            cost4 (costs :dice2016 net-emissions cdr :SSP4 ts)
            cost5 (costs :dice2016 net-emissions cdr :SSP5 ts)

            cost6 (costs :dice2013 net-emissions cdr :SSP1 ts)
            cost7 (costs :dice2013 net-emissions cdr :SSP2 ts)
            cost8 (costs :dice2013 net-emissions cdr :SSP3 ts)
            cost9 (costs :dice2013 net-emissions cdr :SSP4 ts)
            cost10 (costs :dice2013 net-emissions cdr :SSP5 ts)

            cost11 (costs :su2017 net-emissions cdr :SSP1 ts)]

        ; Assert
        (is (real= cost1
                   [0.00000000 0.00013796 0.00038745 0.00081129 0.00120593
                    0.00143719 0.00146662 0.00139314 0.00128054 0.00117507
                    0.00104514 0.00093531 0.00081639 0.00072297 0.00067044
                    0.00064954 0.00069058 0.00080043]))
        (is (real= cost2
                   [0.00000000 0.00013970 0.00042508 0.00094676 0.00153763
                    0.00196732 0.00216838 0.00220463 0.00213775 0.00206221
                    0.00194751 0.00185114 0.00181237 0.00180812 0.00182716
                    0.00194174 0.00217433 0.00267298]))
        (is (real= cost3
                   [0.00000000 0.00016054 0.00051641 0.00120127 0.00205364
                    0.00273841 0.00313972 0.00329752 0.00330988 0.00328668
                    0.00321865 0.00315562 0.00311153 0.00312040 0.00322491
                    0.00349408 0.00404014 0.00511083]))
        (is (real= cost4
                   [0.00000000 0.00015416 0.00047652 0.00107594 0.00171955
                    0.00216714 0.00232694 0.00230396 0.00219456 0.00207571
                    0.00192946 0.00179946 0.00166374 0.00156408 0.00153033
                    0.00156910 0.00173870 0.00210646]))
        (is (real= cost5
                   [0.00000000 0.00014778 0.00044179 0.00098326 0.00153964
                    0.00193899 0.00211376 0.00214188 0.00210419 0.00205823
                    0.00194908 0.00186127 0.00175907 0.00170234 0.00163186
                    0.00165116 0.00176352 0.00207067]))

        (is (real= cost6
                   [0.00000000 0.00004995 0.00015452 0.00034725 0.00054013
                    0.00066066 0.00068348 0.00065369 0.00060282 0.00055447
                    0.00049383 0.00044254 0.00038702 0.00034381 0.00032054
                    0.00031332 0.00033789 0.00039996]))
        (is (real= cost7
                   [0.00000000 0.00005058 0.00016952 0.00040523 0.00068870
                    0.00090436 0.00101052 0.00103445 0.00100635 0.00097309
                    0.00092020 0.00087586 0.00085917 0.00085987 0.00087357
                    0.00093666 0.00106386 0.00133566]))
        (is (real= cost8
                   [0.00000000 0.00005813 0.00020594 0.00051416 0.00091981
                    0.00125882 0.00146319 0.00154725 0.00155813 0.00155087
                    0.00152083 0.00149308 0.00147506 0.00148393 0.00154184
                    0.00168547 0.00197678 0.00255383]))
        (is (real= cost9
                   [0.00000000 0.00005582 0.00019004 0.00046052 0.00077018
                    0.00099621 0.00108441 0.00108106 0.00103309 0.00097946
                    0.00091168 0.00085141 0.00078871 0.00074381 0.00073165
                    0.00075690 0.00085072 0.00105258]))
        (is (real= cost10
                   [0.00000000 0.00005351 0.00017619 0.00042085 0.00068960
                    0.00089133 0.00098506 0.00100501 0.00099055 0.00097121
                    0.00092095 0.00088066 0.00083391 0.00080956 0.00078020
                    0.00079649 0.00086286 0.00103469]))

        (is (real= cost11
                   [0.00000000 0.00014160 0.00034400 0.00064784 0.00089958
                    0.00103110 0.00103083 0.00096921 0.00088652 0.00081063
                    0.00071953 0.00064261 0.00055928 0.00049295 0.00045349
                    0.00043352 0.00045121 0.00050679]))))))

(deftest net-gdp-test
  (testing "calculate gdp net of damages and abatement cost"
    (; Act
     let [gdp (net-gdp [89.5358939 106.6639626 121.5553789 138.4698170
                        153.5602228 166.2937531 174.7730037 180.6741384
                        184.9654521 187.4794874 189.8661596 191.4636119
                        192.5362864 192.1001137 189.1970479 182.8620026
                        172.7708570 157.5834267]
                       [0.009463992 0.011902474 0.014329634 0.016936928
                        0.019208256 0.021420019 0.023619548 0.025871038
                        0.028196037 0.030560726 0.03311472 0.035771194
                        0.038564014 0.041391585 0.044282877 0.04715933
                        0.050010501 0.05258875]
                       [0.00000000 0.00005813 0.00020594 0.00051416 0.00091981
                        0.00125882 0.00146319 0.00154725 0.00155813 0.00155087
                        0.00152083 0.00149308 0.00147506 0.00148393 0.00154184
                        0.00168547 0.00197678 0.00255383])]

      ; Assert
      (is (real= gdp
                 [88.68852688 105.38819752 119.78850116 136.05336776
                  150.46935200 162.52240435 170.38921842 175.72036255
                  179.46195859 181.45922195 183.29004127 184.32885948
                  184.82731240 183.86372328 180.52714731 173.93014386
                  163.78897020 148.89386990])))))

(deftest net-gdp-capita-test
  (testing "calculate net gdp per capita series"
    (; Act
     let [net-gdp (net-gdp-capita [88.68852690 105.37727373 119.75076240
                                   135.95822404 150.29524131 162.27635727
                                   170.09620605 175.40413431 179.13794592
                                   181.13379307 182.96768217 184.01054349
                                   184.51223192 183.54935632 180.20871485
                                   173.59941861 163.43248143 148.49092916]
                                  :SSP3
                                  (range 2015 2105 5))]

      ; Assert
      (is (real= net-gdp
                 [12.16791186 13.68917542 14.77295408 15.96820787 16.91416082
                  17.52970733 17.70512022 17.61593117 17.45006607 17.12952451
                  16.86980381 16.55161496 16.22029087 15.77777087 15.16414263
                  14.30647275 13.20428466 11.76619088])))))

(deftest consumption-test
  (testing "calculate consumption per capita series"
    (; Arrange
     let [net-gdp [89 105 120 136 150 163 170 176 179 181 183 184 185 184 181
                   174 164 149]
          ;capital-stock [197 232 231 246 253 249 235 214 196 176 160 145 133
          ;               119 104 86 66 45]
          investment [23.134694 18.801264 21.919362 21.547892 19.921206
                      17.593598 15.04697 13.927028 12.052792 11.214752 10.10432
                      9.47579 8.092966 6.746338 4.917808 3.043572 1.205532]
          ts (range 2015 2105 5)]

      ; Act
      (let [c1 (consumption net-gdp investment :SSP1 ts)
            c2 (consumption net-gdp investment :SSP2 ts)
            c3 (consumption net-gdp investment :SSP3 ts)
            c4 (consumption net-gdp investment :SSP4 ts)
            c5 (consumption net-gdp investment :SSP5 ts)]

        ; Assert
        (is (real= c1
                   [9.08618447 11.37771137 12.54385146 14.19659987 15.81437799
                    17.33347360 18.31675992 18.99923475 19.61468432 19.99313815
                    20.59369693 21.02967363 21.75130432 22.24740207 22.75280908
                    22.76246256 22.50338606]))
        (is (real= c2
                   [9.09827249 11.32517471 12.35798589 13.85284998 15.25930609
                    16.54767455 17.25897140 17.67597640 17.99600276 18.09170757
                    18.35256704 18.45473454 18.75590766 18.84221995 18.87146568
                    18.47388715 17.80500646]))
        (is (real= c3
                   [9.03660559 11.19776187 12.09963705 13.44232807 14.63901067
                    15.70735080 16.12888428 16.27707539 16.26255004 16.05631129
                    15.94115511 15.69832614 15.55172532 15.23659751 14.81690537
                    14.08866170 13.15273731]))
        (is (real= c4
                   [9.07095932 11.30274667 12.34890076 13.85859505 15.28159238
                    16.58814855 17.30138797 17.71961882 18.02498448 18.10569483
                    18.34438869 18.42424658 18.67687016 18.71454203 18.65966719
                    18.18378601 17.41673150]))
        (is (real= c5
                   [9.11882957 11.41402754 12.56960631 14.21059200 15.80832400
                    17.30410591 18.24909080 18.89182562 19.44864958 19.76775504
                    20.28577731 20.63665721 21.24116396 21.61630024 21.96771156
                    21.83072762 21.41187268]))))))

(deftest investment-test
  (testing "calculate investment series"
    (; Act
     let [xs (investment [307.725014 362.3960547 347.1713046 356.9882661
                          330.7595839 309.1364062 276.065843 246.0266481
                          217.1729677 191.7483076 169.2488432 150.5187677
                          133.96354 118.6176715 102.5552375 85.83332127
                          68.53722793 50.97973782]
                         (range 2015 2105 5))]

      ; Assert
      (is (real= xs
                 [36.13750224 26.63601165 30.39741649 23.99231653 22.7652359
                  18.7047773 16.60250569 14.37933845 12.70196838 11.20467701
                  10.11580366 9.016742572 7.902708153 6.502537731 5.055095816
                  3.570702011 2.10183802])))))
