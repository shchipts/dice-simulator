;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "DICE 2016 (Nordhaus 2017).

Based on DICE-2016R2-083017.gms available at https://sites.google.com/site/williamdnordhaus/dice-rice
Nordhaus, W. (2017). Revisiting the Social Cost of Carbon. PNAS, 114(7): 1518-1523."
      :author "Anna Shchiptsova"}
 dice-simulator.models.dice2016
  (:require [clojure.math.numeric-tower :as math]))

(defn- labor
  "Labor input in 2015-2020 and in n subsequent time periods (billions)."
  [n]
  (take (inc n) (iterate #(* % (math/expt (/ 11.5 %) 0.134)) 7.403)))

(defn- tfp
  "Total factor productivity in 2015-2020 and in n subsequent time periods
(unitless)."
  [n time-step]
  (reduce (fn [seed i]
            (->> (* time-step i)
                 (* 0.005)
                 -
                 (math/expt Math/E)
                 (* 0.076)
                 (- 1)
                 (/ (last seed))
                 (conj seed)))
          [5.115]
          (range n)))

(defn- land-use-emissions
  "CO2 emissions associated with land use change in 2015-2020 and in n
subsequent time periods (GtCO2 per year)."
  [n]
  (take (inc n) (iterate #(* % (- 1 0.115)) 2.6)))

(defn- non-co2-forcing
  "Non-CO2 radiative forcing in n time periods after 2015-2020 (W/m^2)."
  [n]
  (let [fex0 0.5 fex1 1.0]
    (->> (+ n 2)
         (range 2)
         (map #(+ fex0
                  (* (- fex1 fex0)
                     (if (< % 19)
                       (* (/ 1 17) (dec %))
                       1)))))))

(defn- climate-module
  "Climate-carbon module with prescribed data for CO2 emissions associated
with land use change and non-CO2 radiative forcing and prescribed time period."
  [non-co2-forcing land-use-emissions time-step]
  (fn [emissions]
    (let [mateq 588.0 mueq 360.0 mleq 1720.0
          b12 0.12 b23 0.007
          b11 (- 1 b12)
          b21 (* b12 (/ mateq mueq))
          b22 (- 1 b21 b23)
          b32 (* b23 (/ mueq mleq))
          b33 (- 1 b32)
          fco22x 3.6813
          t2xco2 3.1
          c1 0.1005 c3 0.088 c4 0.025]
      (->> (map vector emissions land-use-emissions non-co2-forcing)
           (reduce (fn [seed [e l f]]
                     (let [mat (:MAT seed) mu (:MU seed) ml (:ML seed)
                           tatm (:TATM seed) tocean (:TOCEAN seed)
                           mat_new (+ (* mat b11)
                                      (* mu b21)
                                      (/ (* (+ e l) time-step) 3.666))]
                       {:MAT mat_new
                        :ML (+ (* ml b33) (* mu b23))
                        :MU (+ (* mat b12) (* mu b22) (* ml b32))
                        :TATM (+ tatm
                                 (* c1
                                    (- (+ (/ (* fco22x
                                                (Math/log (/ mat_new 588.0)))
                                             (Math/log 2))
                                          f)
                                       (* (/ fco22x t2xco2) tatm)
                                       (* c3 (- tatm tocean)))))
                        :TOCEAN (+ tocean (* c4 (- tatm tocean)))
                        :temperature (conj (:temperature seed) tatm)}))
                   {:MAT 851.0
                    :ML 1740.0
                    :MU 460.0
                    :TATM 0.85
                    :TOCEAN 0.0068
                    :temperature []})
           ((juxt :temperature :TATM))
           (apply conj)))))

(defn- non-negative-emissions-minimum
  "Lower bound for any emissions pathway before non-negative emissions
become feasible (GtCO2 per time period)."
  [e1]
  (cons e1 (repeat 28 0)))

(defn- carbon-intensity
  "Carbon intensity in 2015-2020 and in n subsequent time periods
(tCO2/thousands 2010 USD)"
  [e1 n time-step]
  (->> (/ e1 (* 105.5 (- 1 0.03)))
       (vector -0.0152)
       (iterate (fn [[g s]]
                  (vector (* g (math/expt (inc -0.001) time-step))
                          (* s (math/expt Math/E (* g time-step))))))
       (map second)
       (take (inc n))))

(defn model
  "DICE 2016 modules and parameters.

Units:
  time step h - years
  depreciation rate \\delta - unitless
  initial industrial CO2 emissions E_ind(1) - GtCO2 per year
  initial global capital stock K(1) - trillions 2010 USD
  lower bound on emissions before non-negative emissions become feasible -
    GtCO2 per time period
  carbon intensity \\sigma(t) - tCO2/thousands 2010 USD (t = 1...n+1)
  labor input L(t) - billions (t = 1...n+1)
  total factor productivity A(t) - unitless (t = 1...n+1)
  capital elasticity \\alpha - unitless
  change in the global mean surface temperature - degree Celsius (t = 1...n+1)"
  [n time-step]
  (let [e1 35.85]
    {:n-steps n
     :time-step time-step
     :depreciation-rate 0.1
     :init {:industrial-emissions e1
            :capital-stock 223}
     :non-negative-emissions-minimum (non-negative-emissions-minimum e1)
     :carbon-intensity (carbon-intensity e1 n time-step)
     :cobb-douglas {:labor (labor n)
                    :tfp (tfp n time-step)
                    :capital-elasticity 0.3}
     :climate-module (climate-module (non-co2-forcing n)
                                     (land-use-emissions n)
                                     time-step)}))
