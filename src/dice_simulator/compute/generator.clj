;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Creates some entities for DICE-like simulation model"
      :author "Anna Shchiptsova"}
 dice-simulator.compute.generator
  (:require [clojure.math.combinatorics :as comb]
            [clojure.math.numeric-tower :as math]
            [dice-simulator.compute.SSPs :as ssp-db]
            [dice-simulator.compute.translator :as translator]
            [utilities-clj.floating-point-comparison :refer :all]))

(def ^:private dice-cost-pars
  "Parameters for generic DICE abatement cost function"
  {:dice2016 {:scale1 550 :power1 2.6}
   :dice2013 {:scale1 344 :power1 2.8}
   :su2017 {:scale1 245.8 :power1 2.3 :scale2 448.4 :power2 12.4}})

(defn parameterize-net-emissions-ffi
  "Returns a map of all ordered pairs (y_ x1 K midpoint-offset dt), where y_ is
in y_s, x1 is in x1s and (K, midpoint-offset, dt) is in logistic-pars. The set
of nth items of map values represents a single net FFI emissions curve"
  [y_s x1s logistic-pars]
  (->> (comb/cartesian-product y_s x1s logistic-pars)
       (map flatten)
       (apply map vector)
       (zipmap [:y_ :x1 :K :midpoint-offset :dt])))

(defn infill-net-emissions-land-use
  "Returns SSP expected radiative forcing in the year 2100 scenario for which
euclidian distance between net FFI emissions curve and scenario net FFI
emissions is minimal"
  [net-emissions-curve ssp]
  (->> (get-in ssp-db/ssp-and-radiative-forcing [ssp :ffi-emissions])
       keys
       (reduce
        (fn [seed k]
          (->> (:time-scale ssp-db/ssp)
               ((juxt #(translator/net-emissions-ffi net-emissions-curve %)
                      #(translator/ssp-emissions-ffi ssp k %)))
               (apply map #(math/expt (- %1 %2) 2))
               (apply +)
               math/sqrt
               (#(if (or (nil? seed)
                         (real< % (second seed)))
                   (vector k %)
                   seed))))
        nil)
       first))

(defn damages
  "Returns collection of global climate damages corresponding to specified
damage function. Supported damage functions: Howard and Sterner 2017 (Howard &
Sterner 2017; Hansel et al. 2020) (:howard-sterner2017)

[1] Howard, P., & Sterner, T. (2017). Few and Not So Far Between: A
Meta-analysis of Climate Damage Estimates. Environmental and Resource
Economics, 68
[2] Hansel, M., Drupp, M., Johansson, D., Nesje, F., Azar, C, Freeman, M.,
Groom, B., & Sterner, T. (2020). Climate Economics Support for the UN Climate
Targets. Nature Climate Change, 10"
  [damage-function temperature]
  (condp = damage-function
    :howard-sterner2017 (map
                         #(-> (- % 0.115)
                              (math/expt 2)
                              (* 0.007438))
                         temperature)))

(defn gross-gdp
  "Returns gross GDP series corresponding to time points ts based on SSP
baseline; measured in trillion 2010 USD"
  [net-emissions cdr-emissions ssp ts]
  (map
   #(/ (+ %1 %2) %3)
   net-emissions
   cdr-emissions
   (translator/baseline-carbon-intensity ssp ts)))

(defn capital-stock
  "Returns capital stock series corresponding to time points ts based on GDP
series and SSP baseline; measured in trillion 2010 USD"
  [gross-gdp ssp ts]
  (let [alpha (get-in ssp-db/ssp [ssp :capital-elasticity])]
    (map
     (fn [gdp tfp labor]
       (->> (- 1 alpha)
            (math/expt labor)
            (* tfp)
            (/ gdp)
            (#(math/expt % (/ 1 alpha)))))
     gross-gdp
     (translator/baseline-tfp ssp ts)
     (translator/baseline-labor ssp ts))))

(defn costs
  "Returns abatement cost corresponding to time points ts based on SSP baseline;
measures the ratio of the abatement cost to the output. Supported cost
functions: DICE2013 (:dice2013), DICE2016 (:dice2016), Su et al. 2017 (:su2017)

[1] DICE2013
www.econ.yale.edu/~nordhaus/homepage/homepage/DICE2013R_100413_vanilla.gms
[2] DICE2016
www.econ.yale.edu/~nordhaus/homepage/homepage/DICE2016R-091916ap.gms
[3] Su, X., Takahashi, K., Fujimori, S., Hasegawa, T., Tanaka, K., Kato, E.,
Shiogama, X., Masui, T, & Emori, S. (2017). Emission Pathways to Achieve 2.0C
and 1.5C Climate Targets. Earth's Future, 5: 592–604. DOI:10.1002/2016EF000492"
  [cost-function net-emissions cdr-emissions ssp ts]
  (map
   (fn [e cdr sigma t]
     (->> (range 2015 2105 5)
          (map-indexed vector)
          (drop-while #(not= t (second %)))
          ffirst
          inc
          (math/expt (- 1 0.025))
          (#(/ % 1000))
          (* sigma)
          (* (->> (get dice-cost-pars cost-function)
                  (#(map
                     (fn [[k1 k2]]
                       (let [scale (get % k1 0) pow (get % k2 1)]
                         (fn [x] (* (/ scale pow) (math/expt x pow)))))
                     [[:scale1 :power1] [:scale2 :power2]]))
                  (reduce
                   (fn [seed costf]
                     (+ seed (costf (- 1 (/ e (+ e cdr))))))
                   0)))))
   net-emissions
   cdr-emissions
   (translator/baseline-carbon-intensity ssp ts)
   ts))

(defn net-gdp
  "Returns gdp net of damages and abatement costs; measured in trillion 2010
USD"
  [gross-gdp damages costs]
  (map #(* %1 (- 1 %2 %3)) gross-gdp damages costs))

(defn net-gdp-capita
  "Returns net gdp per capita; measured in thousands 2010 USD per year"
  [net-gdp ssp ts]
  (map / net-gdp (translator/baseline-labor ssp ts)))

(defn consumption
  "Returns consumption per capita series corresponding to time points ts based
on net-GDP and capital-stock series and SSP baseline; measured in thousands
2010 USD per year"
  [net-gdp capital-stock ssp ts]
  (->> (vector capital-stock ts)
       (reduce
        (fn [seed coll]
          (into seed ((juxt identity rest) coll)))
        [])
       (apply
        map
        (fn [gdp labor K K-next t t-next]
          (let [h (- t-next t)]
            (-> (- 1 0.1)
                (math/expt h)
                (* K)
                (#(- K-next %))
                (/ h)
                (#(- gdp %))
                (/ labor))))
        net-gdp
        (translator/baseline-labor ssp ts))))
