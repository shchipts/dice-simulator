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

(def ^:private burkedf
  "Parameters for damage function from Burke et al (2015)"
  {:T2015-absolute 14.83
   :T2015 1.243
   :Tavg1980_2010 14.42
   :b1 0.0127
   :b2 -0.0005})

(defn parameterize-net-emissions-ffi
  "Returns a map of all ordered pairs (y_ x1 K midpoint-offset dt), where y_ is
in y_s, x1 is in x1s and (K, midpoint-offset, dt) is in logistic-pars. The set
of nth items of map values represents a single net FFI emissions curve"
  [y_s x1s logistic-pars]
  (->> (comb/cartesian-product y_s x1s logistic-pars)
       (map #(conj (flatten %2) (inc %1)) (range))
       (apply map vector)
       (zipmap [:id :y_ :x1 :K :midpoint-offset :dt])))

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
Sterner 2017; Hansel et al. 2020) (:howard-sterner2017), Burke et al. 2015
(Burke et al. 2015; Glanemann et al. 2020) (:burke2015). For :burke2015, damages
start at the value estimated with :howard-sterner2017

[1] Howard, P., & Sterner, T. (2017). Few and Not So Far Between: A
Meta-analysis of Climate Damage Estimates. Environmental and Resource
Economics, 68
[2] Hansel, M., Drupp, M., Johansson, D., Nesje, F., Azar, C, Freeman, M.,
Groom, B., & Sterner, T. (2020). Climate Economics Support for the UN Climate
Targets. Nature Climate Change, 10
[3] Burke, M., Hsiang, S., & Miguel, E. (2015). Global Non-linear Effect of
Temperature on Economic Production. Nature, 527: 235-239.
doi:10.1038/nature15725
[4] Glanemann, N., Willner, S., & Levermann, A. (2020). Paris Climate Agreement
Passes the Cost-benefit Test. Nature Communications, 11: 110.
doi.org/10.1038/s41467-019-13961-1"
  [damage-function temperature gross-gdp ssp ts]
  (let [howard-sternerf #(-> (- % 0.115)
                             (math/expt 2)
                             (* 0.007438))]
    (condp = damage-function
      :howard-sterner2017 (map howard-sternerf temperature)
      :burke2015 (let [hf #(+ (* (:b1 burkedf) %)
                              (* (:b2 burkedf) (math/expt % 2)))
                       base (hf (:Tavg1980_2010 burkedf))]
                   (->> (translator/baseline-labor ssp ts)
                        (map list gross-gdp temperature)
                        ((juxt (fn [[[Y T L] _]]
                                 (->> (howard-sternerf T)
                                      ((juxt vector
                                             #(/ (* Y (- 1 %)) L)))
                                      (#(conj % (/ Y L)))))
                               #(map
                                 (fn [t1 t0 coll]
                                   (conj coll (- t1 t0)))
                                 (rest ts)
                                 ts
                                 (rest %))))
                        (apply
                         reduce
                         (fn [[xs prev-Qc prev-Yc] [step Y T L]]
                           (let [Yc (/ Y L)]
                             (->> (/ Yc prev-Yc)
                                  (#(math/expt % (double (/ 1 step))))
                                  (+ (-> ((juxt :T2015-absolute
                                                :T2015)
                                          burkedf)
                                         (#(apply - %))
                                         (+ T)
                                         hf
                                         (- base)))
                                  (#(math/expt % step))
                                  (* prev-Qc)
                                  ((juxt (fn [Qc]
                                           (->> (* Qc L)
                                                (#(/ % Y))
                                                (- 1)
                                                (conj xs)))
                                         identity
                                         (fn [_] (identity Yc))))))))
                        first)))))

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
