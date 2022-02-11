;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Procedures for DICE-like simulation model"
      :author "Anna Shchiptsova"}
 dice-simulator.commands
  (:require [clojure.math.combinatorics :as comb]
            [dice-simulator.compute.generator :as generator]
            [dice-simulator.compute.condition :as condition]
            [dice-simulator.compute.translator :as translator]))

(defn- net-emissions-pipeline
  "Returns net FFI emissions curves corresponding to SSP scenario"
  [y0 y_s x1s logistic-pars ssp]
  (->> (generator/parameterize-net-emissions-ffi y_s x1s logistic-pars)
       ((juxt :id
              (fn [_] (repeat y0))
              :y_
              :x1
              :K
              :midpoint-offset
              :dt))
       (apply map list)
       (filter #(and (condition/limiting-case-ffi (rest %))
                     (condition/baseline-ffi (rest %) ssp)))
       (map (juxt first
                  rest
                  #(generator/infill-net-emissions-land-use (rest %) ssp)))
       (filter
        (fn [[_ curve scenario]]
          (condition/emissions-quota curve ssp scenario)))))

(defn net-emissions-ffi
  "Returns parameters and values corresponding to time points ts on the net FFI
emissions curves in SSP scenarios (GtCO2) represented by a map of all ordered
pairs (y0 y_ x1 K midpoint-offset dt), where y_ is in y_s, x1 is in x1s and
(K, midpoint-offset, dt) is in logistic-pars. All curves should not exceed the
lower limiting case of minimum gross FFI emissions in deep mitigation pathways
and maximum capacity in massive CDR deployment (Kriegler et al. 2018), should
not surpass the upper limiting case of SSP baseline, and cumulative net amount
of CO2 (FFI and AFOLU) realised to the atmosphere should not exceed remaining
emissions quota. AFOLU values are constructed based on SSP land use emissions
using linear interpolation. Remaining emissions quota, measured in GtCO2, is
taken from Friedlingstein et al. (2014) for 50% probability of global-mean
warming below 3 °C.

A single net emissions curve starts from point with coordinates
(2015, y0), linearly increases until time point x1 (with extrapolation to point
with coordinates (2030, y_)), and follows as generalized logistic curve with
carrying capacity K which reaches K/2 at midpoint and has the length dt of the
time interval needed to de-grow from 5% of K to 95% of K. Net emissions phase
linearly from x1 through x1 + 5 to remove inconsistent initial rate of degrowth.

[1] [SSP Database] https://tntcat.iiasa.ac.at/SspDb/
[2] Kriegler, E., Luderer, G., Bauer, N., Baumstark, L., Fujimori, S., Popp, A.,
Rogelj, J., Strefler, J., & van Vuuren, D. (2018). Pathways Limiting Warming To
1.5°C: A Tale Of Turning Around In No Time?. Philosophical Transactions A, 376:
20160457
[3] Friedlingstein, P., Andrew, R., Rogelj, J., Peters, G., Canadell, J.,
Knutti, R., Luderer, G., Raupach, M., Schaeffer, M., van Vuuren, D., &
Le Quere, C. (2014). Persistent Growth of CO2 Emissions and Implications for
Reaching Climate Targets. Nature Geoscience, Advanced Online Publication"
  [y0 y_s x1s logistic-pars ts]
  (apply
   zipmap
   ((juxt identity
          #(map
            (fn [ssp]
              (->> (net-emissions-pipeline y0 y_s x1s logistic-pars ssp)
                   ((juxt (partial
                           map
                           (fn [[id curve _]]
                             (conj (rest curve) id)))
                          (partial
                           map
                           (fn [[_ curve __]]
                             (translator/net-emissions-ffi curve ts)))))
                   (zipmap [:parameters :paths])))
            %))
    [:SSP1 :SSP2 :SSP3 :SSP4 :SSP5])))

(defn economic-growth
  [net-emissions-pars
   cdr-pars
   temperature-curves
   damage-function
   cost-function
   ssp
   ts]
  (->> (map list net-emissions-pars temperature-curves)
       (#(comb/cartesian-product % cdr-pars))
       (filter
        (fn [[[pars1 _] pars2]]
          (and (condition/baseline-ffi (rest pars1) (rest pars2) ssp)
               (condition/non-negative-gross-gdp
                (rest pars1)
                (rest pars2)
                ssp
                ts))))
       (map
        (fn [[[pars1 temperature] pars2]]
          (list pars1
                pars2
                (translator/net-emissions-ffi (rest pars1) ts)
                (translator/cdr-emissions (rest pars2) ts)
                temperature)))
       (map
        (fn [[pars1 pars2 net-emissions cdr temperature]]
          (let [gross-gdp (generator/gross-gdp net-emissions cdr ssp ts)
                damages (generator/damages damage-function
                                           temperature
                                           gross-gdp
                                           ssp
                                           ts)
                costs (generator/costs cost-function net-emissions cdr ssp ts)
                net-gdp (generator/net-gdp gross-gdp damages costs)
                investment (generator/investment
                            (generator/capital-stock gross-gdp ssp ts)
                            ts)]
            (list pars1
                  pars2
                  gross-gdp
                  damages
                  costs
                  (generator/net-gdp-capita net-gdp ssp ts)
                  (generator/consumption net-gdp investment ssp ts)
                  investment))))
       (filter
        (fn [values]
          (->> (iterate drop-last values)
               (map
                (fn [xs]
                  (->> (last xs)
                       (drop-while #(not (neg? %)))
                       seq
                       nil?)))
               (take 2)
               (every? identity))))
       (#(if (empty? %) % (drop-last (apply map list %))))
       (zipmap [:net-emissions
                :cdr
                :gross-gdp
                :damages
                :costs
                :net-gdp-capita
                :consumption])))

(defn cdr-emissions
  "Returns parameters and values corresponding to time points ts on the CDR
emissions curves (GtCO2) represented by collection of logistic parameters
(K midpoint dt). A single CDR emissions curve linearly increases from 0 until
2020, and follows as generalized logistic curve with carrying capacity K which
reaches K/2 at midpoint and has the length dt of the time interval needed to
grow from 5% of K to 95% of K. CDR emissions are introduced linearly from 2015
through 2020 (with extrapolation to logistic curve value in 2020) to remove
inconsistent initial rate of growth"
  [cdr-pars ts]
  (zipmap [:parameters :paths]
          ((juxt identity
                 #(map
                   (fn [cdr]
                     (translator/cdr-emissions (rest cdr) ts))
                   %))
           cdr-pars)))
