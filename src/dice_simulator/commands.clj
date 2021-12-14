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
  (:require [dice-simulator.compute.generator :as generator]
            [dice-simulator.compute.condition :as condition]
            [dice-simulator.compute.translator :as translator]))

(defn- net-emissions-pipeline
  "Returns net FFI emissions curves corresponding to SSP scenario"
  [y0 y_s x1s logistic-pars ssp]
  (->> (generator/parameterize-net-emissions-ffi y_s x1s logistic-pars)
       ((juxt (fn [_] (repeat y0))
              :y_
              :x1
              :K
              :midpoint-offset
              :dt))
       (apply map list)
       (filter #(and (condition/limiting-case-ffi %)
                     (condition/baseline-ffi % ssp)))
       (map #(vector % (generator/infill-net-emissions-land-use % ssp)))
       (filter
        (fn [[curve scenario]]
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
                   ((juxt (partial map (comp rest first))
                          (partial
                           map
                           (fn [[curve _]]
                             (translator/net-emissions-ffi curve ts)))))
                   (zipmap [:parameters :paths])))
            %))
    [:SSP1 :SSP2 :SSP3 :SSP4 :SSP5])))
