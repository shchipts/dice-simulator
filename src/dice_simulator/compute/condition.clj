;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Conditions on simulations from DICE-like simulation model"
      :author "Anna Shchiptsova"}
 dice-simulator.compute.condition
  (:require [dice-simulator.compute.generator :as generator]
            [dice-simulator.compute.time-scale :as t-scale]
            [dice-simulator.compute.translator :as translator]
            [utilities-clj.floating-point-comparison :refer :all]))

(def ^{:private true} limiting-case
  "Limiting case for minimum gross FFI emissions in deep mitigation pathways
and maximum capacity in massive CDR deployment (Kriegler et al. 2018) measured
in GtCO2"
  (merge-with -
              {2030 14.8
               2040 5.7
               2050 1.5
               2060 1.6667
               2070 0.8333
               2080 0.5
               2100 0.5}
              {2030 1
               2040 3.73
               2050 10
               2060 16.26
               2070 19
               2080 19.76
               2100 19.9873}))

(defn limiting-case-ffi
  "Indicates whether net FFI emissions exceed the lower limiting case of minimum
gross FFI emissions in deep mitigation pathways and maximum capacity in massive
CDR deployment (Kriegler et al. 2018). Returns true if net FFI emissions is
feasible; false, otherwise

[1] Kriegler, E., Luderer, G., Bauer, N., Baumstark, L., Fujimori, S., Popp, A.,
Rogelj, J., Strefler, J., & van Vuuren, D. (2018). Pathways Limiting Warming To
1.5°C: A Tale Of Turning Around In No Time?. Philosophical Transactions A, 376:
20160457"
  [net-emissions-curve]
  (->> (keys limiting-case)
       (translator/net-emissions-ffi net-emissions-curve)
       (map list (vals limiting-case))
       (some (fn [[limit e]] (real< e limit)))
       not))

(defn baseline-ffi
  "Indicates whether gross FFI emissions surpass the upper limiting case of
SSP baseline. Assumes zero CDR emissions if cdr-emissions-curve not supplied.
Returns true if gross FFI emissions are feasible; false, otherwise"
  ([net-emissions-curve ssp]
   (baseline-ffi net-emissions-curve nil ssp))
  ([net-emissions-curve cdr-emissions-curve ssp]
   (->> (rest t-scale/ts)
        ((juxt #(translator/net-emissions-ffi net-emissions-curve %)
               #(if (nil? cdr-emissions-curve)
                  (repeat 0)
                  (translator/cdr-emissions cdr-emissions-curve %))
               #(translator/baseline-emissions-ffi ssp %)))
        (apply map list)
        (some (fn [[e cdr baseline]] (real> (+ e cdr) baseline)))
        not)))

(defn emissions-quota
  "Indicates whether cumulative net amount of CO2 realised to the atmosphere
exceeds remaining emissions quota. Returns true if net FFI emissions is
feasible; false, otherwise

Remaining emissions quota, measured in GtCO2, is taken from Friedlingstein et
al. (2014) for 50% probability of global-mean warming below 3 °C.

[1] Friedlingstein, P., Andrew, R., Rogelj, J., Peters, G., Canadell, J.,
Knutti, R., Luderer, G., Raupach, M., Schaeffer, M., van Vuuren, D., &
Le Quere, C. (2014). Persistent Growth of CO2 Emissions and Implications for
Reaching Climate Targets. Nature Geoscience, Advanced Online Publication"
  [net-emissions-curve ssp scenario]
  (let [quota 3300]
    (->> ((juxt (partial translator/net-emissions-ffi net-emissions-curve)
                (partial translator/net-emissions-land-use ssp scenario)
                (fn [ts]
                  (apply map - ((juxt rest identity) ts))))
          t-scale/ts)
         (apply
          map
          (fn [ffi land-use step]
            (* (+ ffi land-use) step)))
         (reduce
          (fn [s e]
            (if (real> (+ s e) quota)
              (reduced nil)
              (+ s e)))
          0)
         nil?
         not)))

(defn non-negative-gross-gdp
  "Indicates whether gross GDP series is feasible. Returns true if series is
non-negative; false, otherwise"
  [net-emissions-curve cdr-emissions-curve ssp ts]
  (->> (generator/gross-gdp
        (translator/net-emissions-ffi net-emissions-curve ts)
        (translator/cdr-emissions cdr-emissions-curve ts)
        ssp
        ts)
       (drop-while #(not (neg? %)))
       seq
       nil?))
