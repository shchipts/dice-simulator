;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Converts function domain values into codomain values"
      :author "Anna Shchiptsova"}
 dice-simulator.compute.translator
  (:require [clojure.math.numeric-tower :as math]
            [dice-simulator.compute.SSPs :as ssp-db]
            [dice-simulator.compute.time-scale :as t-scale]))

(defn- linear-interpolation
  "Returns value corresponding to time point t on the straight line between
two points with coordinates (x0, y0) and (x1, y1)"
  [x0 y0 x1 y1 t]
  (->> (- x1 x0)
       (/ (- y1 y0))
       (* (- t x0))
       (+ y0)))

(defn- logistic-function
  "Returns value corrresponding to time point t on the generalized logistic
curve with carrying capacity K and lower asymptote A which reaches K/2 at
midpoint and has the length dt of the time interval needed to grow from 5% of K
to 95% of K"
  ([K midpoint dt t]
   (logistic-function K midpoint dt 0 t))
  ([K midpoint dt A t]
   (->> (- t midpoint)
        (#(/ % dt))
        (* (- (Math/log (* 19 19))))
        (math/expt Math/E)
        inc
        (/ (- K A))
        (+ A))))

(defn- interpolate
  "Returns values corresponding to time points ts using linear interpolation"
  [coll transformf ts]
  (map
   (fn [t]
     (->> (:time-scale ssp-db/ssp)
          (map-indexed vector)
          ((juxt first rest))
          (apply
           reduce
           (fn [seed x]
             (if (<= t (second x))
               (reduced (vector seed x))
               x)))
          ((fn [[[idx0 x0] [idx1 x1]]]
             (->> ((juxt #(get % idx0)
                         #(get % idx1))
                   coll)
                  (map transformf)
                  ((fn [[y0 y1]]
                     (linear-interpolation x0 y0 x1 y1 t))))))))
   ts))

(defn- select-from-ssp-scenario
  "Returns values corresponding to time points ts based on SSP scenario. Values
are constructed using linear interpolation; measured in GtCO2"
  [ks ts]
  (interpolate
   (get-in ssp-db/ssp-and-radiative-forcing ks)
   #(/ % 1000)
   ts))

(defn- baseline-gdp
  "Returns gross gdp corresponding to time points ts based on SSP baseline GDP
with no climate change and no abatement; measured in trillion 2010 USD"
  [ssp ts]
  (-> (vector ssp :gdp)
      (#(get-in ssp-db/ssp %))
      (interpolate #(/ (* % 1.134789528) 1000) ts)))

(defn net-emissions-ffi
  "Returns values corresponding to time points ts on the net FFI emissions
curve. Net emissions curve starts from point with coordinates (2015, y0),
linearly increases until time point x1 (with extrapolation to point with
coordinates (2030, y_)), and follows as generalized logistic curve with carrying
capacity K which reaches K/2 at midpoint and has the length dt of the time
interval needed to de-grow from 5% of K to 95% of K. Net emissions phase
linearly from x1 through x1 + 5 to remove inconsistent initial rate of degrowth"
  [[y0 y_ x1 K midpoint-offset dt] ts]
  (let [linear-fn
        #(linear-interpolation (first t-scale/ts) y0 t-scale/t1 y_ %)
        logistic-fn
        #(logistic-function
          K
          (+ x1 midpoint-offset)
          dt
          (linear-interpolation (first t-scale/ts) y0 t-scale/t1 y_ x1)
          %)
        shift-logistic (+ x1 5)]
    (map
     (fn [t]
       (if (> t shift-logistic)
         (logistic-fn t)
         (if (> t x1)
           (linear-interpolation
            x1
            (linear-fn x1)
            shift-logistic
            (logistic-fn shift-logistic)
            t)
           (linear-fn t))))
     ts)))

(defn baseline-emissions-ffi
  "Returns values corresponding to time points ts based on SSP baseline
FFI emissions with no climate change and no abatement. Values are constructed
using linear interpolation; measured in GtCO2"
  [ssp ts]
  (select-from-ssp-scenario [ssp :ffi-emissions :baseline] ts))

(defn net-emissions-land-use
  "Returns values corresponding to time points ts based on SSP land use
emissions. Values are constructed using linear interpolation; measured in GtCO2"
  [ssp scenario ts]
  (select-from-ssp-scenario [ssp :land-use-emissions scenario] ts))

(defn ssp-emissions-ffi
  "Returns values corresponding to time points ts based on SSP FFI emissions.
Values are constructed using linear interpolation; measured in GtCO2"
  [ssp scenario ts]
  (select-from-ssp-scenario [ssp :ffi-emissions scenario] ts))

(defn cdr-emissions
  "Returns values corresponding to time points ts on the CDR emissions curve.
CDR emissions curve starts from point with coordinates (2015, 0), linearly
increases until 2020, and follows as generalized logistic curve with carrying
capacity K which reaches K/2 at midpoint and has the length dt of the time
interval needed to grow from 5% of K to 95% of K. CDR emissions are introduced
linearly from 2015 through 2020 (with extrapolation to logistic curve value in
2020) to remove inconsistent initial rate of growth"
  [[K midpoint dt] ts]
  (let [x0 2015
        x1 2020
        y0 0
        logistic-fn #(logistic-function K midpoint dt %)]
    (map
     (fn [t]
       (if (> t x1)
         (logistic-fn t)
         (linear-interpolation x0 0 x1 (logistic-fn x1) t)))
     ts)))

(defn baseline-carbon-intensity
  "Returns carbon intensity series corresponding to time points ts based on SSP
baseline FFI emissions and GDP with no climate change and no abatement; measured
in tCO2/thousands 2010 USD"
  [ssp ts]
  (map
   #(/ %1 %2)
   (baseline-emissions-ffi ssp ts)
   (-> (vector ssp :gdp)
       (#(get-in ssp-db/ssp %))
       (interpolate #(/ (* % 1.134789528) 1000) ts))))

(defn baseline-labor
  "Returns SSP-based labor input corresponding to time points ts based on SSP
baseline population; measured in billions"
  [ssp ts]
  (-> (vector ssp :population)
      (#(get-in ssp-db/ssp %))
      (interpolate #(/ % 1000) ts)))

(defn baseline-tfp
  "Returns total factor productivity corresponding to time points ts based on
SSP baseline population and GDP with no climate change and no abatement;
unitless"
  [ssp ts]
  (let [alpha (get-in ssp-db/ssp [ssp :capital-elasticity])
        p_k 0.12]
    (map
     (fn [gdp labor]
       (-> (* alpha gdp)
           (/ p_k)
           (math/expt alpha)
           (* (math/expt labor (- 1 alpha)))
           (#(/ gdp %))))
     (baseline-gdp ssp ts)
     (baseline-labor ssp ts))))
