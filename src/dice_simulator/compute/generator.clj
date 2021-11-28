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
