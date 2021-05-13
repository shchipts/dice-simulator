;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "DICE-like generic model."
      :author "Anna Shchiptsova"}
 dice-simulator.compute.generic-structure
  (:require [clojure.math.numeric-tower :as math]))

(defn output
  "Output gross of abatement cost and climate damage."
  [{{labor :labor tfp :tfp a :capital-elasticity} :cobb-douglas}
   capital-stock_t
   t]
  (* (nth tfp t)
     (math/expt (nth labor t) (- 1 a))
     (math/expt capital-stock_t a)))

(defn capital-stock
  "The law of motion of the capital stock."
  [{d :depreciation-rate h :time-step} capital-stock investment]
  (+ (* (math/expt (- 1 d) h)
        capital-stock)
     (* h investment)))

(defn investment
  "The total amount of final goods in the closed economy must be
either consumed or invested."
  [{{labor :labor} :cobb-douglas} gdp_t consumption-per-capita_t t]
  (- gdp_t
     (* consumption-per-capita_t
        (nth labor t))))

(defn emissions
  "Industrial CO2 emissions from the produced output after abatement."
  [{s :carbon-intensity :as model-instance}
   rate_t
   capital-stock_t
   t]
  (* (nth s t)
     (- 1 rate_t)
     (output model-instance capital-stock_t t)))
