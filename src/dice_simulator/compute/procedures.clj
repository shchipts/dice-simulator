;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Collection of procedures for simulations from DICE-like model."
      :author "Anna Shchiptsova"}
 dice-simulator.compute.procedures
  (:require [dice-simulator.compute.binary-search :as binary-search]
            [dice-simulator.compute.generic-structure :as generic]
            [utilities-clj.floating-point-comparison :refer :all]))

(defn- append-emissions
  "Append emission values for which climate module produces feasible output."
  [{ecoll-min :non-negative-emissions-minimum
    cum-e :max-cumulative-emissions
    h :time-step
    climate-module :climate-module}
   emissions
   emissions-max
   feasible?
   grid-step
   t]
  (let [e-min (if-not (nil? ecoll-min) (nth ecoll-min t 0) 0)
        e-max (->> (apply + emissions)
                   (- (/ cum-e h))
                   (max 0)
                   (min (nth emissions-max t)))]
    (map #(conj emissions %)
         (binary-search/run e-min
                            e-max
                            (fn [e]
                              (->> (conj emissions e)
                                   ((fn [x]
                                      (->> ((fnil #(split-at (count x) %) [])
                                            ecoll-min)
                                           second
                                           (concat x))))
                                   climate-module
                                   feasible?))
                            grid-step))));)

(defn- emissions-max
  "Emissions in case of minimum consumption, no abatement, and no damages."
  [{n :n-steps init :init :as model}]
  (->> (inc n)
       (range 1)
       (reduce (fn [[k coll] t]
                 (->> (generic/output model k (dec t))
                      (generic/capital-stock model k)
                      ((juxt identity
                             #(conj coll (generic/emissions model 0 % t))))))
               (->> (:industrial-emissions init)
                    vector
                    (vector (:capital-stock init))))
       second))

(defn non-negative-emissions
  "Emissions pathways until annual emissions are reduced to zero or negative."
  [{climate-module :climate-module :as model}
   feasible?
   grid-step]
  (let [e-max (emissions-max model)]
    (apply
     concat
     (reduce
      (fn [[b b*] t]
        (if (empty? b)
          (reduced [b b*])
          (->> (reduce
                (fn [s x]
                  (reduce conj!
                          s
                          (append-emissions model
                                            x
                                            e-max
                                            feasible?
                                            grid-step
                                            t)))
                (transient [])
                b)
               persistent!
               ((juxt identity
                      (fn [x]
                        (if-not (= t (:n-steps model))
                          (concat b* (filter #(real= 0 (last %)) x))
                          b*)))))))
      (-> (get-in model [:init :industrial-emissions])
          vector
          vector
          (vector []))
      (map inc (range (:n-steps model)))))))
