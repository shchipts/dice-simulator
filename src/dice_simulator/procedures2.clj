;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Collection of procedures for DICE-like simulation model"
      :author "Anna Shchiptsova"}
 dice-simulator.procedures2
  (:require [clojure.math.numeric-tower :as math]
            [dice-simulator.compute.techno-tree :as techno-tree]
            [utilities-clj.floating-point-comparison :refer :all]))

(defn- max-produced
  "Determines upper bound on produced emissions"
  [t pre-emitted pre-abated emax egrowth]
  (#(if (nil? egrowth)
      %
      (min %
           (+ pre-emitted
              pre-abated
              egrowth)))
   (nth emax t)))

(defn- net-emissions-limit
  "Determines limiting case for net emissions based on deep mitigation and
massive CDR deployment"
  [t emin cdr]
  (when-not (nil? emin)
    (- (nth emin t)
       (if (nil? cdr)
         0
         (nth cdr t)))))

(defn emissions-tree
  "Returns a sampled graph of temporal emissions where node values satisfy
constraints on maximum emissions capacity of world economy, on feasible growth
of produced emissions, on maximum mitigation and maximum CDR deployment.
Returned value represents graph structure by the number of nodes at each time
step (:level-size), node labels (:gross and :abated) and by the adjacency list
which associates each node with the collection of its ancestor nodes in the
graph (:edges)"
  [h {e0 :industrial-emissions mu0 :reduction-rate} parameters constraints]
  (techno-tree/graph
   h
   (->> (/ e0 (- 1 mu0))
        (#(- % e0))
        (vector e0)
        (zipmap [:emitted :abated]))
   (let [{s :carbon-intensity
          {labor :labor tfp :tfp a :capital-elasticity} :cobb-douglas
          d :depreciation-rate
          z :time-step} parameters
         {{{emax :maximum egrowth :growth} :produced}
          :decarbonization2} constraints
         divf (fn [t coll]
                (->> ((juxt inc identity) t)
                     (map #(nth coll %))
                     (apply /)))]
     (fn [t pre-emitted pre-abated]
       (->> (+ pre-emitted pre-abated)
            (* (divf t s))
            ((juxt #(* (divf t tfp)
                       (math/expt (divf t labor) (- 1 a))
                       (math/expt (- 1 d) (* a z))
                       %)
                   (fn [_]
                     (max-produced t pre-emitted pre-abated emax egrowth)))))))
   (let [{{{reduction :growth} :net-emitted
           {emin :minimum} :gross-emitted
           {cdr :maximum} :cdr-deployed}
          :decarbonization2} constraints]
     (fn [t pre-emitted pre-abated produced]
       (->> (net-emissions-limit t emin cdr)
            (vector (- pre-emitted reduction))
            (filter some?)
            (map #(- 1 (/ % produced)))
            (apply min))))))
