;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Collection of procedures for DICE-like simulation model"
      :author "Anna Shchiptsova"}
 dice-simulator.procedures
  (:require [clojure.math.numeric-tower :as math]
            [dice-simulator.compute.generic-structure :as generic]
            [dice-simulator.compute.techno-tree :as techno-tree]
            [utilities-clj.floating-point-comparison :refer :all]))

(defn- emitted
  "Returns observed emissions value associated with a node"
  [graph v]
  (->> (vector :gross :abated)
       (map #(nth (get graph %) (dec v)))
       (apply -)))

(defn- inverted-u-shape?
  "Determines whether emissions trajectory, associated with the path, is an
inverted U-shaped curve"
  [path graph e0]
  (or (nil? (second path))
      (let [cur (emitted graph (last path))]
        (reduce
         (fn [seed next]
           (let [v (if (nil? (last next))
                     (int (math/floor (techno-tree/round e0)))
                     (emitted graph (last next)))]
             (if (< v seed)
               (reduced true)
               (if (> v seed)
                 (reduced (>= seed cur))
                 (if (empty? next)
                   (reduced true)
                   v)))))
         (emitted graph (last (butlast path)))
         (drop 2 (iterate butlast path))))))

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

(defn- cumulative-emissions?
  "Determines whether there can exist a path with specified emissions
satisfying cumulative emissions constraint"
  [t
   pre-emitted
   cur-emitted
   e0
   {z :time-step}
   {{peak-t :net-zero-timing
     {{pre-peak-growth :growth} :reduction} :pre-peak}
    :decarbonization
    {{{cummax :maximum} :cumulative-emissions
      {emitted-growth :growth} :emitted} :industrial-emissions}
    :volume}]
  (let [get-path (fn [growth from start end]
                   (->> (range start end)
                        (reduce
                         (fn [seed k]
                           (let [v (- from (* k growth))]
                             (if (pos? v)
                               (conj seed v)
                               (reduced seed))))
                         [])
                        (apply +)))
        get-pre-path (fn [end] (get-path pre-peak-growth e0 0 end))
        insert-and-compare
        (fn [v]
          (->> (+ v pre-emitted cur-emitted)
               (* z)
               (real>= cummax)))]
    (or (nil? cummax)
        (and (> (inc t) peak-t)
             (insert-and-compare (get-pre-path peak-t)))
        (and (< t peak-t)
             (>= t (dec (dec peak-t)))
             (insert-and-compare (get-pre-path t)))
        (and (< (inc t) (dec peak-t))
             (->> (- peak-t (inc t))
                  (get-path pre-peak-growth cur-emitted 1)
                  (+ (get-pre-path t))
                  insert-and-compare)))))

(defn emissions-tree
  "Returns a sampled graph of temporal emissions where node values satisfy
 constraints on feasible economic growth, feasible speed of decarbonization,
feasible produced emissions growth and feasible cumulative emissions. Returned
value represents graph structure by the number of nodes at each time step
(:level-size), node labels (:gross and :abated) and by the adjacency list which
associates each node with the collection of its ancestor nodes in the graph
(:edges)"
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
         {{{{emax :maximum
             egrowth :growth} :produced} :industrial-emissions}
          :volume} constraints
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
   (let [{{peak-t :net-zero-timing
           {pre-peak-reduction :reduction
            pre-peak-rate :reduction-rate} :pre-peak
           {post-peak-rate :reduction-rate} :post-peak}
          :decarbonization} constraints]
     (fn [t pre-emitted pre-abated produced]
       (if (< t peak-t)
         (->> (:growth pre-peak-reduction)
              (- pre-emitted)
              (#(/ % produced))
              (- 1)
              (min (:maximum pre-peak-rate)))
         (->> (+ pre-emitted pre-abated)
              (/ pre-emitted)
              (- 1)
              (* (:growth-rate post-peak-rate))
              (min (:maximum post-peak-rate))))))
   (fn [t pre-emitted cur-emitted]
     (cumulative-emissions?
      t
      pre-emitted
      cur-emitted
      e0
      parameters
      constraints))))

(defn emissions-paths
  "Traverses all paths to the terminal level of a graph. Includes only paths
that satisfy cumulative emissions constraint and constraint on the inverted
U-shaped emissions curve (optional)"
  ([graph h init parameters constraints]
   (emissions-paths graph h init parameters constraints false))
  ([graph
    h
    {e0 :industrial-emissions}
    {z :time-step}
    {{{cummax :maximum} :cumulative-emissions} :industrial-emissions}
    u-shape?]
   (techno-tree/walk
    graph
    (fn [path]
      (and (or (not u-shape?)
               (inverted-u-shape? path graph e0))
           (-> (reduce
                (fn [seed v]
                  (+ seed (emitted graph v)))
                0
                path)
               (* h z)
               (+ (* e0 z))
               (real<= cummax)))))));)
