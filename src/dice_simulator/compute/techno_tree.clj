;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Emissions tree on a grid with value constraints"
      :author "Anna Shchiptsova"}
 dice-simulator.compute.techno-tree
  (:require [clojure.math.numeric-tower :as math]
            [utilities-clj.floating-point-comparison :refer :all]))

; from http://clojure-doc.org/articles/language/functions.html
(defn- round
  "Round down a double to the set precision (number of significant digits)"
  [d]
  (let [precision 6
        factor (math/expt 10 precision)]
    (/ (math/round (* d factor)) factor)))

(defn- find-range
  "Returns a sequence of nums in x range"
  ([x]
   (find-range x false))
  ([x reverse?]
   (->> (map #(int (math/floor %)) x)
        ((juxt first #(inc (second %))))
        ((fn [[xx yy]]
           (if (true? reverse?)
             (range (dec yy) (dec xx) -1)
             (range xx yy)))))))

(defn- next-level
  "Determines descendants for a node"
  [t h [prev-emitted prev-abated] limitf maxf]
  (let [mu-min (- 1 (/ prev-emitted (+ prev-emitted prev-abated)))]
    (->> (limitf t prev-emitted prev-abated)
         (map #(round (/ % h)))
         (#(find-range % true))
         (reduce
          (fn [seed1 next-full]
            (let [mu-max (maxf t prev-emitted prev-abated (* next-full h))]
              (if (real<= mu-min mu-max)
                (->> (vector mu-min mu-max)
                     (map #(* next-full %))
                     find-range
                     (reduce
                      (fn [seed2 abated]
                        (conj! seed2 (list next-full abated)))
                      seed1))
                seed1)))
          (transient []))
         persistent!)))

(defn- next-level-many
  "Determines descendants for a level"
  [t points h limitf maxf]
  (->> (reduce-kv
        (fn [seed1 in idx]
          (->> (map #(* h %) in)
               ((juxt (fn [[gross abated]]
                        (- gross abated))
                      second))
               (#(next-level t h % limitf maxf))
               (reduce
                (fn [[counter coll m] out]
                  (if (contains? coll out)
                    (->> (get coll out)
                         ((fn [x]
                            (->> (get m x)
                                 (#(conj % idx))
                                 (assoc! m x))))
                         (vector counter coll))
                    ((juxt identity
                           #(assoc! coll out %)
                           #(assoc! m % [idx]))
                     (inc counter))))
                seed1)))
        [0 (transient {}) (transient {})]
        points)
       rest
       (map persistent!)))

(defn- append
  "Appends new level specified by points and edges to a preexisting graph"
  [[points edges] pre-graph]
  (let [idx0 (->> (:level-size pre-graph)
                  ((juxt #(apply + %) last))
                  (apply -))]
    (->> (reduce-kv
          (fn [[gross abated layers ends] point idx1]
            (list (conj! gross (first point))
                  (conj! abated (second point))
                  (conj! layers (count (get edges idx1)))
                  (reduce
                   (fn [seed idx2]
                     (conj! seed (+ idx0 idx2)))
                   ends
                   (get edges idx1))))
          (map #(transient (get pre-graph %))
               [:gross :abated :layer-size :heads])
          points)
         (map persistent!)
         (#(conj % (conj (:level-size pre-graph) (count points))))
         (zipmap [:level-size
                  :gross
                  :abated
                  :layer-size
                  :heads]))))

(defn- roots
  "Determines root nodes in a forest"
  [h {e0 :emitted e0_ :abated} limitf maxf]
  (let [t 0]
    ((juxt (fn [_] (inc t))
           (fn [points]
             (->> (range)
                  (map inc)
                  (zipmap points)))
           (fn [points]
             (->> ((juxt #(vector (count %))
                         #(vec (map first %))
                         #(vec (map second %)))
                   points)
                  (zipmap [:level-size :gross :abated])
                  (merge {:layer-size [] :heads []}))))
     (next-level t h [e0 e0_] limitf maxf))))

(defn- initialize-paths
  "Traverses initial level"
  [graph]
  ((juxt (comp rest :level-size)
         :layer-size
         :heads
         (comp first :level-size)
         (fn [{levels :level-size}]
           (->> (first levels)
                inc
                (range 1)
                (#(zipmap % (repeat []))))))
   graph))

(defn- insert-into
  "Appends nodes to paths, for which a node is terminal"
  [paths nodes]
  (persistent!
   (reduce
    (fn [seed1 node]
      (let [sub-paths (get paths node)]
        (if (empty? sub-paths)
          (conj! seed1 (vector node))
          (reduce
           (fn [seed2 path]
             (conj! seed2 (conj path node)))
           seed1
           sub-paths))))
    (transient [])
    nodes)))

(defn- append-paths
  "Appends nodes of next graph level to existing paths"
  [paths layers heads]
  (->> (list layers heads [])
       (iterate
        (fn [[next-layers next-heads coll]]
          (let [m (first next-layers)]
            (->> (take m next-heads)
                 (insert-into paths)
                 (conj coll)
                 (list (rest next-layers)
                       (drop m next-heads))))))
       (drop-while (comp seq first))
       first
       last))

(defn graph
  "Builds emissions graph starting from the specified point of emitted
and abated emissions at time 0. Samples nodes on a grid that has cell sizes
equal to h. Applies limitf and maxf to determine gross emissions value domain
and maximal emissions reduction rate (correspondingly) at a given point.
Returns graph structure represented by the number of nodes at each time step
(:level-size), node labels (:gross and :abated), number of edges coming to a
node (:layer-size), head indexes (:heads)"
  [h init limitf maxf]
  (->> (roots h init limitf maxf)
       (iterate
        (fn [[t terminal-points pre-graph]]
          (->> (next-level-many t terminal-points h limitf maxf)
               ((juxt first #(append % pre-graph)))
               (apply vector (inc t)))))
       (map last)))

(defn walk
  "Traverses all paths to the terminal level of a graph"
  [graph]
  (->> (initialize-paths graph)
       (iterate
        (fn [[levels layers heads counter paths]]
          (let [n (first levels)
                [level-layers m] ((juxt identity
                                        #(apply + %))
                                  (take n layers))]
            (list (rest levels)
                  (drop n layers)
                  (drop m heads)
                  (+ counter n)
                  (->> (take m heads)
                       (append-paths paths level-layers)
                       (zipmap (range (inc counter) (inc (+ counter n)))))))))
       (drop-while (comp seq first))
       first
       last
       ((juxt identity (comp sort keys)))
       (apply insert-into)))
