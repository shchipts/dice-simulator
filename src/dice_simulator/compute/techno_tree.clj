;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Emissions paths on a tree with value constraints"
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
        ((fn [[x y]]
           (if (true? reverse?)
             (range (dec y) (dec x) -1)
             (range x y)))))))

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
