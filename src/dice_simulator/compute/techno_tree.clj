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
(defn round
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

(defn- sort-level
  "Sorts nodes by observed emissions in ascending order and then by abated
emissions in ascending order"
  [points]
  (->> (map #(conj % (apply - %)) points)
       (sort-by (juxt first #(first (drop 2 %))))
       (map rest)))

(defn- next-level
  "Determines descendants for a node"
  [t h [prev-emitted prev-abated] limitf maxf candidate?]
  (let [mu-min (- 1 (/ prev-emitted (+ prev-emitted prev-abated)))]
    (->> (limitf t prev-emitted prev-abated)
         (map #(round (/ % h)))
         (#(find-range % true))
         (filter pos?)
         (reduce
          (fn [seed1 next-full]
            (let [mu-max (maxf t prev-emitted prev-abated (* next-full h))]
              (if (real<= mu-min mu-max)
                (->> (vector mu-min mu-max)
                     (map #(* next-full %))
                     find-range
                     (reduce
                      (fn [seed2 abated]
                        (if (true? (->> (- next-full abated)
                                        (* h)
                                        (candidate? t prev-emitted)))
                          (conj! seed2 (list next-full abated))
                          seed2))
                      seed1))
                seed1)))
          (transient []))
         persistent!)))

(defn- next-level-many
  "Determines descendants for a level"
  [t points h limitf maxf candidate?]
  (->> (reduce-kv
        (fn [seed1 in idx]
          (->> (map #(* h %) in)
               ((juxt (fn [[gross abated]]
                        (- gross abated))
                      second))
               (#(next-level t h % limitf maxf candidate?))
               (reduce
                (fn [[coll m] out]
                  (if (contains? coll out)
                    (vector coll
                            (->> (get m out)
                                 (#(conj % idx))
                                 (assoc! m out)))
                    (vector (assoc! coll out nil)
                            (assoc! m out [idx]))))
                seed1)))
        [(transient {}) (transient {})]
        points)
       (map persistent!)
       ((fn [[children edges]]
          (->> (keys children)
               sort-level
               (#(->> (range)
                      (map inc)
                      (zipmap %)))
               ((juxt identity
                      (fn [coll]
                        (->> (keys edges)
                             (map #(get coll %))
                             (#(zipmap % (vals edges))))))))))))

(defn- append
  "Appends new level specified by points and edges to a preexisting graph"
  [[points edges] pre-graph]
  (->> (let [idx1 (apply + (:level-size pre-graph))
             idx0 (- idx1 (last (:level-size pre-graph)))]
         (reduce-kv
          (fn [[gross abated struct] point head]
            (list (conj! gross (first point))
                  (conj! abated (second point))
                  (->> (get edges head)
                       (reduce
                        (fn [seed end]
                          (conj! seed (+ idx0 end)))
                        (transient []))
                       persistent!
                       (assoc! struct (+ idx1 head)))))
          (map #(transient (get pre-graph %))
               [:gross :abated :edges])
          points))
       (map persistent!)
       (#(conj % (conj (:level-size pre-graph) (count points))))
       (zipmap [:level-size
                :gross
                :abated
                :edges])))

(defn- roots
  "Determines root nodes in a forest"
  [h {e0 :emitted e0_ :abated} limitf maxf candidate?]
  (let [t 0]
    (->> (next-level t h [e0 e0_] limitf maxf candidate?)
         sort-level
         ((juxt (fn [_] (inc t))
                (fn [points]
                  (->> (range)
                       (map inc)
                       (zipmap points)))
                (fn [points]
                  (zipmap [:level-size
                           :gross
                           :abated
                           :edges]
                          ((juxt #(vector (count %))
                                 #(vec (map first %))
                                 #(vec (map second %))
                                 #(->> (count %)
                                       inc
                                       (range 1)
                                       ((fn [ks]
                                          (zipmap ks (repeat []))))))
                           points))))))))

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
  [candidate? paths nodes]
  (let [insert (fn [node seed path]
                 (let [new-path (conj path node)]
                   (if (candidate? new-path)
                     (conj! seed new-path)
                     seed)))]
    (persistent!
     (reduce
      (fn [seed1 node]
        (let [sub-paths (get paths node)]
          (if (empty? sub-paths)
            (insert node seed1 [])
            (reduce
             #(insert node %1 %2)
             seed1
             sub-paths))))
      (transient [])
      nodes))))

(defn- append-paths
  "Appends nodes of next graph level to existing paths"
  [paths layers heads candidate?]
  (->> (list layers heads [])
       (iterate
        (fn [[next-layers next-heads coll]]
          (let [m (first next-layers)]
            (->> (take m next-heads)
                 (insert-into candidate? paths)
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
and maximal emissions reduction rate (correspondingly) at a given point. Applies
candidate? to filter nodes for which, with certainty, there are no feasible
paths containing them. Returns graph structure represented by the number of
nodes at each time step (:level-size), node labels (:gross and :abated) and by
the adjacency list which associates each node with the collection of its
ancestor nodes in the graph (:edges)"
  ([h init limitf maxf]
   (graph h
          init
          limitf
          maxf
          (fn [t pre-emitted cur-emitted]
            (identity true))))
  ([h init limitf maxf candidate?]
   (->> (roots h init limitf maxf candidate?)
        (iterate
         (fn [[t terminal-points pre-graph]]
           (->> (next-level-many t terminal-points h limitf maxf candidate?)
                ((juxt first #(append % pre-graph)))
                (apply vector (inc t)))))
        (map last))))

(defn walk
  "Traverses all paths to the terminal level of a graph. Applies
candidate? to filter paths, which are not feasible"
  ([graph] (walk graph (fn [path] (identity true))))
  ([graph candidate?]
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
                        (#(append-paths paths level-layers % candidate?))
                        (map vector (range (inc counter) (inc (+ counter n))))
                        (reduce
                         (fn [seed [k v]]
                           (if (empty? v)
                             seed
                             (assoc seed k v)))
                         {}))))))
        (drop-while (comp seq first))
        first
        last
        ((juxt identity (comp sort keys)))
        (apply insert-into candidate?))))

(defn- inner-right
  "Returns the greatest subpath, where path contains all nodes of the subpath
apart from the end node. If no subpath exists, returns an empty vector."
  [edges candidate? path]
  (->> (reverse path)
       ((juxt rest drop-last))
       (apply
        reduce
        (fn [rest-path node]
          (->> (first rest-path)
               (get edges)
               (drop-while #(not= % node))
               second
               ((fn [right-node]
                  (if (nil? right-node)
                    (rest rest-path)
                    (->> (conj rest-path right-node)
                         reverse
                         vec
                         (#(if (candidate? %)
                             (reduced %)
                             (rest rest-path))))))))))
       (#(if (seq %) % []))))

(defn- left
  "Returns a full path which is the most left in a graph among all full paths
located from the right of pre-path (including paths starting from pre-path).
A full path starts from a terminal node and ends at a root node of a graph.
Applies candidate? to check whether a subpath can be a part of a feasible path,
where subpath starts from a terminal node and ends at any node of a graph
(including a root). By assumption, if a subpath is not feasible, then it follows
that all subpaths, which have identical nodes apart from the end node, are also
not feasible."
  [edges candidate? terminal-nodes pre-path]
  (if (empty? pre-path)
    (if (seq terminal-nodes)
      (let [path (vec (take 1 terminal-nodes))]
        (if (candidate? path)
          (recur edges candidate? (rest terminal-nodes) path))))
    (let [node (->> (last pre-path)
                    (get edges)
                    first)]
      (if (nil? node)
        [pre-path terminal-nodes]
        (let [path (conj pre-path node)]
          (if (candidate? path)
            (recur edges candidate? terminal-nodes path)
            (recur edges
                   candidate?
                   terminal-nodes
                   (inner-right edges candidate? pre-path))))))))

(defn walk2
  "Returns a lazy sequence containg all paths from roots to terminal nodes
of a graph. Performs graph traversal starting from terminal nodes and returns
each obtained path in its reverse order. Applies candidate? to check whether a
subpath can be a part of a feasible path, where subpath starts from a terminal
node and ends at any node of a graph (including a root)."
  [{levels :level-size edges :edges} candidate?]
  (->> ((juxt #(apply + %) last) levels)
       ((juxt #(apply - %) first))
       (map inc)
       (apply range)
       (#(left edges candidate? % []))
       (iterate
        (fn [[prev-path terminal-nodes]]
          (left edges
                candidate?
                terminal-nodes
                (inner-right edges candidate? prev-path))))
       (take-while seq)
       (map (comp reverse first))))
