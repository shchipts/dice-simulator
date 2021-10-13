;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns dice-simulator.compute.techno-tree-test
  (:require [clojure.test :refer :all]
            [dice-simulator.compute.techno-tree :refer :all]
            [utilities-clj.floating-point-comparison :refer :all]))

;;; tests

(deftest grid-cells
  (testing "find descendants rounding value domains to the greatest grid points
 less than or equal to domain limits"
    (; Arrange
     let [f1 (fn [x y z]
               (fn [t xx yy]
                 (if (and (zero? t) (real= xx x) (real= yy y)) z)))
          f2 (fn [coll]
               (fn [t x y q]
                 (->> (filter (fn [[xx yy qq z]]
                                (and (zero? t)
                                     (real= x xx)
                                     (real= y yy)
                                     (real= q qq)))
                              coll)
                      first
                      last)))
          f3 (fn [t pre-e cur-e] (identity true))]

      (; Act
       let [out1 (graph 2
                        {:emitted 3.5 :abated 0.388889}
                        (f1 3.5 0.388889 [2 2])
                        (f2 [[3.5 0.388889 2 2.1]])
                        f3)
            out2 (graph 0.1
                        {:emitted 3.5 :abated 0.93038}
                        (f1 3.5 0.93038 [1.05 1.12])
                        (f2 [[3.5 0.93038 1 0.29]
                             [3.5 0.93038 1.1 0.38]])
                        f3)
            out3 (graph 3
                        {:emitted 3.5 :abated 0.875}
                        (f1 3.5 0.875 [6 9])
                        (f2 [[3.5 0.875 6 0.2]
                             [3.5 0.875 9 0.2]])
                        f3)
            out4 (graph 1
                        {:emitted 3.5 :abated -7}
                        (f1 3.5 -7 [1.5 1.5])
                        (f2 [[3.5 -7 1 3]])
                        f3)
            out5 (graph 0.15
                        {:emitted 3.5 :abated 0.388889}
                        (f1 3.5 0.388889 [2 2])
                        (f2 [[3.5 0.388889 1.95 0]])
                        f3)]

        ; Assert
        (is (= (first out1)
               {:level-size [3]
                :gross [1 1 1]
                :abated [2 1 0]
                :edges {1 []
                        2 []
                        3 []}}))

        (is (= (first out2)
               {:level-size [4]
                :gross [11 10 11 11]
                :abated [4 2 3 2]
                :edges {1 []
                        2 []
                        3 []
                        4 []}}))

        (is (= (first out3)
               {:level-size [2]
                :gross [2 3]
                :abated [0 0]
                :edges {1 []
                        2 []}}))

        (is (= (first out4)
               {:level-size [2]
                :gross [1 1]
                :abated [3 2]
                :edges {1 []
                        2 []}}))

        (is (= (first out5)
               {:level-size [0]
                :gross []
                :abated []
                :edges {}}))))))

(deftest grid-paths
  (testing "tree with several levels"
    (; Arrange
     let [f1 (fn [coll]
               (fn [t x y]
                 (->> (filter
                       (fn [[tt xx yy z]]
                         (and (= t tt)
                              (real= x xx)
                              (real= y yy)))
                       coll)
                      first
                      last)))
          f2 (fn [coll]
               (fn [t x y q]
                 (->> (filter
                       (fn [[tt xx yy qq z]]
                         (and (= t tt)
                              (real= x xx)
                              (real= y yy)
                              (real= q qq)))
                       coll)
                      first
                      last)))
          f3 (fn [t pre-e cur-e] (identity true))]

      (; Act
       let [out1 (graph 1
                        {:emitted 3.5 :abated 0}
                        (f1 [[0 3.5 0 [2 3]]
                             [1 2 0 [4 4]]
                             [1 3 0 [4 4]]
                             [2 4 0 [5 5]]
                             [3 5 0 [2 2]]
                             [3 4 1 [2 3]]])
                        (f2 [[0 3.5 0 2 0]
                             [0 3.5 0 3 0]
                             [1 2 0 4 0]
                             [1 3 0 4 0.1]
                             [2 4 0 5 0.2]
                             [3 5 0 2 1.6]
                             [3 4 1 2 0.2]
                             [3 4 1 3 0.4]])
                        f3)
            out2 (graph 0.5
                        {:emitted 2 :abated 2}
                        (f1 [[0 2 2 [2 2]]
                             [1 1 1 [1 1]]
                             [1 0.5 1.5 [1 1]]
                             [1 0 2 [1 1]]])
                        (f2 [[0 2 2 2 1]
                             [1 1 1 1 1]
                             [1 0.5 1.5 1 1]
                             [1 0 2 1 1]])
                        f3)
            out3 (graph 1
                        {:emitted 3.5 :abated 0}
                        (f1 [[0 3.5 0 [2 3]]
                             [1 2 0 [4 4]]
                             [1 3 0 [4 4]]
                             [2 4 0 [5 5]]
                             [3 5 0 [2 2]]
                             [3 4 1 [2 3]]])
                        (f2 [[0 3.5 0 2 0]
                             [0 3.5 0 3 0]
                             [1 2 0 4 0]
                             [1 3 0 4 0.1]
                             [2 4 0 5 0.2]
                             [3 5 0 2 1.6]
                             [3 4 1 2 0]
                             [3 4 1 3 0]])
                        f3)
            out4 (graph 0.5
                        {:emitted 2 :abated 2}
                        (f1 [[0 2 2 [2 2]]
                             [1 1 1 [1 1]]
                             [1 0.5 1.5 [1 1]]
                             [1 0 2 [1 1]]])
                        (f2 [[0 2 2 2 1]
                             [1 1 1 1 1]
                             [1 0.5 1.5 1 0.74]
                             [1 0 2 1 1]])
                        f3)
            out5 (graph 0.5
                        {:emitted 2 :abated 2}
                        (f1 [[0 2 2 [2 2]]
                             [1 1 1 [1 1]]
                             [1 0.5 1.5 [3 1]]
                             [1 0 2 [1 1]]])
                        (f2 [[0 2 2 2 1]
                             [1 1 1 1 1]
                             [1 0.5 1.5 1 1]
                             [1 0 2 1 1]])
                        f3)]

        ; Assert
        (is (= (first (drop 3 out1))
               {:level-size [2 1 2 6]
                :gross [2 3 4 5 5 2 2 2 2 3 3]
                :abated [0 0 0 1 0 3 2 1 0 1 0]
                :edges {1 []
                        2 []
                        3 [1 2]
                        4 [3]
                        5 [3]
                        6 [5]
                        7 [5]
                        8 [5]
                        9 [4 5]
                        10 [4]
                        11 [4]}}))

        (is (= (first (drop 1 out2))
               {:level-size [3 2]
                :gross [4 4 4 2 2]
                :abated [4 3 2 2 1]
                :edges {1 []
                        2 []
                        3 []
                        4 [1 2 3]
                        5 [2 3]}}))

        (is (= (first (drop 3 out3))
               {:level-size [2 1 2 4]
                :gross [2 3 4 5 5 2 2 2 2]
                :abated [0 0 0 1 0 3 2 1 0]
                :edges {1 []
                        2 []
                        3 [1 2]
                        4 [3]
                        5 [3]
                        6 [5]
                        7 [5]
                        8 [5]
                        9 [5]}}))

        (is (= (first (drop 1 out4))
               {:level-size [3 2]
                :gross [4 4 4 2 2]
                :abated [4 3 2 2 1]
                :edges {1 []
                        2 []
                        3 []
                        4 [1 3]
                        5 [3]}}))

        (is (= (first (drop 1 out5))
               {:level-size [3 2]
                :gross [4 4 4 2 2]
                :abated [4 3 2 2 1]
                :edges {1 []
                        2 []
                        3 []
                        4 [1 3]
                        5 [3]}}))))))

(deftest grid-rounding
  (testing "produced emissions can take only positive values on a grid"
    (; Arrange
     let [f1 (fn [coll]
               (fn [t x y]
                 (->> (filter
                       (fn [[tt xx yy z]]
                         (and (= t tt)
                              (real= x xx)
                              (real= y yy)))
                       coll)
                      first
                      last)))
          f3 (fn [t pre-e cur-e] (identity true))]

      (; Act
       let [out (graph 0.5
                       {:emitted 2 :abated 2}
                       (f1 [[0 2 2 [0 0.5]]
                            [1 0.5 0 [1 1]]
                            [1 0 0.5 [1 1]]])
                       (fn [t x y q] (identity 1))
                       f3)]

        ; Assert
        (is (= (first (drop 1 out))
               {:level-size [2 3]
                :gross [1 1 2 2 2]
                :abated [1 0 2 1 0]
                :edges {1 []
                        2 []
                        3 [1 2]
                        4 [2]
                        5 [2]}}))))))

(deftest include-only-feasible-nodes
  (testing "check whether node can be included in some path"
    (; Arrange
     let [f1 (fn [coll]
               (fn [t x y]
                 (->> (filter
                       (fn [[tt xx yy z]]
                         (and (= t tt)
                              (real= x xx)
                              (real= y yy)))
                       coll)
                      first
                      last)))
          f2 (fn [coll]
               (fn [t x y q]
                 (->> (filter
                       (fn [[tt xx yy qq z]]
                         (and (= t tt)
                              (real= x xx)
                              (real= y yy)
                              (real= q qq)))
                       coll)
                      first
                      last)))]

      (; Act
       let [out1 (graph 2
                        {:emitted 3.5 :abated 0.388889}
                        (f1 [[0 3.5 0.388889 [2 2]]])
                        (f2 [[0 3.5 0.388889 2 2.1]])
                        (fn [t pre-emitted cur-emitted]
                          (not
                           (and (zero? 0)
                                (real= pre-emitted 3.5)
                                (real= cur-emitted 0)))))
            out2 (graph 1
                        {:emitted 3.5 :abated 0}
                        (f1 [[0 3.5 0 [2 3]]
                             [1 2 0 [4 4]]
                             [1 3 0 [4 4]]
                             [2 4 0 [5 5]]
                             [3 5 0 [2 2]]
                             [3 4 1 [2 3]]])
                        (f2 [[0 3.5 0 2 0]
                             [0 3.5 0 3 0]
                             [1 2 0 4 0]
                             [1 3 0 4 0.1]
                             [2 4 0 5 0.2]
                             [3 5 0 2 1.6]
                             [3 4 1 2 0.2]
                             [3 4 1 3 0.4]])
                        (fn [t pre-emitted cur-emitted]
                          (not
                           (and (= t 3)
                                (real= pre-emitted 4)
                                (real= cur-emitted 3)))))]

        ; Assert
        (is (= (first out1)
               {:level-size [2]
                :gross [1 1]
                :abated [2 0]
                :edges {1 []
                        2 []}}))

        (is (= (first (drop 3 out2))
               {:level-size [2 1 2 5]
                :gross [2 3 4 5 5 2 2 2 2 3]
                :abated [0 0 0 1 0 3 2 1 0 1]
                :edges {1 []
                        2 []
                        3 [1 2]
                        4 [3]
                        5 [3]
                        6 [5]
                        7 [5]
                        8 [5]
                        9 [4 5]
                        10 [4]}}))))))

(deftest traverse-single-level
  (testing "graph with one level of nodes"
    (; Act
     let [paths1 (walk {:level-size [3]
                        :gross [1 1 1]
                        :abated [0 1 2]
                        :layer-size []
                        :heads []})
          paths2 (walk {:level-size [0]
                        :gross []
                        :abated []
                        :layer-size []
                        :heads []})]

      ; Assert
      (is (= paths1
             '([1]
               [2]
               [3])))

      (is (= paths2
             '())))))

(deftest traverse-many-levels
  (testing "graph with more one level of nodes"
    (; Act
     let [paths1 (walk {:level-size [2 1 2 6]
                        :gross [3 2 4 5 5 2 2 2 2 3 3]
                        :abated [0 0 0 0 1 0 1 2 3 0 1]
                        :layer-size [2 1 1 2 1 1 1 1 1]
                        :heads [1 2 3 3 4 5 4 4 4 5 5]})
          paths2 (walk {:level-size [3 2]
                        :gross [4 4 4 2 2]
                        :abated [2 3 4 1 2]
                        :layer-size [2 3]
                        :heads [1 2 1 2 3]})
          paths3 (walk {:level-size [2 1 2 4]
                        :gross [3 2 4 5 5 2 2 2 2]
                        :abated [0 0 0 0 1 0 1 2 3]
                        :layer-size [2 1 1 1 1 1 1]
                        :heads [1 2 3 3 4 4 4 4]})]

      ; Assert
      (is (= paths1
             '([1 3 4 6]
               [2 3 4 6]
               [1 3 5 6]
               [2 3 5 6]
               [1 3 4 7]
               [2 3 4 7]
               [1 3 4 8]
               [2 3 4 8]
               [1 3 4 9]
               [2 3 4 9]
               [1 3 5 10]
               [2 3 5 10]
               [1 3 5 11]
               [2 3 5 11])))

      (is (= paths2
             '([1 4]
               [2 4]
               [1 5]
               [2 5]
               [3 5])))

      (is (= paths3
             '([1 3 4 6]
               [2 3 4 6]
               [1 3 4 7]
               [2 3 4 7]
               [1 3 4 8]
               [2 3 4 8]
               [1 3 4 9]
               [2 3 4 9]))))))

(deftest traverse-only-feasible-paths
  (testing "graph with only feasible paths"
    (; Act
     let [paths1 (walk {:level-size [3]
                        :gross [1 1 1]
                        :abated [0 1 2]
                        :layer-size []
                        :heads []}
                       (fn [path] (not= 3 (first path))))
          paths2 (walk {:level-size [2 1 2 3]
                        :gross [3 2 4 5 5 2 2 2]
                        :abated [0 0 2 0 4 0 1 2]
                        :layer-size [2 1 1 2 1 1]
                        :heads [1 2 3 3 4 5 4 5]}
                       (fn [path]
                         (and (not= path [2 3 4])
                              (not= path [1 3])
                              (not= path [2 3 5 6]))))]

      ; Assert
      (is (= paths1
             '([1]
               [2])))

      (is (= paths2
             '([2 3 5 8]))))))

(deftest traverse-without-conditions
  (testing "traverse all paths to terminal nodes"
    (; Act
     let [paths1 (walk2 {:level-size [3 5 2 4 3]
                         :edges {1 [] 2 [] 3 []
                                 4 [1] 5 [1 3] 6 [2] 7 [3] 8 [2]
                                 9 [4 6 7] 10 [5 6]
                                 11 [9] 12 [9 10] 13 [10] 14 [10]
                                 15 [11] 16 [11 14] 17 [12 14]}}
                        (fn [path] (identity true)))
          paths2 (walk2 {:level-size [3 5 2 4 3]
                         :edges {1 [] 2 [] 3 []
                                 4 [1] 5 [1 3] 6 [2] 7 [3] 8 [2]
                                 9 [6] 10 [6]
                                 11 [9] 12 [9 10] 13 [10] 14 [10]
                                 15 [11] 16 [11] 17 [14]}}
                        (fn [path] (identity true)))
          paths3 (walk2 {:level-size [3]
                         :edges {}}
                        (fn [path] (identity true)))
          paths4 (walk2 {:level-size [0]
                         :edges {}}
                        (fn [path] (identity true)))]

      ; Assert
      (is (= paths1
             '([1 4 9 11 15]
               [2 6 9 11 15]
               [3 7 9 11 15]
               [1 4 9 11 16]
               [2 6 9 11 16]
               [3 7 9 11 16]
               [1 5 10 14 16]
               [3 5 10 14 16]
               [2 6 10 14 16]
               [1 4 9 12 17]
               [2 6 9 12 17]
               [3 7 9 12 17]
               [1 5 10 12 17]
               [3 5 10 12 17]
               [2 6 10 12 17]
               [1 5 10 14 17]
               [3 5 10 14 17]
               [2 6 10 14 17])))
      (is (= paths2
             '([2 6 9 11 15]
               [2 6 9 11 16]
               [2 6 10 14 17])))
      (is (= paths3
             '([1]
               [2]
               [3])))
      (is (= paths4
             '())))))

(deftest traverse-conditional
  (testing "traverse all feasible paths to terminal nodes"
    (; Arrange
     let [tree {:level-size [3 5 2 4 3]
                :edges {1 [] 2 [] 3 []
                        4 [1] 5 [1 3] 6 [2] 7 [3] 8 [2]
                        9 [4 6 7] 10 [5 6]
                        11 [9] 12 [9 10] 13 [10] 14 [10]
                        15 [11] 16 [11 14] 17 [12 14]}}]

      (; Act
       let [paths1 (walk2 tree #(not= % [15 11 9 4 1]))
            paths2 (walk2 tree #(and (not= % [17 12 10 5 1])
                                     (not= % [17 14 10 5 1])))
            paths3 (walk2 tree #(not= % [17 12 9 4]))
            paths4 (walk2 tree #(and (not= % [16 14 10])
                                     (not= % [17 14 10])))
            paths5 (walk2 tree #(and (not= % [15 11])
                                     (not= % [16 11])))
            paths6 (walk2 tree #(not= % [15]))]

        ; Assert
        (is (= paths1
               '([2 6 9 11 15]
                 [3 7 9 11 15]
                 [1 4 9 11 16]
                 [2 6 9 11 16]
                 [3 7 9 11 16]
                 [1 5 10 14 16]
                 [3 5 10 14 16]
                 [2 6 10 14 16]
                 [1 4 9 12 17]
                 [2 6 9 12 17]
                 [3 7 9 12 17]
                 [1 5 10 12 17]
                 [3 5 10 12 17]
                 [2 6 10 12 17]
                 [1 5 10 14 17]
                 [3 5 10 14 17]
                 [2 6 10 14 17])))
        (is (= paths2
               '([1 4 9 11 15]
                 [2 6 9 11 15]
                 [3 7 9 11 15]
                 [1 4 9 11 16]
                 [2 6 9 11 16]
                 [3 7 9 11 16]
                 [1 5 10 14 16]
                 [3 5 10 14 16]
                 [2 6 10 14 16]
                 [1 4 9 12 17]
                 [2 6 9 12 17]
                 [3 7 9 12 17]
                 [2 6 10 12 17]
                 [2 6 10 14 17])))
        (is (= paths3
               '([1 4 9 11 15]
                 [2 6 9 11 15]
                 [3 7 9 11 15]
                 [1 4 9 11 16]
                 [2 6 9 11 16]
                 [3 7 9 11 16]
                 [1 5 10 14 16]
                 [3 5 10 14 16]
                 [2 6 10 14 16]
                 [1 5 10 12 17]
                 [3 5 10 12 17]
                 [2 6 10 12 17]
                 [1 5 10 14 17]
                 [3 5 10 14 17]
                 [2 6 10 14 17])))
        (is (= paths4
               '([1 4 9 11 15]
                 [2 6 9 11 15]
                 [3 7 9 11 15]
                 [1 4 9 11 16]
                 [2 6 9 11 16]
                 [3 7 9 11 16]
                 [1 4 9 12 17]
                 [2 6 9 12 17]
                 [3 7 9 12 17]
                 [1 5 10 12 17]
                 [3 5 10 12 17]
                 [2 6 10 12 17])))
        (is (= paths5
               '([1 4 9 12 17]
                 [2 6 9 12 17]
                 [3 7 9 12 17]
                 [1 5 10 12 17]
                 [3 5 10 12 17]
                 [2 6 10 12 17]
                 [1 5 10 14 17]
                 [3 5 10 14 17]
                 [2 6 10 14 17])))
        (is (= paths6
               '()))))))


;;; test grouping


(deftest graph-test
  (testing "Construct tree:\n"
    (grid-cells)
    (grid-paths)
    (include-only-feasible-nodes)
    (grid-rounding)))

(deftest walk-test
  (testing "Traverse tree:\n"
    (traverse-single-level)
    (traverse-many-levels)
    (traverse-only-feasible-paths)))

(deftest walk2-test
  (testing "Traverse tree:\n"
    (traverse-without-conditions)
    (traverse-conditional)))


;;; tests in the namespace


(defn test-ns-hook
  "Explicit definition of tests in the namespace"
  []
  (graph-test)
  (walk-test)
  (walk2-test))
