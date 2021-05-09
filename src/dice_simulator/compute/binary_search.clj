
;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Binary search implementation."
      :author "Anna Shchiptsova"}
 dice-simulator.compute.binary-search
  (:require [clojure.math.numeric-tower :as math]
            [utilities-clj.floating-point-comparison :refer :all]))

; from http://clojure-doc.org/articles/language/functions.html
(defn- round
  "Round down a double to the given precision (number of significant digits)"
  [d precision]
  (let [factor (math/expt 10 precision)]
    (/ (math/round (* d factor)) factor)))

(defn run
  "Finds feasible grid values between limits using binary search."
  [x-min x-max feasible? grid]
  (let [kf #(int (%2 (round (/ %1 grid) 6)))
        k1 (kf x-min math/ceil)
        k2 (kf x-max math/floor)
        gen (fn [s1 s2]
              (->> (inc s2)
                   (range s1)
                   (map #(* % grid))
                   vec))]
    (if (false? (feasible? (* k1 grid)))
      []
      (if (true? (feasible? (* k2 grid)))
        (gen k1 k2)
        (loop [x1 k1 x2 k2]
          (if (= (inc x1) x2)
            (gen k1 x1)
            (let [z (int (math/floor (float (/ (+ x1 x2) 2))))
                  q (feasible? (* z grid))]
              (recur (if (true? q) z x1) (if (true? q) x2 z)))))))))
