;   Copyright (c) 2021 International Institute for Applied Systems Analysis.
;   All rights reserved. The use and distribution terms for this software
;   are covered by the MIT License (http://opensource.org/licenses/MIT)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Time scale"
      :author "Anna Shchiptsova"}
 dice-simulator.compute.time-scale)

(def t1
  "Time point for reference value for linear part of net FFI emissions curve"
  2030)

(def ts
  "Time points for DICE-like simulation model"
  (range 2015 2105 5))
