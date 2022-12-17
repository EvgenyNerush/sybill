;; # Hi there 👋

;; Let's try to make some AI magic 🪄

^{:nextjournal.clerk/visibility {:code :hide}}
(ns sybill
  (:require [nextjournal.clerk.viewer :as v])
  (:require [clojure.math :as math]))

;; First, we need some data - something that changes day by day.
;; The Bitcoin price serves well. It looks quite random and
;; only a prophet can guess its new value from values in previous
;; days 🧙

;; (Have I warn that you risk to loose all you money if you use
;; crypto trading? If not, I warn now!)

;; *Coingecko* has nice [API](https://www.coingecko.com/en/api/documentation)
;; to fetch historical data, so what about three previous years?


