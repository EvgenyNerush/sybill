;; # Hi there 👋

;; Let's try to make some AI magic 🪄

^{:nextjournal.clerk/visibility {:code :hide}}
(ns sybill
  (:require [nextjournal.clerk.viewer :as v]
            [nextjournal.clerk :as clerk]
            [get-data :as data]))

;; First, we need some data - something that changes day by day.
;; The Bitcoin price serves well. It looks quite random and
;; only a prophet can guess its new value from values in previous
;; days 🧙

;; (Have I warn that you risk to loose all you money if you use
;; crypto trading? If not, I warn now!)

;; *Coingecko* has nice [API](https://www.coingecko.com/en/api/documentation)
;; to fetch historical data, so I downloaded some (with `write-prices` from
;; `src/get_data.clj`). Below
;; is the Bitcoin price in USD for a couple of months - from 1 Oct 2022
;; to 1 Dec 2022:

(def data-1 (data/read-prices "bitcoin-01-10-2022-01-12-2022.txt"))

(v/plotly {:layout {:title "BTC in USD for two months"}
           :data [{:x (range (count data-1))
                   :y data-1
                   :type "line"}]})

;; The network we built
;; is as simple as possible: it gets the prices during a week and predicts
;; prices for the next week, hence it consist only of two layers (input
;; and output) seven nodes each. Values of output nodes are computed as
;; linear combinations of the input nodes:

;; $$
;; u_j = \sum_{i=1}^7 w_{ji} v_i,
;; $$

;; with $$ w_{ij} $$ weights, $$ v_i $$ values of the input layer (some week)
;; and $$ u_j $$ values of the output layer (next week, prediction).
;; No hidden layer is present here.

;; ![image of two-layer neural network](resources/neural-network.png)

;; This neural network is so simple that we can find quite good
;; weights $$ w_{ij} $$ without any training. For instance, if the
;; linear trend works well for most of weeks, it's good to use

;; $$
;; u_1 = v_7 + \frac{1}{6} (v_7 - v_1), \\
;; u_2 = v_7 + \frac{2}{6} (v_7 - v_1), ...
;; $$

(def x14 (vec (range 1 15)))

(def v-linear (map (fn [x] (+ x (rand))) x14))

(def u-linear
  (let [v7 (get (vec v-linear) 6)
        v1 (get (vec v-linear) 0)
        slope (/ (- v7 v1) 6)]
    (map (fn [x] (+ v7 (* x slope))) (take 7 x14))))

(v/plotly {:layout {:title "linear approximation"}
           :data [{:x x14
                   :y v-linear
                   :type "line"
                   :name "data"}
                  {:x (drop 7 x14)
                   :y u-linear
                   :type "line"
                   :name "approximation"}]})

;; Here approximation is done with first seven points of the data,
;; the last seven points of the data are present to show how good/bad
;; the approximation is.

;; The slope of the price curve can vary and it can be optimal to compute
;; it not from a week interval, but from a shorter period. Or longer, if
;; the data is quite noisy, for instance. One more variant of linear approximation
;; is based on the slope computed not from difference of two points, but
;; from difference of two groups of points, say $$ (v_7 + v_6 - v_1 - v_2) / 10 $$.

;; One else good example is the approximation of periodic data. For example,
;; if the price has period of five days, one can set

;; $$
;; u_1 = v_3, \quad u_2 = v_4, \quad u_3 = v_5, \quad u_4 = v_6, \\
;; u_5 = v_7, \quad u_6 = v_3, \quad u_7 = v_4.
;; $$

;; Look how it works for some periodic data with noise:

(def k (* 2 Math/PI 1/5))

(def v-periodic-1 (map (fn [x] (Math/sin (* k (+ x (rand))))) x14))

(def v-periodic-2 (map (fn [x] (Math/sin (* k (+ x 1.5 (rand))))) x14))

(defn u-periodic
  [vs]
  (let [ys (drop 2 vs)]
    (take 7 (concat ys ys))))

(v/plotly {:layout {:title "approximation of periodic functions"}
           :data [{:x x14
                   :y v-periodic-1
                   :type "line"
                   :name "data 1"}
                  {:x (drop 7 x14)
                   :y (u-periodic v-periodic-1)
                   :type "line"
                   :name "approximation 1"}]})

(v/plotly {:layout {:title "approximation of periodic functions"}
           :data [{:x x14
                   :y v-periodic-2
                   :type "line"
                   :name "data 2"}
                  {:x (drop 7 x14)
                   :y (u-periodic v-periodic-2)
                   :type "line"
                   :name "approximation 2"}]})

;; 🤔 Imagine, how the matrix (tensor) $$ w_{ij} $$ looks like for linear approximation,
;; for periodic approximation.
