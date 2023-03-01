;; # Hi there 👋

;; Let's try to make some AI magic 🪄

^{:nextjournal.clerk/visibility {:code :hide}}
(ns sybill
  (:require [nextjournal.clerk.viewer :as v]
            [nextjournal.clerk :as clerk]
            [get-data :as data]
            [scicloj.ml.core :as ml]
            [scicloj.ml.metamorph :as mm]
            [scicloj.ml.dataset :refer [dataset add-column] :as ds]))

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

;; ![image of two-layer neural network](https://raw.githubusercontent.com/EvgenyNerush/sybill/main/src/resources/neural-network.png)

;; This neural network is so simple that we can find quite good
;; weights $$ w_{ij} $$ without any training. For instance, if the
;; linear trend works well for the most of weeks, it's good to use

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

(v/plotly {:layout {:title "approximation of periodic function"}
           :data [{:x x14
                   :y v-periodic-1
                   :type "line"
                   :name "data 1"}
                  {:x (drop 7 x14)
                   :y (u-periodic v-periodic-1)
                   :type "line"
                   :name "approximation 1"}]})

(v/plotly {:layout {:title "approximation of periodic function"}
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

;; We can continue with different appoximation methods. For instance, [linear
;; least squares](https://en.wikipedia.org/wiki/Least_squares#Linear_least_squares)
;; will also yield approximation (output layer) which linearly depends on
;; the input values. Thus, is all the magic of neural network in that the
;; training process just chooses the right way of data approximation?
;; Surely, nope!

;; Let's imagine that the behavior of the prices is governed by a linear equation.
;; For such equation one can write a finite-difference numerical solver that will
;; give (again!) a linear relation between $$ u_i $$ and $$ v_j $$! Differential
;; equation with a time-lag, or integro-differential equation, if it is linear,
;; also can be described with linear relations:

;; $$
;; \frac{dv}{dx} |_{x=x_i} \approx \frac{ v_{i + 1} - v_{i - 1}}{2}, \\
;; \int_{x_i}^{x_j} v(x') \, dx' \approx \sum_{k=i}^{j} v_k.
;; $$

;; Thus, our network can simulate *cause-and-effect relationships* of a wide range
;; of systems. This makes it so powerful tool.

;; ## Neural network goes to school 🚸

;; Unlikely BTC price is governed by a *linear* equation, but lets try to train
;; our primitive neural network with Bitcoin data, and look that this yields.
;; But how to train the network, then?

;; The result of the training is the matrix $$ w_{ij} $$ which will allow us
;; to predict the price for the next week (the first week) from the prices of
;; the current week (the second week). For the training we can use data
;; for a big number of pairs of weeks -
;; one after another. The first weeks in the pairs resemble a cloud in a
;; seven-dimensional space. Every point of the cloud has a corresponding
;; the second week values. It looks confusing
;; until we get down to a one-dimensional space - and we see that our task is
;; absolutely the same as a school task of the interpolation of some
;; experimental data:

;; For coordinate-value pairs $$ (x_i, y_i) $$ obtained in some experiment one
;; need to find a function $$ y(x) $$ which fits data well. But in our case
;; $$ x $$ is a price values in the first week and $$ y $$ is a price values in
;; the second week. Function $$ y(x) $$ that corresponds to our neural network
;; architecture is very simple: $$ y(x) = \hat w x $$, with $$ \hat w $$
;; is the matrix $$ w_{ij} $$, $$ x $$ and $$ y $$ are vectors $$ v $$ and $$ u $$.
;; In one-dimensional case the task is trivial
;; and can be solved analytically with linear regression and least squares.

;; fig?

;; In seven-dimensional spaces we use [scicloj](https://github.com/scicloj/scicloj.ml)
;; library to construct and train the neural network. *Scicloj* uses datasets
;; very similar to DataFrames of *Pandas* in Python:

(def my-first-ds (ds/dataset {:fruit ["apple" "banana"] :price [3 5]}))

;; For the beginning, lets try ordinary least squares with the following dataset:

(def my-training-ds (ds/dataset {:xs [1 2 3 4] :ys [10 20 30 40]}))

;; To give `x` values to the model, we also should use a dataset.
;; Here `:ys` are arbitrary.

(def my-test-ds (ds/dataset {:xs [2.5 5] :ys []}))

;; Then, accordingly to [Scicloj intro](https://scicloj.github.io/scicloj.ml-tutorials/userguide-intro.html),
;; we make a function which consumes a dataset and returns a trained model:

(def my-pipe-fn
  (ml/pipeline                   ;; to describe operations on a dataset
   (mm/select-columns [:ys :xs])
   (mm/set-inference-target :ys) ;; column to predict
   (mm/model {:model-type :smile.regression/ordinary-least-square})))

;; [Ordinary least squares](https://en.wikipedia.org/wiki/Least_squares#Linear_least_squares)
;; approximates `:ys` with a linear function of `:xs`, and the coefficients of this
;; linear function is computed to minimize the squares of the error on the
;; dataset for training.

;; The training model is get in the `:fit` mode:

(def my-trained-ctx
  (my-pipe-fn {:metamorph/data my-training-ds
               :metamorph/mode :fit}))

;; `:transform` mode yields values predicted by the model

(def my-test-ctx
  (my-pipe-fn
   (assoc my-trained-ctx
          :metamorph/data my-test-ds
          :metamorph/mode :transform)))

(def my-result
  (-> my-test-ctx :metamorph/data
      (ds/column :ys)))

(println my-result)

;; We get exactly that we expected, so its time to try some method with BTC prices.
;; As it was mentioned above, `data-1` is the Bitcoin price in USD for a couple of months.
;; Now we make `xs` and `ys` from it, which are the points in 7-dimensional space. One
;; point is a price for 7 deys.

(defn week-prices
  "From the list of values, makes a vector of vectors, 7 elements each:
     week prices starting from some day, week prices starting from
     the next day, and so on."
  [previous-result
   rest-prices]
  (let [values (take 7 rest-prices)]
    (if (< (count values) 7)
      previous-result
      (week-prices (conj previous-result (vec values)) (rest rest-prices)))))

(assert (= (week-prices (vector) (range 1 9)) [[1 2 3 4 5 6 7] [2 3 4 5 6 7 8]]))

(def training-week-values (week-prices (vector) data-1))

;; These are our points for training:

(def training-xs (vec (drop-last 7 training-week-values)))
(def training-ys (vec (drop 7 training-week-values)))

(assert (= (first training-ys) (first (drop 7 training-xs))))

;; We check that we didn't mix up the order

(let [j 15
      ts-1 (range j (+ j 14))
      ts-2 (range (+ j 7) (+ j 14))
      prices-1 (concat (get training-xs j) (get training-xs (+ j 7)))
      prices-2 (get training-ys j)]
  (v/plotly {:layout {:title "prices from xs and ys"}
             :data [{:x ts-1
                     :y prices-1
                     :type "line"
                     :name "price from xs"}
                    {:x ts-2
                     :y prices-2
                     :type "line"
                     :name "price from ys"}]}))

;; Now it's time for training! But in the *ordinary least squares* (that we want
;; to use) the response value is usually a scalar. See details in the
;; [Smile documentation](https://haifengl.github.io/regression.html#ols)
;; (which is under the hood of *scicloj*) or in
;; [wiki](https://en.wikipedia.org/wiki/Ordinary_least_squares)
;; So, we can use it directly to predict a value in a single day, not in all
;; next seven days. Also, xs should be not a vector of vectors, but columns
;; with scalar values.

(def pipe-fn
  (ml/pipeline
   (mm/select-columns [:0 :1 :2 :3 :4 :5 :6 :i])
   (mm/set-inference-target :i)
   (mm/model {:model-type :smile.regression/ordinary-least-square})))

(defn trained-ctx-fn
  "Gets the index of day, i, in (0..6), returns the model trained for (i+1)-th day
   after the given 7 days."
  [i]
  (let [training-ds (ds/dataset {:0 (map (fn [x] (get x 0)) training-xs)
                                 :1 (map (fn [x] (get x 1)) training-xs)
                                 :2 (map (fn [x] (get x 2)) training-xs)
                                 :3 (map (fn [x] (get x 3)) training-xs)
                                 :4 (map (fn [x] (get x 4)) training-xs)
                                 :5 (map (fn [x] (get x 5)) training-xs)
                                 :6 (map (fn [x] (get x 6)) training-xs)
                                 :i (map (fn [x] (get x i)) training-ys)})]
    (pipe-fn {:metamorph/data training-ds
              :metamorph/mode :fit})))

;; Vector of trained models:

(def trained-ctx-vector
  (vec (map trained-ctx-fn (range 7))))

;; Now it's time to test the trained models. For this let's first use some random
;; points from the training set.

(defn predict-day
  "Gets prices for a week, and returns the predicted prices for the (i+1)-th day"
  [prices i]
  (let [test-ds (ds/dataset {:0 (vector (get prices 0))
                             :1 (vector (get prices 1))
                             :2 (vector (get prices 2))
                             :3 (vector (get prices 3))
                             :4 (vector (get prices 4))
                             :5 (vector (get prices 5))
                             :6 (vector (get prices 6))
                             :i (vector)})
        test-ctx (pipe-fn (assoc (get trained-ctx-vector i)
                                 :metamorph/data test-ds
                                 :metamorph/mode :transform))]
    (first (-> test-ctx :metamorph/data
        (ds/column :i)))))

;; some numbers...

(predict-day [10000 11000 12000 13000 14000 15000 16000] 0)
(predict-day [1 2 3 4 5 6 7] 0)

(defn predict-week
  "Gets prices for a week, and returns the predicted prices for the next week"
  [prices]
  (vec (map (fn [i] (predict-day prices i)) (vec (range 7)))))

;; It looks strange, but our network predicts that the price will fade after the
;; increase during all the week.

(predict-week [10000 11000 12000 13000 14000 15000 16000])

;; Let's plot some predictions based on the training set.

(defn plot-prices
  "Plot prices for 14 days, starting from j-th day, and the prediction based on
   the first half of these prices"
  [j]
  (let [ts-2 (range (+ j 7) (+ j 14))
        ts-all (range j (+ j 14))
        ts-test (range (+ j 7) (+ j 14))
        prices-2 (get training-ys j)
        prices-all (concat (get training-xs j) (get training-ys j))
        test-prices (get training-xs j)]
  (v/plotly {:layout {:title (str "real and predicted prices, j = " j)}
             :data [{:x ts-all
                     :y prices-all
                     :type "line"
                     :name "real prices"}
                    {:x ts-test
                     :y (predict-week test-prices)
                     :type "line"
                     :name "predicted prices"}
                    {:x ts-2
                     :y prices-2
                     :type "line"
                     :name "real prices"}]})))

(plot-prices 0)
(plot-prices 7)
(plot-prices 14)
(plot-prices 28)
(plot-prices 35)
(plot-prices 42)
