(ns get-data
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as json])
  (:require [java-time.api :as jt])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string]))

(defn gecko-fmt
  "String representation of the date for Coingecko API"
  [date]
  (jt/format "dd-MM-yyyy" date))

(assert (= (gecko-fmt (jt/local-date 3210 12 31)) "31-12-3210") "gecko-fmt")


(defn price-at-date
  "Usd price of the given coin at the date. Wrapper of the Coigecko web API,
   see https://www.coingecko.com/en/api/documentation
   Yields `nil` if any exception is raised by the get request"
  [coin-id date]
  (let [request (str "https://api.coingecko.com/api/v3/coins/"
                     coin-id
                     "/history?date="
                     (gecko-fmt date))
        response (try
                   (client/get request {:socket-timeout 10000 :connection-timeout 10000})
                   (catch Exception e (do (println e) nil)))
        body-str (:body response)]
    (:usd (:current_price (:market_data (json/parse-string body-str true))))))

(assert (= (price-at-date "bitcoin" (jt/local-date 2022 12 1)) 17186.502572066118)
        "coingecko 1 Dec 22")
(assert (= (price-at-date "bitcoin" (jt/local-date 1900 1 1)) nil) "coingecko 1900")

(defn get-prices
  "Vector of prices by days, taken from web; date-from and date-to inclusive.
   If one of the requests fails, the loop stops and the result is incomplete.
   This function is very slow because of Coingecko's API limitations, and
   impure - the dates and prices are printed out."
  [coin-id date-from date-to]
  (loop [date date-from
         res []]
    (let [price (price-at-date coin-id date)]
      (if (and price (not (jt/after? date date-to)))
        (do (println (gecko-fmt date) price)
            (Thread/sleep 4000)
            (recur (jt/plus date (jt/days 1)) (conj res price)))
        res))))

(defn write-prices
  "Writes prices obtained from `get-prices` to a file"
  [coin-id date-from date-to]
  (let [prices (get-prices coin-id date-from date-to)
        str-prices (reduce (fn [x y] (str x \newline y)) prices)
        filename (str coin-id "-"
                      (gecko-fmt date-from) "-"
                      (gecko-fmt date-to)   ".txt")]
    (spit filename str-prices)))

(defn read-prices
  "Reads prices from recources/filename"
  [filename]
  (mapcat (fn [s] (try [(Double/parseDouble s)]
                       (catch Exception _ [])))
          (string/split-lines (slurp (io/resource (str "resources/" filename))))))

(assert (= (first (read-prices "bitcoin-01-10-2022-01-12-2022.txt")) 19476.92659600407) "read-prices")
