(ns get-data
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as json])
  (:require [java-time.api :as jt]))

(defn gecko-fmt
  "String representation of the date for Coingecko API"
  [date]
  (jt/format "dd-MM-yyyy" date))

(assert (= (gecko-fmt (jt/local-date 3210 12 31)) "31-12-3210") "gecko-fmt")


(defn price-at-date
  "Usd price of the given coin at the date. Wrapper of the Coigecko web API,
   see https://www.coingecko.com/en/api/documentation"
  [coin-id date]
  (let [request (str "https://api.coingecko.com/api/v3/coins/"
                     coin-id
                     "/history?date="
                     (gecko-fmt date))
        response (client/get request)
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
            (Thread/sleep 2000)
            (recur (jt/plus date (jt/days 1)) (conj res price)))
        res))))
