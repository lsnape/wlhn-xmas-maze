(ns reindeer-presents.core
  (:require [aleph.tcp :as tcp]
            [manifold.stream :as stream]
            [byte-streams :as b]
            [clojure.string :as s]))

(defn next-message [conn]
  (b/to-string @(stream/take! @conn)))

(defn create-new-user! []
  (let [conn (tcp/client {:host "10.112.156.136"
                          :port 8080})]

    (next-message conn) ;; new user
    (stream/put! @conn "TurkeyHeads!\n")
    conn))

(defn ->pos [direction]
  (get {:north "N\n"
        :south "S\n"
        :east "E\n"
        :west "W\n"}
       direction))

(defn parse-position [pos]
  (-> pos
      s/trim
      (s/split #"\s+")
      (->> (map (comp read-string #(apply str %) rest))
           (zipmap [:north :east :south :west :present]))

      (update-in [:present] (fn [p]
                              (if (= '? p)
                                -1
                                p)))))

(defn turn-left [last-direction]
  (get {:north :east
        :east :south
        :south :west
        :west :north}

       last-direction))

(defn next-move [valid-moves last-direction]
  (turn-left last-direction))

(defn valid-moves [msg]
  (->> msg
       (filter (fn [[_ v]]
                 (and (not= nil v)
                      ((complement symbol?) v)
                      (pos? v))))
       keys
       set))

(defn run-present-hunt! []
  (let [conn (create-new-user!)]
    
    (loop [msg (next-message conn)
           last-direction :east]

      (Thread/sleep 50)
      (let [current-position (parse-position msg)
            {:keys [present] :as moves} (valid-moves current-position)]

        (if (zero? present)
          :success!
          (let [direction (next-move moves last-direction)]
            (stream/put! @conn (->pos direction))
            (recur (next-message conn) direction)))))))

(comment
  (def conn (create-new-user!))

  (next-message conn)

  "N0 E1 S0 W1 P?\n"

  (parse-position "N0 E1 S0 W1 P?\n")

  {:present ?, :west 1, :south 0, :east 1, :north 0}

  (valid-moves {:present -1 :west 1, :south 0, :east 1, :north 0})

  (next-move {:west 1, :east 1} :east)
  )
