(ns resource-scope.test.core
  (:use [resource-scope.core])
  (:use [clojure.test]))

(def flag (atom false))

(defn set-flag! []
  (reset! flag true))

(defn clear-flag! []
  (reset! flag false))

(defn get-flag []
  @flag)

(deftest exit-scope
  (set-flag!)
  (scope
    (when-scope :exits (clear-flag!))
    (is (get-flag)))
  (is (not (get-flag)))
  )

