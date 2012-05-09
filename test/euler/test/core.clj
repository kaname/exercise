(ns euler.test.core
  (:use [euler.core])
  (:use [clojure.test]))

(deftest test-p001
  (is (= 23 (p001 10))))

