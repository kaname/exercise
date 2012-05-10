(ns euler.test.core
  (:use [euler.core])
  (:use [clojure.test]))

(deftest test-p001
  (is (= 23 (p001 10))))

(deftest test-p002
  (is (= 10 (p002 20))))

(deftest test-p003
  (is (= 29 (p003 13195))))

