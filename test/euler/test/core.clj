(ns euler.test.core
  (:use [euler.core])
  (:use [clojure.test]))

(deftest test-multiple?
  (is (not (multiple? 8 3)))
  (is (multiple? 9 3)))

(deftest test-p001
  (is (= 23 (p001 10))))

(deftest test-fib
  (is (= '(0 1 1 2 3 5) (take 6 (fib)))))

(deftest test-p002
  (is (= 10 (p002 20))))

(deftest test-prime?
  (is (not (prime? 1)))
  (is (not (prime? 4)))
  (is (prime? 2))
  (is (prime? 5)))

(deftest test-p003
  (is (= 29 (p003 13195))))

(deftest test-palindromic?
  (is (not (palindromic? 1234)))
  (is (palindromic? 9009))
  (is (palindromic? 12321)))

(deftest test-p004
  (is (= 9009 (p004 2))))

(deftest test-p005
  (is (= 2520 (p005 10))))

(deftest test-p006
  (is (= 2640 (p006 10))))

(deftest test-p007
  (is (= 13 (p007 6))))

