(ns euler.core
  (:use clojure.contrib.math))

(defn multiple? [n m]
  (zero? (rem n m)))

(defn p001 [m]
  (reduce + (filter #(or (multiple? % 3) (multiple? % 5)) (range m))))

(defn fib []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn p002 [m]
  (reduce + (filter even? (take-while #(< % m) (fib)))))

(defn prime? [n]
  (empty? (drop-while #(not (multiple? n %)) (range 2 (inc (/ n 2))))))

(defn p003 [m]
  (first (filter #(and (multiple? m %) (prime? %)) (iterate dec (int (sqrt m))))))

(defn -main []
  (p003 600851475143))

