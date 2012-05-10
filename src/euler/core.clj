(ns euler.core
  (:use clojure.contrib.math)
  (:use clojure.contrib.lazy-seqs))

(defn multiple? [n m]
  (zero? (rem n m)))

(defn p001 [m]
  (reduce + (filter #(or (multiple? % 3) (multiple? % 5)) (range m))))

(defn fib []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn p002 [m]
  (reduce + (filter even? (take-while #(< % m) (fib)))))

(defn prime? [n]
  (if (< n 2)
      false
      (not (some #(multiple? n %) (range 2 (inc (/ n 2)))))))

(defn p003 [m]
  (first (filter #(and (multiple? m %) (prime? %)) (iterate dec (int (sqrt m))))))

(defn palindromic? [n]
  (let [s (str n)]
    (= s (apply str (reverse s)))))

(defn range-digit [d]
  (range (expt 10 (dec d)) (expt 10 d)))

(defn mul [d v]
  (map #(* v %) (take-while #(< % v) (range-digit d))))

(defn p004 [d]
  (last (sort (filter palindromic? (flatten (map (partial mul d) (range-digit d)))))))

(defn p005 [n]
  (reduce lcm (range 1 (inc n))))

(defn p006 [n]
  (let [[a b] (reduce (fn [[x y] z] [(+ x z) (+ y (* z z))])
                      [0 0]
                      (range 1 (inc n)))]
    (- (* a a) b)))

(defn p007 [n]
  (nth primes (dec n)))

(defn -main []
  (p007 10001))

