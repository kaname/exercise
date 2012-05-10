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

(defn mul [s e v]
  (map #(* v %) (take-while #(< % v) (range s e))))

(defn p004 [s e]
  (last (sort (filter palindromic? (flatten (map (partial mul s e) (range s e)))))))

(defn p005 [n]
  (reduce lcm (range 1 (inc n))))

(defn p006 [n]
  (let [[a b] (reduce (fn [[x y] z] [(+ x z) (+ y (* z z))])
                      [0 0]
                      (range 1 (inc n)))]
    (- (* a a) b)))

(defn p007 [n]
  (nth primes (dec n)))

(defn to-num-seq [s]
  (map #(- (int %) (int \0)) s))

(defn p008 [s]
  (loop [v 0
         coll (to-num-seq s)]
    (if (empty? coll)
        v
        (recur (max v (apply * (take 5 coll))) (rest coll)))))

(defn -main []
  nil)

