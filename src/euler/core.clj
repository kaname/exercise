(ns euler.core)

(defn multiple? [n m]
  (zero? (rem n m)))

(defn p001 [m]
  (reduce + (filter #(or (multiple? % 3) (multiple? % 5)) (range m))))

(defn fib [n]
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn fib-seq []
  (range 20))

(defn p002 [m]
  (reduce + (filter #(and (< % m) (multiple? % 2)) (fib-seq))))

(defn -main []
  (p002 10))

