(ns euler.core)

(defn multiple? [n m]
  (zero? (rem n m)))

(defn p001 [m]
  (reduce + (filter #(or (multiple? % 3) (multiple? % 5)) (range m))))

(defn fib [a b]
  (let [n (+ a b)]
    (lazy-seq (cons n (fib b n)))))

(defn p002 [m]
  (reduce + (filter even? (take-while #(< % m) (fib 0 1)))))

(defn -main []
  (p002 4000000))

