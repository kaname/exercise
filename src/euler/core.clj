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

(defn char-to-int [c]
  (- (int c) (int \0)))

(defn to-num-seq [s]
  (map char-to-int s))

(defn slice-dup [coll n]
  (lazy-seq
    (let [c (take n coll)]
      (if (not (= n (count c)))
          nil
          (cons c (slice-dup (rest coll) n))))))

(defn p008 [s]
  (loop [v 0
         coll (to-num-seq s)]
    (if (empty? coll)
        v
        (recur (max v (apply * (take 5 coll))) (rest coll)))))

(defn p009 [n]
  (first (for
           [c (range 1 (/ n 2)) b (range 1 c) a (range 1 b)
             :when (and (= n (+ a b c)) (= (* c c) (+ (* a a) (* b b))))]
           (* a b c))))

(defn p010 [n]
  (reduce + (take-while #(< % n) primes)))

(defn max-prod [n coll]
  (reduce #(max %1 (apply * %2)) 0 (slice-dup coll n)))

(defn p011 [coll]
  (reduce #(max %1 (max-prod 4 %2))
          0
          (concat coll
                  (for [i (range 20)] (map #(nth % i) coll))
                  (for [i (range 20)] (map-indexed #(nth %2 (+ %1 i) 1) coll))
                  (for [i (range 20)] (map-indexed #(nth %2 (- %1 i) 1) coll))
                  (for [i (range 20)] (map-indexed #(nth %2 (+ (- 19 %1) i) 1) coll))
                  (for [i (range 20)] (map-indexed #(nth %2 (- (- 19 %1) i) 1) coll)))))

(defn triangle-number [i]
  (/ (* i (inc i))))

(defn triangle-numbers []
  (map triangle-number (iterate inc 1)))

(defn count-div [n p]
  (loop [c 1
         n n]
    (if (not (multiple? n p))
        c
        (recur (inc c) (/ n p)))))

(defn prime-dec1 [n]
  (map #(count-div n %) (take-while #(< % (inc (/ n 2))) primes)))

(defn count-divisors [n]
  (reduce * (prime-dec1 n)))

(defn p012 [n]
  (first (drop-while #(< (count-divisors %) n) (triangle-numbers))))

(defn count-terms [n]
  (loop [c 1
         n n]
    (if (= n 1)
        c
        (recur (inc c)
               (if (even? n)
                   (/ n 2)
                   (inc (* 3 n)))))))

(defn p014 []
  (reduce (fn [r v]
            (let [c (count-terms v)]
              (if (> c (first r))
                  [c v]
                  r)))
          [0 0]
          (range 1 1000000)))

(defn factorial [n]
  (loop [f 1
         n n]
    (if (< n 2)
        f
        (recur (* f n) (dec n)))))

(defn p015 []
  (apply * (cons (/ 1 (factorial 20))
                 (range 21 41))))

(defn p016 [e]
  (reduce #(+ %1 (char-to-int %2)) 0 (str (reduce (fn [r _] (* r 2N)) 1N (range e)))))

(defn letters-20 [n]
  (count (nth ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
               "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
               "eighteen" "nineteen"] n)))

(defn letters-100 [n]
  (if (< n 20)
      (letters-20 n)
      (let [words ["twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]]
        (+ (count (nth words (- (int (/ n 10)) 2)))
           (letters-20 (rem n 10))))))

(defn letters-1000 [n]
  (+ (letters-20 (int (/ n 100)))
     (count "hundred")
     (if (multiple? n 100)
         0
         (+ (count "and")
            (letters-100 (rem n 100))))))

(defn p017 []
  (+ (reduce + (map letters-100 (range 1 100)))
     (reduce + (map letters-1000 (range 100 1000)))
     (count "one")
     (count "thousand")))

(defn conv [c1 c2]
  (map #(max (+ %1 %2) (+ %1 %3)) c2 c1 (rest c1)))

(defn p018 [coll]
  (first (reduce conv (reverse coll))))

(defn -main []
  nil)

