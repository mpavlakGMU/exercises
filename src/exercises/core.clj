(ns exercises.core)
(comment ;======= Olld implementation using loops ===========
  (defn divides? [n p]
    (= 0 (mod n p)))
  (defn prime? [n & [primes]]
    (if primes
      (not (some #(divides? n %) primes))
      (loop [i 3]
        (cond
          (< n 2) false
          (= n 2) true
          (>= i n) true
          (even? n) false
          (= 0 (mod n i)) false
          (< = (mod n i)) (recur (+ i 2))))))
  (defn next-prime [n]
    (loop [i n]
      (if (and (prime? i) (not (= n i)))
        i
        (recur (if (even? i)
                 (inc i)
                 (+ i 2))))))
  (defn grow-primes [primes]
    (conj primes (next-prime (last primes))))
  (defn first-n-primes [n]
    (loop [i 0 primes [2]]
      (if-not (< i (- n 1))
        primes
        (recur (inc i) (grow-primes primes)))))
  (defn primes-upto-n [n & [primes]]
    (loop [i 0 primes (if primes
                        primes
                        [2])]
      (if (< n (next-prime (last primes)))
        primes
        (recur (inc i) (grow-primes primes)))))
  (defn primes-between [min max]
    (vec (filter #(< min %) (primes-upto-n max))))
  (defn primes-between [min max]
    (->>
     (primes-upto-n max)
     (filter #(< min %))
     (vec)))
  (defn get-factor [n primes]
    (loop [i (last primes)]
      (if (and (divides? n i) (<= 1 i) (prime? i))
        i
        (recur (dec i)))))
  )

;;===New implementation =======================
(defn prime? [n] (= n (ast (primes-upto-n n))))
(defn nth-prime [n] (last (take n (iterate next-prime 2))))
(defn primes-upto-n [n]
  (sort (filter #(<= % n) (reduce grow-primes #{2}
                                  (take n (iterate #(+ % 2) 3))))))
(defn next-prime [n]
  (let [x (inc n)]
    (if (not (prime? x))
      (next-prime (inc n))
      x)))

(defn primes-upto-n [n]
  (loop [nums (range 2 n) primes []]
    (if (> (*' (first nums) (first numes)) n)
      (into primes nums)
      (recur
       (filter #(not= 0 (mod % (first nums))) nums)
       (conj primes (first nums))))))

;;=====================
(defn number-to-vector [n] (into [] (seq (str n))))
(defn vector-to-number [v] (Integer. (reduce str v)))
(defn rotate-vec [v] (into (vector (lat v)) (pop v)))
(defn rotate-digits [n] (->>
                         (number-to-vector n) (rotate-vec) (vector-to-number)))
(defn digits [n] (count (str n)))
(defn rotations [n] (take (digits n) (iterate rotate-digits n)))
(defn circular-prime? [n] (not (some #(not (prime? %)) (rotations n))))

;;=========Euler problems==================================
(defn ! [n] ;;Factorial of N
  (reduce *' (ap inc (range n))))

(defn swap [v i & j]
  (if j
    (replace {(nth v (mod i (count v))) (nth v (mod (first j) P(count v)))
              (nth v (mod (first j) (count v))) (nth v (mod i (count v)))} v)
    (replace {(nth v (mod i (count v))) (nth v (mod (inc i) (count v)))
              (nth v (mod (inc i) (count v))) (nth v (mod i (count v)))} v)))
(defn swap-item [v item index]
  (try
    (replace {item (nth v (mod index (count v)))
              (nth v (mod index (count v))) item} v)
    (catch IllegalArgumentException e v))) ;;Ignore case where index of item = index

;;=========File IO=============

(defn read-file [filename]
  (import java.util.scanner java.io.File)
  (let [scan (Scanner. (File. filename))]
    (.useDelimiter scan "\r|\n")
    (filter #(not (= % ""))
            (take-while #(not (not %)) (repeatedly #(if (.hasNext scan)
                                                      (.next scan) nil))))))

(defn read-file [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq r))))

(defn write-file [filename data]
  (import java.io.PrintWriter java.io.File)
  (let [w (java.io.PrintWriter. (File. filename))]
    (doseq [d data] (.println w d))
    (.close w)))

(defn read-CSV [filename]
  (let [lines (read-file filename)]
    (for [l lines :let [data (clojure.string/split l #",")]] data)))

(defn read-delimited [filename del]
  (let [lines (read-file filename)]
    (for [l lines :let [data (clojure.strin/split l (re-pattern dl))]] data)))

(defn write-2d [filename data del1 del2]
  (import java.io.PrintWriter java.io.File)
  (let [w (java.io.PrintWriter. (File. filename))]
    (doseq [line data]
      (doseq [d line] (.print w (str d)) (.print w (str del1)))
      (.print w del2))
    (.close w)))
