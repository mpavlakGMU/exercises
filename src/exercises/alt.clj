;;A re-imagining of the original prime stuff.
(ns exercises.alt)

(defn divides? [n p]  (zero? (rem n p)))
(defn prime-number? [n] 
  (loop [i 3]
    (cond
      (< n  2)   false
      (= n  2)   true
      (>= i n)   true
      (even? n)  false
      (zero? (rem n i)) false
      (<=  (rem n i)) (recur (+ i 2)))))

;;T: This implementation takes a candidate n, and a seq of
;;known primes, and seeks to prove if the candidate divides?
;;any known prime.
(defn prime?
  ([n]
   (loop [i 3]
     (cond
       (< n  2)   false
       (= n  2)   true
       (>= i n)   true
       (even? n)  false
       (zero? (rem n i)) false
       (<=  (rem n i)) (recur (+ i 2)))))
  ([n known-primes]
   (or (not (some #(divides? n %) known-primes))
       (prime? n))))

(defn next-prime [known-primes]
  (let [n (inc (last known-primes))
        n (if (even? n) (inc n) n) ;;ensures odds
        ]
    (loop [i n]
      (if (prime? i known-primes) i
          (recur (+ i 2))))))

(comment ;;wip
(defn prime-by-seive?
  ([n known-primes]
   (or 
       (not (some #(divides? n %) known-primes))
       (prime-number? n))))


(defn seive->next-prime [[max-prime known-primes]]
  (let [n (inc max-prime)
        n (if (even? n) (inc n) n) ;;ensures odds
        ]
    (loop [i n]
      (if (prime-by-seive? i known-primes) i
          (recur (+ i 2))))))

(defn grow-seive
  ([[_ primes] n]      [n (assoc primes n n)])
  ([[n primes :as sv]] (grow-sieve sv (next-prime-seive n primes))))

)

;;T: looks like it adds to our seq of primes, computing
;;the next from the last-known prime...
;;Problem: We always have to traverse to the last prime if
;;primes is a list/sequence, else if vector we're okay.
;;More importantly, every time we call next-prime, we
;;ignore all previous primes....and thus end up recomputing them
;;over and over again.  Inefficient algorithm?
(defn grow-primes
  ([primes n] (conj primes n))
  ([primes]   (conj primes (next-prime primes))))



(defn first-n-primes [n]
  (loop [i       0
         primes [2]]
    (if-not (< i (- n 1))
      primes
      (recur (unchecked-inc i) (grow-primes primes)))))

(defn primes-upto-n [n & [primes]]
  (loop [i 0
         primes (or primes [2])]
    (let [np (next-prime  primes)]
      (if (< n np )
        primes
        (recur (unchecked-inc i) (grow-primes primes np))))))

;;T: uses primes-upto-n as a workhorse to generate all primes,
;;then naively filters between min and max.....why the call to vec?
(defn primes-between [min max]
  (vec (filter #(< min %) (primes-upto-n max))))

;;T: looks like a thread-macro reimagination of the earlier
;;primes-between.  why the call to vec?
(defn primes-between [min max]
  (->>
   (primes-upto-n max)
   (filter #(< min %))
   (vec)))

;;T: dunno where this is used, appears to be a random function.
(defn get-factor [n primes]
  (loop [i (last primes)]
    (if (and (divides? n i) (<= 1 i) (prime? i))
      i
      (recur (dec i)))))
(comment
;;===New implementation =======================
;;T: this is out of order.  primes-upto-n needs to be forward-declared
;;or this comes later.  As it stands, we're using the old implementation of
;;primte-upto-n here, looping and recomputing primes and all.
(defn prime? [n] (= n (last (primes-upto-n n))))
;;T: good use of composition, but inefficient execution.  Relies on next-prime, which we know will continually re-compute all primes (memoryless, no caching of previous primes....)
(defn nth-prime [n] (last (take n (iterate next-prime 2))))

;;Tom: Why is this sorted?
;;Also, why are you using a hash-set for the accumulator to grow-primes?
;;you're going to create a couple of problems that way, particularly using
;;calls to (last ...) on an unordered container like a hash-set.  Rethink
;;this...
(defn primes-upto-n [n]
  (sort (filter #(<= % n) (reduce grow-primes #{2}
                                  (take n (iterate #(+ % 2) 3))))))
;;Again, uses prime? 
(defn next-prime [n]
  (let [x (inc n)]
    (if (not (prime? x))
      (next-prime (inc n))
      x)))

(defn primes-upto-n [n]
  (loop [nums (range 2 n) ;;T: I like my bindings on separate lines...
         primes []]
    (if (> (*' (first nums) (first nums)) n) ;;T: Why the bignum multiply?
      (into primes nums)
      (recur
       (filter #(not= 0 (mod % (first nums))) nums)
       (conj primes (first nums))))))
)
