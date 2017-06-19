;;TLS 2014
(ns exercises.primes)


;;This is a pretty simple sieve...
;;we just keep track of a table of potential primes, and 
;;the factor(s) that led us to them.
;;The idea is that, as we "find" new factors, we add their 
;;squares to a table.  For an initial entry, the prime 
;;squared, there is only one factor listed (the sqrt of the prime).
;;There is, however, another way to "discover" primes, that is 
;;by discovering factors.  As we advance the natural numbers, 
;;we continue to add primes to the factor table.  
;;If we ever add the same composite to an existing entry, we simply 
;;conj the prime factor onto the factor list for that composite.
;;We simply advance forward through the natural numbers:
;; if the natural number has already been discovered, it is a
;;composite, and we can "pop" it's next factors onto the table.
;;We pop the factors by adding the entry to each factor, which gives 
;;new entries (relative to the added factor) to push onto the table.
;;This causes new entries to be added, or new factors to be identified 
;;throughout existing entries in the table.

;;On the other hand, if we visit a number that has not been recorded, 
;;the implication is that it is a prime number (and thus should be 
;;recorded/emitted).

;;if the natural number n does not exist in comps, it is prime.
;;we should add its composite (its square) to the comps.

(defn push-entry  [comps factor n] 
  (if-let [known-factors (get comps n)]
    (assoc comps n (conj known-factors factor))
    (assoc comps n [factor])))

;;When we're visiting an entry, i.e. we're done with it,
;;we check to see if it's a composite. 
;;If it's not in the comps, it must be prime.
;;We should push its entry onto comps.
;;If it IS a composite, we should push its prime 
;;factors onto other composites...
(defn pop-entry [comps n]
  (let [nxt-comps (dissoc comps n)]
    (if-let [known-factors (get comps n)]
      (reduce (fn [acc fac]
                (push-entry acc fac (+ n fac)))
              nxt-comps known-factors)
      (push-entry nxt-comps n (* n n)))))
;;Primes returns an inifinite, lazy sequence, of 
;;prime numbers computed using the incremental method.
;;This is "one" way to implement the algorithm...are there 
;;other, cleaner versions?  There's also a couple of optimizations 
;;that I'm not using here...
(defn primes []
  (let [knowns (atom (push-entry {} 2 (* 2 2)))
        step (fn [n] (do (swap! knowns pop-entry n)  n))]
   (iterate (fn [last-prime]               
               (loop [idx (unchecked-inc last-prime)]
                 (if (get @knowns idx)
                   (do (step idx)
                       (recur (unchecked-inc idx)))
                   (do (step idx)))))
             2)))

;;One optimization is to use transient maps instead of the 
;;default persistent hash map.  

;;We can define transient versions that use the same conventions:
(defn push-entry!  [comps factor n] 
  (if-let [known-factors (get comps n)]
    (assoc! comps n (conj known-factors factor))
    (assoc! comps n [factor])))

(defn pop-entry! [comps n]
    (if-let [known-factors (get comps n)]
      (reduce (fn [acc fac]
                (push-entry! acc fac (+ n fac)))
              (dissoc! comps n) known-factors)
      (push-entry! (dissoc! comps n) n (* n n))))

(defn primes! []
  (let [knowns (atom (push-entry! (transient {}) 2 (* 2 2)))
        step (fn [n] (do (swap! knowns pop-entry! n)  n))]
    (iterate (fn [last-prime]               
               (loop [idx (unchecked-inc last-prime)]
                 (if (get @knowns idx)
                   (do (step idx)
                       (recur (unchecked-inc idx)))
                    (step idx))))
             2)))

;;primes-below uses the primes! function to draw primes below x lazily.
(defn primes-below [x]
  (take-while (fn [n] (< n x)) (primes!)))

;;Since we have a lazy sequence of monotonically increasing primes, we can do
;;some generic things to it....
(defn largest-prime [n]
  (last (primes-below n)))

;;Food for thought:
;;How would you test that these functions are correct? 
;;I haven't written any... (here)
;;Also, what other optimizations can be exploited here? 
;;Are these "good enough"?  Are they "fast"  ?  How would you 
;;find that out?  
