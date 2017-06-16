(ns exercises.blah)

(def m {4 #{2}, 9 #{3}})

;;original
(reduce (fn [acc n]
          (update acc 4 conj (rand-int 1000)))
        m
        (range 10))

;;meta data tracks previous history
(reduce (fn [acc n]
          (let [res (update acc 4 conj (rand-int 1000))]
            (vary-meta res
                       #(update % :history conj acc))))
        (with-meta m {:history []})
        (range 10))

;;intermediate values via reductions
(reductions (fn [acc n]
          (update acc 4 conj (rand-int 1000)))
        m
        (range 10))

;;using state
(let [history (atom [])
      res     (reduce (fn [acc n]
                        (do (swap! history conj acc)
                            (update acc 4 conj (rand-int 1000))))
                  m
                  (range 10))]
  [res (conj @history res)])

;;most idiomatic
(reduce (fn [[acc history] n]
          [(update acc 4 conj (rand-int 1000))
           (conj history acc)])
        [m [m]]
        (range 10))

(reduce (fn [acc n]
          (let [[res history] acc]
            [(update res 4 conj (rand-int 1000))
             (conj history res)])
          [m [m]])
        (range 10))

;;recursively with explicit args...
(defn count-to-ten [n history]
  (if (= n 10) [n history]
      (recur (unchecked-inc n) (conj history n))))


;;conditional processing
(reduce (fn [acc n]
          (if (odd? n)
            acc
            (update acc 4 conj (rand-int 1000))))
        m
        (range 10))

(reduce (fn [acc n]
            acc
            (update acc 4 conj (rand-int 1000)))
        m
        (filter even? (range 10)))

(->> (range 10)
     (filter even?)     
     (reduce (fn [acc n]
               acc
               (update acc 4 conj (rand-int 1000)))
             m))

;;using iterate
(let [m {4 #{2}, 9 #{3}}] 
  (->> (iterate (fn [{:keys [acc n] :as m}]
                  {:acc (update acc 4 conj n)
                   :n   (inc n)})
                {:acc m :n 8})
       (take 10)
       (map :acc)))

;;multiple arity /w recursive defaults
(defn add-them
  ([x y z] (+ x y z))
  ([x y ]  (add-them x y 0))
  ([x]     (add-them x 0 0)))

;;this throws errors
#_(defn add-them
    ([x y z] (+ x y z))
    ([x y ]  (recur x y 0))
    ([x]     (recur x 0 0)))
