(ns exercises.subtleties)

;;Using Floats (possibly unintentionally from Java) alters Equality...
;;====================================================================
;; Equality vs doubles and floats (this is an age old issue in
;; programming, not unique to clojure).  You were converting things to
;; floats and Comparing them to things that were inferred to be Doubles
;; (clojure defaults to reading floating point values as doubles
;; automatically).

;; https://groups.google.com/forum/#!topic/clojure/IPO8vAG4ue4 

;; -Tom



;;A Note on Randomness....
;;========================
;; FYI, I think this was screwing up some of my simulated annealing code
;; from last fall.  I was unintentionally biasing what should have been a
;; uniform distribution.  Below is a small example:

;; If you want to compute a stream of random integers between [-5 5], you
;; might be tempted to do this:

(defn draw [] (- (rand-int 10) 5))

;; 5 is half of 10 right?  Wrong....in this case, as is idiomatic (and
;; described in the docs), rand-int (like range) draws from 0
;; up-to-but-not-including 10, so the (rand-int 10) returns integers
;; within [0 9].  You end up precluding any draws of 5, and since we
;; modify the draw by subtracting 5 (to allow half the draws to be
;; negative), the result is a bias towards negative integers.

;; I noticed this show up in what was supposed to be uniform movement of
;; the entities in the cartoons I'm working on.  They kept drifting "down
;; and right" when using this scheme to draw random positional changes
;; for x,y coordinates in each frame.

;; The fix is simple, but less intuitive.
(defn draw [] (- (rand-int 11) 5))
;; To hide the counter-intuitiveness, maybe a helper function...

(defn ->random-stepper [half-width]
  (let [w (inc (* half-width 2))]
    (fn [] (- (rand-int w) half-width)))

;;So we can verify if the draws are uniformly distributed using
(frequencies (take (100000 (repeatedly (->random-stepper 5)))))

;;-Tom
