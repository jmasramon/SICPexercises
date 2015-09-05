(ns Chapter2
 (:require [clojure.test :as test])
 (:require [clojure.math.numeric-tower :as math])
 (:use clojure.pprint)
 (:use Chapter1)
 (:gen-class)
)


;2.1
(defn make-rat [num den] 
  (let [g (gcd num den)
        sign (if (or (and (< num 0) 
                          (> den 0))
                     (and (> num 0) 
                          (< den 0)))
               -1
               1)]
    (cons (/ (* sign num) g) [(/ den g)])))

(defn numer [rat] (first rat))

(defn denom [rat] (first (rest rat)))

(defn add-rat-no-closure [x y] (/ (+ (* (numer x) 
                                        (denom y)) 
                                     (* (numer y) 
                                        (denom x)))
                                  (* (denom x) 
                                     (denom y))))

(defn add-rat [x y] (make-rat (+ (* (numer x) 
                                    (denom y)) 
                                 (* (numer y) 
                                    (denom x)))
                              (* (denom x) 
                                 (denom y))))

(defn subs-rat [x y] (make-rat (- (* (numer x) 
                                     (denom y)) 
                                  (* (numer y) 
                                     (denom x)))
                               (* (denom x) 
                                  (denom y))))

(defn mul-rat [x y] (make-rat (* (numer x) 
                                 (numer y)) 
                              (* (denom x) 
                                 (denom y))))

(defn div-rat [x y] (make-rat (* (numer x) 
                                 (denom y)) 
                              (* (denom x) 
                                 (numer y))))
;2.2
(defn make-point [x y]
  (cons x [y]))

(defn x-point [p] 
  (first p))

(defn y-point [p] 
  (first (rest p)))

(defn print-point [p]
  (print "(")
  (print (x-point p))
  (print ", ")
  (print (y-point p))
  (println ")"))

(defn make-segment [a b]
  (cons a (cons b nil)))

(defn start-segment [s]
  (first s))

(defn end-segment [s]
  (first (rest s)))

(defn midpoint-segment [s]
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s)))
                 2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
                 2)))
                    
;2.3

(defn make-rectangle [left-lower-point right-upper-point]
  (cons left-lower-point (cons right-upper-point nil)))

(defn rup-rectangle [r]
  (first (rest r)))

(defn llp-rectangle [r]
  (first r))

(defn l-rectangle [r]
  (- (x-point (rup-rectangle r))
     (x-point (llp-rectangle r))))

(defn L-rectangle [r]
  (- (y-point (rup-rectangle r))
     (y-point (llp-rectangle r))))

(defn rectangle-perimeter [r]
  (+ (* 2 (l-rectangle r))
     (* 2 (L-rectangle r))))
  
(defn rectangle-area [r]
  (* (l-rectangle r)
     (L-rectangle r)))
  
(defn make-rectangle-bis [l L]
  (cons l (cons L nil)))

(defn l-rectangle-bis [r]
  (first r))

(defn L-rectangle-bis [r]
  (first (rest r)))

(defn rectangle-perimeter-bis [r]
  (+ (* 2 (l-rectangle-bis r))
     (* 2 (L-rectangle-bis r))))
  
(defn rectangle-area-bis [r]
  (* (l-rectangle-bis r)
     (L-rectangle-bis r)))

;Pure functional representation: Message-Passing
(defn cons-bis [x y]
  (defn dispatch [m]
    (cond 
      (= m 0) x
      (= m 1) y
      true (throw (Exception. "failed"))))
  dispatch)

(defn first-bis [l] 
  (l 0))

(defn rest-bis [l]
  (l 1))

;2.4
(defn cons-bbis [x y]
  (fn [m] (m x y)))

(defn car-bbis [consed-p]
  (consed-p (fn [x y] x)))

(defn cdr-bbis [consed-p]
  (consed-p (fn [x y] y)))

;2.5
(defn num-powers [n prime]
  (defn iter [rest acc]
    (if (zero? (mod rest prime)) 
      (iter (/ rest prime) (+ acc 1))
      acc))
  (iter n 0))
    
      