(ns Chapter1
 (:require [clojure.test :as test])
 (:use clojure.pprint) )

(+ 1 2)

(def size 2)
size
(* 5 size)

(def pi 3.14159)

(def radius 10)

(* pi (* radius radius))


(defn square [x] (* x x))

(square 3)

(defn abs [x]
  (cond (> x 0) x
        (= x 0) 0
        (< x 0) (- x)))

(abs -3)

(defn abs2 [x]
  (cond (< x 0) (- x)
        true x))

(abs2 -3)

(defn abs3 [x]
  (if (< x 0) (- x) x))

(abs3 -3)

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 1.3
(defn sqrSumBiggest [x y z]
   (cond (and (> x y) (> y z)) (+ (square x) (square y))
         true 0))
(sqrSumBiggest 3 2 1)

(defn a-plus-abs-b [a b]
  ( (if (> b 0) + -) a b))

(a-plus-abs-b 2 -3)

(a-plus-abs-b 2 3)

(defn p [] (p))

(defn testOrder [x y]
  (if (= x 0) 0 y))

(if (= 0 0) 0 (p))

;(testOrder 0 (p)) ;stack overflow

;; 1.1.7

(defn good-enough? [guess x]
  (if (< (abs (- (square guess) x)) 0.001) true false))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))


(defn sqrt-iter [guess x]
  (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)
  )
)

(sqrt-iter 1.0 2)

(defn sqrt [x]
  (sqrt-iter 1.0 x))

(sqrt 9)

(sqrt (+ 5 1))


;; 1.6

;; 1.7
(square (sqrt 9)) ;; OK error ~ 5E-4

(square (sqrt 0.0001))

(square (sqrt 2.2E-10)) ;; Very far away from giving back the same num

;(square (sqrt 2.2E9)) ;; NullPointerException

(defn good-enough-improved? [guess nextGuess]
  (if (< (abs (- (square guess) nextGuess)) (* guess 0.001)) true false))

(defn sqrt-iter-improved [guess x]
  (def nextGuess (improve guess x))
  (if (good-enough-improved? guess nextGuess) guess (sqrt-iter nextGuess x)
  )
)

(defn sqrt-improved [x]
  (sqrt-iter-improved 1.0 x))

(square (sqrt-improved 9))

(square (sqrt-improved 2.2E-10)) ;; Very far away from giving back the same num

;(square (sqrt-improved 2.2E9)) ;; NullPointerException

;; 1.8
(defn cube [x]
  (* x x x))

(defn good-enough-cbrt? [guess x]
  (if (< (abs (- (cube guess) x)) 0.001) true false))

(defn improve-cbrt [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn cbrt-iter [guess x]
  (if (good-enough-cbrt? guess x) guess (cbrt-iter (improve-cbrt guess x) x)))

(defn cbrt [x]
  (cbrt-iter 1.0 x))

(cbrt  27)

(cube (cbrt  27))

;; 1.1.8
(defn cbrt2 [x]
  (defn good-enough-cbrt2? [guess]
    (if (< (abs (- (cube guess) x)) 0.001) true false))
  (defn improve-cbrt2 [guess]
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (defn cbrt-iter2 [guess]
    (if (good-enough-cbrt2? guess) guess (cbrt-iter2 (improve-cbrt2 guess))))
  (cbrt-iter2 1.0))

(cube (cbrt2  27))

;; 1.9
;(defn + [a b]
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))  ; recursive
;
;(defn + [a b]
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b)))) ; iterative

; 1.10
(defn A [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (A (- x 1)
                 (A x (- y 1)))))

(A 1 10)

(A 2 4)

; 1.11
(defn f [n]
  (if (< n 3) n
              (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))  )))

(defn ff [n]
  (defn ff-rec [n fn-3 fn-2 fn-1]
    (cond (< n 3) n
          (= n 3) (+ fn-1 (* 2 fn-2) (* 3 fn-3))
          true    (ff-rec (- n 1)
                          fn-2
                          fn-1
                          (+ fn-1 (* 2 fn-2) (* 3 fn-3)) )))
  (ff-rec n 0 1 2) )

; 1.12
(defn pascal [row col]
  (cond (< row 0) 0
        (< col 0) 0
        (> col row) 0
        (= row col 0) 1
        true (+ (pascal (- row 1) (- col 1))
                (pascal (- row 1) col))))


; 1.13

; 1.14

; 1.15
(defn cube [x]
  (* x x x))

(defn small-enough? [x]
  (< x 0.1))

(defn arg-reduction [x]
  (sinus (/ x 3.0)))

(defn reduced-form [x]
  (- (* 3 (arg-reduction x)) (* 4 (cube (arg-reduction x)))))

(defn sinus [x]
  (if (small-enough? x)  x (reduced-form x)))

(sinus 30)

; 1.16
(defn expt [b n]
  (if (= n 0) 1 (* b (expt b (- n 1)))))

(expt 2 4)

(defn expt-linear [b n]
  (defn expt-rec [b n acc]
    (if (= n 0) acc (expt-rec b (- n 1) (* b acc)) ))
  (expt-rec b n 1))

(expt-linear 2 4)

(defn expt-succ [b n]
  (cond (= n 0) 1
        (even? n) (square (expt-succ b (/ n 2)))
        true (* b (expt-succ b (- n 1)))))

(expt-succ 2 4)

(defn expt-succ-iter [b n]
  (defn expt-rec [b n acc]
    (cond (= n 0) acc
          (even? n) (expt-rec (square b) (/ n 2)  acc )
          true (expt-rec b (- n 1) (* acc b))) )
  (expt-rec b n 1))

(even? 1)

(expt-succ-iter 2 4)

; 1.17
(defn mult-add [x y]
  (cond (or (= x 0) (= y 0)) 0
        (= y 1) x
        true (+ x (mult-add x (- y 1))) ))

(mult-add 3 3)

(defn doub [x]
  (+ x x))

(defn halve [x]
  (/ x 2))

(defn fast-mult-add [x y]
  (cond (or (= x 0) (= y 0)) 0
        (even? y) (doub (fast-mult-add x (halve y)))
        true (+ x (fast-mult-add x (- y 1))) ))

(fast-mult-add 7 11)

; 1.18
(defn fast-iterative-mult-add [x y]
  (defn recurs [x y acc]
    (cond (= x 0) 0
        (= y 0) acc
        (even? y) (recurs (doub x) (halve y) acc)
        true (recurs x (- y 1) (+ x acc)) ))
    (recurs x y 0))

(fast-iterative-mult-add 3 3)

; 1.19
(defn fib [n]
  (defn fib-iter [a b p q i]
    (cond (= i 0) b
          (even? i) (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ i 2))
          true (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p ) (* a q)) p q (- i 1))))
  (fib-iter 1 0 0 1 n))

(fib 7)


; 1.20
(defn gcd [a b]
  (if (= b 0) a (gcd b (rem a b))))

(gcd 40 30)


; 1.21
(rem 206 40)
(rem 40 6)
(rem 6 4)
(rem 4 2)
; 1.21      smallest divisor
(defn divides? [a b]
  (= (rem b a) 0))

(test/is (= false (divides? 2 15)))
(test/is (= true (divides? 3 15)))
(test/is (= true (divides? 2 10)))

(defn find-divisor [n test-divisor]
  (cond (divides? test-divisor n) test-divisor
        (> (square  test-divisor) n) n
        true (find-divisor n (+ test-divisor 1))))

(test/is (= (find-divisor 1 2) 1))
(test/is (= (find-divisor 2 2) 2))
(test/is (= (find-divisor 5 2) 5))
(test/is (= (find-divisor 10 2) 2))
(test/is (= (find-divisor 10 5) 5))

(defn smallest-divisor [n]
  (find-divisor n 2))

(test/is (= (smallest-divisor 10) 2))

(defn prime? [n]
  (= (smallest-divisor n) n))

(test/is (= (prime? 2) true))
(test/is (= (prime? 3) true))
(test/is (= (prime? 4) false))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

; 1.22
(System/nanoTime)

(defn report-prime [elapsed-time]
  (println "***")
  (println elapsed-time))

(let [out *out*]
  (report-prime "12"))
