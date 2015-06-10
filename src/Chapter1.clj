(ns Chapter1)

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

(testOrder 0 (p))

;; 1.1.7

(defn good-enough? [guess x] 
  (if (< (abs (- (square guess) x)) 0.0000001) true false))

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

(sqrt 2.2E-10)

(sqrt 2.2E6)

;; 1.7

;; 1.8 
(defn cube [x]
  (* x x x))

(defn good-enough-cbrt? [guess x] 
  (if (< (abs (- (cube guess) x)) 0.0000001) true false))

(defn improve-cbrt [guess x]
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defn cbrt-iter [guess x]
  (if (good-enough-cbrt? guess x) guess (cbrt-iter (improve-cbrt guess x) x)))

(defn cbrt [x]
  (cbrt-iter 1.0 x))

(cbrholalola
  27)
