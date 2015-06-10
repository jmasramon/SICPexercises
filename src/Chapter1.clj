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

(square (sqrt 2.2E9)) ;; NullPointerException
  
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

(square (sqrt-improved 2.2E9)) ;; NullPointerException

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





