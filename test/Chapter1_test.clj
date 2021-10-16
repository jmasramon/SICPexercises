(ns Chapter1_test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [Chapter1 :refer :all]))

(comment
  (defn myfixture [block]
    (do
      (println "before test")
      (block)
      (println "after test")))

  (use-fixtures :each myfixture)

  (deftest mytest []
    (is (= 2 (+ 1 1)))))

(deftest first_test
  (is (= (* pi (* radius radius)) 314.159))
  (is (= (square 4) 16))
  (is (= (abs -3) 3))
  (is (= (abs2 -3) 3))
  (is (= (abs3 -3) 3))
  (is (= (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))) -37/150))
  (is (= (sqrSumBiggest 3 2 1) 13))
  (is (= (a-plus-abs-b 2 -3) 5))
  (is (= (a-plus-abs-b 2 3) 5))

  (is (= (sqrt-iter 1.0 2) 1.4142156862745097))
  (is (= (sqrt 9) 3.00009155413138))
  (is (= (sqrt (+ 5 1)) 2.4494943716069653))

  (is (= (square (sqrt 9)) 9.00054933317044)) ;; OK error ~ 5E-4))
  (is (= (square (sqrt 0.0001)) 0.0010438358335233748))
  (is (= (square (sqrt 2.2E-10)) 9.765626465234407E-4)) ;; Very far away from giving back the same num))
  (is (= (square (sqrt-improved 9)) 9.00054933317044))
  (is (= (square (sqrt-improved 2.2E-10)) 9.765626465234407E-4)) ;; Very far away from giving back the )	)same num
  
  (is (= (cbrt  27) 3.0000005410641766))
  (is (= (cube (cbrt  27)) 27.000014608735402))
  (is (= (cube (cbrt2  27)) 27.000014608735402))

  (is (= (A 1 10) 1024))
  (is (= (A 2 4) 65536))

  (is (= 11 (f 4)))
  (is (= 11 (ff 4)))

  (is (= 6 (pascal 4 2)))

  (is (= -0.9880316240928617 (sinus 30)))

  (is (= (expt 2 4) 16))
  (is (= (expt-linear 2 4) 16))
  (is (= (expt-succ 2 4) 16))
  (is (= (even? 1) false))
  (is (= (expt-succ-iter 2 4) 16))

  (is (= (mult-add 3 3) 9))
  (is (= (fast-mult-add 7 11) 77))

  (is (= (fast-iterative-mult-add 3 3) 9))

  (is (= (fib 7) 13))
  
  (is (= (gcd 40 30) 10))
  
  (is (= (rem 206 40) 6))
  (is (= (rem 40 6) 4))
  (is (= (rem 6 4) 2))
  (is (= (rem 4 2) 0)))



(deftest divides?_test
  (is (= false (divides? 2 15)))
  (is (= true (divides? 3 15)))
  (is (= true (divides? 2 10))))

(deftest find-divisor_test
  (is (= (find-divisor 1 2) 1))
  (is (= (find-divisor 2 2) 2))
  (is (= (find-divisor 5 2) 5))
  (is (= (find-divisor 10 2) 2))
  (is (= (find-divisor 10 5) 5)))

(deftest prime?_test
  (is (= (prime? 2) true))
  (is (= (prime? 3) true))
  (is (= (prime? 4) false)))

(deftest smallest-divisor_test
  (is (= (smallest-divisor 199) 199))
  (is (= (smallest-divisor 1999) 1999))
  (is (= (smallest-divisor 19999) 7)))

