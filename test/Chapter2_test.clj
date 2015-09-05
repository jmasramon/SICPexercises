(ns Chapter2_test
 (:use clojure.test)
 (:use Chapter2)
)

(deftest mytest2 []
    (is (= 2 (+ 1 1)))
)

;2.1
(deftest rationals_test 
  (is (= (numer (make-rat 3 2)) 
         3))
  (is (= (denom (make-rat 3 2)) 
         2))
  
  (is (= (make-rat 6 12) 
         (make-rat 1 2)))
 
  (def added (add-rat-no-closure (make-rat 3 2) 
                      (make-rat 2 3))) 
         
  (is (= added (/ 13 6))) 
  
  (def added (add-rat (make-rat 3 2) 
                      (make-rat 2 3))) 
         
  (is (= added (make-rat 13 6))) 
  
  (def substracted (subs-rat (make-rat 3 2) 
                             (make-rat 2 3))) 
         
  (is (= substracted (make-rat 5 6))) 
  
  (def multiplied (mul-rat (make-rat 3 2) 
                           (make-rat 2 3))) 
         
  (is (= multiplied (make-rat 6 6))) 
  (is (= multiplied (make-rat 1 1))) 
  
  (is (= (div-rat (make-rat 3 2)
                  (make-rat 2 3)) 
         (make-rat 9 4)))
  
  (is (= (make-rat -2 3) (make-rat 2 -3)))
  (is (= (make-rat -2 -3) (make-rat 2 3)))
)
;2.2
(deftest points_test 
  (is (= (make-point 2 3) (cons 2 [3])))
  
  (is (= (x-point (make-point 2 3)) 2))
  (is (= (y-point (make-point 2 3)) 3))
  
  (is (= (make-segment (make-point 2 3) (make-point 4 6))
         (cons '(2,3) (cons '(4 6) nil))))
  
  (is (= (start-segment (make-segment (make-point 2 3) 
                                      (make-point 4 6)))
         '(2 3)))
  
  (is (= (end-segment (make-segment (make-point 2 3) 
                                    (make-point 4 6)))
         '(4 6)))
  
  (is (= (midpoint-segment (make-segment (make-point 2 3) 
                                         (make-point 4 6)))
         '(3 9/2)))
)
;2.3
(deftest rectangles_test 
  (is (= (rectangle-perimeter (make-rectangle (make-point 0 0) 
                                              (make-point 4 2))) 
         12))
  (is (= (rectangle-area (make-rectangle (make-point 0 0) 
                                              (make-point 4 2))) 
         8))
  (is (= (rectangle-perimeter-bis (make-rectangle-bis 2 4)) 
         12))
  (is (= (rectangle-area-bis (make-rectangle-bis 2 4))
         8))
)
; Pure functional representation: Message-Passing
(deftest rectangles_test
  (is (= (first-bis (cons-bis 2 3)) 2))  
  (is (= (rest-bis (cons-bis 2 3)) 3))  
)
;2.4 
(deftest rectangles_test
  (is (= (car-bbis (cons-bbis 2 3)) 2))  
  (is (= (cdr-bbis (cons-bbis 2 3)) 3))  
  (is (= (car-bbis (car-bbis (cons-bbis (cons-bbis 2 3) (cons-bbis 4 5)))) 2))  
  (is (= (cdr-bbis (cdr-bbis (cons-bbis (cons-bbis 2 3) (cons-bbis 4 5)))) 5))  
)
;2.5
(deftest powers
  (is (= (num-powers 8 2) 3))
  (is (= (num-powers 9 3) 2))
  (is (= (num-powers 72 2) 3))
  (is (= (num-powers 72 3) 2))
)