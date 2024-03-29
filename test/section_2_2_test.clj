(ns section-2-2-test
  (:require [clojure.test :refer [deftest is]]
            [section-2-2 :refer :all]))

;; 2.17
(deftest  length-test
  (is (= 4 (length '(23 72 149 34))))
  (is (zero? (length '()))))

(deftest  length-iter-test
  (is (= 4 (length-iter '(23 72 149 34))))
  (is (zero? (length-iter '()))))

(deftest  append-test
  (is (= '(34 149 72 23 1 2 3)
         (append '(23 72 149 34) '(1 2 3)))))

(deftest  last-pair-test
  (is (= '(34)
         (last-pair '(23 72 149 34)))))

;; 2.18
(deftest  my-reverse-test
  (is (= '(34 149 72 23)
         (my-reverse '(23 72 149 34)))))

;; 2.19
(deftest count-change-test
  (is (= 292 (count-change 100))))

(deftest  parametrized-cc-test
  (is (= 292 (cc 100 5)))
  (is (= 292 (count-change 100)))
  (is (= 292 (parametrized-cc 100 us-coins)))
  (is (= 292 (parametrized-cc 100 (reverse us-coins))))
  (is (= 559 (parametrized-cc 25 uk-coins)))
  (is (= 559 (parametrized-cc 25 (reverse uk-coins)))))

(deftest same-parity-test
  (is (= '(1 3 5)
         (same-parity 1 2 3 4 5))))

;; 2.21
(deftest square-list-wo-map-test
  (is (= '(1 4 9 16)
         (square-list-wo-map '(1 2 3 4)))))

(deftest square-list-test
  (is (= '(1 4 9 16)
         (square-list '(1 2 3 4)))))

;; 2.23
;; (deftest fore-each-test
;;   (is (= nil
;;          (for-each #(print % " ") '(1 2 3 4)))))

;; 2.27
(deftest deep-reverse-test
  (is (= '((3 4) (1 2))
         (my-reverse '((1 2) (3 4)))))
  (is (= '((4 3) (2 1))
         (deep-reverse '((1 2) (3 4)))))
  (is (= '((4 3) (2 1))
         (better-deep-reverse '((1 2) (3 4))))))


;; 2.28
(def x '((1 2) (3 4)))

(deftest fringe-test
  (is (= '(1 2 3 4)
         (fringe x)))
  (is (= '(1 2 3 4 1 2 3 4)
         (fringe (list x x))))
  (is (= '(1 2 3 4 1 2 3 4)
         (better-fringe (list x x)))))

;; 2.29
(deftest is-mobile?-test
  (is (= false
         (is-mobile? (make-branch 3 2))))
  (is (= false
         (is-mobile? (make-branch 1 7))))
  (is (= true
         (is-mobile? (make-mobile (make-branch 3 2)  (make-branch 1 7))))))

(deftest is-branch?-test
  (is (= true
         (is-branch? (make-branch 3 2))))
  (is (= true
         (is-branch? (make-branch 1 7))))
  (is (= true
         (is-branch?  (make-branch 3 (make-mobile (make-branch 3 2)
                                                  (make-branch 1 7))))))
  (is (= false
         (is-branch? (make-mobile (make-branch 3 2)  (make-branch 1 7))))))

(deftest is-final?-test
  (is (= true
         (is-final? (make-branch 3 2))))
  (is (= true
         (is-final? (make-branch 1 7))))
  (is (= false
         (is-final?  (make-branch 3 (make-mobile (make-branch 3 2)
                                                 (make-branch 1 7)))))))

(deftest total-weight-test
  (is (= 9
         (total-weight (make-mobile (make-branch 3 2)
                                    (make-branch 1 7)))))
  (is (= 16
         (total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 3 2)
                                                                (make-branch 1 7)))
                                    (make-branch 1 7))))))

(deftest simpler-total-weight-test
  (is (= 3
         (simpler-total-weight 3)))
  (is (= 9
         (simpler-total-weight (make-mobile (make-branch 3 2)
                                            (make-branch 1 7)))))
  (is (= 16
         (simpler-total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 3 2)
                                                                        (make-branch 1 7)))
                                            (make-branch 1 7))))))

(deftest torque-test
  (is (= 6
         (torque (make-branch 3 2))))
  (is (= 7
         (torque (make-branch 1 7))))
  (is (= 18
         (torque (make-branch 2 (make-mobile  (make-branch 3 2)
                                              (make-branch 1 7))))))
  (is (= 36
         (torque (make-branch 2 (make-mobile  (make-branch 3
                                                           (make-mobile  (make-branch 3 2)
                                                                         (make-branch 1
                                                                                      (make-mobile  (make-branch 3 2)
                                                                                                    (make-branch 1 7)))))
                                              (make-branch 1 7)))))))
(deftest simpler-torque-test
  (is (= 6
         (simpler-torque (make-branch 3 2))))
  (is (= 7
         (simpler-torque (make-branch 1 7))))
  (is (= 18
         (simpler-torque (make-branch 2 (make-mobile  (make-branch 3 2)
                                                      (make-branch 1 7))))))
  (is (= 36
         (simpler-torque (make-branch 2 (make-mobile  (make-branch 3
                                                                   (make-mobile  (make-branch 3 2)
                                                                                 (make-branch 1
                                                                                              (make-mobile  (make-branch 3 2)
                                                                                                            (make-branch 1 7)))))
                                                      (make-branch 1 7)))))))



(deftest is-balanced?-test
  (is (= false
         (is-balanced? (make-mobile (make-branch 3 2)
                                    (make-branch 1 7)))))
  (is (= true
         (is-balanced? (make-mobile (make-branch 3 2)
                                    (make-branch 1 6)))))
  (is (= false
         (is-balanced? (make-mobile (make-branch 3 2)
                                    (make-branch 1 7)))))
  (is (= false
         (is-balanced? (make-mobile (make-branch 3 2)
                                    (make-branch 1
                                                 (make-mobile  (make-branch 3 2)
                                                               (make-branch 1 4)))))))
  (is (= true
         (is-balanced? (make-mobile (make-branch 3 2)
                                    (make-branch 1
                                                 (make-mobile  (make-branch 4 2)
                                                               (make-branch 2 4))))))))

(deftest simpler-is-balanced?-test
  (is (= false
         (simpler-is-balanced? (make-mobile (make-branch 3 2)
                                            (make-branch 1 7)))))
  (is (= true
         (simpler-is-balanced? (make-mobile (make-branch 3 2)
                                            (make-branch 1 6)))))
  (is (= false
         (simpler-is-balanced? (make-mobile (make-branch 3 2)
                                            (make-branch 1 7)))))
  (is (= false
         (simpler-is-balanced? (make-mobile (make-branch 3 2)
                                            (make-branch 1
                                                         (make-mobile  (make-branch 3 2)
                                                                       (make-branch 1 4)))))))
  (is (= true
         (simpler-is-balanced? (make-mobile (make-branch 3 2)
                                            (make-branch 1
                                                         (make-mobile  (make-branch 4 2)
                                                                       (make-branch 2 4))))))))

;; 2.30.
;; previous
(deftest scale-tree-test
  (is (= '(2 4)
         (scale-tree '(1 2) 2)))
  (is (= '(3 (6 9))
         (scale-tree '(1 (2 3)) 3)))
  (is (= '((4 (8 12)) (8 12))
         (scale-tree '((1 (2 3)) (2 3)) 4))))

(deftest scale-tree-map-test
  (is (= '(2 4)
         (scale-tree-map '(1 2) 2)))
  (is (= '(3 (6 9))
         (scale-tree-map '(1 (2 3)) 3)))
  (is (= '((4 (8 12)) (8 12))
         (scale-tree-map '((1 (2 3)) (2 3)) 4))))

;; actual
(deftest square-tree-test
  (is (= '(1 4)
         (square-tree '(1 2))))
  (is (= '(1 (4 9))
         (square-tree '(1 (2 3)))))
  (is (= '((1 (4 9)) (4 9))
         (square-tree '((1 (2 3)) (2 3))))))

;; 2.31
(defn square-tree-map
  [tree]
  (tree-map #(* % %) tree))

(deftest square-tree-map-test
  (is (= '(1 4)
         (square-tree-map '(1 2))))
  (is (= '(1 (4 9))
         (square-tree-map '(1 (2 3)))))
  (is (= '((1 (4 9)) (4 9))
         (square-tree-map '((1 (2 3)) (2 3))))))

(deftest subsets-test
  (is (= '((2 3) (2) () (3) (1 3) (1) (1 2) (1 2 3))
         (subsets '(1 2 3)))))

;;  sequences as conventional interfaces
(deftest my-map-test
  (is (= '(2 4 6)
         (my-map (partial * 2) '(1 2 3)))))

(deftest my-filter-test
  (is (= '(1 3)
         (my-filter odd? '(1 2 3))))
  (is (= '(2)
         (my-filter even? '(1 2 3)))))

(deftest my-accumulator-test
  (is (= 6
         (my-accumulator + 0 '(1 2 3))))
  (is (= 24
         (my-accumulator * 1 '(2 3 4))))
  (is (= '(2 3 4)
         (my-accumulator cons '() '(2 3 4)))))

;; 2.33 
(deftest accomulator-map-test
  (is (= '(2 4 6)
         (accomulator-map (partial * 2) '(1 2 3)))))

(deftest accomulator-filter-test
  (is (= '(1 3)
         (accomulator-filter odd? '(1 2 3))))
  (is (= '(2)
         (accomulator-filter even? '(1 2 3)))))

(deftest  accomulator-append-test
  (is (= '(23 72 149 34 1 2 3)
         (accomulator-append '(23 72 149 34) '(1 2 3)))))

(deftest  accomulator-length-test
  (is (= 4
         (accomulator-length '(23 72 149 34))))
  (is (= 7
         (accomulator-length '(23 72 149 34 2 3 4)))))

;; 2.34 
(deftest horner-eval-test
  (is (= 34
         (horner-eval 3 '(1 2 3))))
  (is (= 79
         (horner-eval 2 '(1 3 0 5 0 1)))))

(deftest simpler-horner-eval-test
  (is (= 34
         (simpler-horner-eval 3 '(1 2 3))))
  (is (= 79
         (simpler-horner-eval 2 '(1 3 0 5 0 1)))))

;; 2.35
(deftest accomulator-count-leaves-test
  (is (= 8
         (accomulator-count-leaves '(((1 2)
                                      3 4)
                                     ((1 2)
                                      3 4))))))

(deftest accomulator-count-leaves-mapped-test
  (is (= 8
         (accomulator-count-leaves-mapped '(((1 2)
                                      3 4)
                                     ((1 2)
                                      3 4))))))
;; 2.36 
(deftest accumulate-n-test
  (is (= '(22 26 30)
         (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))))

(deftest simpler-accumulate-n-test
  (is (= '(22 26 30)
         (simpler-accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))))

;; 2.37
(def matrix '((1 2 3 4) (5 6 7 8) (9 10 11 12)))

(deftest dot-product-test
  (is (= 70
         (dot-product '(1 2 3 4) '(5 6 7 8)))))

(deftest simpler-dot-product-test
  (is (= 70
         (simpler-dot-product '(1 2 3 4) '(5 6 7 8)))))

(deftest matrix-*-vector-test
  (is (=  '(70 174 278)
          (matrix-*-vector '((1 2 3 4) (5 6 7 8) (9 10 11 12)) 
                           '(5 6 7 8)))))

(deftest simpler-matrix-*-vector-test
  (is (=  '(70 174 278)
          (simpler-matrix-*-vector '((1 2 3 4) (5 6 7 8) (9 10 11 12)) 
                                   '(5 6 7 8)))))


(deftest matrix-*-matrix-test
  (is (=  '((38 98 158) (44 116 188) (50 134 218))
          (matrix-*-matrix '((1 2 3) (5 6 7) (9 10 11)) 
                           '((1 2 3) (5 6 7) (9 10 11)))))
  (is (=  '((0 -3) (-10 -1))
          (matrix-*-matrix '((0 4 -2) (-4 -3 0))
                           '((0 1) (1 -1) (2 3))))))

(deftest transpose-test
  (is (=  '((1 5 9) (2 6 10) (3 7 11) (4 8 12))
          (transpose '((1 2 3 4) (5 6 7 8) (9 10 11 12))))))

(deftest simpler-transpose-test
  (is (=  '((1 5 9) (2 6 10) (3 7 11) (4 8 12))
          (simpler-transpose '((1 2 3 4) (5 6 7 8) (9 10 11 12))))))


;; 2.38
(deftest fold-left-test
  (is (= 6
         (fold-left + 0 '(1 2 3))))
  (is (= 24
         (fold-left * 1 '(2 3 4))))
  (is (= (reverse '(2 3 4))
         (fold-left conj '() '(2 3 4))))
  (is (= '(((() 2) 3) 4)
         (fold-left list '() '(2 3 4))))
   (is (= '(2 (3 (4 ())))
          (my-accumulator list '() '(2 3 4))))
  (is (= 1/6
         (fold-left / 1 '(1 2 3))))
  (is (= 3/2
         (my-accumulator / 1 '(1 2 3))))
  )

