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