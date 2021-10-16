(ns section-2-2-test
  (:require [clojure.test :refer [deftest is]]
            [section-2-2 :refer :all]))

;; 2.17
(deftest  length-test
  (is (= 4 (length '(23 72 149 34))))
  (is (= 0 (length '()))))

(deftest  length-iter-test
  (is (= 4 (length-iter '(23 72 149 34))))
  (is (= 0 (length-iter '()))))

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
  )
