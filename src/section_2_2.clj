(ns section-2-2)

(defn length [items]
  (if (empty? items)
    0
    (inc (length (rest items)))))

(defn length-iter [items]
  (loop [items items acc 0]
    (if (empty? items)
      acc
      (recur (rest items) (inc acc)))))

(defn append [l1 l2]
  (if (zero? (length l1))
    l2
    (append (rest l1) (cons (first l1) l2))))

;; 2.17 without using last !!
(defn last-pair [list]
  (if (= 1 (length list))
    list
    (last-pair (rest list))))

;; 2.18 without using first !!
(defn my-reverse [l]
  (loop [l1 l
         l2 '()]
    (if (zero? (length l1))
      l2
      (recur (rest l1) (cons (first l1) l2)))))

;;2.19
;; from 1.2.2 ... 

(defn first-denomination [kinds-of-coins]
  (cond (= 1 kinds-of-coins) 1
        (= 2 kinds-of-coins) 5
        (= 3 kinds-of-coins) 10
        (= 4 kinds-of-coins) 25
        (= 5 kinds-of-coins) 50))

(defn cc [amount kinds-of-coins]
  (cond (zero? amount) 1
        (or (neg? amount)
            (zero? kinds-of-coins)) 0
        :else (+ (cc amount
                     (dec kinds-of-coins))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5))

(def us-coins '(50 25 10 5 1))
(def uk-coins '(100 50 25 10 5 2 1 0.5))

(defn parametrized-cc [amount coins]
  (let [first-denomination (first coins)
        coins-minus-first (rest coins)]
    (cond
      (zero? amount) 1
      (or (neg? amount)
          (empty? coins)) 0
      :else (+ (parametrized-cc amount coins-minus-first)
               (parametrized-cc (- amount
                                   first-denomination)
                                coins)))))

;; 2.20 
(defn same-parity
  [first & rest]
  (when (seq rest)
    (if (odd? first)
      (cons first (filter odd? rest))
      (cons first (filter even? rest)))))

;; 2.21 
(defn square-list-wo-map
  [items]
  (when (seq items)
    (cons (* (first items) (first items)) (square-list-wo-map (rest items)))))

(defn square-list [items]
  (map #(* % %) items))

;; 2.23       
(defn for-each
  [f list]
  (when (seq list)
    (f (first list))
    (for-each f (rest list))))

;; 2.27
(defn deep-reverse
  [tree]
  (letfn [(my-loop [l1 l2]
            (if (zero?
                 (length l1))
              l2
              (let [cur (first l1)
                    res (rest l1)]
                (if (seq? cur)
                  (my-loop res
                           (cons (my-loop cur '())
                                 l2))
                  (my-loop res
                           (cons cur
                                 l2))))))]
    (my-loop tree '())))

(defn better-deep-reverse
  [tree]
  (if-not (seq? tree)
    tree
    (map better-deep-reverse (my-reverse tree))))

;; 2.28
(defn fringe
  [tree]
  (letfn [(my-loop [l1 l2]
            (cond
              (not (seq? l1)) (cons l1 l2)
              (empty? l1) l2
              :else (concat (my-loop (first l1) l2)
                            (my-loop (rest l1) '()))))]
    (my-loop tree '())))

(defn better-fringe
  [tree]
  (cond
    (not (seq? tree)) (list tree)
    (empty? tree) tree
    :else (concat (better-fringe (first tree)) (better-fringe (rest tree)))))

;; 2.29
;; mobile API
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

;; a.
(defn left-branch
  [mobile]
  (first mobile))

(defn right-branch
  [mobile]
  (second mobile))

(defn b-length
  [branch]
  (first branch))

(defn structure
  [branch]
  (second branch))

;; b.
(defn is-mobile? [potential-mobile]
  (and
   (seq? (left-branch potential-mobile))
   (seq? (right-branch potential-mobile))))

(defn is-branch? [potential-branch]
  (not (is-mobile? potential-branch)))

(defn is-final? [branch]
  (not (seq? (structure branch))))

(defn has-sub-mobile? [branch] (not (is-final? branch)))

(defn total-weight
  [mobile]
  (let [lb (left-branch mobile)
        rb (right-branch mobile)
        total-branch-weight #(if (is-final? %)
                               (structure %)
                               (total-weight (structure %)))]
    (+ (total-branch-weight lb)
       (total-branch-weight rb))))

(defn simpler-total-weight
  [elem]
  (cond
    (nil? elem) 0
    (not (seq? elem)) elem
    :else (+ (simpler-total-weight (structure (left-branch elem)))
             (simpler-total-weight (structure (right-branch elem))))))

;; c.
(defn torque [branch]
  (* (b-length branch)
     (if (is-final? branch)
       (structure branch)
       (total-weight (structure branch)))))

(defn simpler-torque [branch]
  (* (b-length branch) (simpler-total-weight (structure branch))))

(defn is-balanced?
  [elem]
  (if (is-mobile? elem)
    (and (= (torque (left-branch elem))
            (torque (right-branch elem)))
         (is-balanced? (left-branch elem))
         (is-balanced? (right-branch elem)))
    (if (is-final? elem)
      true
      (is-balanced? (structure elem)))))

(defn simpler-is-balanced?
  [elem]
  (cond
    (not (seq? elem)) true
    :else (and
           (= (torque (left-branch elem))
              (torque (right-branch elem)))
           (simpler-is-balanced? (structure (left-branch elem)))
           (simpler-is-balanced? (structure (right-branch elem))))))


;; 2.30.
;; previous
(defn scale-tree
  [tree factor]
  (cond
    (not (seq? tree)) (* factor tree)
    :else (list (scale-tree (left-branch tree) factor)
                (scale-tree (right-branch tree) factor))))

(defn scale-tree-map
  [tree factor]
  (cond
    (not (seq? tree)) (* factor tree)
    :else (map #(scale-tree-map % factor) tree)))

;; actual
(defn square-tree
  [tree]
  (cond
    (not (seq? tree)) (* tree tree)
    :else (map #(square-tree %) tree)))

;; 2.31
(defn tree-map
  [f tree]
  (cond
    (not (seq? tree)) (f tree)
    :else (map #(tree-map f %) tree)))

;; 2.32
(defn subsets
  [s]
  ;;(println "s: " s)
  (if (empty? s)
      ;; (println "empty s")
    '(())
    (let [pending (subsets (rest s))]
      ;; (println "pending: " pending)
      ;; (println "(first s): " (first s))
      ;; (println "appending: "       (map #(cons (first s) %) pending))
      ;; (println "sub-result: " (append pending
      ;;                                 (map #(cons (first s) %)
      ;;                                      pending)))
      (append pending
              (map #(cons (first s) %)
                   pending)))))

;;  sequences as conventional interfaces
(defn my-map
  [f list]
  (if (empty? list)
    '()
    (cons (f (first list)) (my-map f (rest list)))))

(defn my-filter
  [p xs]
  (cond
    (empty? xs) '()
    (p (first xs)) (cons (first xs) (my-filter p (rest xs)))
    :else (my-filter p (rest xs))))

(defn my-accumulator
  "f has [x acc]"
  [f i xs]
  (cond
    (empty? xs) i
    :else (let [x (first xs)
                acc (my-accumulator f i (rest xs))]
            (f x acc))))

;; 2.33
(defn accomulator-map
  [f xs]
  (my-accumulator #(cons (f %1) %2)
                  '() xs))

(defn accomulator-filter
  [p xs]
  (my-accumulator #(if (p %1) (cons %1 %2) %2)
                  '() xs))

(defn accomulator-append
  [xs ys]
  (my-accumulator #(cons %1 %2)
                  ys xs))

(defn accomulator-length
  [xs]
  (my-accumulator #(inc %2) 0 xs))

;; 2.34
(defn horner-eval
  [x-val coef-xs]
  (+ (first coef-xs)
     (my-accumulator #(* x-val (+ %2 %1)) 0 (rest coef-xs))))

(defn simpler-horner-eval
  [x-val coef-xs]
  (my-accumulator #(+ %1 (* x-val %2)) 0  coef-xs))

;; 2.35
(defn accomulator-count-leaves
  [t]
  (my-accumulator #(cond
                     (seq? %1) (+ %2 (accomulator-count-leaves %1))
                     :else (inc %2)) 0 t))

(defn accomulator-count-leaves-mapped
  [t]
  (my-accumulator + 0 (map #(cond
                              (seq? %) (accomulator-count-leaves-mapped %)
                              :else 1)
                           t)))

;; 2.36 
(defn accumulate-n
  [f i xss]
  (letfn  [(pf [f i xss acc]
             (let [all-empty? (my-accumulator #(and %1 %2) true (map empty? xss))]
               (cond
                 all-empty? (reverse acc)
                 :else (let [sum-of-firsts (my-accumulator f i (map first xss))
                             new-acc (cons sum-of-firsts acc)
                             nex-xss (map rest xss)]
                         (pf
                          f
                          i
                          nex-xss
                          new-acc)))))]
    (pf f i xss '())))

(defn simpler-accumulate-n
  [f i xss]
  (cond (my-accumulator #(and %1 %2) true (map empty? xss)) '()
        :else (cons  (my-accumulator f i (map first xss))
                     (simpler-accumulate-n f i (map rest xss))))
)

;; 2.37
(defn dot-product
  [v w]
  (my-accumulator #(+ (* (first %1) (second %1)) %2) 0 (map list v w))
)

(defn simpler-dot-product
  [v w]
  (my-accumulator + 0 (map * v w)))

(defn matrix-*-vector
  [m v]
)

(defn matrix-*-matrix
  [m n]
)

(defn transpose
  [m]
)