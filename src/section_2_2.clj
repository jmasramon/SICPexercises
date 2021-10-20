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
    (not (seq? elem)) elem
    :else (+ (simpler-total-weight (structure (right-branch elem)))
             (simpler-total-weight (structure (left-branch elem))))))

;; c.
(defn torque [branch]
  (* (b-length branch)
     (if (is-final? branch)
       (structure branch)
       (total-weight (right-branch branch)))))

(defn simpler-torque
  [elem]
  (* (b-length elem)
     (simpler-total-weight (structure elem))))

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
  (if (seq? elem)
    (let [lb  (left-branch elem)
          rb (right-branch elem)]
      (and
       (= (simpler-torque lb)
          (simpler-torque rb))
       (simpler-is-balanced? (structure lb))
       (simpler-is-balanced? (structure rb))))
    true))
