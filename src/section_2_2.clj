(ns section-2-2)

(defn length [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(defn length-iter [items]
  (loop [items items acc 0]
    (if (empty? items)
      acc
      (recur (rest items) (+ 1 acc)))))

(defn append [l1 l2]
  (if (= 0 (length l1))
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
    (if (= 0 (length l1))
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
  (cond (= 0 amount) 1
        (or (< amount 0)
            (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (- kinds-of-coins 1))
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
    (cond (= 0 amount) 1
          (or (< amount 0)
              (empty? coins)) 0
          :else (+ (parametrized-cc amount coins-minus-first)
                   (parametrized-cc (- amount
                                       first-denomination)
                                    coins)))))
