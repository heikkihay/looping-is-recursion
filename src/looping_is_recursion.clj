(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [result base n]
                 (if (zero? n)
                   result
                   (recur (* result base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [result a-seq]
               (if (empty? a-seq)
                   result
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq )))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (not= (count a-seq) (count b-seq)) false
      (not= (first a-seq) (first b-seq)) false
      :else (recur (rest a-seq) (rest b-seq))))]
      (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq1 a-seq]
    (cond
      (empty? seq1) nil
      (pred (first seq1)) index
      :else (recur (inc index) (rest seq1)))))

(defn avg [a-seq]
  (loop [sum 0
          seq1 a-seq]
    (if (empty? seq1) (/ sum (count a-seq))
      (recur (+ sum (first seq1)) (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem)
      (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         seq1 a-seq]
    (if (empty? seq1) a-set
      (recur (toggle a-set (first seq1)) (rest seq1)))))

(defn fast-fibo [n]
  (loop [ n n
          fib-2 0
          fib-1 1 ]
          (if (< n 1) fib-2
          (recur (dec n) fib-1 (+ fib-2 fib-1)))))

(defn cut-at-repetition [a-seq]
  (loop [index 0
         a-set []
         a-seq a-seq]
    (if (or (empty? a-seq)
      (some (partial = (first a-seq)) a-set)) a-set
    (recur (inc index) (conj a-set (first a-seq)) (rest a-seq)))))
