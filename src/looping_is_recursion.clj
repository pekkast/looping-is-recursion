(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc left]
                 (if (zero? left)
                   acc
                   (recur (* acc base) (dec left))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq) nil
      (let [a (first a-seq)
            b (rest a-seq)]
        (if (empty? b) a (last-element b)))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not (== (count seq1) (count seq2))) false
    (== (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         cur a-seq]
    (if (empty? cur)
      nil
      (if (pred (first cur))
        acc
        (recur (inc acc) (rest cur))))))

(defn avg [a-seq]
  (if (empty? a-seq) nil
      (loop [sum 0
             cur a-seq]
        (if (empty? cur)
          (/ sum (count a-seq))
          (recur (+ sum (first cur)) (rest cur))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         o-seq a-seq]
    (if (empty? o-seq) res (recur (toggle res (first o-seq)) (rest o-seq)))))

(defn fast-fibo [n]
  (if (zero? n) n
      (loop [cur 1
             prev 0
             cnt 1]
        (if (= cnt n) cur (recur (+ cur prev) cur (inc cnt))))))

(defn cut-at-repetition [a-seq]
  (loop [o-seq a-seq
         r-seq []]
    (if (or (empty? o-seq) (some #(= (first o-seq) %) r-seq))
      r-seq
      (recur (rest o-seq) (conj r-seq (first o-seq))))))
