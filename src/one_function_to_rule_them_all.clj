(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (cond
    (empty? a-seq) ""
    :else (reduce str (interpose " " a-seq))))

(defn my-interpose [x a-seq]
  (let [my-conj (fn [a-seq elem]
                  (cond
                    (empty? a-seq) (conj a-seq elem)
                    :else (conj (conj a-seq x) elem)))]
    (reduce my-conj '[] a-seq)))

(defn my-count [a-seq]
  (let [helper (fn [c elem] (inc c))]
    (reduce helper 0 a-seq)))

(defn my-reverse [a-seq]
  (let [helper (fn [b-seq elem] (conj b-seq elem))]
    (reduce helper '() a-seq)))

(defn min-max-element [a-seq]
  (let [min- (reduce min a-seq)
        max- (reduce max a-seq)]
    [min- max-]))

(defn insert-helper [sorted-seq result-seq n]
  (cond
    (empty? sorted-seq) (conj result-seq n)
    (< n (first sorted-seq)) (into [] (concat (conj result-seq n) sorted-seq))
    :else (insert-helper (rest sorted-seq) (conj result-seq (first sorted-seq)) n)))

(defn insert [sorted-seq n]
  (insert-helper sorted-seq [] n))

(defn insertion-sort [a-seq]
  (reduce insert nil a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         seq a-seq]
    (if (empty? seq) res
        (recur (toggle res (first seq)) (rest seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x & more] (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))

; from predicates: https://github.com/papapezs/predicates/blob/master/src/predicates.clj
(defn pred-and-helper [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (pred-and-helper x y))
  ([x y & more] (reduce pred-and-helper (pred-and-helper x y) more)))

(defn my-map
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (my-map (partial apply f)
             (partition (count colls)
                        (apply interleave colls))))))