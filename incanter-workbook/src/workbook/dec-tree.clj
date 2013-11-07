(ns workbook.dec-tree)

;; An example drawn from:
;; http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture11.html

(defn log2
  "log2(x), log2(0) = 0"
  [x]
  (if (= x 0) 0 (/ (Math/log x) (Math/log 2))))

(defn entropy
  "Entropy(S) = -p + log2(p)"
  [p-pos]
  (* (- p-pos) (log2 p-pos)))

(defn rows-entropy
  "Returns the entropy of `rows`, optionally multiplying each subset by `multiplier`"
  ([rows] (rows-entropy rows 1))
  ([rows multiplier]
     (let [num-rows (count rows)]
       (reduce + (map #(* multiplier (entropy (/ % num-rows)))
                      (vals (frequencies (map last rows))))))))

(defn column-entropy
  "The summed output entropy of the unique subsets of column `column` in `rows`"
  [column rows]
  (let [num-rows (count rows)]
    (reduce + (map #(rows-entropy % (/ (count %) num-rows))
                   (vals (group-by #(nth % column) rows))))))

(defn gain
  "The gain in `current-entropy` provided by partitioning `rows` by the unique values in `column`, then summing the entropy of those partitions."
  [current-entropy column rows]
  (- current-entropy (column-entropy column rows)))

(defn make-decision-tree
  "Builds a decision tree from a table of input data."

  ;; This function signature slices and dices the input data and
  ;; passes the pieces to the other signature. It's a common pattern
  ;; to have a recursive function that takes an input that it then
  ;; uses to start a recurive process via another signature.

  ([data] (make-decision-tree (first data)
                              (rest data)
                              (set (range (- (count (first data)) 1)))
                              (rows-entropy (rest data))))

  ;; The work gets done here:
  ([column-names rows columns current-entropy]
     ;; if there are no more columns to consider, or there's only one
     ;; row left, we great a leaf node and return it.
     (if (or (empty? columns) (< (count rows) 2))

       ;; the leaf node is a brand new hash-map containing a single
       ;; K/V pair, :decision and the value of the remaining possible
       ;; decision. (N.B. We made a new hash-map at each leaf and
       ;; branch, building the tree depth first.)
       (hash-map :decision (last (first rows)))

       ;; otherwise, we get the gain in entropy for each column in
       ;; columns, sort out the highest and use that column
       (let [[max-gain best] (first (max (map #(vector (gain current-entropy % rows) %) columns)))]

         ;; if the max gain turns out to be <= 0 (it can be less than
         ;; zero because of floating point imprecision), we return a
         ;; leaf node, as above.
         (if (<= max-gain 0)
           (hash-map :decision (last (first rows)))

           ;; otherwise, we reduce the results of a series of
           ;; recursive calls to this function into the current
           ;; branch, which we'll then return to our caller.
           (reduce (fn [branch [group rows-for-group]]
                     ;; each reduction step assoc's a vector of the
                     ;; name of the column we're evaluating for this
                     ;; branch and the sub-group (column value) of the
                     ;; column to the result of another recursive call
                     (assoc branch
                       (vector (nth column-names best) group)
                       (make-decision-tree column-names
                                           ;; subset of rows from
                                           ;; group-by below
                                           rows-for-group
                                           ;; disjunction of columns
                                           ;; by this column so the
                                           ;; next recursive step
                                           ;; considers only the
                                           ;; remaining columns
                                           (disj columns best)
                                           ;; decrement current
                                           ;; entropy for the recursion
                                           (- current-entropy max-gain))))
                   {}
                   ;; this group-by will return a map where each key
                   ;; in a unique column value and the value is a
                   ;; vector of the rows that match that column value
                   (group-by #(nth % best) rows)))))))

;; how will they spend the weekend?
(def weekend-data
  [[:weather :parents :money :decision] ;; column headings
   [:sunny   :yes     :rich  :cinema]
   [:sunny   :no      :rich  :tennis]
   [:windy   :yes     :rich  :cinema]
   [:rainy   :yes     :poor  :cinema]
   [:rainy   :no      :rich  :stay-in]
   [:rainy   :yes     :poor  :cinema]
   [:windy   :no      :poor  :cinema]
   [:windy   :no      :rich  :shopping]
   [:windy   :yes     :rich  :cinema]
   [:sunny   :no      :rich  :tennis]])

(make-decision-tree weekend-data)
;; =>
;; {[:weather :rainy]
;;  {[:parents :no] {:decision :stay-in}, [:parents :yes] {:decision :cinema}},
;;  [:weather :windy]
;;  {[:parents :no] {[:money :rich] {:decision :shopping}, [:money :poor] {:decision :cinema}}, [:parents :yes] {[:money :rich] {:decision :cinema}}},
;;  [:weather :sunny]
;;  {[:parents :no] {:decision :tennis}, [:parents :yes] {:decision :cinema}}}


(reduce #(hash-map %2 %1) {} [:a :b :c :d])
;; => {:d {:c {:b {:a {}}}}}
