;; Homework III

;; first, some useful functions from the standard library...

(frequencies [1 2 1 2 1 2 3 4 2 3 4 5])
;; => {1 3, 2 4, 3 2, 4 2, 5 1}
;; a convenience function that returns a map from each value in the
;; input to the count of its occurence

;; Evan, here's the python equivalent:
;; import collections
;; a = [1 2 1 2 1 2 3 4 2 3 4 5]
;; counter=collections.Counter(a)
;; print(counter)
;; # Counter({1: 3, 2: 4, 3: 2, 4: 2, 5: 1})

(vals (frequencies [1 2 1 2 1 2 3 4 2 3 4 5]))
;; => (3 4 2 2 1)

;; Python:
;; print(counter.values())
;; # [3, 4, 2, 2, 1]

;; Also:
(keys (frequencies [1 2 1 2 1 2 3 4 2 3 4 5]))
;; => (1 2 3 4 5)

;; Python:
;; print(counter.keys())
;; # [1, 2, 3, 4, 5]

;; to get the entropy for a set of rows, we must take the frequency of
;; each decision within that set of rows, divide it by the number of
;; rows in set, then pass that fraction to entropy. This gives us a
;; list of entropies, one for each decision, which we must then sum.

(frequencies (map last (rest weekend-data)))
;; => {:cinema 6, :tennis 2, :stay-in 1, :shopping 1}

(vals (frequencies (map last (rest weekend-data))))
;; => (6 2 1 1)

(let [rows (rest weekend-data)
      num-rows (count rows)]
  (reduce + (map #(entropy (/ % num-rows)) (vals (frequencies (map last rows))))))
;; => 1.5709505944546687

;; ...reduce with + is summation, the map feeds the per-decision freqs
;; to the entropy function, first dividing by the number of rows. This
;; result is the "system entropy" in the original weekend-data example.

;; Part one:
;; Turn the above into a function (rows-entropy [rows multiplier]) that
;; does the same thing and returns the result multiplied by `multiplier`
;; (we'll need that to scale the results in part two)
(defn rows-entropy [rows multiplier]
  (let [num-rows (count rows)
        row-entropy (map #(entropy (/ % num-rows)) (vals (frequencies (map last rows))))]
    (* multiplier (reduce + row-entropy))))

(rows-entropy (rest weekend-data) 1)
;; => 1.5709505944546687

;; Part two:
;; Create a function (column-entropy [column rows]) where `column` is
;; a number between 0 and (num-columns - 1) and `rows` is a set of rows.
;; Column entropy should return the sum of the rows-entropy for each
;; subset of `column` (that is, groupby the column and pass each set of
;; values to rows-entropy with a `multiplier` = (size of subset /
;; number of rows)

(defn column-entropy [column rows]
  (let [cols (group-by #(nth % column) rows)
        num-rows (count rows)
        col-entropy #(rows-entropy % (/ (count %) num-rows))]
    (reduce + (map col-entropy (vals cols)))))

(column-entropy 0 (rest weekend-data)) ;; weather
;; => 0.8754887502163469
(column-entropy 1 (rest weekend-data)) ;; parents
;; => 0.9609640474436811


;; Part three
;; create a function (gain [current-entropy column rows]) that returns
;; the entropy gain of processing `column`. You should be able to
;; figure out how to do this (and check your work) using the info at
;; http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture11.html

(defn gain [current-entropy column rows]
  (- current-entropy (column-entropy column rows)))

(gain (rows-entropy (rest weekend-data) 1) 0 (rest weekend-data)) ;; weather
;; => 0.6954618442383218
(gain (rows-entropy (rest weekend-data) 1) 1 (rest weekend-data)) ;; parents
;; => 0.6099865470109875
(gain (rows-entropy (rest weekend-data) 1) 2 (rest weekend-data)) ;; money
;; => 0.2812908992306926

;; Matches!! Success!

;; in the next installment, we will finish the ID3 algorithm and have
;; a working machine learning implementation.)
