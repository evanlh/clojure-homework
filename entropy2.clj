;; Response to https://gist.github.com/jackrusher/5289206
;; Fill in this definition of seq-entropy that takes a second
;; parameter that's the function that returns 'true' when a given item
;; is positive.

(defn seq-entropy
  "Calculate the entropy of sequence 'sq' assuming the all positive
  numbers represent positive samples."
  [sq pos-func]
  (let [len (count sq)
        pos (count (filter pos-func sq))
        neg (- len pos)]
    (entropy (/ pos len) (/ neg len))))


;; ... validate it against your bit strings by passing 'pos?' as the
;; discriminator function, then use it for the rest of this
;; assignment.

(seq-entropy '(1 0 1 0 1 1 1 1 1) pos?)
;; 0.7642045065086203

;; next, we'll redefine entropy to the simplified measurement used in
;; the example we're pulling from
;; http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture11.html

(defn entropy
  "Entropy(S) = —p+log2(p+)"
  [p-pos]
  (* (- p-pos) (log2 p-pos)))

;; play with this to get a feel for the range it produces, note that
;; the maximum score is now 0.5
(entropy 1/4) ;;0.5
(entropy 1/2) ;;0.5
(entropy 1)  ;;-0.0
(entropy 8/9) ;; 0.1510444457264998

;; (remember to change your seq-entropy function to use this new
;; entropy API)
(defn seq-entropy
  "Calculate the entropy of sequence 'sq' assuming the all positive
  numbers represent positive samples."
  [sq pos-func]
  (let [len (count sq)
        pos (count (filter pos-func sq))]
    (entropy (/ pos len))))

(seq-entropy '(1 0 1 0 1 1 1 1 1) pos?)
;; 0.28199895063255087
;; ^^ range has changed

;; here's the old familiar weekend data
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

(rest weekend-data)
;; removes the first row, which is all headings

;; returns the last column, minus headings
(map last (rest weekend-data))

;; seq-entropy using the decision of :cinema as the positive
(seq-entropy (map last (rest weekend-data)) (partial = :cinema))

;; explain what (partial = :cinema) returns

;; (partial = :cinema) partially applies the the :cinema parameter to
;; the '=' function. In other words, it creates a new function that when
;; executed applies the :cinema parameter, along with a variable
;; number of additional parameters, to the = function and returns the result.

;; seq-entropy for each distinct decision in decisions
;; (pause to appreciate the beauty of the notation)
(let [decisions (map last (rest weekend-data))]
  (for [decision (distinct decisions)]
    (seq-entropy decisions (partial = decision))))

;; * pauses *

;; => (0.44217935649972373 0.46438561897747244 0.33219280948873625
;; 0.33219280948873625)

;; ^ compare these values to those at the worked example here:
;; http://www.doc.ic.ac.uk/~sgc/teaching/pre2012/v231/lecture11.html

;; They match!

;; group rows by the value of the nth column, in this case 0 (weather)
(group-by #(nth % 0) (rest weekend-data))
;; press: C-u C-x C-e on the next uncommented line, inspect and explain what's
;; returned and why it's useful to us.

;; {:sunny [[:sunny :yes :rich :cinema] [:sunny :no :rich :tennis] [:sunny :no :rich :tennis]], :windy [[:windy :yes :rich :cinema] [:windy :no :poor :cinema] [:windy :no :rich :shopping] [:windy :yes :rich :cinema]], :rainy [[:rainy :yes :poor :cinema] [:rainy :no :rich :stay-in] [:rainy :yes :poor :cinema]]}

;; The returned data is a map with the unique entries for the weather
;; outcome as the key and a vector of the column values that contained
;; that outcome as the corresponding values. I assume this data
;; structure is useful to us because it makes it easy-ish for us
;; to calculate the entropy of a specific outcome as it relates to an
;; attribute value, which is something we'll have to do when building our
;; decision tree.
