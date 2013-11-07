(defn log2
  "log2(x), log2(0) = 0"
  [x]
  (if (= x 0) 0 (/ (Math/log x) (Math/log 2))))

(defn entropy
  "Entropy(S) = —p+log2(p+) —p- log2(p-)"
  [p-pos p-neg]
  (+ (* (- p-pos) (log2 p-pos))
     (* (- p-neg) (log2 p-neg))))

(entropy 1/4 1)
(entropy 1 0)     ; all positive => 0.0
(entropy 0 1)     ; all negative => 0.0
(entropy 1/2 1/2) ; 1/2 of each  => 1.0

(entropy )

(defn seq-entropy
  "Calculate the entropy of sequence 'sq' assuming the all positive
  numbers represent positive samples."
  [sq]
  (let [len (count sq)
        pos (count (filter pos? sq))
        neg (- len pos)]
    (entropy (/ pos len) (/ neg len))))

(seq-entropy '(1 0 1 0 1 1 1 1 1))

0.7642045065086203
(seq-entropy '(0 0 1 1 1 1 1 1 1 1))

(seq-entropy '(0 0 0 0 1 1 1 1))
;; 1.0
(seq-entropy '(0 0 0 0))
;; 0.0
(seq-entropy '(1 1 1 1))
;; 0.0


(defn string-to-bits
  "Returns a list containing 1s and 0s for the bits that make up the
  bytes of string 's'."
  [s]
  (mapcat #(for [i (range 8)]
             (if (not= 0 (bit-and (int (Math/pow 2 i)) %)) 1 0))
          (.getBytes s)))

(seq-entropy (string-to-bits "    "))
;;0.5435644431995964
(seq-entropy (string-to-bits "abcd"))
;;0.9744894033980523

(defn random-bit-sequence [len odds]
  (let [random-bit #(if (< (Math/random) odds) 1 0)]
    (repeatedly len random-bit)))

(random-bit-sequence 10 3/4)

(entropy 1/2 1/2)
=> 1.0
(seq-entropy (random-bit-sequence 10 1/2))
=> 0.8812908992306927
(seq-entropy (random-bit-sequence 100 1/2))
=> 0.9858150371789198
(seq-entropy (random-bit-sequence 10000 1/2))
=> 0.999970453403856

(entropy 0 1)
=> 0.0
(seq-entropy (random-bit-sequence 10 0.9))
(seq-entropy (random-bit-sequence 1000 0.9))

(string-to-bits " ")
=> (0 0 0 0 0 1 0 0)
(string-to-bits "a")
=> (1 0 0 0 0 1 1 0)

(seq-entropy (string-to-bits "aaaa"))
=> 0.9544340029249649
(seq-entropy (string-to-bits "    "))
=> 0.5435644431995964

(string-to-bits "abcd")
=> (1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 0 0 1 0 0 1 1 0)
(string-to-bits "    ")
=> (0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0)

(seq-entropy (string-to-bits "abcd"))
=> 0.9744894033980523
(seq-entropy (string-to-bits "@#$%"))
=> 0.8571484374283718