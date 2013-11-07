## Entropy homework
In response to https://gist.github.com/jackrusher/5201340

Some observations about the entropy of bit sequences:

```lisp
(seq-entropy '(0 0 0 0 1 1 1 1))
;; 1.0
(seq-entropy '(0 0 0 0))
;; 0.0
(seq-entropy '(1 1 1 1))
;; 0.0
```
As shown above, the length of the sequence matters, since the first
sequence is merely the concatenation of the next two. The entropy
calculation is by definition only working on the bits in aggregate and
isn't concerned with any inherent structure contained within them. I
suspect this property comes to play in compression (where low-entropy
sequences are more highly compressible) and error correction (where
smaller packet sizes on a noisey line are preferable to large ones.)

Since Math/random returns a floating point value which is (pseudo)
uniformly distributed over 0 and 1, we can divide the probability
space in two-- any samples that occur in the partition from 0 to
`odds` are positive samples.

```lisp
(defn random-bit-sequence [len odds]
  (let [random-bit #(if (< (Math/random) odds) 1 0)]
    (repeatedly len random-bit)))
```
As demonstrated below, the sequence entropy of random bit sequences
with odds 1/2 converges towards 1.0.

```lisp
(entropy 1/2 1/2)
=> 1.0
(seq-entropy (random-bit-sequence 10 1/2))
=> 0.8812908992306927
(seq-entropy (random-bit-sequence 100 1/2))
=> 0.9858150371789198
(seq-entropy (random-bit-sequence 10000 1/2))
=> 0.999970453403856
```

The question about the entropy values of `"    "` versus `"abcd"` is
slightly misleading. The `" "` character itself, when converted to its
ASCII numeric code and thence to binary only contains a single 1,
whereas `"a"` contains 3:

```lisp
(string-to-bits " ")
=> (0 0 0 0 0 1 0 0)
(string-to-bits "a")
=> (1 0 0 0 0 1 1 0)
```

Thus, the entropy for a string of `"aaaa"`'s will be greater than that
of a string of `"    "`'s, even though intuitively these strings have
the same quantity of disorder.

```lisp
(seq-entropy (string-to-bits "aaaa"))
=> 0.9544340029249649
(seq-entropy (string-to-bits "    "))
=> 0.5435644431995964
```

The string `"abcd"` has higher entropy than `"    "` not because `"abcd"` is
more complex than `"    "` in and of itself but because the binary
_encoding_ of `"abcd"` has a more equal ratio and is therefore more
complex.

```lisp
(string-to-bits "abcd")
=> (1 0 0 0 0 1 1 0 0 1 0 0 0 1 1 0 1 1 0 0 0 1 1 0 0 0 1 0 0 1 1 0)
(string-to-bits "    ")
=> (0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0)
```

To further illustrate the point, notice that the entropy of
`"abcd"` is lower than that of `"@#$%"` even though their symbolic
representation follows a similar pattern.

```lisp
(seq-entropy (string-to-bits "abcd"))
=> 0.9744894033980523
(seq-entropy (string-to-bits "@#$%"))
=> 0.8571484374283718
```
