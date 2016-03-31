(ns chapter11)
(class (inc (Integer. 5)))

(dec 1)
(dec 1.0)
(dec 1N)
(dec 1M)
(dec 5/4)

(class 1)
(class 1.0)
(class 1N)
(class 1M)
(class 5/4)

(* 3 0.08 1/4 6N 1.2M)
(< 1 1.6 7/3 9N 14e9000M)

(+ 0.1 0.1 0.1)
(+ 1/10 1/10 1/10)
(+ 7/10 1/10 1/10 1/10)
(double 1/3)
(rationalize 0.45)

(+ 1 1)
(+ 1 1.5)
(+ 1 1N)
(+ 1.1M 1N)

(defn squares-sum
  [& vals]
  (reduce + (map * vals vals)))
(squares-sum 1 4 10)
(squares-sum 1 4 10 20.5)
(squares-sum 1 4 10 9N)
(squares-sum 1 4 10 9N 5.6M)
(squares-sum 1 4 10 25/2)

(.hashCode (BigInteger. "6948736584"))
(.hashCode (Long. "6948736584"))

(def k Long/MAX_VALUE)
k

(inc k)

(inc (bigint k))

(* 100 (bigdec Double/MAX_VALUE))

102233726364547159000
(* 2 102233726364547159000)

(inc' 1)
(inc' (dec' Long/MAX_VALUE))

(unchecked-dec Long/MIN_VALUE)
(unchecked-multiply 92233720368547758 1000)

(inc Long/MAX_VALUE)
(set! *unchecked-math* true)
(inc Long/MAX_VALUE)
(set! *unchecked-math* false)

(binding [*unchecked-math* true]
  (inc Long/MAX_VALUE))
(/ 22M 7)

(with-precision 10 (/ 22M 7))
(with-precision 10 :rounding FLOOR (/ 22M 7))

(set! *math-context* (java.math.MathContext. 10 java.math.RoundingMode/FLOOR))
(/ 22M 7)
(/ 355M 113)

(identical? "foot" (str "fo" "ot"))
(let [a (range 10)]
  (identical? a a))

(identical? 5/4 (+ 3/4 1/2))
(identical? 5.4321 5.4321)
(identical? 2600 2600)

(identical? 127 (dec 128))
(identical? 128 (dec 129))

(= {:a 1 :b ["hi"]}
   (into (sorted-map) [[:b ["hi"]] [:a 1]])
   (doto (java.util.HashMap.)
     (.put :a 1)
     (.put :b ["hi"])))
(= 1 1N (Integer. 1) (Short. (short 1)) (Byte. (byte 1)))
(= 1.25 (Float. 1.25))

(= 1 1.0)
(= 1N 1M)
(= 1.25 5/4)

(== 0.125 0.125M 1/8)
(== 4 4N 4.0 4.0M)

(defn equiv?
  "Same as `==`, but doesn't throw an exception if any arguments are not numbers."
  [& args]
  (and (every? number? args)
       (apply == args)))
(equiv? "foo" 1)
(equiv? 4 4N 4.0 4.0M)
(equiv? 0.125 0.125M 1/8)
