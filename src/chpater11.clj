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
