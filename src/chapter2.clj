(ns chapter2)

(apply hash-map [:a 5 :b 6])

(def args [2 -2 10])
(apply * 0.5 3 args)
(def only-strings (partial filter string?))
(only-strings ["a" 5 "b" 6])

(#(filter string? %) ["a" 5 "b" 6])
(#(* 1 3 % ) 4)

(#(filter % ["a" 5 "b" 6]) string?)
(#(filter % ["a" 5 "b" 6]) number?)

(#(map *) [1 2 3] [4 5 6] [7 8 9]) 
(#(map * % %2 %3) [1 2 3] [4 5 6] [7 8 9]) 
(#(map * % %2 %3) [1 2 3] [4 5 6] ) 
(#(apply map * %&) [1 2 3] [4 5 6] [7 8 9]) 
((partial map *) [1 2 3] [4 5 6] [7 8 9])

(defn negated-sum-str
  [& numbers]
  (str (- (apply + numbers))))
(negated-sum-str 10 12 3.4)

(def negated-sum-str1 (comp str - +))
(negated-sum-str1 10 12 3.4)

(require '[clojure.string :as str])
(def camel->keyword (comp keyword
                          str/join
                          (partial interpose \-)
                          (partial map str/lower-case)
                          #(str/split % #"(?<=[a-z])(?=[A-Z])")
                          ))
(camel->keyword "CameCase")
(camel->keyword "lowerCameCase")
(#(str/split % #"(?<=[a-z])(?=[A-Z])") "CameCase")

(defn camel->keyword1
  [s]
  (->> (str/split s #"(?<=[a-z])(?=[A-Z])")
       (map str/lower-case)
       (interpose \-)
       str/join
       keyword))

(camel->keyword1 "CameCase")
(camel->keyword1 "lowerCameCase")

(def camel-pairs->map (comp (partial apply hash-map)
                            (partial map-indexed (fn [i x]
                                                   (if (odd? i)
                                                     x
                                                     (camel->keyword x))))))

(camel-pairs->map ["CamelCase" 5 "lowerCamelCase" 3])

(defn adder
  [n]
  (fn [x] (+ n x)))
((adder 5) 3)

(defn doubler
  [f]
  (fn [& args]
    (* 2 (apply f args))))
(def double-+ (doubler +))
(double-+ 1 2 3)

(defn print-logger
  [writer]
  #(binding [*out* writer]
     (println %)))

(def *out*-logger (print-logger *out*))
(*out*-logger "hello")

(def writer (java.io.StringWriter.))
(def retained-logger (print-logger writer))
(retained-logger "hello")
(str writer)

(require 'clojure.java.io)

(defn file-logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
     ((print-logger f) %)))
(def log->file (file-logger "message.log"))
(log->file "hello logger")

(defn multi-logger
  [& logger-fns]
  #(doseq [f logger-fns]
     (f %)))
(def log (multi-logger
           (print-logger *out*)
           (file-logger "message.log")))
(log "hello again")

(defn timestamped-logger
  [logger]
  #(logger (format "[%1$tY-%1$tm-%1$te %1$tH-%1$tM-%1$tS] %2$s" (java.util.Date.) %)))
(def log-timestamped (timestamped-logger
                       (multi-logger
                         (print-logger *out*)
                         (file-logger "message.log"))))
(log-timestamped "goodbye, now")

