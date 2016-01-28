(ns chapter1)

(defn hypot
  [x y]
  (let [x2 (* x x) 
        y2 (* y y)]
    (Math/sqrt (+ x2 y2))))
(hypot 3 4)

(def v [42 "df" 99.2 [5 12]])
(let [[x y z] v]
  (+ x z))
(let [[x _ _ [y z]] v]
  (+ x y z))
(let [[x & rest] v] rest)

(let [[x _ z :as original-vector] v]
  (conj original-vector (+ x z)))


(def m {:a 5 :b 6
        :c [7 8 9]
        :d {:e 10 :f 11}
        "foo" 88
        42 false})

(let [{a :a b :b} m]
  (+ a b))

(let [{f "foo"} m]
  (+ f 12))
(let [{v 42} m]
  (if v 1 0))

(let [{x 3 y 8} [12 0 0 -18 44 6 0 0 1]] 
  (+ x y))
(let [{{e :e} :d} m]
  (* 2 e))

(let [{[x _ y] :c} m] 
  (+ x y))

(def map-in-vector ["James" {:birthday (java.util.Date. 73 1 6)}])

(let [[name {bd :birthday}] map-in-vector]
  (str name " was born on " bd))

(let [{r1 :x r2 :y :as randoms}
      (zipmap [:x :y :z] (repeatedly (partial rand-int 10)))]
  (assoc randoms :sum (+ r1 r2)))

(let [{k :unkown x :a 
       :or {k 50}} m]
  (+ k x))

(let [{k :unkown x :a} m
      k (or k 50)]
  (+ k x))

(let [{opt1 :option} {:option false} 
      opt1 (or opt1 true)
      {opt2 :option :or {opt2 true}} {:option false}]
  {:opt1 opt1 :opt2 opt2})

(def chas {:name "Chas" :age 31 :location "Massachusetts"})
(let [{name :name age :age location :location} chas]
  (format "%s is %s years old and lives in %s." name age location))

(let [{:keys [name age location]} chas]
  (format "%s is %s years old and lives in %s." name age location))

(def chas {"name" "Chas" "age" 31 "location" "Massachusetts"})
(let [{:strs [name age location]} chas]
  (format "%s is %s years old and lives in %s." name age location))

(def chas {'name "Chas" 'age 31 'location "Massachusetts"})
(let [{:syms [name age location]} chas]
  (format "%s is %s years old and lives in %s." name age location))

(def user-info ["robert8990" 2011 :name "bob" :city "Boston"])
(let [[username account-year & extra-info] user-info
      {:keys [name city]} (apply hash-map extra-info)]
  (format "%s is in %s" name city))
(let [[username account-year & {:keys [name city]}] user-info]
  (format "%s is in %s" name city))


(fn [x]
     (+ 10 x))

((fn [x] (+ 10 x)) 8)

(let [x 8]
  (+ 10 x))

((fn [x y z] (+ x y z))
 3 4 12)

(let [x 3
      y 4
      z 12]
  (+ x y z))

(def strange-adder (fn adder-self-reference
                     ([x] (adder-self-reference x 1))
                     ([x y] (+ x y))))

(strange-adder 10)
(strange-adder 10 50)

(defn concat-rest [x & rest]
  (apply str (butlast rest)))

(concat-rest 0 1 2 3 4)

(defn make-user
  [& [user-id]]
  {:user-id (or user-id
                (str (java.util.UUID/randomUUID)))})

(make-user)
(make-user "bobby")

(defn make-user
  [username & {:keys [email join-date]
               :or {join-date (java.util.Date.)}}]
  {:user-name username
   :join-date join-date
   :email email
   ;; 2.592e9 -> one month in ms
   :exp-date (java.util.Date. (long (+ 2.592e9 (.getTime join-date))))})

(make-user "Bobby")
