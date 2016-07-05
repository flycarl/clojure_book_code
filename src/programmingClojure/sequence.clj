(ns programmingClojure.sequence)

(first '(1 2 3))
(rest '(1 2 3))
(cons 0 '(1 2 3))

(first [1 2 3])
(rest [1 2 3])
(cons 0 [1 2 3])
(class (rest [1 2 3]))

(first {:fname "Aaron" :lnam "Bedra"})
(rest {:fname "Aaron" :laname "Bedra"})
(cons [:mname "james"] {:fname "Aaron" :lname "Bedra"})

(first #{:the :quick :brown :fox})
(rest #{:the :quick :brown :fox})
(cons :jumped #{:the :quick :brown :fox})
#{:the :quick :brown :fox}
(sorted-set :the :quick :brown :fox)

{:a 1 :b 2 :c 3}

(sorted-map :c 3 :b 2 :a 1)

(conj '(1 2 3) :a)
(into '(1 2 3) '(a b c))

(conj [1 2 3] :a)
(into [1 2 3] [ :a :b :c])

(range 10)

(range 10 20)

(range 1 25 2)

(repeat 5 1 )

(repeat 10 "x")

(take 10 (iterate inc 1))

(defn whole-numbers [] (iterate inc 1))

(take 20 (repeat 1))

(take 10 (cycle (range 3)))

(interleave (whole-numbers) ["A" "B" "C" "D" "E"])
(interpose "," ["apples" "bananas" "grapes"])

(apply str (interpose "," ["apples" "bananas" "grapes"]))


(use '[clojure.string :only (join)])
(join \, ["apples" "bananas" "grapes"])

(set [1 2 3])

(hash-set 1 2 3)

(vec (range 3))


(take 10 (filter even? (whole-numbers)))

(take 10 (filter odd? (whole-numbers)))

(take-while (complement #{\a\e\i\o\u}) "the-quick-brown-fox")

(drop-while (complement #{\a\e\i\o\u}) "the-quick-brown-fox")

(split-at 5 (range 10))

(split-with #(<= % 10) (range 0 20 2))
(split-with odd? (range 0 20 2))

(every? odd? [1 3 5])

(every? odd? [1 3 5 8])
(some even? [1 2 3])
(some even? [1 3 5])
(some identity [nil false 1 nil 2])

(not-every? even? (whole-numbers))
(not-any? even? (whole-numbers))

(map #(format "<p>%s</p>" %) ["the" "quick" "brown" "fox"])
(map #(format "<%s>%s</%s>" %1 %2 %1) ["h1" "h2" "h3" "h1"] ["the" "quick" "brown" "fox"])

(reduce + (range 1 11))
(reduce * (range 1 11))

(sort [42 1 7 11])
(sort-by #(.toString %) [42 1 7 11])

(sort > [42 1 7 11])

(sort-by :grade > [{:grade 83} {:grade 90} {:grade 77}])

(for [word ["the" "quick" "brown" "fox"]]
  (format "<p>%s</p>" word))

(take 10 (for [n (whole-numbers) :when (even? n)] n))

(for [n (whole-numbers) :while (even? n)] n)

(for [file "ABCDEFGH" rank (range 1 9)] (format "%c%d" file rank))
(for [rank (range 1 9) file "ABCDEFGH"] (format "%c%d" file rank))

(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2 4 6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(def ordinals-and-primes (map vector (iterate inc 1) primes))

(take 5 (drop 1000 ordinals-and-primes))

(def x (for [i (range 1 3)] (do (println i) i)))
(doall x)
(dorun x)

(first (.getBytes "hello"))
(rest (.getBytes "hello"))
(cons (int \h) (.getBytes "ello"))

(first (System/getProperties))
(rest (System/getProperties))

(first "Hello")
(rest "Hello")
(cons \H "ello")

(reverse "hello")

(apply str (reverse "hello"))

(re-seq #"\w+" "the quick brown fox")
(sort (re-seq #"\w+" "the quick brown fox"))
(drop 2 (re-seq #"\w+" "the quick brown fox"))
(map #(.toUpperCase %) (re-seq #"\w+" "the quick brown fox"))

(import '(java.io File))
(.listFiles (File. "."))

(seq (.listFiles (File. ".")))
(map #(.getName %) (seq (.listFiles (File. "."))))
(map #(.getName %) (.listFiles (File. ".")))

(count (file-seq (File. ".")))

(defn minutes-to-millis [mins] (* mins 1000 60))

(defn recently-modified? [file]
  (> (.lastModified file)
     (- (System/currentTimeMillis) (minutes-to-millis 30))))

(filter recently-modified? (file-seq (File. ".")))

(use '[clojure.java.io :only (reader)])

(take 2 (line-seq (reader "src/trySP.clj")))

(with-open [rdr (reader "src/trySP.clj")]
  (count (line-seq rdr)))
(with-open [rdr (reader "src/trySP.clj")]
  (count (filter #(re-find #"\S" %) (line-seq rdr))))


(use '[clojure.java.io :only (reader)])
(defn non-blank? [line] (if (re-find #"\S" line) true false))
(defn non-svn? [file] (not (.contains (.toString file) ".svn")))
(defn clojure-source? [file] (.endsWith (.toString file) ".clj"))
(defn clojure-loc [base-file]
  (reduce
   +
   (for [file (file-seq base-file)
         :when (and (clojure-source? file) (non-svn? file))]
     (with-open [rdr (reader file)]
       (count (filter non-blank? (line-seq rdr)))))))

(clojure-loc (java.io.File. "/users/flycarl/ClojureProjects"))

(use '[clojure.xml :only (parse)])
(parse (java.io.File. "data/sequences/compositions.xml"))

(for [x (xml-seq
         (parse (java.io.File. "data/sequences/compositions.xml")))
      :when (= :composition (:tag x))]
  (:composer (:attrs x)))

(peek '(1 2 3))
(pop '(1 2 3))

(rest ())
(pop ())

(peek [1 2 3])
(pop [1 2 3])

(get [:a :b :c] 1)
(get [:a :b :c] 5)


([:a :b :c] 1)
([:a :b :c] 5)

(assoc [0 1 2 3 4] 2 :two)

(subvec [1 2 3 4 5] 3)
(subvec [1 2 3 4 5] 1 3)

(take 2 (drop 1 [1 2 3 4 5]))

(keys {:sundance "spaniel", :darwin "beagle"})

(vals {:sundance "spaniel", :darwin "beagle"})

(get {:sundance "spaniel", :darwin "beagle"} :darwin)
(get {:sundance "spaniel", :darwin "beagle"} :snoopy)

({:sundance "spaniel", :darwin "beagle"} :darwin)
({:sundance "spaniel", :darwin "beagle"} :snoopy)

(:darwin {:sundance "spaniel", :darwin "beagle"})
(:snoopy {:sundance "spaniel", :darwin "beagle"})

(def score {:stu nil :joey 100})
(:stu score)

(contains? score :stu)

(get score :stu :score-not-found)
(get score :aaron :score-not-found)

(def song {:name "Agnus Dei"
           :artist "Krzysztof Penderecki"
           :album "Polish Requiem"
           :genre "Classical"})

(assoc song :kind "MPEG Audio File")

(dissoc song :genre)

(select-keys song [:name :artist])

(merge song {:size 8118166, :time 507245})

(merge-with concat
            {:rubble ["Barney"], :flintstone ["Fred"]}
            {:rubble ["Betty"], :flintstone ["Wilma"]}
            {:rubble ["Bam-Bam"], :flintstone ["Pebbles"]})

(use 'clojure.set)
(def languages #{"java" "c" "d" "clojure"})
(def beverages #{"java" "chai" "pop"})

(union languages beverages)
(difference languages beverages)
(intersection languages beverages)

(select #(= 1 (.length %)) languages)

(def compositions
  #{{:name "The Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering" :composer "J. S. Bach"}
    {:name "Requiem" :composer "Giuseppe Verdi"}
    {:name "Requiem" :composer "W. A. Mozart"}})
(def composers
  #{{:composer "J. S. Bach" :country "Germany"}
    {:composer "W. A. Mozart" :country "Austria"}
    {:composer "Giuseppe Verdi" :country "Italy"}})
(def nations
  #{{:nation "Germany" :language "German"}
    {:nation "Austria" :language "German"}
    {:nation "Italy" :language "Italian"}})

(rename compositions {:name :title})
(select #(= (:name %) "Requiem") compositions)
(project compositions [:name])

(join compositions composers)
(join composers nations {:country :nation})

(project
 (join
  (select #(= (:name %) "Requiem") compositions)
  composers)
 [:country])
