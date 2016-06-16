(ns programmingClojure.exploring)

(defn indexed [coll] (map-indexed vector coll))

(indexed "abcde")

(defn index-filter [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

(index-filter #{\a \b} "abcdbbb")

(defn index-of-any [pred coll]
  (first (index-filter pred coll)))

(index-of-any #{\z \a} "zzabyycdxx")
(index-of-any #{\b \y} "zzabyycdxx")

(index-of-any #{\a} "e")

(nth (index-filter #{:h} [:t :t :h :t :h :t :t :t :h :h]) 2)
(index-filter #{:h} [:t :t :h :t :h :t :t :t :h :h])

(meta #'str)

(defn ^{:tag String} shout [^{:tag String} s] (.toUpperCase s))
(meta #'shout)

(defn ^String shout [^String s] (.toUpperCase s))
