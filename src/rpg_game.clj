(ns rpg-game)
(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

(defn character 
  [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500}
              opts)))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(defn loot 
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
       (alter to update-in [:items] conj item)
       (alter from update-in [:items] disj item))))

(wait-futures 1
              (while (loot smaug bilbo))
              (while (loot smaug gandalf)))

(deref smaug)
(deref bilbo)
(deref gandalf)


(map (comp count :items deref) [bilbo gandalf])
(filter (:items (deref bilbo)) (:items (deref gandalf)))

(def x (ref 0))

(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (alter x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (alter x - (apply + (range 1000)))))))


(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (commute x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (commute x - (apply + (range 1000)))))))
(defn flawed-loot 
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
       (commute to update-in [:items] conj item)
       (commute from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (flawed-loot smaug bilbo))
              (while (flawed-loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
(filter (:items (deref bilbo)) (:items (deref gandalf)))

(defn fixed-loot 
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
       (commute to update-in [:items] conj item)
       (alter from update-in [:items] disj item))))

(wait-futures 1
              (while (fixed-loot smaug bilbo))
              (while (fixed-loot smaug gandalf)))

(defn attack
  [aggressor target]
  (dosync 
    (let [damage (* (rand 0.1) (:strength @aggressor))]
      (commute target update-in [:health] #(max 0 (- % damage))))))
(defn heal
  [healer target]
  (dosync 
    (let [aid (* (rand 0.1) (:mana @healer))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (commute target update-in [:health] + aid)))))

(def alive? (comp pos? :health))
(defn play 
  [character action other]
  (while (and (alive? @character)
              (alive? @other)
              (action character other))
    (Thread/sleep (rand-int 50))))

(wait-futures 1 
              (play bilbo attack smaug)
              (play smaug attack bilbo))

(map (comp :health deref) [smaug bilbo])

(dosync
  (alter smaug assoc :health 500)
  (alter bilbo assoc :health 100))

(wait-futures 1 
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))
(map (comp #(select-keys % [:name :health :mana]) deref) [smaug bilbo gandalf])

(dosync (ref-set bilbo {:name "Bilbo"}))
(dosync (alter bilbo (constantly {:name "Bilbo"})))

(defn- enforce-max-health
  [name max-health]
  (fn [character-data]
    (or (<= (:health character-data) max-health)
        (throw (IllegalStateException. (str (format  "%s is already at max health %d"name max-health)))))))

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 500}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* ( enforce-max-health name (:health cdata))
                          (:validator cdata))]
    (ref (dissoc cdata :validator)
         :validator #(every? (fn [v] (v %)) validators))))

(def bilbo (character "Bilbo" :health 100 :strength 100))

(heal gandalf bilbo)

(dosync (alter bilbo assoc-in [:health] 95))
(heal gandalf bilbo)

(defn heal
  [healer target]
  (dosync
    (let [aid (min (* (rand 0.1) (:mana @healer))
                   (- (:max-health @target) (:health @target)))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (alter target update-in [:health] + aid)))))

(dosync (alter bilbo assoc-in [:health] 95))
(heal gandalf bilbo)

(def daylight (ref 1))

(defn attack
  [aggressor target]
  (dosync 
    (let [damage (* (rand 0.1) (:strength @aggressor) @daylight)]
      (commute target update-in [:health] #(max 0 (- % damage))))))



(require '[clojure.java.io :as io])
(def console (agent *out*))
(def character-log (agent (io/writer "character-states.log" :append true)))
(defn write
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w
    (.write "\n")
    .flush))
(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write new)))))

(def smaug (character "Smaug" :health 500 :strength 400))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 1000))

(log-reference bilbo console character-log)
(log-reference smaug console character-log)

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))
;=============================================================================
(defn attack
  [aggressor target]
  (dosync 
    (let [damage (* (rand 0.1) (:strength @aggressor) (ensure daylight))]
      (send-off console write
                (:name @aggressor) "hits" (:name target) "for" damage)
      (commute target update-in [:health] #(max 0 (- % damage))))))

(defn heal
  [healer target]
  (dosync
    (let [aid (min (* (rand 0.1) (:mana @healer))
                   (- (:max-health @target) (:health @target)))]
      (when (pos? aid)
       (send-off console write
                (:name @healer) "heals" (:name target) "for" aid) 
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (alter target update-in [:health] + aid)))))
(dosync 
  (alter smaug assoc :health 500)
  (alter bilbo assoc :health 100)
  )
(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))
