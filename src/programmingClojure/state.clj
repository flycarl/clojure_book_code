(ns programmingClojure.state)

(def current-track (ref "Mars, the Bringer of War"))
(deref current-track)
@current-track

(ref-set current-track "Venus, the Bringer of Peace")
(dosync (ref-set current-track "Venus, the Bringer of Peace"))

@current-track

(def current-composer (ref "Holst"))

(dosync
 (ref-set current-track "Credo")
 (ref-set current-composer "Byrd"))

@current-track
@current-composer

(defrecord Message [sender text])
(Message. "Aaron" "Hello")

(def messages (ref ()))
(defn naive-add-message [msg]
  (dosync (ref-set messages (cons msg @messages))))

(defn add-message [msg]
  (dosync (alter messages conj msg)))

(add-message (Message. "user 1" "hello"))
(add-message (Message. "user 2" "howdy"))

(defn add-message-commute [msg]
  (dosync (commute messages conj msg)))

(def counter (ref 0))
(defn next-counter [] (dosync (alter counter inc)))

(next-counter)

(def validate-message-list
  (partial every? #(and (:sender %) (:text %u))))

(def messages (ref () :validator validate-message-list))

(add-message "not a valid message")
@messages

(add-message (Message. "Aaron" "Real Message"))

(def current-track (atom "Venus, the Bringer of Peace"))

(deref current-track)
@current-track

(reset! current-track "Credo")

(def current-track (atom {:title "Credo" :composer}))

(reset! current-track {:title "Spem in Alium" :composer "Tallis"})
(swap! current-track assoc :title "Sancte Deus")

(def counter (agent 0))

(send counter inc)

@counter

(def counter (agent 0 :validator number?))
(send counter (fn [_] "boo"))
@counter
(agent-errors counter)
(clear-agent-errors counter)
@counter

(def backup-agent (agent "output/messages-backup.clj"))

(defn add-message-with-backup [msg]
  (dosync
   (let [snapshot (commute messages conj msg)]
     (send-off backup-agent (fn [filename]
                              (spit filename snapshot)
                              filename))
     snapshot)))
(add-message-with-backup (Message. "John" "Message One"))
(add-message-with-backup (Message. "Jane" "Message Two"))
