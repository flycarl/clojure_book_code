(ns chapter7)
(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  (fn [node value] (:tag node)))

(defmethod fill :div
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(fill {:tag :div} "hello")
(fill {:tag :input} "hello")
;(fill {:span :input} "hello")

(defmethod fill :default
  [node value]
  (assoc node :content [(str value)]))

(fill {:span :input} "hello")

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  (fn [node value] (:tag node))
  :default nil)

(defmethod fill nil
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill :default
  [node value]
  (assoc node [:attrs :name] (str value)))

(ns-unmap *ns* 'fill)
(defn- fill-dispatch [node value]
  (if (= :input (:tag node))
    [(:tag node) (-> node :attrs :type)]
    (:tag node)))

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  #'fill-dispatch
  :default nil)

(defmethod fill nil
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill [:input nil]
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill [:input "hidden"]
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill [:input "text"]
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill [:input "radio"]
  [node value]
  (if (= value (-> node :attrs :value))
    (assoc-in node [:attrs :checked] "checked")
    (update-in node [:attrs] dissoc :checked)))

(defmethod fill [:input "checkbox"]
  [node value]
  (if (= value (-> node :attrs :value))
    (assoc-in node [:attrs :checked] "checked")
    (update-in node [:attrs] dissoc :checked)))

(defmethod fill :default
  [node value]
  (assoc-in node [:attrs :name] (str value)))

(fill {:tag :input
       :attrs {:value "first choice"
               :type "checkbox"}}
      "first choice")
(fill *1 "off")

(derive ::checkbox ::checkable)
(derive ::radio ::checkable)
(derive ::checkable ::input)
(derive ::text ::input)

(isa? ::radio ::input)
(isa? ::radio ::text)

(isa? java.util.ArrayList Object)
(isa? java.util.ArrayList java.util.List)
(isa? java.util.ArrayList java.util.Map)
(derive java.util.Map ::collection)
(derive java.util.Collection ::collection)
(isa? java.util.Collection ::collection)
(isa? java.util.Collection ::collection)

(def h (make-hierarchy))
(isa? h java.util.ArrayList java.util.Collection)

(ns-unmap *ns* 'fill)

(def fill-hierarchy (-> (make-hierarchy)
                        (derive :input.radio ::checkable)
                        (derive :input.check ::checkable)
                        (derive ::checkable :input)
                        (derive ::input.text :input)
                        (derive ::input.hidden :input)))

(defn- fill-dispatch [node value]
  (if-let [type (and (= :input (:tag node))
                     (-> node :attrs :type))]
    (keyword (str "input." type))
    (:tag node)))

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
   with the provided value."
  #'fill-dispatch
  :default nil
  :hierarchy #'fill-hierarchy)

(defmethod fill nil [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input [node value]
  (assoc-in node [:attrs :value] (str value)))
