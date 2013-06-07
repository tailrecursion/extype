(ns tailrecursion.extype)

(defmulti extype identity)

(defn find-first [pred? coll] (first (filter pred? coll)))

(defmacro defex [type message m]
  `(defmethod extype ~type [_#] ~{::message message ::data m}))

(defmacro extend-ex
  [type parent & more]
  (let [message (find-first string? more)
        data    (or (find-first map? more) {}) 
        ext     (extype parent)
        message (or message (::message ext))
        data    (merge (::data ext) data)]
    `(do
       (defex ~type ~message ~data)
       (derive ~type ~parent))))

(defn ex
  "Create new exception."
  [type & more]
  (let [message (or (find-first string? more) (::message (extype type))) 
        data    (find-first map? more)
        cause   (find-first (partial instance? Throwable) more)]
    (ex-info message {::type type ::data data} cause)))

(defn ex->clj
  "Get exception properties and data as a clj map."
  [e & [dfl]]
  (if-let [e (cond (::type (ex-data e)) e dfl (ex dfl e))]
    (let [p  (ex-data e)
          t  (::type p)
          a  (ancestors t)
          m  (.getMessage e)
          d  (::data p)
          c  (loop [cx (.getCause e), cc []]
               (if cx (recur (.getCause cx) (conj cc (.getMessage cx))) cc))
          td (::data (extype t))]
      (into {:type t :isa a :message m :data d :cause c} td))))
