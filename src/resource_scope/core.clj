(ns resource-scope.core)

(def ^:dynamic *scope* nil)

(defn run-scope-actions []
  (let [failed (= (first *scope*) :failed)
        entries (if failed (rest *scope*) *scope*)]
    (doseq [e entries]
        (let [cause (first e)
              action (second e)]
          (when (or (= cause :exits)
                    (and (= cause :fails) failed)
                    (and (= cause :succeeds) (not failed)))
            (action))))))

(defmacro scope
  "Creates a scope for use with when-scope."
  [& body]
  `(binding [*scope* (list)]
     (try
      ~@body
      (catch Throwable t#
        (set! *scope* (conj *scope* :failed))
        (throw t#))
      (finally
       (run-scope-actions)))))

(defmacro when-scope 
  "Causes a body of expressions to be executed at the termination of
  the nearest dynamically enclosing scope (created with scope). If no
  scope is in effect, throws IllegalStateException. Cause must be one of:

  :exits - will run unconditionally on scope exit
  :fails - will run only if scope exits due to an exception
  :succeeds - will run only if scope exits normally"

  [cause & body]
  `(do
     (when-not *scope*
       (throw (IllegalStateException. "No scope in effect")))
     (set! *scope* (conj *scope* [~cause (fn [] ~@body)]))))

