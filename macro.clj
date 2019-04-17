;----------------------------------------------------------
; Examples with Clojure Macros - Metaprogramming
; Author: Carlos Rivero
;----------------------------------------------------------

(defmacro my-or
  "Evaluates its expressions one at a time, from left to right.
  If a form returns a logical true value, it returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expression. (or) returns nil."
  ([] true)
  ([a] a)
  ([a & b]
   `(let [temp# ~a]
      (if (not temp#)
        (my-or ~@b)
        temp#))))

(defmacro do-loop
  "Implements a post-test loop control statement. It combines
  the functionality of C's do-while statement and Pascal's repeat-until
  statement.\n\nThis construct consists of a body (one or more expressions,
  presumably with side effects) and a final conditional form prefaced with
  a :while or :until keyword. First, the expressions in the body are evaluated
  sequentially, and then the condition is evaluated. If the final form uses a
  :while keyword, the body of the loop is repeated while the condition holds true.
  On the other hand, if the final form uses an :until keyword, the body of the
  loop is repeated while the condition holds false (or in other words, the loop
  terminates when the condition is true). Returns nil."
  ([] nil)
  ([a & b]
   `(while
      (if (= ~(first (last b)) :while)
             ~(last (last b))
             (not ~(last (last b))))
      (do ~a ~@(butlast b)))))

(defmacro def-pred
  "Takes a name, an arg vector, and a body of one or more expressions.
  The macro should define two predicate functions: a regular one and
  its negated version. The name of the negated predicate should be the
  same as name but with a \"not-\" prefix, and its result should be negated
  using the not function "
  [name arg & body]
  (let [notname (str "not-" name)
        notsym (symbol notname)]
    `(do (defn ~name ~arg ~@body)
         (defn ~notsym ~arg (not (do ~@body))))))


(defmacro defn-curry
  "Performs a currying transformation to a function definition.
  It takes as parameters a name, an args vector, and a body of
  one or more expressions"
  [name args & exprs]
  (if (zero? (count args))
    `(defn ~name [] (do ~@exprs))
    `(defn ~name [~(first args)]
       ~((fn pb [params exprs]
           (if (empty? params)
             `(do ~@exprs)
             `(fn [~(first params)]
                ~(pb (rest params) exprs)))) (rest args) exprs))))


(defmacro IF
  "Write an IF macro that  provides a conditional
  statement that is syntactically a bit more similar to
  those found in languages like Pascal or Fortran."
  ([a] nil)
  ([a & b]
   (let [else-forms (take-while #(not= % :THEN) (rest (drop-while #(not= % :ELSE) b)))
         then-forms (take-while #(not= % :ELSE) (rest (drop-while #(not= % :THEN) b)))]
     `(if ~a
        (do ~@then-forms)
        (do ~@else-forms)))))

(defmacro nth-expr
  "Evaluates only the nth provided expression, the
  rest are ignored."
  [nth & exprs]
  `(case ~nth
     ~@(interleave (range (count exprs)) exprs)
     (throw (java.lang.RuntimeException. "Bad nth value!"))))


(deftest test-nth-expr
  (is (= '(clojure.core/case (- 5 4)
            0 (* 2 3)
            1 (- 5 2)
            2 (+ 7 2)
            3 (/ 20 2)
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- 5 4)
                                   (* 2 3)
                                   (- 5 2)
                                   (+ 7 2)
                                   (/ 20 2)))))
  (is (= '(clojure.core/case (- (* 2 2) (+ 2 2))
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- (* 2 2) (+ 2 2))))))
  (is (= '(clojure.core/case (- (* 2 3) (+ 2 3))
            0 (println "zero")
            1 (println "one")
            2 (println "two")
            3 (println "three")
            4 (println "four")
            5 (println "five")
            (throw (java.lang.RuntimeException. "Bad nth value!")))
         (macroexpand-1 '(nth-expr (- (* 2 3) (+ 2 3))
                                   (println "zero")
                                   (println "one")
                                   (println "two")
                                   (println "three")
                                   (println "four")
                                   (println "five")))))
  (is (= 3 (nth-expr (- 5 4)
                     (* 2 3)
                     (- 5 2)
                     (+ 7 2)
                     (/ 20 2))))
  (is (thrown?
        RuntimeException
        (nth-expr (- (* 2 2) (+ 2 2)))))
  (is (thrown?
        RuntimeException
        (nth-expr :wat 0 1 2 3 4 5)))
  (is (= "one\n"
         (with-out-str (nth-expr (- (* 2 3) (+ 2 3))
                                 (println "zero")
                                 (println "one")
                                 (println "two")
                                 (println "three")
                                 (println "four")
                                 (println "five")))))
  (is (thrown?
        RuntimeException
        (nth-expr (* (* 2 3) (+ 2 3))
                  (println "zero")
                  (println "one")
                  (println "two")
                  (println "three")
                  (println "four")
                  (println "five")))))

;==========================================================
(run-tests)

