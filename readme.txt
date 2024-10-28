// tokens -> SIR -> MIR -> tokens

// todo:

- method calls
- tail call optimization
- macros
- threading macro
- probably start with a way to manipulate the syntax, only do runtime if absolutely necc


okay so how do we do this, we want to be able to

// - quote/reflection/etc
// - go down list of clojure features
// - use blocks

so how do we represent rn?
SIRNodes

okay so we need some set of operations that compile comfortably to rust
that we can use to build macros, and then we need to use it to plugably manipulate
SIRNodes


so we need if, let, loop/recur, and quasi-quoting? or alternatively just a list

- if
- loop



- slash

- dot, dot dot
- threading



(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"}

  [x & forms]
  // start defining x to x and forms to forms
  (loop [x x, forms forms]
    (if forms
    // if any left do this otherwise return

      (let [form (first forms)
            // if multiple, we have to
            threaded (if (seq? form)
            // quasi-quotes, repeats the ~@, tagged with the meta
            // implicitly it creates a list
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
            // just do a function
              (list form x))]
        (recur threaded (next forms)))

      x)))
