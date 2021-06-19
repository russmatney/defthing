(ns defthing.defcom
  (:require
   [defthing.core :as defthing]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exec
  "Executes a passed defcom, passing the command in as the first argument."
  [cmd & args]
  (apply (:defcom/fn cmd) cmd args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defcom-2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defcom
  "Creates a command via defthing-like syntax.

  The last argument to a defcom is the command itself, and can be a named
  function, an anonymous function, or a clojure form that will be treated as the
  function body.

  The first arg passed is the built defcom (itself), and the rest are any
  command line args.

  Commands are registered for a few integrations by default,
  including ralph.rofi support.

  Every command gets a :name and :doc via defthing.
  "
  [command-name & xorfs]
  (let [f (last xorfs)
        command-fn
        ;; ensures the command is a function, and has the right arity
        (cond
          ;; named function, wrap if only [] arity
          (symbol? f)
          `(do
             (let [arglists# (:arglists (meta (var ~f)))]
               (cond
                 ;; named function with only zero arity
                 (and (-> arglists# count #{1}) (-> arglists# first count zero?))
                 ~(list 'fn '[& _rest] (list f))

                 ;; ignore the command, pass the args
                 ;; named function with only one arity
                 (and (-> arglists# count #{1}) (-> arglists# first count #{1}))
                 ~(list 'fn '[cmd & rst] (list f 'rst))

                 ;; named function with only two arity
                 (and (-> arglists# count #{1}) (-> arglists# first count #{2}))
                 ~(list 'fn '[cmd & rst] (list f 'cmd 'rst))

                 :else
                 ~f)))

          ;; anonymous function, use it as-is
          (some-> f first (#{'fn 'fn*})) f

          ;; just a body, wrap as a variadic fn
          :else (list 'fn '[& _rest] f))
        xorfs (->> xorfs reverse (drop 1) reverse)]
    (apply defthing/defthing :defcom/command command-name
           (conj xorfs {:defcom/fn command-fn}))))

