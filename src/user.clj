(ns user
  (:require
   [defthing.core :as defthing]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def x-key :clawe/xs)
(defmacro defx [title & args]
  (apply defthing/defthing x-key title args))

(defn list-xs []
  (defthing/list-things x-key))

(defn get-x [x]
  (defthing/get-thing x-key (comp #{(:name x x)} :name)))


;; A consumer
(defx my-x
  {:some/key "some-data"})

;; In this simple case, this is roughly equivalent to:
(def _my-x
  {:name     "my-x"
   :some/key "some-data"})

;; the reality:
{:defthing.core/registry-key :user/my-x
 :name                       "my-x"
 :type                       :clawe/xs
 :ns                         "user"
 :some/key                   "some-data"}

;; Now you can list all your ~xs~, or search for an ~x~ by name.
(get-x "my-x")
(list-xs)

;; ~defthing~ can be handed functions or maps. The maps are merged, and the
;; functions are called with the state of the map - whatever has built up to that
;; point.

(defx my-other-x
  {:number 7}
  ;; functions are handed the map to this point
  (fn [{:keys [name number]}] ;; :name is set by default to the first arg
    {:x/id (str number "-" name)}) ;; `my-other-x` gets {:x/id "7-my-other-x"} merged in

  {:some/key :another/value} ;; merges in
  (fn [{:some/keys [key]}]
    (println key)) ;; prints :another/value

  ;; functions might fit some api
  {:some/important-function (fn [] (println "do something important"))})

my-other-x ;; evals to:
{:defthing.core/registry-key :user/my-other-x

 :name     "my-other-x"
 :type     :clawe/xs
 :ns       "user"
 :number   7
 :x/id     "7-my-other-x"
 :some/key :another/value

 ;; note that functions like these don't always transit very well!
 :some/important-function #function[user/fn--8463]
 }
