(ns slice.core
  (:use [clojure.contrib.ns-utils :only (immigrate)]
        [clojure.contrib.def :only (defn-memo)])
  (:use [clj-blocks.utils :only (defn-map* decompose-defn-args*)])
  (:require [clojure.walk])
  (:require [hiccup.core :as hiccup]
            [hiccup.page-helpers :as page-helpers]
            [com.reasonr.scriptjure :as scriptjure]
            [gaka.core :as gaka]))

(def *slice-memoize* false)

(defn slice-memoize! [b]
  (alter-var-root #'*slice-memoize* (constantly b)))

(defrecord Slice [])

(defn slice? [x]
  (instance? slice.core.Slice x))

(defn to-slice
  "If given a function, call it. Otherwise return what given. For using slices
  without enclosing ()"
  [sl]
  (if (fn? sl) (sl) sl))

(defn concat-or [a b]
  (if (or (coll? a) (coll? b))
    (concat a b)
    (or a b)))

(defn slices
  "Combine slice parts with concat keeping them as vectors"
  [& maps]
  (reduce #(update-in %1 %2 distinct)
          (apply merge-with concat-or (map to-slice maps))
          (map vector [:html :head :title :js :css :dom])))

(defn- javascript-tag [s]
  [:script {:type "text/javascript"} (str "//<![CDATA[\n" s "\n//]]>")])

(defn- css-tag [s]
  [:style {:type "text/css"}
   (str "/*<![CDATA[*/\n" s "\n/*]]>*/")])

(defmacro just-html [& body] `(hiccup/html ~@body))

(defmacro just-js [& body] `(scriptjure/js ~@body))

(defmacro just-css [& body] `(gaka/css ~@body))

(defmacro js* [& body] `(scriptjure/js* ~@body))

(defmacro js [& body] `(assoc (Slice.) :js (seq [(scriptjure/js ~@body)])))

(defmacro dom [& body] `(assoc (Slice.) :dom (seq [(scriptjure/js ~@body)])))

(defmacro css [& body] `(assoc (Slice.) :css (seq [(gaka/css ~@body)])))

(defmacro head [& body] `(assoc (Slice.) :head (seq [(hiccup/html ~@body)])))

(defn title [s] (assoc (Slice.) :title [s]))

(defmacro doc-type [type] `(assoc (Slice.) :doctype (page-helpers/doctype ~type)))

(defn simple-html [body]
  (assoc (Slice.) :html (seq [(hiccup/html body)])))

(defn walk-html
  "walk the hiccup datastructure, pulling out slices"
  [& body]
  (let [slice-atom (atom [])
        walk-fn (fn [form]
                  (if (instance? Slice form)
                    (do
                      (swap! slice-atom conj (dissoc form :html))
                      (:html form))
                    form))
        post-body (clojure.walk/prewalk walk-fn body)]
    (apply slices (simple-html post-body) @slice-atom)))

(defmacro html [& body] `(walk-html ~@body))

(defmacro slice
  "Defines a slice. Slices are functions. If their arglist is empty it can be
  ommited. Their body is merged into a map, so top-level forms in a slice
  should return a map"
  [& slice-args]
  (let [arg-map (apply decompose-defn-args* slice-args)
        body (:body arg-map)
        arg-map (update-in arg-map [:params] #(if (vector? %) % [])) 
        impure? (or (some :impure (map #(meta (if (slice? %)
                                                %
                                                (if (list? %)
                                                  (resolve (symbol (first %)))
                                                  (resolve (symbol %)))))
                                       body))
                    (:impure (meta name)))
        arg-map (update-in arg-map [:attr-map] assoc :impure impure?)
        pure? (not impure?)
        can-memoize? (and pure? (= 0 (count (:params arg-map))))
        body-slices `(slices ~@(:body arg-map))
        arg-map (assoc arg-map :body body-slices)]
    (if (and *slice-memoize* can-memoize?)
      `(defn-memo ~@(defn-map* arg-map))
      `(defn ~@(defn-map* arg-map)))))

(defn-memo render-int
  ([sl]
     ;; TODO potential for optimizing by prerendering pure slices. either render could return a function 
     (let [{:keys [title html css js dom head doctype]} sl]
       (hiccup/html
        (when doctype
          doctype)
        [:html
         (when (or title head)
           [:head (when title [:title (apply str (interpose " - " title))])
            (when head (apply #(hiccup/html %&) head))])
         [:body
          (when html (apply #(hiccup/html %&) html))
          (when css (css-tag (apply str css)))
          ;; TODO fix ugly interposing ;
          (when js (javascript-tag (apply str (interpose ";" js))))
          (when dom (javascript-tag (scriptjure/js ($ (fn [] (quote (clj (apply str (interpose ";" dom)))))))))]]))))

(defn render [sl & sls]
  ;; separate from render-int so slices passed as functions always get invoked
  ;; looked up in memoized render
  (render-int (apply slices sl sls)))

(slice jquery [& [version]]
  (head (page-helpers/include-js
         (str "http://ajax.googleapis.com/ajax/libs/jquery/" (or version "1.4.2") "/jquery.min.js"))))

(defn dot [s]
  (str "." s))

(defn wo# [s]
  (and s (.replace ^String s "#" "")))

(defn w# [s]
  (str "#" (wo# s)))

(defmacro dice
  "for advanced merging of slices"
  [[name sl key & more] & body]
  `(let [sl# (to-slice ~sl)
         ~name (~key sl#)]
     (slices (dissoc sl# ~key)
             ~(if more
                `(dice ~more ~@body)
                `(slices ~@body)))))

(defmacro let-html [[& bindings] & body]
  `(dice [~@(mapcat #(concat % [:html]) (partition 2 bindings))]
         ~@body))

(slice div [id-or-map sl]
  (dice [h sl :html] (html [:div (if (map? id-or-map)
                                   id-or-map
                                   {:id (wo# id)}) h])))

(defn slice-or-html [x]
  (if (or (instance? Slice x) (fn? x)) ;; we're going to assume functions will return slices
    x
    (html x)))
