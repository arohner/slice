(ns slice.core
  (:use [clojure.contrib.ns-utils :only (immigrate)]
        [clojure.contrib.def :only (defn-memo)])
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
  {:post (map? %)}
  (if (fn? sl) (sl) sl))

(defn concat-or [a b]
  (if (or (coll? a) (coll? b))
    (concat a b)
    (or a b)))

(defn slices
  "Combine slice parts with concat keeping them as vectors"
  [& maps]
  ;; all to not get empty slots
  (reduce (fn [slice key]
            (if (key slice)
              (update-in slice [key] distinct)
              slice))
          (apply merge-with concat-or (map to-slice maps))
          [:html :top :title :head :css :js :dom]))

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

(defmacro top [& body] `(assoc (Slice.) :top (seq [(hiccup/html ~@body)])))

(defmacro css [& body] `(assoc (Slice.) :css (seq [(gaka/css ~@body)])))

(defmacro head [& body] `(assoc (Slice.) :head (seq [(hiccup/html ~@body)])))

(defn title [s] (assoc (Slice.) :title [s]))

(defmacro doc-type [type] `(assoc (Slice.) :doctype (page-helpers/doctype ~type)))

(defn simple-html [body]
  (assoc (Slice.) :html (seq [(hiccup/html body)])))

(defn walk-html
  "walk the hiccup datastructure, pulling out slices"
  [& body]
  {:post [(slice? %)]}
  (let [slice-atom (atom [])
        walk-fn (fn [form]
                  (if (slice? form)
                    (do
                      (swap! slice-atom conj (dissoc form :html))
                      (:html form))
                    form))
        post-body (clojure.walk/prewalk walk-fn body)]
    (apply slices (simple-html post-body) @slice-atom)))

(defmacro html [& body]
  `(walk-html ~@body))

(defmacro slice
  "Defines a slice. Slices are functions. If their arglist is empty it can be
  ommited. Their body is merged into a map, so top-level forms in a slice
  should return a map"
  [name & body]
  (let [[docstring body] (if (string? (first body))
                           [(first body) (rest body)]
                           ["" body])
        [args body] (if (vector? (first body))
                      [(first body) (rest body)]
                      [[] body])
        impure? (or (some :impure (map #(meta (if (slice? %)
                                                %
                                                (if (list? %)
                                                  (resolve (symbol (first %)))
                                                  (resolve (symbol %)))))
                                       body))
                    (:impure (meta name)))]
    (if *slice-memoize*
      (if impure?
        `(let [var# (defn ~name ~docstring ~args (slices ~@body))]
           (alter-meta! var# assoc :impure true)
           var#)
        (if (= args [])
          `(let [val# (slices ~@body)]
             (defn ~name ~docstring [] val#))
          `(defn-memo ~name ~docstring ~args (slices ~@body))))
      `(defn ~name ~docstring ~args (slices ~@body)))))

(defn-memo render-int
  ([sl]
     ;; TODO potential for optimizing by prerendering pure slices. either render could return a function 
     (let [{:keys [title html top css js dom head doctype]} sl]
       (hiccup/html
        (when doctype
          doctype)
        [:html
         (when (or (seq title) (seq head))
           [:head (when (seq title) [:title (apply str (interpose " - " title))])
            (when (seq head) (apply #(hiccup/html %&) head))])
         [:body
          (when (seq top) (apply #(hiccup/html %&) top))
          (when (seq html) (apply #(hiccup/html %&) html))
          (when (seq css) (css-tag (apply str css)))
          ;; TODO fix ugly interposing ;
          (when (seq js) (javascript-tag (apply str (interpose ";" js))))
          (when (seq dom) (javascript-tag (scriptjure/js ($ (fn [] (quote (clj (apply str (interpose ";" dom)))))))))]]))))

(defn render [sl & sls]
  ;; separate from render-int so slices passed as functions always get invoked
  ;; looked up in memoized render
  (render-int (apply slices sl sls)))

(slice jquery [& [version]]
  (head (page-helpers/include-js
         (str "http://ajax.googleapis.com/ajax/libs/jquery/" (or version "1.4.2") "/jquery.min.js"))))

(defn wodot [s]
  (and s (str (.replace s "." ""))))

(defn wdot [s]
  (str "." (wodot s)))

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
                                   {:id (wo# id-or-map)}) h])))

(defn slice-or-html
  "Utility for combining legacy (hiccup/compojure) code with
  slices. Assumes returned fns are slices, anything else can be
  handled by hiccup."
  [x]
  (if (or (slice? x) (fn? x)) ;; we're going to assume functions will return slices
    x
    (html x)))
