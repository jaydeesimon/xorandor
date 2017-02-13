(ns xorandor.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)

(defn case-input [n]
  (-> (io/resource (str "case0" n ".txt"))
      (slurp)))

(defn parse-components [diagram-grid]
  (let [s       (str/join (map (partial apply str) diagram-grid))
        re      #"\[.*?\]|[01]"
        matcher (re-matcher re s)]
    (loop [matches []
           found   (.find matcher)]
      (if (not found)
        matches
        (recur (conj matches {:start (.start matcher) :text (.group matcher)})
               (.find matcher))))))

(defn widen [width s]
  (let [fmt (str "%1$-" width "s")]
    (format fmt s)))

(defn assoc-ids [components]
  (map (fn [component id]
         (assoc component :id id))
       components
       (rest (range))))

(defn assoc-coords [components cols]
  (let [idx->coord (fn [idx]
                     (let [col (mod idx cols)
                           row (/ (- idx col) cols)]
                       [row col]))]
    (map (fn [{start :start text :text :as component}]
           (let [indices (range start (+ start (count text)))]
             (assoc component :coords (map idx->coord indices))))
         components)))

(defn assoc-types [components]
  (map (fn [{text :text :as component}]
         (let [type (str (first (remove #{\space \[ \]} text)))]
           (assoc component :type type)))
       components))

(defn assoc-names [components]
  (let [type (fn [{type :type}]
               (cond (#{"0" "1"} type) "I"
                     (#{"<" ">"} type) "K"
                     :else "G"))]
    (->> (group-by type components)
         (mapcat (fn [[type components]]
                   (map (fn [component n]
                          (assoc component :name (keyword (str type n))))
                        components
                        (rest (range))))))))

(defn assoc-pins [components direction prop diagram-grid]
  (map (fn [{coords :coords :as component}]
         (let [pin-coords (->> (map #(mapv + % direction) coords)
                               (filter #(= (get-in diagram-grid %) \|)))]
           (if (seq pin-coords)
             (assoc component prop pin-coords)
             component)))
       components))

(defn assoc-toggle? [components]
  (map (fn [{type :type :as component}]
         (assoc component :toggle? (some? (#{"<" ">" "1" "0"} type))))
       components))

(defn find-input-dep [input outputs wires]
  (let [outputs   (set outputs)
        neighbors (fn [coord]
                    (->> (map #(mapv + coord %) [[1 0] [0 -1] [0 1]])
                         (filter (set wires))))]
    (loop [frontier (list input)
           visited  #{input}]
      (let [[coord & frontier] frontier]
        (cond (nil? coord) nil
              (outputs coord) coord
              :else (recur (into frontier (->> (neighbors coord)
                                               (remove visited)))
                           (into visited [coord])))))))

(defn input-output-deps [inputs outputs wires]
  (into {} (map #(vector % (find-input-dep % outputs wires)) inputs)))

(defn wires [components diagram-grid]
  (let [coords      (for [row (range (count diagram-grid))
                          col (range (count (first diagram-grid)))]
                      [row col])
        gate-coords (mapcat :coords components)]
    (->> (remove (set gate-coords) coords)
         (filter (fn [coord]
                   (#{\| \- \+} (get-in diagram-grid coord)))))))

(defn find-toggle-deps [components deps component]
  (let [inputs   (set (keys deps))
        branch?  (fn [component]
                   (seq (intersection inputs (set (:inputs component)))))
        children (fn [component]
                   (let [output-deps (set (map deps (:inputs component)))]
                     (filter (fn [component]
                               (seq (intersection output-deps (set (:outputs component)))))
                             components)))]
    (->> (filter :toggle? (tree-seq branch? children component))
         (map :name))))

(defn component-children [components deps component component-output-map]
  (let [output-deps (map deps (:inputs component))]
    (map (fn [output]
           (let [component (get component-output-map output)]
             (let [output-position (->> (map vector [first second] (:outputs component))
                                        (filter (fn [[_ coord]]
                                                  (= output coord)))
                                        (ffirst))]
               [output-position component]))
           #_(some (fn [component]
                   (when ((set (:outputs component)) output)
                     (let [output-position (->> (map vector [first second] (:outputs component))
                                                (filter (fn [[_ coord]]
                                                          (= output coord)))
                                                (ffirst))]
                       [output-position component])))
                 components))
         output-deps)))

(def component-children-memo (memoize component-children))

(defn led [& currents]
  (if (every? true? currents) [true] [false]))

(defn and' [current1 current2]
  [(and current1 current2)])

(defn or' [current1 current2]
  [(or current1 current2)])

(defn input [default toggle]
  (let [current (if toggle (not default) default)]
    (constantly [current])))

(defn left-switch [toggle]
  (fn [current]
    (if current
      (if toggle
        [false true]
        [true false])
      [false false])))

(defn right-switch [toggle]
  (fn [current]
    (if current
      (if toggle
        [true false]
        [false true])
      [false false])))

(defn xor [current1 current2]
  (if (not= current1 current2) [true] [false]))

(defn nand [current1 current2]
  (if (and current1 current2) [false] [true]))

(defn nor [current1 current2]
  (if (or current1 current2) [false] [true]))

(defn xnor [current1 current2]
  (if (= current1 current2) [true] [false]))

(defn toggle-permutations [n]
  (let [symbols (repeatedly n gensym)
        args (vec (interleave symbols (cycle [[true false]])))
        body (vec symbols)]
    (eval (list 'for args body))))

(defn component-as-fn [component toggle]
  (case (:type component)
    "@" led
    "&" and'
    "|" or'
    "0" (input false toggle)
    "1" (input true toggle)
    "<" (left-switch toggle)
    ">" (right-switch toggle)
    "+" xor
    "^" nand
    "-" nor
    "=" xnor
    "~" (fn [x] [(not x)])
    (throw (ex-info (format "unknown gate: %s" (:type component)) {}))))

#_{:g3 [{:i1 true :i2 false} {:i3 false :i1 true}]}

#_(defn add-short-circuit-val! [short-circuits toggles component]
  (swap! short-circuits (fn [short-circuits]
                          (merge-with (fn [k1 k2]
                                        (if (coll? k1)
                                          (conj k1 k2)
                                          [k1 k2]))
                                      short-circuits
                                      (into {} (filter (fn [[k v]]
                                                         ((set (:input-deps component)) k))
                                                       toggles))))))

(defn eval-circuit [components deps position-fn component toggles cache component-output-map]
  (let [toggle (-> component :name toggles)
        f      (component-as-fn component toggle)]
    (if-let [child-components (seq (component-children-memo components deps component component-output-map))]
      (if-let [cached-val (get @cache (:name component))]
        (position-fn cached-val)
        (let [f-result (apply f (map (fn [[position-fn component']]
                                       (eval-circuit components
                                                     deps
                                                     position-fn
                                                     component'
                                                     toggles
                                                     cache
                                                     component-output-map))
                                     child-components))
              _        (swap! cache assoc (:name component) f-result)
              #__        #_(when (:short-circuitable? component)
                         (add-short-circuit-val! short-circuits toggles component))]
          (position-fn f-result)))
      (position-fn (f)))))

(defn minimum-toggles* [components deps]
  (let [toggle-names (map :name (filter :toggle? components))
        component-output-map (reduce (fn [m component]
                                       (into m (map (fn [output]
                                                      (vector output component))
                                                    (:outputs component))))
                                     {}
                                     components)]
    (->> (toggle-permutations (count toggle-names))
         (map (partial zipmap toggle-names))
         (filter (fn [toggles]
                (let [evaled (eval-circuit components deps first (first components) toggles (atom {}) component-output-map)]
                  (when evaled
                    toggles))))
         (sort-by (fn [toggles]
                    (count (filter false? (vals toggles)))) >)
         (first)
         (filter (fn [[_ v]]
                   (true? v)))
         (map (fn [[k _]]
                (name k)))
         (sort)
         (partition-by #(first %))
         (map #(vector %1 %2) [:inputs :switches])
         (into {}))))

(defn minimum-toggles [cols raw-diagram]
  (let [diagram-grid      (mapv #(vec (widen cols %)) (str/split-lines raw-diagram))
        components        (-> (parse-components diagram-grid)
                              (assoc-ids)
                              (assoc-coords cols)
                              (assoc-types)
                              (assoc-toggle?)
                              (assoc-names)
                              (assoc-pins [1 0] :inputs diagram-grid)
                              (assoc-pins [-1 0] :outputs diagram-grid))
        deps (input-output-deps (mapcat :inputs components)
                                             (mapcat :outputs components)
                                             (wires components diagram-grid))]
    {:components components :deps deps}
    #_(minimum-toggles* components deps)))

(defn my-main [n]
  (let [input       (case-input n)
        cols        (read-string (second (str/split input #" ")))
        raw-diagram (str/join "\n" (rest (str/split-lines input)))]
    (minimum-toggles cols raw-diagram)))

(defn -main [& _]
  (let [_    (read)
        cols (read)
        _    (read-line)
        min-toggles (minimum-toggles cols (slurp *in*))]
    (doseq [toggle (concat (:switches min-toggles) (:inputs min-toggles))]
      (println toggle))))
