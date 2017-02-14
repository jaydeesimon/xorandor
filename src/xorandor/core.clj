(ns xorandor.core
  (:require [xorandor.util :as util]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(defn- parse-dimensions [s]
  (mapv read-string (take 2 (re-seq #"\S+" s))))

(defn- widen [width s]
  (let [fmt (str "%1$-" width "s")]
    (format fmt s)))

(defn- parse-into-grid [s]
  (let [[_ width] (parse-dimensions s)]
    (->> (str/split-lines s)
         (map (partial widen width))
         (rest)
         (mapv vec))))

(defn initialize-components [grid]
  (let [s (str/join (map (partial apply str) grid))
        re #"\[.*?\]|[01]"
        matcher (re-matcher re s)]
    (loop [matches []
           found (.find matcher)]
      (if (not found)
        matches
        (recur (conj matches {:start (.start matcher) :text (.group matcher)})
               (.find matcher))))))

(defn assoc-order [components]
  (map (fn [component id]
         (assoc component :order id))
       components
       (rest (range))))

(defn assoc-coords [components width]
  (let [idx->coord (fn [idx]
                     (let [col (mod idx width)
                           row (/ (- idx col) width)]
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

(defn assoc-toggle? [components]
  (map (fn [{type :type :as component}]
         (assoc component :toggle? (some? (#{"<" ">" "1" "0"} type))))
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

(defn assoc-pins [components direction prop grid]
  (map (fn [{coords :coords :as component}]
         (let [pin-coords (->> (map #(mapv + % direction) coords)
                               (filter #(= (get-in grid %) \|)))]
           (if (seq pin-coords)
             (assoc component prop pin-coords)
             component)))
       components))

(defn- output-coord-belongs-to [components coord]
  (some (fn [component]
          (when ((set (:output-coords component)) coord)
            (let [output-pos (->> (map vector (:output-coords component) (range))
                                  (filter (fn [[output-coord _]]
                                            (= output-coord coord)))
                                  (first)
                                  (second))]
              [(:name component) output-pos]))
          )
        components))

(defn- wire-coords [components grid]
  (let [coords      (for [row (range (count grid))
                          col (range (count (first grid)))]
                      [row col])
        gate-coords (mapcat :coords components)]
    (->> (remove (set gate-coords) coords)
         (filter (fn [coord]
                   (#{\| \- \+} (get-in grid coord)))))))

(defn- find-input-dep [input-coord output? wire?]
  (let [neighbors (fn [coord]
                    (->> (map #(mapv + coord %) [[1 0] [0 -1] [0 1]])
                         (filter wire?)))]
    (loop [frontier (list input-coord)
           visited  #{input-coord}]
      (let [[coord & frontier] frontier]
        (cond (nil? coord) nil
              (output? coord) coord
              :else (recur (into frontier (->> (neighbors coord)
                                               (remove visited)))
                           (into visited [coord])))))))

(defn assoc-component-dependencies [components grid]
  (let [output? (set (mapcat :output-coords components))
        wire?   (set (wire-coords components grid))]
    (map (fn [{input-coords :input-coords :as component}]
           (let [dependencies (->> input-coords
                                   (map #(find-input-dep % output? wire?))
                                   (map (partial output-coord-belongs-to components)))]
             (if (seq dependencies)
               (assoc component :dependencies dependencies)
               component)))
         components)))

(defn assoc-toggle-dependencies [components]
  (let [components-as-map (zipmap (map :name components) components)
        branch? (comp seq :dependencies)
        children #(map (comp components-as-map first) (:dependencies %))]
    (map (fn [component]
           (if (:toggle? component)
             component
             (let [toggle-dependencies (->> (tree-seq branch? children component)
                                            (filter :toggle?)
                                            (map :name)
                                            (sort)
                                            (distinct))]
               (assoc component :toggle-dependencies toggle-dependencies))))
         components)))

(defn dissoc-unnecessary-props [components]
  (let [unused [:start :input-coords :output-coords :text :coords :toggle?]]
    (map #(apply dissoc (concat [%] unused)) components)))

(defmulti component-fn :type)

(defmethod component-fn "@" [_]
  (fn [& currents]
    (if (every? true? currents) [true] [false])))

(defmethod component-fn "~" [_]
  (fn [current] [(not current)]))

(defmethod component-fn "&" [_]
  (fn [current1 current2]
    [(and current1 current2)]))

(defmethod component-fn "|" [_]
  (fn [current1 current2]
    [(or current1 current2)]))

(defmethod component-fn "+" [_]
  (fn [current1 current2]
    (if (not= current1 current2) [true] [false])))

(defmethod component-fn "^" [_]
  (fn [current1 current2]
    (if (and current1 current2) [false] [true])))

(defmethod component-fn "-" [_]
  (fn [current1 current2]
    (if (or current1 current2) [false] [true])))

(defmethod component-fn "=" [_]
  (fn [current1 current2]
    (if (= current1 current2) [true] [false])))

(defmethod component-fn "<" [_]
  (fn [toggle]
    (fn [current]
      (if current
        (if toggle
          [false true]
          [true false])
        [false false]))))

(defmethod component-fn ">" [_]
  (fn [toggle]
    (fn [current]
      (if current
        (if toggle
          [true false]
          [false true])
        [false false]))))

(defn- input-fn [default]
  (fn [toggle]
    (if toggle
      [(not default)] [default])))

(defmethod component-fn "0" [_]
  (input-fn false))

(defmethod component-fn "1" [_]
  (input-fn true))

(defn parse-circuit [s]
  (let [grid (parse-into-grid s)]
    (-> (initialize-components grid)
        (assoc-order)
        (assoc-coords (count (first grid)))
        (assoc-types)
        (assoc-toggle?)
        (assoc-names)
        (assoc-pins [1 0] :input-coords grid)
        (assoc-pins [-1 0] :output-coords grid)
        (assoc-component-dependencies grid)
        (assoc-toggle-dependencies)
        (dissoc-unnecessary-props))))

(defn- eval-circuit* [circuit-map [component-name position] toggles]
  (let [dependencies (:dependencies (component-name circuit-map))]
    (if (seq dependencies)
      (cons [component-name position] (map (fn [dependency]
                                             (eval-circuit* circuit-map dependency toggles))
                                           dependencies))
      (list [component-name position]))))

(defn eval-circuit [circuit toggles]
  (let [led (first circuit)
        circuit-map (zipmap (map :name circuit) circuit)]
    (eval-circuit* circuit-map [(:name led) 0] toggles)))