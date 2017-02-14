(ns xorandor.core
  (:require [xorandor.util :as util]
            [clojure.string :as str]))

(defn take-until
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(defn parse-dimensions [s]
  (let [first-line (str/split (first (str/split-lines s)) #" ")]
    [(read-string (first first-line)) (read-string (second first-line))]))

(defn widen [width s]
  (let [fmt (str "%1$-" width "s")]
    (format fmt s)))

(defn parse-into-grid [s]
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
            component))
        components))

(defn- wire-coords [components grid])

(defn assoc-dependencies [components grid]
  (let [output-coord? (set (mapcat :output-coords components))
        wires
        branch? (fn [coord])]
    (map (fn [{input-coords :input-coords :as component}]
           (take-until output-coord? (tree-seq)))
         components)))

(defn parse-components [s]
  (let [grid (parse-into-grid s)]
    (-> (initialize-components grid)
        (assoc-order)
        (assoc-coords (count (first grid)))
        (assoc-types)
        (assoc-toggle?)
        (assoc-names)
        (assoc-pins [1 0] :input-coords grid)
        (assoc-pins [-1 0] :output-coords grid)
        #_(assoc-dependencies grid))))