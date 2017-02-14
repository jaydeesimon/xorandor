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
            component))
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
                         (filter (set wire?))))]
    (loop [frontier (list input-coord)
           visited  #{input-coord}]
      (let [[coord & frontier] frontier]
        (cond (nil? coord) nil
              (output? coord) coord
              :else (recur (into frontier (->> (neighbors coord)
                                               (remove visited)))
                           (into visited [coord])))))))

(defn assoc-dependencies [components grid]
  (let [output? (set (mapcat :output-coords components))
        wire? (wire-coords components grid)]
    (map (fn [{input-coords :input-coords :as component}]
           (let [dependencies (->> input-coords
                                   (map #(find-input-dep % output? wire?))
                                   (map (partial output-coord-belongs-to components))
                                   (map :name))]
             (if (seq dependencies)
               (assoc component :dependencies dependencies)
               component)))
         components)))

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
        (assoc-dependencies grid))))