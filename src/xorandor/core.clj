(ns xorandor.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

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
               (cond (#{"0" "1"} type) "i"
                     (#{"<" ">"} type) "s"
                     :else "g"))]
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

(defn go [cols raw-diagram]
  (let [diagram-grid      (mapv #(vec (widen cols %)) (str/split-lines raw-diagram))
        components        (-> (parse-components diagram-grid)
                              (assoc-ids)
                              (assoc-coords cols)
                              (assoc-types)
                              (assoc-names)
                              (assoc-pins [1 0] :inputs diagram-grid)
                              (assoc-pins [-1 0] :outputs diagram-grid))
        input-output-deps (input-output-deps (mapcat :inputs components)
                                             (mapcat :outputs components)
                                             (wires components diagram-grid))]
    input-output-deps))

(defn my-main [n]
  (let [input       (case-input n)
        cols        (read-string (second (str/split input #" ")))
        raw-diagram (str/join "\n" (rest (str/split-lines input)))]
    (go cols raw-diagram)))

(defn -main [& _]
  (let [_    (read)
        cols (read)
        _    (read-line)]
    (go cols (slurp *in*))))



;; how-how-are-you
;[{:id 1 :name :i1 :symbol "&" :positions #{[0 0] [0 1]} :inputs [[0 0] [0 1] :outputs [[1 0]]]}
; {{:id 1 :name :i1 :symbol "&" :positions #{[0 0] [0 1]} :inputs [[0 0] [0 1] :outputs [[1 0]]]}}]