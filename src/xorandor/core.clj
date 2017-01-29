(ns xorandor.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn pad-str [width s]
  (let [fmt (str "%1$-" width "s")]
    (format fmt s)))

(defn case-input [n]
  (-> (io/resource (str "case0" n ".txt"))
      (slurp)))

(defn input->grid [input]
  (let [lines (str/split-lines input)
        width (read-string (second (str/split (first lines) #" ")))]
    (->> (map (partial pad-str width) (rest lines))
         (mapv vec))))

(defn input->grid [input]
  (let [lines (str/split-lines input)
        width (read-string (second (str/split (first lines) #" ")))]
    (->> (map (partial pad-str width) (rest lines))
         (apply str))))

(defn parse-gates-and-inputs-from-line [line row]
  (let [re #"\[.*?\]|[01]"
        matcher (re-matcher re line)]
    (loop [matches []
           found (.find matcher)]
      (if (not found)
        matches
        (recur (conj matches {:start (.start matcher) :gate (.group matcher) :row row})
               (.find matcher))))))

(defn parse-gate-symbol [gate]
  (if (str/index-of gate "[")
    (str/trim (last (flatten (re-seq #"\[(.*?)\]" gate))))
    gate))

(defn coord-range [row col len]
  (set (map #(vector row %) (range col (+ col len)))))

(defn raw-gate-info [grid]
  (->> (map #(hash-map :line (apply str %1) :row %2) grid (range))
       (mapcat #(parse-gates-and-inputs-from-line (:line %) (:row %)))
       (map #(assoc % :gate-symbol (parse-gate-symbol (:gate %))))
       (map #(assoc %2 :id %1) (rest (range)))
       (map #(assoc % :coords (coord-range (:row %) (:start %) (count (:gate %)))))))

(defn identify-gates [gate-symbols prefix raw-gate-info]
  (let [pred (fn [{:keys [gate-symbol]}]
               ((set gate-symbols) gate-symbol))
        identified (map #(assoc %1 :name (keyword (str prefix %2)))
                        (filter pred raw-gate-info) (rest (range)))
        others (remove pred raw-gate-info)]
    (sort-by :id (concat identified others))))

(defn identify-inputs [raw-gate-info]
  (identify-gates ["0" "1"] "i" raw-gate-info))

(defn identify-switches [raw-gate-info]
  (identify-gates ["<" ">"] "k" raw-gate-info))

(defn remove-useless-keys [raw-gate-info]
  (map #(dissoc % :start :gate :row) raw-gate-info))

(defn init-gates [grid]
  (->> grid
       raw-gate-info
       identify-switches
       identify-inputs
       remove-useless-keys))

#_(defn grid-and-gates-first-pass [n]
  (let [grid (input->grid (case-input n))
        gates (init-gates grid)
        gate-coords (reduce (fn [m gate]
                              (into m (map #(vector % (:id gate)) (:coords gate))))
                            {}
                            gates)
        path-coords ]
    {:grid grid :gates-first-pass gates :gate-coords gate-coords}))

(defn find-neighbors [grid gates coord]
  (let [down-left-right [[1 0] [0 -1] [0 1]]
        neighbor-coords (map #(mapv + coord %) down-left-right)
        gate-coords (reduce clojure.set/union #{} (map :coords (vals gates)))]
    (filter (fn [coord]
              (let [c (get-in grid coord)]
                (when (or (and c (not= c \space)) (gate-coords coord))
                  coord)))
            neighbor-coords)))

(defn find-input-pins [grid {coords :coords}]
  (let [down [1 0]]
    (->> (map #(mapv + % down) coords)
         (filter #(= (get-in grid %) \|)))))
