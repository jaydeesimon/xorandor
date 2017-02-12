(ns xorandor.garbage
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

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

(defn find-input-pins [grid {coords :coords}]
  (let [down [1 0]]
    (->> (map #(mapv + % down) coords)
         (filter #(= (get-in grid %) \|)))))

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

(defn identify-switches [raw-gate-info]
  (identify-gates ["<" ">"] "k" raw-gate-info))

(defn identify-inputs [raw-gate-info]
  (identify-gates ["0" "1"] "i" raw-gate-info))

(defn remove-useless-keys [raw-gate-info]
  (map #(dissoc % :start :gate :row) raw-gate-info))

(defn init-gates [grid]
  (->> grid
       raw-gate-info
       identify-switches
       identify-inputs
       remove-useless-keys))

(defn init-grid-first-pass [n]
  (let [grid (input->grid (case-input n))]
    {:grid grid :gates-first-pass (init-gates grid)}))

(defn gate-from-coord [coord gates]
  (let [gates (filter (fn [gate]
                        ((:coords gate) coord))
                      gates)]
    (when (seq gates)
      (:id (first gates)))))

(defn find-neighbors [grid gates coord]
  (let [down-left-right [[1 0] [0 -1] [0 1]]
        neighbor-coords (map #(mapv + coord %) down-left-right)
        gate-coords (reduce clojure.set/union #{} (map :coords gates))]
    (filter (fn [coord]
              (let [c (get-in grid coord)]
                (when (or (and c (not= c \space)) (gate-coords coord))
                  coord)))
            neighbor-coords)))


(defn traverse-until [pred start {grid :grid gates :gates-first-pass}]
  (loop [visited #{}
         frontier (list start)]
    (let [cur-coord (peek frontier)
          pred-result (pred cur-coord grid gates)]
      (cond pred-result pred-result
            (not (seq frontier)) nil
            :else (let [neighbors (find-neighbors grid gates cur-coord)
                        neighbors-not-visited (remove visited neighbors)]
                    (recur (conj visited cur-coord)
                           (apply conj (pop frontier) neighbors-not-visited)))))))

(defn starting-coord [gates]
  (first (:coords (some #(and (= (:gate-symbol %) "@") %) gates))))

#_(defn traverse-circuit [grid]
  (let [gates (init-gates grid)]
    (loop [path []
           gates-visited []
           frontier (list (starting-coord gates))
           parent-gate nil]
      (if (not (seq frontier))
        gates-visited
        (let [cur-coord (peek frontier)
              gate-visit (gate-from-coord cur-coord gates)
              neighbors (find-neighbors grid gates cur-coord)
              neighbors-not-visited (remove (set path) neighbors)]
          (recur (conj path cur-coord)
                 (if (and gate-visit (not ((set gates-visited) gate-visit)))
                   (conj gates-visited [parent-gate gate-visit])
                   gates-visited)
                 (apply conj (pop frontier) neighbors-not-visited)
                 (if (and gate-visit (not ((set gates-visited) gate-visit)))
                   gate-visit
                   parent-gate)))))))

(defn gate-edges [gate-traversal]
  (let [valid-edges (filter (fn [[parent child]]
                              (and (some? parent) (< parent child)))
                            gate-traversal)]
    (reduce (fn [m [parent child]]
              (merge-with (comp vec concat) m {parent [child]}))
            {}
            valid-edges)))

(defn to-tree [adj-map node]
  (if-let [children (get adj-map node)]
    (cons node (lazy-seq (map #(to-tree adj-map %) children)))
    (list node)))

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

(defn led [& currents]
  (if (every? true? currents) [true] [false]))

(defn and' [current1 current2]
  (and current1 current2))

(defn or' [current1 current2]
  (or current1 current2))

(defn xor [current1 current2]
  (if (not= current1 current2) true false))

(defn nand [current1 current2]
  (not (and' current1 current2)))

(defn nor [current1 current2]
  (not (or' current1 current2)))

(defn xnor [current1 current2]
  (not (xor current1 current2)))

(defn input [default toggle]
  (let [current (if toggle (not default) default)]
    (constantly current)))

(defn on-input [toggle]
  ((input true toggle)))

(defn off-input [toggle]
  ((input false toggle)))



