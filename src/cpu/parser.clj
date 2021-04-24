(ns cpu.parser
  (:require
    [cpu.instructions :refer :all]

    [clojure.string :as str]
    [clojure.edn :as edn]))

(defn split-lines [lines]
  (map str/trim (str/split-lines lines)))

(defn parse-value [value]
  (let [value (edn/read-string value)]
    (cond
      (symbol? value) {:label (str value)}
      (number? value) {:literal value}
      :else nil)))

(defn find-instruction
  [opcode instructions]
  (first (filter #(= opcode (:name %)) instructions)))

(defn make-instruction
  [name value]
  (if-let [definition (find-instruction name @instructions)]
    (let [value (parse-value value)
          instruction (select-keys definition [:size :name :opcode :mode])]
      (assoc instruction :value value))))

(defn make-label [label]
  {:label (str/replace (str label) #":" "")})

(defn label? [value]
  (str/ends-with? value ":"))

(defn as->keyword [name]
  (keyword (.toLowerCase (str name))))

(defn parse-line
  [line]
  (let [[name value] (str/split line #" ")]
    (or (and (label? name) (make-label name))
        (make-instruction (as->keyword name) value))))

(defn not-blank?
  [line]
  (if (str/blank? line) false line))

(defn parse
  [program]
  (let [lines (split-lines program)]
    (loop [address      0
           labels       []
           instructions []
           [line & rest] lines]
      (if (nil? line)
        {:labels labels :instructions instructions}
        (if-let [line (not-blank? line)]
          (let [instruction (parse-line line)
                instruction (assoc instruction :address address)]
            (if-let [label (and (contains? instruction :label) instruction)]
              (recur address (conj labels label) instructions rest)
              (recur (+ (:size instruction) address)
                     labels (conj instructions instruction) rest)))
          (recur address labels instructions rest))))))

