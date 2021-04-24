(ns cpu.assemble
  (:require [clojure.string :as str]
            [cpu.parser :refer :all]))

(defn label-value?
  [value]
  (contains? value :label))

(defn literal-value?
  [value]
  (contains? value :literal))

(defn find-label
  [label labels]
  (first (filter #(= label (:label %)) labels)))

(defn assemble-code
  [program]
  (let [labels (:labels program)]
    (for [instruction (:instructions program)]
      (let [{:keys [opcode value mode]} instruction]
        (cond
          (label-value? value) [opcode
                                (let [instruction-address (:address instruction)
                                      label-value         (:label value)
                                      {:keys [address]} (find-label label-value labels)]
                                  (if (= :absolute mode)
                                    address
                                    (- address instruction-address)))]
          (literal-value? value) [opcode (:literal value)]
          (not (nil? value)) [opcode value]
          :else [opcode])))))

(defn assemble
  [program]
  (-> program
      (assemble-code)
      (flatten)
      (vec)))