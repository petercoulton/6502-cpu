(ns cpu.assemble
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn split-lines
  [^CharSequence lines]
  (map str/triml (str/split-lines lines)))

(defn ^String strip-comments
  [^CharSequence line]
  (str/trimr
    (if-let [idx (str/index-of line ";")]
      (. line (subSequence 0 idx))
      line)))

(defn parse-value
  [value]
  (edn/read-string value))

(defn make-instruction
  [opcode value]
  {:opcode (symbol (.toUpperCase opcode))
   :value (parse-value value)})

(defn tokenize-line
  [line]
  (map str/trim (str/split line #" ")))

(defn parse-line
  [^CharSequence line]
  (let [tokens (tokenize-line line)
        opcode (first tokens)
        operand (second tokens)]
    (make-instruction opcode operand)))

(defn opcode->byte
  [opcode]
  (condp = opcode
    'BRK 0
    'LDA 0xa9
    'ADC 0x69
    'STA 0x8d))

(defn assemble-code
  [code]
  (for [instruction code]
    (let [opcode-byte (opcode->byte (:opcode instruction))
          value (:value instruction)]
      (if (nil? value)
        opcode-byte
        [opcode-byte value]))))

(defn parse-program
  [^CharSequence program]
  (for [line (split-lines program)]
    (-> line
        (strip-comments)
        (parse-line))))

(defn asm
  [^CharSequence program]
  (-> program
      (parse-program)
      (assemble-code)
      (flatten)
      (vec)))

;(asm "LDA 100 ; foo
;      ADC 7
;      STA 15
;      BRK")

