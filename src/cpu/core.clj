(ns cpu.core
  (:gen-class)
  (:require [cpu.debug :refer :all]
            [cpu.helpers :refer :all]
            [cpu.instructions :refer :all]
            [cpu.assemble :refer :all]))

(defn step
  [vm]
  (let [opcode (next-opcode vm)
        instruction (lookup-instruction opcode @instructions)
        func (if (not (nil? instruction))
               (:func instruction)
               (fn [vm] vm))]
    (func vm)))

(defn run
  [vm]
  (loop [vm (step vm)]
    (if (not (cpu-brk-flag? vm))
      (recur (step vm))
      vm)))

;
; Main
;

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;
; Test
;

(def program
  "LDA 100 ; foo
   ADC 7
   STA 15
   LDX 12
   INX
   LDY 24
   BRK"
  )

(def mem (load-program (asm program)))

(def cpu {:pc  0
          :ar  0
          :xr  0
          :yr  0
          :brk false
          :eq  false})

(def vm {:cpu cpu :mem mem})

;(dump!! (run vm))


