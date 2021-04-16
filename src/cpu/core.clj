(ns cpu.core
  (:gen-class)
  (:require [cpu.debug :refer :all]
            [cpu.assemble :refer :all]))

;
; Helpers
;

(defn inc-pc
  ([pc]
   (mod (inc pc) 256))
  ([pc n]
   (mod (+ n pc) 256)))

(defn cpu-pc
  [vm]
  (:pc (:cpu vm)))

(defn cpu-brk
  [vm]
  (:brk (:cpu vm)))

(defn cpu-ar
  [vm]
  (:ar (:cpu vm)))

(defn cpu-xr
  [vm]
  (:xr (:cpu vm)))

(defn cpu-yr
  [vm]
  (:yr (:cpu vm)))

(defn cpu-brk-flag?
  [vm]
  (= true (cpu-brk vm)))

(defn read-mem
  [vm address]
  (nth (:mem vm) address))

(defn write-mem
  [vm address value]
  (assoc-in vm [:mem address] value))

(defn next-opcode
  [vm]
  (read-mem vm (cpu-pc vm)))

(defn incr-pc
  ([vm]
   (let [pc (inc-pc (cpu-pc vm))]
     (assoc-in vm [:cpu :pc] pc)))
  ([vm n]
   (let [pc (inc-pc (cpu-pc vm) n)]
     (assoc-in vm [:cpu :pc] pc))))

(defn set-ar
  [vm n]
  (assoc-in vm [:cpu :ar] n))

(defn inc-ar
  [vm n]
  (let [ar (cpu-ar vm)]
    (set-ar vm (+ ar n))))

(defn set-xr
  [vm n]
  (assoc-in vm [:cpu :xr] n))

(defn incr-xr
  [vm]
  (let [xr (cpu-xr vm)]
    (set-xr vm (inc xr))))

(defn set-yr
  [vm n]
  (assoc-in vm [:cpu :yr] n))

(defn incr-yr
  [vm]
  (let [yr (cpu-yr vm)]
    (set-yr vm (inc yr))))


(defn set-flag-brk
  [vm]
  (assoc-in vm [:cpu :brk] true))

(defn make-mem
  [mem size]
  (let [padding (- size (.length mem))]
    (vec (into mem (repeat padding 0)))))

(defn load-program
  ([program]
   (make-mem program 256))
  ([program mem-size]
   (make-mem program mem-size)))

;
; Operations
;

(defn lda
  [vm]
  (let [pc    (cpu-pc vm)
        value (read-mem vm (+ 1 pc))]
    (-> vm
        (set-ar value)
        (incr-pc 2))))

(defn brk
  [vm]
  (-> vm
      (set-flag-brk)
      (incr-pc)))

(defn adc
  [vm]
  (let [pc    (cpu-pc vm)
        value (read-mem vm (+ 1 pc))]
    (-> vm
        (inc-ar value)
        (incr-pc 2))))

(defn sta
  [vm]
  (let [pc      (cpu-pc vm)
        ar      (cpu-ar vm)
        address (read-mem vm (+ 1 pc))]
    (-> vm
        (write-mem address ar)
        (incr-pc 2))))

(defn ldx
  [vm]
  (let [pc    (cpu-pc vm)
        value (read-mem vm (+ 1 pc))]
    (-> vm
        (set-xr value)
        (incr-pc 2))))

(defn inx
  [vm]
  (-> vm
      (incr-xr)
      (incr-pc)))

(defn ldy
  [vm]
  (let [pc    (cpu-pc vm)
        value (read-mem vm (+ 1 pc))]
    (-> vm
        (set-yr value)
        (incr-pc 2))))

;

(defn step
  [vm]
  (let [opcode (next-opcode vm)]
    (condp = opcode
      0x00 (brk vm)
      0xa9 (lda vm)
      0x69 (adc vm)
      0x8d (sta vm)
      0xa2 (ldx vm)
      0xe8 (inx vm)
      0xa0 (ldy vm)
      ;; UNKNOWN
      :else vm)))

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

(dump!! (run vm))
