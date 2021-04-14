(ns cpu.core
  (:gen-class))

;
; Helpers
;

(defn inc-pc
  ([pc]
   (mod (inc pc) 16))
  ([pc n]
   (mod (+ n pc) 16)))

(defn cpu-pc
  [vm]
  (:pc (:cpu vm)))

(defn cpu-brk
  [vm]
  (:brk (:cpu vm)))

(defn cpu-ar
  [vm]
  (:ar (:cpu vm)))

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

(defn op-name
  [opcode]
  (cond
    ;; BRK
    (= 0 opcode) :brk
    ;; LDA
    (= 0xa9 opcode) :lda
    ;; ADC
    (= 0x69 opcode) :adc
    ;; STA
    (= 0x8d opcode) :sta
    ;; UNKNOWN
    :else :unknown))

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

(defn set-flag-brk
  [vm]
  (assoc-in vm [:cpu :brk] true))

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

;

(defn step
  [vm]
  (let [opcode (next-opcode vm)]
    (prn (cpu-pc vm) (op-name opcode) vm)
    (cond
      ;; BRK
      (= 0 opcode) (brk vm)
      ;; LDA
      (= 0xa9 opcode) (lda vm)
      ;; ADC
      (= 0x69 opcode) (adc vm)
      ;; STA
      (= 0x8d opcode) (sta vm)
      ;; UNKNOWN
      :else vm)))

(defn run
  [vm]
  (loop [vm (step vm)]
    (if (not (cpu-brk-flag? vm))
      (recur (step vm))
      (prn (cpu-pc vm) :fin vm))))

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

(def mem [0xa9 100
          0x69 7
          0x8d 15
          0 0
          0 0
          0 0
          0 0
          0 0])

(def cpu {:pc  0
          :ar  0
          :brk false})

(def vm {:cpu cpu :mem mem})

(run vm)
