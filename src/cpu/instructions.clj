(ns cpu.instructions
  (:require [cpu.helpers :refer :all]))

(def instructions (atom []))

(defn lookup-instruction
  [opcode instructions]
  (first (filter #(= opcode (:opcode %)) instructions)))

(defn- make-instruction
  "Create a new instruction"
  [name doc [opcode size mode] func]
  (let []
    {:name   name
     :doc    doc
     :opcode opcode
     :size   size
     :func   func
     :mode   mode}))

(defn defasm
  "Define a new instruction"
  ([name doc [opcode size mode] func]
   (let [mode (if (nil? mode) :absolute mode)
         inst (make-instruction name doc [opcode size mode] func)]
     (swap! instructions conj inst))))

(defasm :brk
        "Stop the program"
        [0x00 1]
        (fn [vm]
          (-> vm
              (set-flag-brk)
              (incr-pc))))

(defasm :lda
        "Load Accumulator with memory"
        [0xa9 2]
        (fn [vm]
          (let [pc    (cpu-pc vm)
                value (read-mem vm (+ 1 pc))]
            (-> vm
                (set-ar value)
                (incr-pc 2)))))

(defasm :adc
        "Add memory to accumulator with Carry"
        [0x69 2]
        (fn [vm]
          (let [pc    (cpu-pc vm)
                value (read-mem vm (+ 1 pc))]
            (-> vm
                (inc-ar value)
                (incr-pc 2)))))

(defasm :sta
        "Store the accumulator in memory"
        [0x8d 2]
        (fn [vm]
          (let [pc      (cpu-pc vm)
                ar      (cpu-ar vm)
                address (read-mem vm (+ 1 pc))]
            (-> vm
                (write-mem address ar)
                (incr-pc 2)))))

(defasm :ldx
        "Load the X register with memory"
        [0xa2 2]
        (fn [vm]
          (let [pc    (cpu-pc vm)
                value (read-mem vm (+ 1 pc))]
            (-> vm
                (set-xr value)
                (incr-pc 2)))))

(defasm :inx
        "Increment X register by one"
        [0xe8 1]
        (fn [vm]
          (-> vm
              (incr-xr)
              (incr-pc))))

(defasm :iny
        "Increment Y register by one"
        [0xc8 1]
        (fn [vm]
          (-> vm
              (incr-yr)
              (incr-pc))))

(defasm :cpy
        "Compare memory and Y register"
        [0xe0 2]
        (fn [vm]
          (let [pc    (cpu-pc vm)
                yr    (cpu-yr vm)
                value (read-mem vm (+ 1 pc))]
            (-> vm
                (set-flag-eq (= yr value))
                (incr-pc 2)))))

(defasm :bne
        "Branch if not equal Pz=0"
        [0xd0 2 :relative]
        (fn [vm]
          (let [pc     (cpu-pc vm)
                value  (read-mem vm (+ 1 pc))
                pc-inc (if (not (cpu-flag-eq vm))
                         value
                         2)]
            (-> vm
                (incr-pc pc-inc)))))

(defasm :stax
        "Store the value from the A register in the memory location stored in the X register"
        ;; This is a non-standard instruction
        ;; I picked 0x8f because I think it's unused
        [0x8f 1]
        (fn [vm]
          (let [ar (cpu-ar vm)
                xr (cpu-xr vm)]
            (-> vm
                (write-mem xr ar)
                (incr-pc)))))

(defasm :dey
        "Decrement Y register by one"
        [0x88 1]
        (fn [vm]
          (let [yr (cpu-yr vm)]
            (-> vm
                (set-yr (dec yr))
                (incr-pc)))))

(defasm :ldy
        "Load the Y register with memory"
        [0xa0 2]
        (fn [vm]
          (let [pc    (cpu-pc vm)
                value (read-mem vm (+ 1 pc))]
            (-> vm
                (set-yr value)
                (incr-pc 2)))))

(defasm :nop
        "No operation"
        [0xea 1]
        (fn [vm]
          (-> vm
              (incr-pc))))

(defasm :jsr
        "Jump to new location Saving Return"
        [0x20 2]
        (fn [vm]
          (let [address    (cpu-sp vm)
                value      (cpu-pc vm)
                next-value (read-mem vm (inc (cpu-pc vm)))]
            (-> vm
                (write-mem address value)
                (dec-sp)
                (set-pc (dec next-value))))))

(defasm :rts
        "Return from subroutine"
        [0x60 1]
        (fn [vm]
          (let [sp             (cpu-sp vm)
                return-address (read-mem vm (inc sp))]
            (-> vm
                (inc-sp)
                (set-pc (inc return-address))
                (incr-pc)))))