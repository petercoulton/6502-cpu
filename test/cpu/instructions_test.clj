(ns cpu.instructions-test
  (:require [clojure.test :refer :all]
            [cpu.helpers :refer :all]
            [cpu.assemble :refer :all]
            [cpu.debug :refer :all]
            [cpu.core :refer :all]))

(defn make-memory
  ([program]
   (make-memory program []))
  ([program additional-memory]
   (vec (into (asm program) (reverse additional-memory)))))

(defn make-cpu
  ([] {:pc 0 :ar 0 :xr 0 :yr 0 :sp 0 :brk false :eq false})
  ([cpu-parts]
   (merge (make-cpu) cpu-parts)))

(deftest instructions-test
  (testing "BRK stops the program"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "BRK")}
          final-state   (-> initial-state (step))]
      (is (cpu-brk-flag? final-state) "Sets the break cpu flag")
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "LDA loads the accumulator with memory"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "LDA 0x12")}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (cpu-ar final-state)) "Loads the value into the A register")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "ADC Adds memory to accumulator with Carry"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "ADC 0x12")}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (cpu-ar final-state)) "Add memory to accumulator")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "STA Stores the accumulator in memory"
    (let [initial-state {:cpu (make-cpu {:ar 0x12})
                         :mem (make-memory "STA 2" [0])}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (read-mem final-state 2)) "Stores the accumulator in memory")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "LDX Loads the X register with memory"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "LDX 0x12")}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (cpu-xr final-state)) "Loads the X register with memory")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "INX Increment X register by one"
    (let [initial-state {:cpu (make-cpu {:xr 0})
                         :mem (make-memory "INX")}
          final-state   (-> initial-state (step))]
      (is (= 1 (cpu-xr final-state)) "Increments X register by one")
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "INY Increment Y register by one"
    (let [initial-state {:cpu (make-cpu {:yr 0})
                         :mem (make-memory "INY")}
          final-state   (-> initial-state (step))]
      (is (= 1 (cpu-yr final-state)) "Increments Y register by one")
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "CPY Compare memory and Y register - sets the equal flag true when equal"
    (let [initial-state {:cpu (make-cpu {:yr 0x12 :eq false})
                         :mem (make-memory "CPY 0x12")}
          final-state   (-> initial-state (step))]
      (is (= true (cpu-flag-eq final-state)) "Sets the equal flag if equal")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "CPY Compare memory and Y register - sets the equal flag false when not equal"
    (let [initial-state {:cpu (make-cpu {:yr 0x12 :eq true})
                         :mem (make-memory "CPY 0x23")}
          final-state   (-> initial-state (step))]
      (is (= false (cpu-flag-eq final-state)) "Sets the equal flag if not equal")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "NOP No operation"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "NOP")}
          final-state   (-> initial-state (step))]
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "BNE Branch if not equal Pz=0 - increment PC"
    (let [initial-state {:cpu (make-cpu {:eq false})
                         :mem (make-memory "BNE 3")}
          final-state   (-> initial-state (step))]
      (is (= 3 (cpu-pc final-state)) "Modifies PC if equal flag set false")))
  (testing "BNE Branch if not equal Pz=0 - decrement PC"
    (let [initial-state {:cpu (make-cpu {:eq false})
                         :mem (make-memory "NOP
                                            BNE -1")}
          final-state   (-> initial-state
                            (step)
                            (step))]
      (is (= 0 (cpu-pc final-state)) "Modifies PC if equal flag set false")))
  (testing "BNE Branch if not equal Pz=0 - nop"
    (let [initial-state {:cpu (make-cpu {:eq true})
                         :mem (make-memory "BNE -1")}
          final-state   (-> initial-state (step))]
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2")))
  (testing "STAX Store the value from the A register in the memory location stored in the X register"
    (let [initial-state {:cpu (make-cpu {:xr 1 :ar 0x12})
                         :mem (make-memory "STAX" [0])}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (read-mem final-state 1)) "Store the value from the A register in the memory location stored in the X register")
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "DEY Decrement Y register by one"
    (let [initial-state {:cpu (make-cpu {:yr 2})
                         :mem (make-memory "DEY")}
          final-state   (-> initial-state (step))]
      (is (= 1 (cpu-yr final-state)) "Decrements Y register by one")
      (is (= 1 (cpu-pc final-state)) "Increments the program counter by 1")))
  (testing "LDY Loads the Y register with memory"
    (let [initial-state {:cpu (make-cpu)
                         :mem (make-memory "LDY 0x12")}
          final-state   (-> initial-state (step))]
      (is (= 0x12 (cpu-yr final-state)) "Loads the Y register with memory")
      (is (= 2 (cpu-pc final-state)) "Increments the program counter by 2"))))

(deftest jsr-tests
  (testing "JSR Jump to new location Saving Return"
    (let [initial-state {:cpu (make-cpu {:sp 15})
                         :mem (make-memory "NOP
                                            JSR 3"
                                           [0 0 0 0 0 0 0 0 0 0 0 0 0 0])}
          final-state   (-> initial-state
                            (step)
                            (step))]
      (is (= 1 (read-mem final-state 15)) "Pushes the current PC onto the stack")
      (is (= 14 (cpu-sp final-state)) "Decrements SP")
      (is (= 2 (cpu-pc final-state)) "Sets the PC to the next value in memory - 1"))))

(deftest rts-tests
  (testing "RTS Return from subroutine"
    (let [initial-state {:cpu (make-cpu {:sp 15})
                         :mem (make-memory "NOP    ; 0
                                            JSR 5  ; 1
                                            BRK    ; 3
                                            NOP    ; 4
                                            RTS    ; 5
                                            "
                                           [0 0 0 0 0 0 0 0 0 0 0 0 0 0])}
          final-state   (-> initial-state
                            (step)                          ; 0  NOP
                            (step)                          ; 1  JSR 5
                            (step)                          ; 3  NOP
                            (step))]                        ; 4  RTS
      (is (= 15 (cpu-sp final-state)) "Increments SP")
      (is (= 3 (cpu-pc final-state)) "Sets the PC to the return address + 1"))))
