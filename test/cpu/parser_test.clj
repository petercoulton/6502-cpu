(ns cpu.parser-test
  (:require [clojure.test :refer :all]
            [cpu.parser :refer :all]))

(deftest parse-value-tests
  (testing "numbers"
    (is (= {:literal 0} (parse-value "0"))))
  (testing "text"
    (is (= {:label "tomato"} (parse-value "tomato")))))

(deftest parse-line-tests
  (testing "NOP"
    (is (= {:name :nop, :value nil, :size 1, :opcode 234, :mode :absolute}
           (parse-line "NOP"))))
  (testing "BRK"
    (is (= {:name :brk :value nil :size 1, :opcode 0, :mode :absolute}
           (parse-line "BRK"))))
  (testing "start:"
    (is (= {:label "start"}
           (parse-line "start:"))))
  (testing "DEY"
    (is (= {:name :dey :value nil :size 1, :opcode 136, :mode :absolute}
           (parse-line "DEY")))))

(deftest parser-tests
  (testing "Parse a single instruction"
    (is (= {:labels       []
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}]}
           (parse "NOP"))))
  (testing "Parse a label"
    (is (= {:labels       [{:label "start" :address 0}]
            :instructions []}
           (parse "start:"))))
  (testing "Parse multiple instructions"
    (is (= {:labels       []
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}
                           {:name :nop :value nil :address 1 :size 1, :opcode 234, :mode :absolute}]}
           (parse "NOP
                   NOP"))))
  (testing "Parse a label before instruction"
    (is (= {:labels       [{:label "start" :address 0}]
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}]}
           (parse "start:
                     NOP"))))
  (testing "Parse a label after instruction"
    (is (= {:labels       [{:label "start" :address 1}]
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}]}
           (parse "  NOP
                   start:"))))
  (testing "Parse an instruction referencing a label"
    (is (= {:labels       [{:label "start" :address 0}]
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}
                           {:name :bne :value {:label "start"} :address 1 :size 2, :opcode 208, :mode :relative}]}
           (parse "start:
                     NOP
                     BNE start"))))
  (testing "Ignore blank lines"
    (is (= {:labels       [{:label "start" :address 0}]
            :instructions [{:name :nop :value nil :address 0 :size 1, :opcode 234, :mode :absolute}
                           {:name :bne :value {:label "start"} :address 1 :size 2, :opcode 208, :mode :relative}]}
           (parse "

                   start:


                     NOP


                     BNE start


                   ")))))

(defn label
  [label address]
  {:label label :address address})

(defn instr
  ([name value size opcode mode address]
   {:size size, :name name, :opcode opcode, :value value, :address address, :mode mode})
  ([name value opcode mode address]
   (instr name value 2 opcode mode address))
  ([name opcode mode address]
   (instr name nil 1 opcode mode address)))

(defn l [label] {:label label})
(defn v [n] {:literal n})

(defn dey [address] (instr :dey 0x88 :absolute address))
(defn brk [address] (instr :brk 0x00 :absolute address))
(defn inx [address] (instr :inx 0xe8 :absolute address))
(defn rts [address] (instr :rts 0x60 :absolute address))
(defn stax [address] (instr :stax 0x8f :absolute address))
(defn jsr [value address] (instr :jsr (l value) 0x20 :absolute address))
(defn cpy [value address] (instr :cpy (v value) 0xe0 :absolute address))
(defn bne [value address] (instr :bne (l value) 0xd0 :relative address))
(defn lda [value address] (instr :lda (v value) 0xa9 :absolute address))

(deftest parser-full-program-tests
  (testing "Parse an full program"
    (is (= {:labels       [(label "loop" 0)
                           (label "print" 8)
                           (label "print-who" 11)]
            :instructions [(jsr "print-who" 0)
                           (dey 2)
                           (cpy 0 3)
                           (bne "loop" 5)
                           (brk 7)
                           (stax 8)
                           (inx 9)
                           (rts 10)
                           (lda 0x77 11)
                           (jsr "print" 13)
                           (lda 0x68 15)
                           (jsr "print" 17)
                           (lda 0x6F 19)
                           (jsr "print" 21)
                           (lda 0x20 23)
                           (jsr "print" 25)
                           (rts 27)]}
           (parse "loop:
                     JSR print-who
                     DEY
                     CPY 0
                     BNE loop
                     BRK

                   print:
                     STAX
                     INX
                     RTS

                   print-who:
                     LDA 0x77
                     JSR print
                     LDA 0x68
                     JSR print
                     LDA 0x6F
                     JSR print
                     LDA 0x20
                     JSR print
                     RTS"
                  )))))
