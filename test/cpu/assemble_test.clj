(ns cpu.assemble-test
  (:require [clojure.test :refer :all]
            [cpu.parser :refer :all]
            [cpu.assemble :refer :all]))

(deftest assemble-single-instruction-tests
  (testing "BRK"
    (is (= [0x0] (assemble (parse "BRK")))))
  (testing "NOP"
    (is (= [0xea] (assemble (parse "NOP")))))
  (testing "ADC 10"
    (is (= [0x69 10] (assemble (parse "ADC 10")))))
  (testing "BNE -10"
    (is (= [0xd0 -10] (assemble (parse "BNE -10"))))))

(deftest assemble-multiple-instructions-tests
  (testing "NOPs"
    (is (= [0xea 0xea] (assemble (parse "NOP
                                         NOP"))))))

(deftest assemble-with-labels-tests
  (testing "absolute addressing"
    (is (= [0x20 0x00]
           (assemble (parse "loop:
                               JSR loop"))))
    (is (= [0xea 0xea 0xea 0xea 0x20 0x02]
           (assemble (parse "  NOP
                               NOP
                             loop:
                               NOP
                               NOP
                               JSR loop")))))
  (testing "relative addressing"
    (is (= [0xd0 0x00]
           (assemble (parse "loop:
                               BNE loop"))))
    (is (= [0xea 0xea 0xea 0xea 0xd0 -0x02]
           (assemble (parse "  NOP
                               NOP
                             loop:
                               NOP
                               NOP
                               BNE loop")))))
  (testing "relative addressing"
    (is (= [0xd0 0x00]
           (assemble (parse "loop:
                               BNE loop"))))
    (is (= [0xea 0xea 0xd0 0x03 0xea 0xea]
           (assemble (parse "  NOP
                               NOP
                               BNE loop
                               NOP
                             loop:
                               NOP"))))))

(deftest assemble-with-literal-values-tests
  (testing "numbers"
    (is (= [0x69 10]
           (assemble (parse "ADC 10"))))))

(deftest assemble-dojo-programs-test
  (testing "dojo-1"
    (is (= [169 100 105 7 141 15 162 12 232 160 24 0]
           (assemble (parse "LDA 100 ; foo
                             ADC 7
                             STA 15
                             LDX 12
                             INX
                             LDY 24
                             BRK"))))))
