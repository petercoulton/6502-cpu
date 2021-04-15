(ns cpu.assemble-test
  (:require [clojure.test :refer :all]
            [cpu.assemble :refer :all]))


(deftest assembler-tests
  (testing "Assembles a simple program"
    (let [program  "LDA 100 ; foo
                   ADC 7
                   STA 15
                   BRK"
          expected [169 100 105 7 141 15 0]]
      (is (= expected (asm program))))))


