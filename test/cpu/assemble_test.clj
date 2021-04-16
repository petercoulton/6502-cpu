(ns cpu.assemble-test
  (:require [clojure.test :refer :all]
            [cpu.assemble :refer :all]
            [cpu.core :refer :all]))


(deftest assembler-tests
  (let [program "LDA 100 ; foo
                 ADC 7
                 STA 15
                 BRK"]
    (testing "Assembles a simple program"
      (let [expected [169 100 105 7 141 15 0]]
        (is (= expected (asm program)))))
    (testing "Assembled programs work as expected"
      (let [expected {:cpu {:pc 7, :ar 107, :brk true}
                      :mem [169 100 105 7 141 15 0 0
                            0 0 0 0 0 0 0 107]}]
        (is (= expected
               (run {:cpu {:pc 0, :ar 0, :brk false}
                     :mem (load-program (asm program) 16)})))))))


