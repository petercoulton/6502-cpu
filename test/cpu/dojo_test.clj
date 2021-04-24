(ns cpu.dojo-test
  (:require [clojure.test :refer :all]
            [cpu.helpers :refer :all]
            [cpu.parser :refer :all]
            [cpu.assemble :refer :all]
            [cpu.core :refer [run step]]))

(defn create-cpu
  []
  {:pc  0
   :ar  0
   :xr  0
   :yr  0
   :sp  (dec max-mem)
   :brk false
   :eq  false})

(defn- create-vm
  [mem cpu]
  {:cpu cpu :mem mem})



(deftest execute-dojo-programs-test
  (testing "dojo-1"
    (is (= [169 100 105 7 141 15 162  12
            232 160  24 0   0  0   0 107]
           (-> "LDA 100 ; foo
                ADC 7
                STA 15
                LDX 12
                INX
                LDY 24
                BRK"
               (parse)
               (assemble)
               (load-program 16)
               (create-vm (create-cpu))
               (run)
               (get-mem))))))
