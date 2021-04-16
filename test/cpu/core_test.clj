(ns cpu.core-test
  (:require [clojure.test :refer :all]
            [cpu.core :refer :all]))

(deftest program-tests
  (testing "Simple program adds 2 numbers and stores in memory"
    (is (=
          {:cpu {:pc 7, :ar 107, :xr 0, :yr 0, :brk true, :eq false}
           :mem [169 100
                 105 7
                 141 15
                 0 0
                 0 0
                 0 0
                 0 0
                 0 107]}
          (run
            {:cpu {:pc 0, :ar 0, :xr 0, :yr 0, :brk false, :eq false}
             :mem [169 100
                   105 7
                   141 15
                   0 0
                   0 0
                   0 0
                   0 0
                   0 0]})))))
