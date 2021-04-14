(ns cpu.core-test
  (:require [clojure.test :refer :all]
            [cpu.core :refer :all]))

(deftest program-tests
  (testing "Simple program adds 2 numbers and stores in memory"
    (is (=
          (list
            {:pc 7, :ar 107, :brk true}
            [169 100
             105 7
             141 15
             0 0
             0 0
             0 0
             0 0
             0 107])
          (execute
            {:pc 0, :ar 0, :brk false}
            [0xa9 100
             0x69 7
             0x8d 15
             0 0
             0 0
             0 0
             0 0
             0 0])))))
