(ns cpu.core
  (:gen-class))

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

(defn inc-pc
  [pc]
  (mod (inc pc) 16))

(defn step
  [opcode vm]
  (let [cpu (first vm)
        mem (second vm)
        pc (inc-pc (:pc cpu))]
    (cond
      (= 0 opcode)
      (list (merge cpu {:pc  pc
                        :brk true})
            mem)
      (= 0xa9 opcode)
      (list (merge cpu {:pc (inc-pc pc)
                        :ar (nth mem pc)})
            mem)
      (= 0x69 opcode)
      (let [operand (nth mem pc)]
        (list (merge cpu {:pc (inc-pc pc)
                          :ar (+ operand (:ar cpu))})
              mem))
      (= 0x8d opcode)
      (let [operand (nth mem pc)]
        (list (merge cpu {:pc (inc-pc pc)})
              (assoc mem operand (:ar cpu))))
      :else
      (list cpu mem))))

;(->>
;  (list cpu mem)
;  (step 0xa9)
;  (step 0x69)
;  (step 0x8d)
;  (step 0x0))
;
;(step 0x0 (step 0x8d (step 0x69 (step 0xa9 (list cpu mem)))))

(defn execute
  [cpu mem]
  (let [opcode (nth mem (:pc cpu))]
    (loop [vm (step opcode (list cpu mem))]
      (if (= true (:brk (first vm)))
        (list (first vm) (second vm))
        (recur (step (nth (second vm) (:pc (first vm)))
                     (list (first vm) (second vm))))))))

(execute cpu mem)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
