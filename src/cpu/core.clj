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
  ([pc]
   (mod (inc pc) 16))
  ([pc n]
   (mod (+ n pc) 16)))

(defn cpu-pc
  [vm]
  (:pc (:cpu vm)))

(defn cpu-brk
  [vm]
  (:brk (:cpu vm)))

(defn cpu-brk-flag?
  [vm]
  (= true (cpu-brk vm)))

(defn read-mem
  [vm loc]
  (nth (:mem vm) loc))

(defn next-opcode
  [vm]
  (read-mem vm (cpu-pc vm)))

(defn step
  [opcode vm]
  (let [cpu (:cpu vm)
        mem (:mem vm)
        pc  (cpu-pc vm)]
    (cond
      (= 0 opcode)
      {:cpu (merge cpu {:brk true
                        :pc  (inc-pc pc)})
       :mem mem}

      (= 0xa9 opcode)
      (let [operand (nth mem (+ pc 1))]
        {:cpu (merge cpu {:ar operand
                          :pc (inc-pc pc 2)})
         :mem mem})

      (= 0x69 opcode)
      (let [operand (nth mem (+ pc 1))
            ar      (:ar cpu)]
        {:cpu (merge cpu {:ar (+ ar operand)
                          :pc (inc-pc pc 2)})
         :mem mem})

      (= 0x8d opcode)
      (let [operand (nth mem (+ pc 1))]
        {:cpu (merge cpu {:pc (inc-pc pc 2)})
         :mem (assoc mem operand (:ar cpu))})

      :else vm)))

;(->>
;  (list cpu mem)
;  (step 0xa9)
;  (step 0x69)
;  (step 0x8d)
;  (step 0x0))
;
;(step 0x0 (step 0x8d (step 0x69 (step 0xa9 (list cpu mem)))))

(defn run
  [vm]
  (let [opcode (next-opcode vm)]
    (loop [vm (step opcode vm)]
      (if (cpu-brk-flag? vm)
        vm
        (recur (step (next-opcode vm) vm))))))

(run {:cpu cpu :mem mem})

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
