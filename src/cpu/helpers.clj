(ns cpu.helpers)

(defn inc-pc
  ([pc]
   (mod (inc pc) 256))
  ([pc n]
   (mod (+ n pc) 256)))

(defn cpu-pc
  [vm]
  (:pc (:cpu vm)))

(defn cpu-brk
  [vm]
  (:brk (:cpu vm)))

(defn cpu-ar
  [vm]
  (:ar (:cpu vm)))

(defn cpu-xr
  [vm]
  (:xr (:cpu vm)))

(defn cpu-yr
  [vm]
  (:yr (:cpu vm)))

(defn cpu-brk-flag?
  [vm]
  (= true (cpu-brk vm)))

(defn read-mem
  [vm address]
  (nth (:mem vm) address))

(defn write-mem
  [vm address value]
  (assoc-in vm [:mem address] value))

(defn next-opcode
  [vm]
  (read-mem vm (cpu-pc vm)))

(defn incr-pc
  ([vm]
   (let [pc (inc-pc (cpu-pc vm))]
     (assoc-in vm [:cpu :pc] pc)))
  ([vm n]
   (let [pc (inc-pc (cpu-pc vm) n)]
     (assoc-in vm [:cpu :pc] pc))))

(defn set-ar
  [vm n]
  (assoc-in vm [:cpu :ar] n))

(defn inc-ar
  [vm n]
  (let [ar (cpu-ar vm)]
    (set-ar vm (+ ar n))))

(defn set-xr
  [vm n]
  (assoc-in vm [:cpu :xr] n))

(defn incr-xr
  [vm]
  (let [xr (cpu-xr vm)]
    (set-xr vm (inc xr))))

(defn set-yr
  [vm n]
  (assoc-in vm [:cpu :yr] n))

(defn incr-yr
  [vm]
  (let [yr (cpu-yr vm)]
    (set-yr vm (inc yr))))


(defn set-flag-brk
  [vm]
  (assoc-in vm [:cpu :brk] true))

(defn make-mem
  [mem size]
  (let [padding (- size (.length mem))]
    (vec (into mem (repeat padding 0)))))

(defn load-program
  ([program]
   (make-mem program 256))
  ([program mem-size]
   (make-mem program mem-size)))

(defn lookup-instruction
  [opcode instructions]
  (first (filter #(= opcode (:opcode %)) instructions)))

