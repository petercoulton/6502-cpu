(ns cpu.helpers)

(def max-mem 640)

(defn inc-pc
  ([pc]
   (mod (inc pc) max-mem))
  ([pc n]
   (mod (+ n pc) max-mem)))

(defn cpu-pc
  [vm]
  (get-in vm [:cpu :pc]))

(defn set-pc
  [vm value]
  (assoc-in vm [:cpu :pc] value))

(defn cpu-brk
  [vm]
  (get-in vm [:cpu :brk]))

(defn cpu-ar
  [vm]
  (get-in vm [:cpu :ar]))

(defn cpu-xr
  [vm]
  (get-in vm [:cpu :xr]))

(defn cpu-yr
  [vm]
  (get-in vm [:cpu :yr]))

(defn cpu-brk-flag?
  [vm]
  (= true (cpu-brk vm)))

(defn read-mem
  [vm address]
  (nth (:mem vm) address))

(defn get-mem
  [vm]
  (:mem vm))

(defn get-cpu
  [vm]
  (:cpu vm))

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

(defn set-flag-eq
  [vm equal?]
  (assoc-in vm [:cpu :eq] equal?))

(defn cpu-flag-eq
  [vm]
  (get-in vm [:cpu :eq]))

(defn cpu-sp
  [vm]
  (get-in vm [:cpu :sp]))

(defn set-sp
  [vm value]
  (assoc-in vm [:cpu :sp] value))

(defn inc-sp
  [vm]
  (let [sp (cpu-sp vm)]
    (set-sp vm (+ 1 sp))))

(defn dec-sp
  [vm]
  (let [sp (cpu-sp vm)]
    (set-sp vm (dec sp))))

(defn make-mem
  [mem size]
  (let [padding (- size (.length mem))]
    (vec (into mem (repeat padding 0)))))

(defn load-program
  ([program]
   (make-mem program max-mem))
  ([program mem-size]
   (make-mem program mem-size)))


