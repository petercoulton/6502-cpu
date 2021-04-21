(ns cpu.debug)

(def mem-row-format
  (str "%08x  "
       "%02x %02x %02x %02x %02x %02x %02x %02x  "
       "%02x %02x %02x %02x %02x %02x %02x %02x  "
       "|%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c|"))

(defn is-ascii?
  [chr]
  (and (>= chr 32)
       (<= chr 126)))

(defn long->ascii
  [chr]
  (byte
    (if (is-ascii? chr)
      chr
      46)))

(defn format-mem-row
  [row]
  (let [loc   (first row)
        data  (second row)
        chars (map long->ascii data)]
    (apply (partial format mem-row-format)
           (flatten (list loc data chars)))))

(defn group-memory-into-rows
  ([mem]
   (group-memory-into-rows mem 16))
  ([mem width]
   (map-indexed #(list (reduce + %2) (* width %) %2)
                (partition width mem))))

(defn dedup-mem
  [mem]
  (let [rows (group-memory-into-rows mem)
        end  (second (last rows))]
    (reverse
      (reduce (fn [s e]
                (let [row-sum      (first e)
                      last-row-sum (first (first s))
                      pos          (second e)]
                  (if (and (= last-row-sum row-sum)
                           (not (= pos end)))
                    s
                    (conj s e))))
              '() rows))))

(defn format-mem
  [mem]
  (let [lines (dedup-mem mem)]
    (map #(format-mem-row (rest %)) lines)))

(defn format-register
  [[k v]]
  (format "%-4s = 0x%02x % 4d" k v v))

(defn format-flags
  [[k v]]
  (format "%-4s = %b" k v))

(defn format-registers
  [cpu]
  (map format-register (select-keys cpu [:pc :ar :xr :yr :sp])))

(defn format-status-flag
  [cpu]
  (map format-flags (select-keys cpu [:brk :eq])))

(defn format-dump
  [vm]
  (flatten
    (list (format-registers (:cpu vm))
          (format-status-flag (:cpu vm))
          (format-mem (:mem vm)))))

;

(defn dump!!
  [vm]
  (doall (map println (format-dump vm)))
  nil)

(defn dump!
  [vm]
  (dump!! vm)
  vm)
