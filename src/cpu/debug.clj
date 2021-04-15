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

(defn dedup-mem
  [mem]
  (let [rows (map-indexed #(list (reduce + %2) (* 16 %) %2)
                          (partition 16 mem))
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

(defn format-registers
  [cpu]
  (map (fn [reg]
         (let [k (name (first reg))
               v (second reg)]
           (format "%-3s = 0x%02x % 4d" k v v)))
       (select-keys cpu [:pc :ar])))

(defn format-status-flag
  [cpu]
  (map (fn [reg]
         (let [k (name (first reg))
               v (second reg)]
           (format "%-3s = %b" k v)))
       (select-keys cpu [:brk])))

(defn format-dump
  [vm]
  (flatten
    (list (format-registers (:cpu vm))
          (format-status-flag (:cpu vm))
          (format-mem (:mem vm)))))

(defn dump!!
  [vm]
  (doall (map println (format-dump vm)))
  nil)

(defn dump!
  [vm]
  (dump!! vm)
  vm)
