(ns steganos.bit-utils
  "bit manipulation functions")

(defn- int-to-binary-
  [int]
  (loop [res () val int]
    (if (= 0 val)
      res
      (recur (conj res (bit-and 1 val))
             (bit-shift-right val 1)))))

(defn int-to-binary
  [int bits-required]
  (let [bin (int-to-binary- int)]
    (concat (repeat (- bits-required (count bin)) 0) bin)))

(defn binary-to-int
  [binary]
  (loop [total 0 multiplier 1 val (reverse binary)]
    (if (empty? val)
      total
      (recur (if (= 1 (first val)) (+ total multiplier) total)
             (+ multiplier multiplier)
             (rest val)))))

;;; todo: check that all ints can be represented by bits-per-int
(defn ints->binary-stream
  [ints bits-per-int]
  (mapcat #(int-to-binary % bits-per-int) ints))
