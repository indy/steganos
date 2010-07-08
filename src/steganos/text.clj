(ns steganos.text
  "Convert between text and binary"
  (:require [steganos.bit-utils :as bit-utils]))

;;; 97 chars = 7 bits
(def +legal-chars+ "\n\t !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")
(def +bits-required+ 7)

;;; start at 1 since 0 is used as terminator
(def char-map (zipmap +legal-chars+ (iterate inc 1)))
(def int-map (zipmap (iterate inc 1) +legal-chars+))

(defn encode
  [message]
  (bit-utils/ints->binary-stream (concat (map char-map message) '(0))
                                 +bits-required+))

(defn decode
  [encoded-message]
  (apply str (map #(int-map (bit-utils/binary-to-int %)) 
                  (take-while #(not (= % (repeat +bits-required+ 0)))
                              (partition +bits-required+ encoded-message)))))

(defn max-message-size
  "returns the largest message that can be stored in an image of the given size"
  [width height bpcc]
  (int (- (/ (* width height 3 bpcc) 
             +bits-required+) 
          1)))                          ; terminator

(defn can-fit?
  [message w h bpcc]
  (<= (count message) (max-message-size w h bpcc)))
