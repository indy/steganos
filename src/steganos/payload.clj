(ns steganos.payload
  "Extracts/inserts binary payload into image"
  (:require [steganos.image :as image]))

(defn- make-mask
  [bpcc]
  (int (- (Math/pow 2 bpcc) 1)))

(defn- make-clearing-mask
  [bpcc]
  (- 256 (int (Math/pow 2 bpcc))))

(defn- encode-section
  [section val mask]
  (bit-or section (bit-and val mask)))

(defn- encode-rgb
  [old-rgb triplet mask]
  [(encode-section (nth triplet 0) (old-rgb 0) mask)
   (encode-section (nth triplet 1) (old-rgb 1) mask)
   (encode-section (nth triplet 2) (old-rgb 2) mask)])

(defn- store-triplet
  [triplet x y image mask]
  (let [old-rgb (image/get-rgb image x y)
        new-rgb (encode-rgb old-rgb triplet mask)]
    (image/set-pixel image x y new-rgb)))

(defn- store-row
  [row row-num image mask]
  (dorun
   (map #(store-triplet %1 %2 row-num image mask) row (iterate inc 0))))

(defn- store-payload
  [payload image bpcc] ; payload = a list of row of triplets
  (let [mask (make-clearing-mask bpcc)]
    (map #(store-row %1 %2 image mask) payload (iterate inc 0))))

(defn- integer-to-binary
  [i bpcc]
  (loop [res '() int i]
    (if (= 0 int)
      (concat (repeat (- bpcc (count res)) 0) res)
      (recur (conj res (bit-and 1 int))
             (bit-shift-right int 1)))))

(defn- binary-to-integer
  [s]
  (loop [res 0 binseq (reverse s) multiplier 1]
    (if (empty? binseq)
      res
      (recur (+ res (* (first binseq) multiplier))
             (rest binseq)
             (* multiplier 2)))))

(defn- batch-message
  [m bpcc]
  (map #(binary-to-integer %) (partition bpcc bpcc (repeat 0) m)))

(defn- into-triplets
  [encoded-message bpcc]
  (let [batched-message (batch-message encoded-message bpcc)]
  (partition 3 3 (repeat 0) batched-message)))

(defn- into-rows
  [image-width triplets]
  (partition image-width image-width (repeat '(0 0 0)) triplets))

(defn- pack-payload
  [encoded-message image-width bpcc]
  (into-rows image-width
             (into-triplets encoded-message bpcc)))

(defn save-payload
  [content bi bpcc]
  (let [[w _] (image/image-dimensions bi)]
    (dorun
       (store-payload (pack-payload content w bpcc) bi bpcc))))

; ------------------------------------------------------------

(defn- retrieve-triplet
  [x y bi mask]
  (let [rgb (image/get-rgb bi x y)]
    (map #(bit-and (nth rgb %) mask) (range 3))))

(defn- retrieve-row
  [row width bi mask]
  (map #(retrieve-triplet % row bi mask) (range width)))

(defn- retrieve-packets
  [bi mask]
  (let [[width height] (image/image-dimensions bi)]
    (mapcat #(retrieve-row % width bi mask) (range height))))

(defn- triplet-to-binary
  [t bpcc]
  (mapcat #(integer-to-binary % bpcc) t))

(defn- unpack-payload
  [payload bpcc]
  (mapcat #(triplet-to-binary % bpcc) payload))

(defn load-payload
  [bi bpcc]
  (unpack-payload (retrieve-packets bi (make-mask bpcc)) 
                  bpcc))

