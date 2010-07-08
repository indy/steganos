(ns steganos.core
  "Top level functions"
  (:require [steganos [image :as image]
                      [payload :as payload]
                      [text :as text]])
  (:use [clojure.contrib.duck-streams :only [spit]]))

(defn embed-text
  "loads fname, inserts the message and saves result as out-fname"
  [message carrier-fname out-fname bpcc]
  (let [carrier-bi (image/read-image carrier-fname)
        [carrier-w carrier-h] (image/image-dimensions carrier-bi)]
    (if (text/can-fit? message carrier-w carrier-h bpcc)
      (do
        (payload/save-payload (text/encode message) bi bpcc)
        (image/write-image bi out-fname))
      (throw (Exception. "message is too large for the image")))))

(defn extract-text
  "returns message that's in fname"
  [fname bpcc]
  (let [bi (image/read-image fname)]
    (text/decode (payload/load-payload bi bpcc))))





