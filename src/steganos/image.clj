(ns steganos.image
  "Basic image manipulation functions"
  (:import [java.io File]
           [javax.imageio ImageIO]
           [java.awt.image BufferedImage]))

(defn write-image
  "writes a bufferedImage out as png"
  [bi filename]
  (ImageIO/write bi "png" (File. filename)))

(defn read-image
  "reads image file, returning a bufferedImage"
  [filename]
  (ImageIO/read (File. filename)))

(defn image-dimensions
  [bi]
  [(.getWidth bi) (.getHeight bi)])

(defn set-pixel
  "set pixel on a buffered image"
  ([bi x y r g b] 
     (. bi setRGB x y (rgba-to-int r g b 255)))
  ([bi x y [r g b]] 
     (. bi setRGB x y (rgba-to-int r g b 255))))

(defn get-rgb
  "return rgb value at the x,y co-ordinate"
  [bi x y]
  (let [pixel (. bi getRGB x y)]
    [(bit-and (bit-shift-right pixel 16) 0xff)
     (bit-and (bit-shift-right pixel 8) 0xff)
     (bit-and pixel 0xff)]))

(defn each-pixel
  [bi fun]
  "passes the rgb triplet of each pixel into fun"
  (let [[w h] (image-dimensions bi)
        co-ords (for [y (range h) x (range w)] [x y])]
    (map (fn [[x y]] (fun (get-rgb bi x y))) co-ords)))