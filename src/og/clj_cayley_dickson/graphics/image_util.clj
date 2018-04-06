(ns og.clj-cayley-dickson.graphics.image-util
  (:require [mikera.image.core :as imgz]
            [mikera.image.colours :as c]
            [image-resizer.resize :as resize]
            [image-resizer.scale-methods :as scale-fns])
  (:import (java.awt.image BufferedImage)))


(defn combine
  [{:keys [img1 img2 xoffset yoffset canvas-width canvas-height]
    :or   {xoffset 0 yoffset 0}}]
  (let [img1    (or
                  img1
                  (BufferedImage.
                    canvas-width
                    canvas-height
                    BufferedImage/TYPE_INT_ARGB))
        new-img (BufferedImage.
                  canvas-width
                  canvas-height
                  BufferedImage/TYPE_INT_ARGB)
        g       (.getGraphics new-img)]
    (doto g
      (.drawImage img1 0 0 nil)
      (.drawImage img2 xoffset yoffset nil)
      (.dispose))
    new-img))

(defn resize-fit [img width height]
  (-> img
      ((resize/resize-fn
         width
         height
         scale-fns/ultra-quality))))

(defn img->vec [img]
  (-> img
      imgz/get-pixels
      vec))

(defn imgs->distance [image1 image2 rescale?]
  (let [target-w     (if rescale? 128
                                  (.getWidth image1))
        target-h     (if rescale? 128
                                  (.getHeight image1))
        i1-small     (if rescale? (resize-fit image1 target-w target-h)
                                  image1)
        i2-small     (if rescale? (resize-fit image2 target-w target-h)
                                  image2)
        i1-vec       (img->vec i1-small)
        i2-vec       (img->vec i2-small)
        both         (map vector i1-vec i2-vec)
        diffs        (map
                       (fn [[v1 v2]]
                         (let [[r1 g1 b1 a1] (c/components-argb v1)
                               [r2 g2 b2 a2] (c/components-argb v2)
                               denom  (+ (max r1 r2) (max g1 g2) (max b1 b2))
                               answer (if (pos? denom)
                                        (float
                                          (/
                                            (+
                                              (Math/abs (- r1 r2))
                                              (Math/abs (- g1 g2))
                                              (Math/abs (- b1 b2)))
                                            denom))
                                        0.0)]
                           answer))
                       both)
        total-sum    (* target-h target-w)
        sum-of-diffs (reduce + diffs)
        dist         (float (/ sum-of-diffs total-sum))]
    ;(println "DIST: " dist " = " sum-of-diffs " / " total-sum)
    dist))
