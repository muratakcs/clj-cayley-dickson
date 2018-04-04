(ns og.clj-cayley-dickson.graphics.fractal
  (:require [og.clj-cayley-dickson.core :refer :all]
            [og.clj-cayley-dickson.construction
             :refer [complex quaternion]]
            [og.clj-cayley-dickson.graphics.colors
             :refer [argb-int]]
            [me.raynes.fs :as fs])
  (:import (java.awt Dimension Color)
           (javax.swing JFrame JLabel)
           (java.awt.image BufferedImage WritableRaster)
           (org.apache.commons.math3.complex Complex)
           (java.io File)
           (javax.imageio ImageIO)))

; credit to:
; https://nakkaya.com/2009/09/29/fractals-in-clojure-mandelbrot-fractal/
; https://sites.google.com/site/drjohnbmatthews/raster

(defn calc-iterations-og-quat
  "5x slower than apache commons math"
  [a b c d max-iterations impl]
  (let [
        q-number (quaternion {:a a :b b :c c :d d :impl impl})
        ]
    (loop [z-q        q-number
           iterations 0]
      (if (or
            (> (mag z-q) 2.0)
            (> iterations max-iterations))
        (if (= 0 iterations)
          0
          (- iterations 1))
        (recur
          (plus q-number (times z-q z-q))
          (inc iterations))))))

(defn calc-iterations-og-complex
  "5x slower than apache commons math"
  [p q max-iterations impl]
  (let [c (complex {:a p :b q :impl impl})]
    (loop [z          c
           iterations 0]
      (if (or (> (mag z) 2.0)
              (> iterations max-iterations))
        (if (= 0 iterations)
          0
          (- iterations 1))
        (recur (plus c (times z z)) (inc iterations))))))


(defn calc-iterations-commons-complex
  "fast"
  [p q max-iterations]
  (let [c (Complex. p q)]
    (loop [z          c
           iterations 0]
      (if (or (> (.abs z) 2.0)
              (> iterations max-iterations))
        (if (= 0 iterations)
          0
          (- iterations 1))
        (recur (.add c (.multiply z z)) (inc iterations))))))

(defn cartesian2d->quaternion-coeffs [p q]
  [(/ p 1.8)
   (/ q 1.8)
   (+ (/ q 6) (/ p 6))
   0.01])


(defn calc-iterations [p q max-iters impl]
  (case impl
    :og-plain-quat (apply calc-iterations-og-quat
                          (concat
                            (cartesian2d->quaternion-coeffs p q)
                            [max-iters :plain]))
    :og-plain (calc-iterations-og-complex p q max-iters :plain)
    :og-apache (calc-iterations-og-complex p q max-iters :apache)
    :apache (calc-iterations-commons-complex p q max-iters)))


(defn calc-pixel-color
  [iterations max-iterations]
  (if (or (< iterations 10)
          (= iterations max-iterations))
    (Color. 0 0 0)
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    gray
          g    (min (int (/ (* 5 (* gray gray)) 255)) 255)
          b    (min (int (+ 40 (/ (* 5 (* gray gray)) 255))) 255)]
      (Color. r g b))))

(defn calc-pixel-color-argb
  [iterations max-iterations]
  (if (or (< iterations 10)
          (= iterations max-iterations))
    (int-array [1.0 0 0 0])
    (let [gray (int (/ (* iterations 255) max-iterations))
          r    gray
          g    (min (int (/ (* 5 (* gray gray)) 255)) 255)
          b    (min (int (+ 40 (/ (* 5 (* gray gray)) 255))) 255)]
      (int-array [1.0 r g b]))))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x    (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn intensity
  [i j x y width height max-iterations surface-width surface-height impl]
  (let [p          (+ x
                      (* width
                         (/ i surface-width)))
        q          (+ y
                      (* height
                         (/ j surface-height)))
        iterations (calc-iterations
                     p q max-iterations (first impl))
        color      (case (second impl)
                     :draw-lines (calc-pixel-color
                                   iterations max-iterations)
                     :draw-raster (calc-pixel-color-argb
                                    iterations max-iterations))]
    [color i j]))


(defn generate-intensities [x y width height max-iterations surface-width surface-height impl]
  (let [cross-prod (cart
                     [(range surface-width)
                      (range surface-height)])
        intenses   (pmap
                     (fn [[i j]]
                       (intensity
                         i j x y
                         width height max-iterations
                         surface-width surface-height impl))
                     cross-prod)]
    intenses))

(defn draw-intensities-w-lines [intensities graphics]
  (doseq [[color i j] intensities]
    (.setColor graphics color)
    (.drawLine graphics i j i j)))

(defn draw-intensities-w-raster [intensities image]
  ; Maybe look at this if performance is not improving with this technique:
  ; https://stackoverflow.com/questions/25178810/create-a-writableraster-based-on-int-array
  (let [^WritableRaster raster (.getRaster image)]
    (doseq [[[a r g b] i j] intensities]
      (let [pixel (int-array [a r g b])]
        (.setPixel raster i j pixel)))))

(defn generate
  "docs"
  [x y width height max-iterations image surface-width surface-height impl]
  (let [graphics    (.createGraphics image)
        intensities (generate-intensities
                      x y width height max-iterations
                      surface-width surface-height impl)]
    (case (second impl)
      :draw-lines (draw-intensities-w-lines intensities graphics)
      :draw-raster (draw-intensities-w-raster intensities image))))

(defn draw
  [x y width height iterations surface-width surface-height impl outdir]
  (if-not (fs/exists? outdir)
    (fs/mkdir outdir))
  (let [filename (str outdir "/fractal-" (System/currentTimeMillis) ".png")
        image    (BufferedImage.
                   surface-width
                   surface-height
                   BufferedImage/TYPE_INT_RGB)
        canvas   (proxy [JLabel] []
                   (paint [g]
                     (.drawImage g image 0 0 this)))]
    (println "Draw to: " filename)
    (generate x y width height
              iterations image
              surface-width surface-height impl)
    (ImageIO/write image "png" (File. filename))
    (doto (JFrame.)
      (.add canvas)
      (.setSize
        (Dimension.
          surface-width
          surface-height))
      ;TODO so much show!
      ;(.show)
      )))


(defn offsets-map [i-deltas j-deltas w-deltas h-deltas]
  (->> (map vector i-deltas j-deltas w-deltas h-deltas)
       (map
         (fn [[i-delt j-delt w-delt h-delt]]
           {:i-delt i-delt :j-delt j-delt
            :w-delt w-delt :h-delt h-delt}))))

(defn xy-offsets-pan [periods images-count]
  "Takes number of periods (float) and number of frames to produce (int).
  Returns a coll of maps with keys representing the 2d plane offsets,
  :i-delt, (x offset), :j-delt (y offset),
  :w-delt (horizontal stretch),:h-delt (vertical stretch)
  This implementation specifically returns a transform which
  pans camera diagonally."
  (let [period-length      (* periods 2 Math/PI)
        frame-domain-delta (/ period-length images-count)
        i-deltas           (map
                             (fn [iter]
                               (*
                                 (* 0.6
                                    (- 1.0

                                       (/ iter images-count)))))
                             (range images-count))
        j-deltas           (map
                             (fn [iter]
                               (*
                                 (* 0.9
                                    (- 1.0
                                       (/ iter images-count)))))
                             (range images-count))
        w-deltas           (repeat images-count 0.0)
        h-deltas           (repeat images-count 0.0)
        domain-range       (offsets-map i-deltas j-deltas w-deltas h-deltas)]
    domain-range))

(defn xy-offsets-periodic [periods images-count]
  "Takes number of periods (float) and number of frames to produce (int).
  Returns a coll of maps with keys representing the 2d plane offsets,
  :i-delt, (x offset), :j-delt (y offset),
  :w-delt (horizontal stretch),:h-delt (vertical stretch)
  This implementation specifically returns a transform which
  pans camera in a periodic sprial."
  (let [period-length      (* periods 2 Math/PI)
        frame-domain-delta (/ period-length images-count)
        i-deltas           (map
                             (fn [iter]
                               (*
                                 (* 0.6
                                    (- 1.0

                                       (/ iter images-count)))
                                 (* (Math/cos (* iter frame-domain-delta)))))
                             (range images-count))
        j-deltas           (map
                             (fn [iter]
                               (*
                                 (* 0.9
                                    (- 1.0
                                       (/ iter images-count)))
                                 (* (Math/sin (* iter frame-domain-delta)))))
                             (range images-count))
        w-deltas           (repeat images-count 0.0)
        h-deltas           (repeat images-count 0.0)
        domain-range       (offsets-map i-deltas j-deltas w-deltas h-deltas)]
    domain-range))
