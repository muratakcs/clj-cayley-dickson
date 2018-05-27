(ns og.clj-cayley-dickson.visualize
  (:require
    [clojure.java.shell :refer [sh]]
    [me.raynes.fs :as fs]
    [og.clj-cayley-dickson.graphics.image-util :as img-util]
    [og.clj-cayley-dickson.graphics.fractal :as frac]
    [og.clj-cayley-dickson.graphics.generative-genetic :as gg]
    [mikera.image.core :as imgz]))

(defn- animate-results [glob output]
  (println "Creating animated gif from files, to file: " glob " -> " output)
  (try
    (sh "convert" "-delay" "15" "-loop" "0" glob output)
    (catch Throwable t
      (println "Could not create animated gif:" t))))

(defn- draw-images-to-files
  [{:keys [domain-range impl-algo impl-draw x0 y0 w0 h0 iters width height outdir]}]
  (let [count* (atom 0)]
    (doseq [{:keys [i-delt j-delt w-delt h-delt]
             :or   {i-delt 0.0 j-delt 0.0 w-delt 0.0 h-delt 0.0}} domain-range]
      (println "\n-- iter: "
               @count* "of " (count domain-range)
               "with i, j deltas: " i-delt j-delt " --")
      (let [x (+ x0
                 i-delt)
            y (+ y0
                 j-delt)
            w (+ w0
                 w-delt)
            h (+ h0
                 h-delt)]
        (frac/draw-w-io x y w h iters width height [impl-algo impl-draw] outdir)
        (swap! count* inc)
        ;(time (draw x y w h 64 (* (inc (+ j i)) 300) (* (inc (+ j i)) 200) [impl :draw-raster]))
        outdir))))

(defn- draw [w h x y iters width height]
  (println "draw: " w h x y iters width height)
  (frac/draw
    w h x y iters (max 2 width) (max 2 height)
    [:og-plain-quat :draw-lines]))

(defn- rand-size [max]
  (gg/if-not-pos-then-default
    (* max (rand))
    (float (/ max 2)))
  )
(defn generate-img-compose-seq
  "Generated a seq of:
   [x-offset,y-offset, w, h, x, y] where
   the offsets are randomly generated;
   This is used to generate a composition
    of images later."
  [{:keys [candidate-pop canvas-width canvas-height seq-size]}]
  (let [imgs           (take seq-size (shuffle candidate-pop))
        imgs-w-offsets (map
                         (fn [[w h x y]]
                           [[(* 0.9 (rand-size canvas-width))
                             (* 0.9 (rand-size canvas-height))]
                            [w h x y]
                            [(* 0.2 (rand-size canvas-width))
                             (* 0.2 (rand-size canvas-height))]])
                         imgs)]
    imgs-w-offsets))


(defn img-compose-seq->composed-img [{:keys [imgs canvas-width canvas-height]}]
  "Takes a seq of [off-x, off-y, w, h, x, y] and renders
  to one image."
  (loop [imgs-left    imgs
         composed-img nil]
    (if (pos? (count imgs-left))
      (let [[[xoffset yoffset]
             [w h x y]
             [img-w img-h]] (first imgs-left)
            recomposed (img-util/combine
                         {:xoffset       xoffset
                          :yoffset       yoffset
                          :canvas-width  canvas-width
                          :canvas-height canvas-height
                          :img1          composed-img
                          :img2          (draw
                                           w h x y 64
                                           img-w img-h)})]
        (println "w h x y iw ih" w h x y img-w img-h)
        (recur (rest imgs-left) recomposed))
      composed-img)))

(defn mandelbrot-experiment [exp-name impl-algo impl-draw fn-domain-range iters]
  (if-not (fs/exists? "fractals")
    (fs/mkdir "fractals"))
  (let [periods      4
        images-count 128
        domain-range (fn-domain-range
                       periods
                       images-count)
        x            -1.2
        y            -0.6
        w            2.8
        h            2.3
        outdir       (str "fractals/fractals-" (name impl-algo) "-" (name impl-draw) "-" exp-name "-" (System/currentTimeMillis))]
    (draw-images-to-files
      {:domain-range domain-range
       :impl-algo    impl-algo
       :impl-draw    impl-draw
       :x0           x
       :y0           y
       :w0           w
       :h0           h
       :iters        iters
       :width        60
       :height       40
       :outdir       outdir})

    (animate-results
      (str outdir "/*")
      (str "fractal-gifs/movie-" exp-name "-"
           (System/currentTimeMillis) ".gif"))))



(defn test-generate-seq-and-compose-it [& {:keys [canvas-width canvas-height pop-size seq-size]
                                           :or   {canvas-width  100
                                                  canvas-height 100
                                                  pop-size      100
                                                  seq-size      50}}]
  (imgz/show
    (let [rand-pop (repeatedly pop-size gg/generate-random-view)
          img-seq  (generate-img-compose-seq
                     {:candidate-pop rand-pop
                      :canvas-width  canvas-width
                      :canvas-height canvas-height
                      :seq-size      seq-size})]
      (println "img seq: " (vec img-seq))
      (img-compose-seq->composed-img
        {:imgs          img-seq
         :canvas-width  canvas-width
         :canvas-height canvas-height}))))


;(println "1. apache time: (reference)")
;(time (mandelbrot-01 :apache))
;(println "2. og apache time: (10x slower than 1.)")
;(time (mandelbrot-01 :og-apache))
;(println "3. og plain time: (2x slower than 1.) w imagez (0-2 seconds slower than 4.)")
;(mandelbrot-experiment "periodic-pan-imgz"
;                       :og-plain-quat
;                       :draw-imagezlib
;                       f/xy-offsets-periodic)
(println "4. og plain time: (2x slower than 1.)")
(mandelbrot-experiment "periodic-pan-64"
                       :og-plain-quat
                       :draw-lines
                       frac/xy-offsets-periodic
                       64)

(test-generate-seq-and-compose-it)