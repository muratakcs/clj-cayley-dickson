(ns og.clj-cayley-dickson.visualize
  (:require
    [clojure.java.shell :refer [sh]]
    [me.raynes.fs :as fs]
    [og.clj-cayley-dickson.graphics.image-util :as img-util]
    [og.clj-cayley-dickson.graphics.fractal :as frac]
    [og.clj-cayley-dickson.graphics.generative-genetic :as gg]
    [mikera.image.core :as imgz]))

(defn- animate-results [glob output]
  "Uses glob to aggregate image files into animated gif."
  (println "Creating animated gif from files, to file: " glob " -> " output)
  (try
    (sh "convert" "-delay" "15" "-loop" "0" glob output)
    (catch Throwable t
      (println "Could not create animated gif:" t))))

(defn- draw-images-to-files
  "Creates and draws fractals."
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
        (frac/draw-fractal-w-io x y w h iters width height [impl-algo impl-draw] outdir)
        (swap! count* inc)
        ;(time (draw x y w h 64 (* (inc (+ j i)) 300) (* (inc (+ j i)) 200) [impl :draw-raster]))
        outdir))))



(defn mandelbrot-experiment [exp-name impl-algo impl-draw fn-domain-range iters]
  "Creates a mandelbrot fractal movie gif."
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
        outdir       (str "fractals/fractals-"
                          (name impl-algo) "-"
                          (name impl-draw) "-"
                          exp-name "-"
                          (System/currentTimeMillis))]
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


(defn- test-generate-seq-and-compose-it
  "Test generate random img specs and compose them into one image."
  [& {:keys [canvas-width canvas-height pop-size seq-size]
      :or   {canvas-width  100
             canvas-height 100
             pop-size      100
             seq-size      50}}]
  (imgz/show
    (let [rand-pop (repeatedly pop-size gg/generate-random-view)
          img-seq  (gg/generate-img-compose-seq
                     {:candidate-pop rand-pop
                      :canvas-width  canvas-width
                      :canvas-height canvas-height
                      :seq-size      seq-size})]
      (println "img seq: " (vec img-seq))
      (gg/img-compose-seq->composed-img
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
(when false
  (println "4. og plain time: (2x slower than 1.)")
  (mandelbrot-experiment "periodic-pan-64"
                         :og-plain-quat
                         :draw-lines
                         frac/xy-offsets-periodic
                         64)

  (test-generate-seq-and-compose-it))


(def fractal-objective
  "The BufferedImage we wish to reproduce"
  (gg/draw
    0.655 0.05 0.01 0.05 64 100 100))

(def iter-results (gg/iterate-pop fractal-objective 5000 10))

(clojure.pprint/pprint (:total-distances-ts iter-results))

(imgz/show
  (gg/img-compose-seq->composed-img
    {:imgs          [[[0 0] (first (:final-pop iter-results)) [100 100]]]
     :canvas-height 100
     :canvas-width  100}))

#_(imgz/show
  fractal-objective)