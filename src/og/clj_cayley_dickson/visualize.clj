(ns og.clj-cayley-dickson.visualize
  (:require [og.clj-cayley-dickson.graphics.fractal :as f]
            [clojure.java.shell :refer [sh]]))

(defn animate-results [glob output]
  (println "Creating animated gif from files, to file: " glob " -> " output)
  (try
    (sh "convert" "-delay" "15" "-loop" "0" glob output)
    (catch Throwable t
      (println "Could not create animated gif:" t))))

(defn draw-images-to-files
  [{:keys [domain-range impl x0 y0 w0 h0 iters width height outdir]}]
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
                 h-delt
                 )]
        (println "Drawing to: " outdir)
        (time (f/draw x y w h iters width height [impl :draw-lines] outdir))
        (swap! count* inc)
        ;(time (draw x y w h 64 (* (inc (+ j i)) 300) (* (inc (+ j i)) 200) [impl :draw-raster]))
        outdir))))

(defn mandelbrot-experiment [exp-name impl fn-domain-range]
  (let [periods      1.5
        images-count 48
        domain-range (fn-domain-range
                       periods
                       images-count)
        x            -1.2
        y            -0.6
        w            2.8
        h            2.3
        outdir       (str "fractals-" exp-name "-" (System/currentTimeMillis))]
    (draw-images-to-files
      {:domain-range domain-range
       :impl         impl
       :x0           x
       :y0           y
       :w0           w
       :h0           h
       :iters        64
       :width        600
       :height       400
       :outdir       outdir})

    (animate-results
      (str outdir "/*")
      (str "fractal-gifs/movie-" exp-name "-"
           (System/currentTimeMillis) ".gif"))))


;(println "1. apache time: (reference)")
;(time (mandelbrot-01 :apache))
;(println "2. og apache time: (10x slower than 1.)")
;(time (mandelbrot-01 :og-apache))
(println "3. og plain time: (2x slower than 1.)")
(mandelbrot-experiment "periodic-pan" :og-plain-quat f/xy-offsets-periodic)
(mandelbrot-experiment "xy-pan" :og-plain-quat f/xy-offsets-pan)


