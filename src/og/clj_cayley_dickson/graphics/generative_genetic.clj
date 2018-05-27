(ns og.clj-cayley-dickson.graphics.generative-genetic
  (:require
    [og.clj-cayley-dickson.graphics.image-util :as img-util]
    [og.clj-cayley-dickson.graphics.fractal :as frac]))


(defn draw [w h x y iters width height]
  #_(println "draw: " w h x y iters width height)
  (frac/draw
    w h x y iters (max 2 width) (max 2 height)
    [:og-plain-quat :draw-lines]))

(defn if-not-pos-then-default [v default]
  (if (or (zero? v) (neg? v))
    default
    v))

(defn- random-around-zero-w-radius [radius]
  "Random float in [-radius/2,radius/2]"
  (* radius
     (- (rand)
        0.5)))

(defn generate-random-view []
  "A random-ish [w h x y] 'view'
  by sampling values likely to produce visually
  appealing results"
  [(+
     (rand-nth [0.6 0.62 0.64 0.66])
     (if-not-pos-then-default (random-around-zero-w-radius 0.005) 0.0))
   (+
     (rand-nth [0.0 0.005 0.01])
     (if-not-pos-then-default (random-around-zero-w-radius 0.002) 0.0))
   (+
     (rand-nth [0.005 0.01 0.05 0.1])
     (random-around-zero-w-radius 0.001))
   (+
     (rand-nth [0.005 0.01 0.05 0.1])
     (random-around-zero-w-radius 0.002))])

(defn- init-pop [count]
  "Initial population using random sampling"
  (take
    count
    (repeatedly generate-random-view)))


(defn- compare-img-dist-fn-unmemo [fractal-objective w h x y dim-w dim-h]
  "Computes distance between objective
  image and generated image"
  (img-util/imgs->distance
    fractal-objective
    (draw
      x y w h 64
      dim-w dim-h)
    false))

(def compare-img-dist-fn
  "Memoized version of above"
  (memoize compare-img-dist-fn-unmemo))

(defn- pop-w-distances [pop fractal-objective]
  "compute and conj distances between images"
  (pmap
    (fn [[w h x y]]
      [(compare-img-dist-fn fractal-objective w h x y 20 20)
       w
       h
       x
       y])
    pop))

(defn- sorted-pop-by-fitness [pop fractal-objective]
  "Sort population by its calculated fitness"
  (let [w-distances    (sort-by
                         first
                         (pop-w-distances pop fractal-objective))
        sorted         (map
                         rest
                         w-distances)
        total-distance (reduce + (map first w-distances))]
    (println "  pop sum distances : "
             total-distance
             "\n  top 5: "
             (take 5 (map first w-distances)))
    ;(clojure.pprint/pprint w-distances)
    [total-distance sorted]))

(defn- selection-linear [pop total-fitness]
  "TODO wip select new pop linearly weights"
  (let [pop-count     (count pop)
        pop-w-weights (map
                        (fn [dist w h x y]
                          [(int
                             (*
                               pop-count
                               (/ dist total-fitness)))
                           w h x y])
                        pop)]))

(defn- mutate [w h x y]
  "Mutate a view vector by some small (or large)
  amount in one component"
  (let [which-one    (rand-nth [0 1 2 3])
        sign         (rand-nth [1 -1])
        how-much     (rand-nth [0.01 0.05 0.1 0.2])
        transform-fn (fn [v] (+ v (* sign how-much v)))]
    (case which-one
      0 [(transform-fn w) h x y]
      1 [w (transform-fn h) x y]
      2 [w h (transform-fn x) y]
      3 [w h x (transform-fn y)])))

(defn- hybrid [w1 h1 x1 y1 w2 h2 x2 y2]
  "Hybrid of two view components"
  (let [which-one (rand-nth [0 1 2 3 4 5 6 7 8 9])]
    (case which-one
      0 [w2 h2 x1 y1]
      1 [w1 h2 x2 y2]
      2 [w1 h1 x2 y2]
      3 [w1 h1 x1 y2]
      4 [w2 h1 x2 y2]
      5 [w1 h2 x2 y1]
      6 [w2 h1 x1 y2]
      7 [w1 h2 x1 y2]
      8 [w2 h1 x2 y1]
      9 [w1 h1 x2 y1])))

(defn- hybrid-many [base-pop repro-pop take-count]
  "Convenience to hybridize between populations"
  (->>
    base-pop
    (map
      (fn [[w h x y]]
        (let [[w2 h2 x2 y2] (rand-nth repro-pop)]
          (hybrid w h x y w2 h2 x2 y2))))
    (take
      take-count)))

(defn- sorted-old-pop->new-pop [sorted-pop]
  "Heuristic makeup of new pop from the old"
  (let [how-many            (count sorted-pop)
        one-third           (int (/ how-many 3))

        top-third-pop       (take one-third sorted-pop)

        ; top 1/6th hybridized w random other top 1/3rd
        first-sixth-mingled (hybrid-many
                              top-third-pop
                              top-third-pop
                              (int (/ one-third 2)))
        ; random top 1/3rd
        second-sixth        (->>
                              (shuffle top-third-pop)
                              (take
                                (int (/ one-third 2))))

        ; random person in pop hybridized with top 1/3rd
        third-sixth         (hybrid-many
                              (shuffle sorted-pop)
                              top-third-pop
                              (int (/ one-third 2)))
        ; random hybrid w random
        fourth-sixth        (hybrid-many
                              (shuffle sorted-pop)
                              sorted-pop
                              (int (/ one-third 2)))
        ; random from pop
        bottom-third        (->>
                              (shuffle sorted-pop)
                              (take
                                one-third))
        new-pop             (concat first-sixth-mingled
                                    second-sixth
                                    third-sixth
                                    fourth-sixth
                                    bottom-third)
        new-pop-mutated     (map
                              (fn [[w h x y]]
                                (if (< 0.9 (rand))
                                  (mutate w h x y)
                                  [w h x y]))
                              new-pop)]
    new-pop-mutated))

(defn- rel-err [a b]
  (/
    (Math/abs
      (- a b))
    a))

(defn- distances-converged? [distances]
  "Converged based on first-order delta (etc) between distances"
  (let [c? (or
             (and (< 10 (count distances))
                  (let [last1      (last distances)
                        last-third (nth distances
                                        (* 2
                                           (int
                                             (/
                                               (count distances)
                                               3.0))))
                        is-stuck?  (or (< last-third last1)
                                       (> 0.01 (rel-err last-third last1)))]
                    (when is-stuck? (println "\n-- Stuck! --\n"))
                    is-stuck?))
             (and
               (< 2 (count distances))
               (let [last1    (last distances)
                     last2    (second (reverse distances))
                     rel-conv (rel-err last1 last2)]
                 (println "Convergence check, rel error: " rel-conv)
                 (> 0.000005 rel-conv))))]
    (when c? (println "Converged!"))
    c?))


(defn- rand-size [max]
  (if-not-pos-then-default
    (* max (rand))
    (float (/ max 2))))
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


(defn img-compose-seq->composed-img
  [{:keys [imgs canvas-width canvas-height]}]
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


(defn iterate-pop [fractal-objective init-size iters]
  "Main function to iterate through population using GA"
  (loop [the-iters   iters
         the-pop     (init-pop init-size)
         total-dists []]
    (println "iter: " (- iters the-iters) (count the-pop))
    (if (and (pos? the-iters)
             (not (distances-converged? total-dists)))
      (let [[total-distance sorted-old-pop]
            (time
              (sorted-pop-by-fitness the-pop fractal-objective))]
        (recur
          (dec the-iters)
          (sorted-old-pop->new-pop
            sorted-old-pop)
          (conj total-dists total-distance)))
      {:total-distances-ts total-dists
       :final-pop          the-pop})))
