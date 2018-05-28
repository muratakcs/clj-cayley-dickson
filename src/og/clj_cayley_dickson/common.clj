(ns og.clj-cayley-dickson.common)


(defn cart [colls]
  "Cartesian (tensor) product of collections."
  (if (empty? colls)
    '(())
    (for [x    (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn if-not-pos-then-default [v default]
  (if (or (zero? v) (neg? v))
    default
    v))


(defn random-around-zero-w-radius [radius]
  "Random float in [-radius/2,radius/2]"
  (* radius
     (- (rand)
        0.5)))
(defn rel-err [a b]
  (/
    (Math/abs
      (- a b))
    a))
