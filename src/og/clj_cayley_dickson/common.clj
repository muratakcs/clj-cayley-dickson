(ns og.clj-cayley-dickson.common)


(defn cart [colls]
  "Cartesian (tensor) product of collections."
  (if (empty? colls)
    '(())
    (for [x    (first colls)
          more (cart (rest colls))]
      (cons x more))))