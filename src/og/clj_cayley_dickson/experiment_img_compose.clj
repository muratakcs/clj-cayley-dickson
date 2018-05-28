(ns og.clj-cayley-dickson.experiment-img-compose
  (:require [og.clj-cayley-dickson.graphics.generative-genetic :as gg]))

(time
  (gg/generate-random-seq-of-fractals-and-compose-it))