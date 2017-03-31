(ns carbon-snake.apple
  (:require [carbon.rx :as rx :include-macros true]
            [carbon-snake.cell :as cell]
            ))

(defn initial-state [] nil)

(defn apple [position]
  (when-let [[x y] @position]
    [cell/cell {:x x :y y :fill "#aa3939"}]))
