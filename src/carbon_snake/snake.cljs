(ns carbon-snake.snake
  (:require [carbon-snake.cell :refer [cell]]
            [carbon-snake.grid :as grid]))

(defn initial-state []
  {:position '([0 0])
   :direction [1 0]
   :speed 10
   :cell-done 0})

(defn length [position]
  [:span "Length: " (count @position)])

(defn snake [position]
  [:g
   (for [[i [x y]] (map-indexed vector @position)]
     ^{:key i}
     [cell {:x x :y y :fill "#ffd700"}])])

(defn macro-move [{:keys [position direction] :as snake} grid-size]
  (->> position
       butlast
       (cons (grid/wrap-move grid-size (first position) direction))
       (assoc snake :position)))

(defn micro-move [{:keys [speed last-update] :as state}]
  (->> last-update
       (- (system-time))
       (* speed)
       (update state :cell-done +)))

(defn move [snake grid-size]
  (let [snake (micro-move snake)]
    (if (<= 1000 (get snake :cell-done))
      (-> snake
          (update :cell-done rem 1000)
          (macro-move grid-size))
      snake)))

(defn bitten? [position]
  (< (count (set position))
     (count position)))

(defn grow-snake [{:keys [direction] [head] :position :as snake} grid-size]
  (update snake :position conj (grid/wrap-move grid-size head direction)))

(defn direction-logic [[x y :as d] [x1 y1]]
  (if (or (and (zero? x) (zero? y1))
          (and (zero? y) (zero? x1)))
    [x1 y1]
    d))
