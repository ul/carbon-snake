(ns carbon-snake.snake
  (:require [carbon.rx :as rx :include-macros true]
            [carbon-snake.cell :as cell]
            [carbon-snake.grid :as grid]
            ))

(def LEFT 37)
(def UP 38)
(def RIGHT 39)
(def DOWN 40)

(def initial-state
  {:position '([0 0])
   :direction [1 0]
   :cell-done 0
   :last-update nil
   :speed 10})

(defn bitten? [position]
  (< (count (set position))
     (count position)))

(defn grow-snake [{:keys [direction] [head] :position :as snake}]
  (update snake :position conj (mapv + head direction)))

(defn direction-logic [[x y :as d] [x1 y1]]
  (if (or (and (zero? x) (zero? y1))
          (and (zero? y) (zero? x1)))
    [x1 y1]
    d))

(defn update-direction [direction keyCode]
  (condp = keyCode
    LEFT (direction-logic direction [-1 0])
    UP (direction-logic direction [0 -1])
    RIGHT (direction-logic direction [1 0])
    DOWN (direction-logic direction [0 1])
    direction))

(defn macro-move [{:keys [position direction] :as snake} grid-size]
  (assoc snake :position
         (cons (grid/wrap-move grid-size (first position) direction) (butlast position))))

(defn micro-move [{:keys [cell-done speed last-update] :as state}]
  (update state :cell-done + (* speed (- (system-time) last-update))))

(defn move [snake grid-size]
  (as-> snake snake
        (micro-move snake)
        (if (<= 1000 (get snake :cell-done))
          (-> snake
              (update :cell-done rem 1000)
              (macro-move grid-size))
          snake)))

(defn snake [position]
  [:g
   (for [[i [x y]] (map-indexed vector @position)]
     ^{:key i}
     [cell/cell {:x x :y y :fill "#ffd700"}])])

(defn length [position]
  [:div "Length: " (count @position)])
