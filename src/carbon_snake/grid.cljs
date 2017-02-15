(ns carbon-snake.grid)

(def initial-state
  {:size 15})

(defn random-position [size]
  [(rand-int size) (rand-int size)])

(defn wrap-move* [size x1 x2]
  (let [x (+ x1 x2)]
    (cond
      (< x 0) (dec size)
      (>= x size) 0
      :else x)))

(defn wrap-move [size x1 x2]
  (mapv (partial wrap-move* size) x1 x2))

(defn background [size biten?]
  [:rect
   {:x 0
    :y 0
    :width @size
    :height @size
    :fill (if @biten? "#aa3939" "#00ced1")
    :stroke "#ffffff"
    :stroke-width 0.01}])

(defn grid [size bitten?]
  [:g
   [background size bitten?]
   (for [i (range @size)]
     ^{:key i}
     [:line {:x1 0 :y1 i :x2 @size :y2 i :stroke "white" :stroke-width 0.01}])
   (for [i (range @size)]
     ^{:key i}
     [:line {:x1 i :y1 0 :x2 i :y2 @size :stroke "white" :stroke-width 0.01}])])
