(ns carbon-snake.cell)

(defn cell [props]
  [:rect
   (merge
    {:fill "#ffffff"
     :width 1
     :height 1
     :stroke "#ffffff"
     :stroke-width 0.01}
    props)])
