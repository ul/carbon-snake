(ns carbon-snake.core
  (:require [carbon.rx :as rx :include-macros true]
            [carbon.vdom :as vdom]
            [carbon-snake.apple :as apple]
            [carbon-snake.cell :as cell]
            [carbon-snake.grid :as grid]
            [carbon-snake.snake :as snake]))

(enable-console-print!)

(def LEFT 37)
(def UP 38)
(def RIGHT 39)
(def DOWN 40)

(defn initial-state []
  {:apple (apple/initial-state)
   :cell {:size 50}
   :grid (grid/initial-state)
   :raf nil
   :snake (snake/initial-state)})

(defonce state (rx/cell (initial-state)))

(def snake-speed (rx/cursor state [:snake :speed]))
(def apple-position (rx/cursor state [:apple :position]))
(def cell-size (rx/cursor state [:cell :size]))
(def grid-size (rx/cursor state [:grid :size]))
(def raf (rx/cursor state [:raf]))
(def last-update (rx/cursor state [:snake :last-update]))
(def snake-position (rx/cursor state [:snake :position]))
(def snake-set (rx/rx (set @snake-position)))
(def bitten? (rx/rx false (snake/bitten? @snake-position)))
(def svg-size (rx/rx (str (* @grid-size @cell-size) "px")))

(defn spawn-apple []
  (let [s @snake-set]
    (loop [p (grid/random-position @grid-size)]
      (if (contains? s p)
        (recur (grid/random-position @grid-size))
        (reset! apple-position p)))))

(defn stop []
  (js/cancelAnimationFrame @raf))

(declare update-snake-position)
(defn next-move []
  (reset! last-update (system-time))
  (reset! raf (js/requestAnimationFrame update-snake-position)))

(defn start []
  (stop)
  (when-not @apple-position
    (spawn-apple))
  (next-move))

(defn reset []
  (stop)
  (reset! state (initial-state)))

(defn grow-snake []
  (swap! state update :snake snake/grow-snake @grid-size))

(defn eat-apple []
  (rx/dosync
   (grow-snake)
   (spawn-apple)))

(defn update-snake-position []
  (swap! state update :snake snake/move @grid-size)
  (when (contains? @snake-set @apple-position)
    (eat-apple))
  (if @bitten?
    (stop)
    (next-move)))

(defn update-direction [direction keyCode]
  (condp = keyCode
    LEFT (snake/direction-logic direction [-1 0])
    UP (snake/direction-logic direction [0 -1])
    RIGHT (snake/direction-logic direction [1 0])
    DOWN (snake/direction-logic direction [0 1])
    direction))

(defn set-direction [e]
  (when (#{LEFT UP RIGHT DOWN} (.-keyCode e))
    (.preventDefault e)
    (swap! state update-in [:snake :direction]
           update-direction (.-keyCode e))))

(defn app []
  [:div.flex.flex-1.column
   {:on-key-down set-direction}
   [:svg
    {:xmlns "http://www.w3.org/2000/svg"
     :style {:width @svg-size
             :height @svg-size}}
    [:g
     {:transform (str "scale(" @cell-size ")")}
     [grid/grid grid-size bitten?]
     [apple/apple apple-position]
     [snake/snake snake-position]]]
   [:div.tools
    [:button {:on-click start} "Start"]
    [:button {:on-click reset} "Reset"]
    [:button {:on-click stop} "Stop"]
    [snake/length snake-position]]])

(vdom/mount [app] (js/document.getElementById "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
