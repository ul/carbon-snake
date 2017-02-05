(ns carbon-snake.core
  (:require [carbon.rx :as rx :include-macros true]
            [carbon.vdom :as vdom]))

(enable-console-print!)

(def LEFT 37)
(def UP 38)
(def RIGHT 39)
(def DOWN 40)

(def initial-state
  {:snake {:position (mapv #(vector % 0) (reverse (range 1)))
           :velocity (vec (repeat 1 [1 0]))}
   :cell {:size 5}
   :grid {:size 150}
   :speed 3
   :apple nil})

(defonce state (rx/cell initial-state))

(def cell-size (rx/cursor state [:cell :size]))
(def grid-size (rx/cursor state [:grid :size]))
(def snake-position (rx/cursor state [:snake :position]))
(def snake-set (rx/rx (set @snake-position)))
(def snake-velocity (rx/cursor state [:snake :velocity]))
(def apple-position (rx/cursor state [:apple]))
(def move-timeout (rx/rx (/ 1000 (get @state :speed))))

(def snake-self-bite
  (rx/rx
   (< (count @snake-set)
      (count @snake-position))))

(declare stop)
(add-watch snake-self-bite ::fail
           (fn [_ _ _ biten]
             (when biten (stop))))

(declare spawn-apple wrap-move next-move)

(defn eat-apple []
  (rx/dosync
   (swap! snake-position (fn [p] (into [(mapv wrap-move (get p 0) (get @snake-velocity 0))] p)))
   (swap! snake-velocity (fn [v] (into [(get v 0)] v)))
   (spawn-apple)))

(defn random-position []
  [(rand-int @grid-size) (rand-int @grid-size)])

(defn spawn-apple []
  (let [s @snake-set]
    (loop [p (random-position)]
      (if (contains? s p)
        (recur (random-position))
        (reset! apple-position p)))))

(defn wrap-move [x1 x2]
  (let [x (+ x1 x2)]
    (cond
      (< x 0) (dec @grid-size)
      (>= x @grid-size) 0
      :else x)))

(defn dec* [x]
  (if (> x 1)
    (dec x)
    x))

(defn velocity-logic [[x y] [x1 y1]]
  #_[(+ x x1) (+ y y1)]
  (if (or (and (zero? x) (zero? y1))
          (and (zero? y) (zero? x1)))
    [x1 y1]
    (do
      (if (or (and (< x 0) (< x1 0))
              (and (> x 0) (> x1 0))
              (and (< y 0) (< y1 0))
              (and (> y 0) (> y1 0)))
        (swap! state update :speed inc)
        (swap! state update :speed dec*))
      [x y])))

(defn update-snake-position []
  (rx/dosync
   (swap! snake-position
          (fn [p]
            (mapv #(mapv wrap-move %1 %2) p @snake-velocity)))
   (swap! snake-velocity (fn [v] (into (subvec v 0 1) (butlast v))))
   (when (contains? @snake-set @apple-position)
     (eat-apple))
   (next-move)))

(defn update-snake-velocity [e]
  (let [keyCode (.-keyCode e)]
    (when (#{LEFT UP RIGHT DOWN} keyCode)
      (.preventDefault e))
    (condp = keyCode
      LEFT (swap! snake-velocity update 0 velocity-logic [-1 0])
      UP (swap! snake-velocity update 0 velocity-logic [0 -1])
      RIGHT (swap! snake-velocity update 0 velocity-logic [1 0])
      DOWN (swap! snake-velocity update 0 velocity-logic [0 1])
      nil)))

(defn stop []
  (js/clearTimeout (get @state :move-timer)))

(defn next-move []
  (swap! state assoc :move-timer (js/setTimeout update-snake-position @move-timeout)))

(defn start []
  (stop)
  (spawn-apple)
  (next-move))

(defn reset []
  (stop)
  (reset! state initial-state))

(defn cell [props]
  [:rect
   (merge
    {:fill "#ffffff"
     :width 1
     :height 1
     :stroke "#ffffff"
     :stroke-width 0.01}
    props)])

(defn grid [n]
  [:g
   {:transform (str "translate(" 0.01 " " 0.01 ")")}
   [:rect
    {:x -0.01
     :y -0.01
     :width (+ n 0.01)
     :height (+ n 0.01)
     :stroke "#ffffff"
     :stroke-width 0.01
     }]
   (let [biten @snake-self-bite]
     (for [i (range n)]
       (for [j (range n)]
         ^{:key (str i "#" j)}
         [cell {:x i :y j :fill (if biten "#aa3939" "#00ced1")}])))])

(defn snake []
  [:g
   {:transform (str "translate(" 0.01 " " 0.01 ")")}
   (for [[x y] @snake-position]
     [cell {:x x :y y :fill "#ffd700"}])])

(defn apple []
  (when-let [[x y] @apple-position]
    [cell {:x x :y y :fill "#aa3939"}]))

(defn app []
  [:div.flex.flex-1.column
   {:on-key-down update-snake-velocity}
   [:svg
    [:g
     {:transform (str "scale(" @cell-size ")")}
     [grid @grid-size]
     [apple]
     [snake]]]
   [:div.flex
    [:button {:on-click start} "Start"]
    [:button {:on-click reset} "Reset"]
    [:button {:on-click stop} "Stop"]
    [:div.flex-1]
    [:div "Length: " (count @snake-position)]
    [:div.flex-1]]
   [:div.flex-1]])

(vdom/mount [app] (js/document.getElementById "app"))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
