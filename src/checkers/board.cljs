(ns checkers.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [cljs.core.match :refer-macros [match]]))

(enable-console-print!)

(def board-events (chan))

(def board-commands (chan))

; == Board State ==========================================
; initialize a board, where positions are indexed 1-32.
; each position is an atom containing the symbol of the
; piece in it.
(defn create-board []
  (atom
   (apply sorted-map
          (flatten
           (map-indexed (fn [i v] (vector (inc i) v))
                        (flatten
                         [(repeat 12 :red-piece)
                          (repeat 8 :empty-piece)
                          (repeat 12 :black-piece)]))))))

(def board (create-board))

(defn piece-to-player [piece]
  (case piece
    (:red-piece :prom-red-piece) :red
    (:black-piece :prom-black-piece) :black
    nil))

(def current-move (atom :red))
(defn toggle-current-move []
  (if-not (compare-and-set! current-move :red :black)
          (compare-and-set! current-move :black :red)))

(def move-start (atom [nil nil]))
(defn move-start-components [] (let [v (deref move-start)] v))

(defn send-update-position [position piece]
  (put! board-commands
        {:command :update-board-position
         :position position
         :piece piece}))

(def num-rows 8)
(def positions-per-row (/ (count (deref board)) num-rows))

(defn position-to-row [position]
  (+ 1 (quot (- position 1) positions-per-row)))

(defn is-jump [from-row to-row] (= 2 (Math/abs (- from-row to-row))))
(defn is-slide [from-row to-row] (= 1 (Math/abs (- from-row to-row))))

(defn row-pos-to-abs-pos [row row-pos]
  (+ 1 (+ (- row-pos 1) (* (- row 1) positions-per-row))))

(defn in-range [upper]
  (fn [n] (and (> n 0) (< n (+ upper 1)))))

(defn valid-slides [position]
  (let [row (position-to-row position)
        pos-in-row (+ 1 (mod (- position 1) 4))
        is-even (= 0 (mod row 2))
        slides [pos-in-row (+ pos-in-row 1)]
        adjusted (if is-even (map dec slides) slides)
        filtered (filter (in-range 4) adjusted)
        rows (filter (in-range 8) [(- row 1) (+ row 1)])
        res (map (fn [r] (map #(row-pos-to-abs-pos r %) filtered)) rows)]
   (vec (flatten res))))

(defn valid-slides-up [pos]
  (filter #(< % pos) (valid-slides pos)))

(defn valid-slides-down [pos]
  (filter #(> % pos) (valid-slides pos)))

(defn valid-jumps [pos]
  (let [row (position-to-row pos)
        pos-in-row (+ 1 (mod (- pos 1) 4))
        jumps [(+ 1 pos-in-row) (- pos-in-row 1)]
        filtered (filter (in-range 4) jumps)
        rows (filter (in-range 8) [(- row 2) (+ row 2)])
        res (map (fn [r] (map #(row-pos-to-abs-pos r %) filtered)) rows)]
    (vec (flatten res))))

(defn valid-jumps-up [pos]
  (filter #(< % pos) (valid-jumps pos)))

(defn valid-jumps-down [pos]
  (filter #(> % pos) (valid-jumps pos)))

(defn do-jump [from to]
  (let [mi (Math/min from to)
        ma (Math/max from to)
        piece (get (deref board) from)
        row (position-to-row from)
        is-even (= 0 (mod row 2))
        mid (int ((if is-even Math/floor Math/ceil) (+ mi (/ (- ma mi) 2))))
        mid-piece (get (deref board) mid)]
    (if (and (not (= mid-piece piece)) (not (= mid-piece :empty-piece)))
      (do
        (send-update-position mid :empty-piece)
        (do-slide from to)) false)))

(defn do-slide [from to]
  (do
    (send-update-position to (get (deref board) from))
    (send-update-position from :empty-piece))
    true)

(defn move [from to]
  (let [piece (get (deref board) from)
        player (piece-to-player piece)
        from-row (position-to-row from)
        to-row (position-to-row to)
        is-down (< from-row to-row)
        is-jump (= 2 (Math/abs (- from-row to-row)))]
    (case [player is-down is-jump]
      [:red true true] (if (some #{to} (valid-jumps-down from)) (do-jump from to) false)
      [:red true false] (if (some #{to} (valid-slides-down from)) (do-slide from to) false)
      [:black false true] (if (some #{to} (valid-jumps-up from)) (do-jump from to) false)
      [:black false false] (if (some #{to} (valid-slides-up from)) (do-slide from to) false)
      false)))

(defn handle-click [position]
  (let [extant-piece (get (deref board) position)
        [start-player start-position] (move-start-components)
        starting-move
          (match [start-player start-position extant-piece]
            [nil nil _]
              (do (if (= (deref current-move) (piece-to-player extant-piece))
                    (reset! move-start [(piece-to-player extant-piece) position])) true)
            [_ _ :empty-piece] (do (if (move start-position position) (toggle-current-move)) false)
            :else false)]
    (if-not starting-move (reset! move-start [nil nil]))))

(go (while true
      (let [event (<! board-events)]
        (case (get event :event)
          :board-clicked (handle-click (get event :position))))))

(go (while true
      (let [command (<! board-commands)]
        (swap! board assoc (:position command)
                           (:piece command)))))

