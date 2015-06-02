(ns checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [checkers.board :refer [board board-events]]))

(enable-console-print!)

(defn board-click [board-pos]
  (put! board-events {:event :board-clicked
                      :position board-pos}))

(defn draw-piece [piece-type]
  (apply dom/div #js {:className piece-type} nil))

(defn draw-tuple [piece row-odd?]
  (let [piece-type (name (last piece))
        piece-pos (first piece)
        white-square (dom/td #js {:className "white"})
        green-square (dom/td #js {:className "green"
                                  :onClick
                                    (fn [e] (board-click
                                             piece-pos))}
                                 (draw-piece piece-type))]
    (if row-odd?
      [white-square green-square]
      [green-square white-square])))

(defn draw-row [row]
  (let [curr-row (/ (first (last row)) 4)
        row-odd? (odd? curr-row)]
    (apply dom/tr nil
      (mapcat #(draw-tuple % row-odd?)
           row))))

(defn checkerboard [board owner]
  (om/component
   (apply dom/table nil
      (map draw-row
           (partition 4 board)))))

(defn bootstrap-ui []
  (om/root
    checkerboard ; our UI
    board        ; our game state
    {:target (. js/document (getElementById "checkers"))}))

(bootstrap-ui)

