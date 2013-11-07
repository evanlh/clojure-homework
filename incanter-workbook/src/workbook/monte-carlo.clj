(ns workbook.monte-carlo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monte Carlo simulation

;; load an image, use Monte Carlo sampling to add pixels to the render
;; surface until the the original image becomes sufficiently clear.

;; Quil is a wrapper for processing
(use 'quil.core)

;; just like processing, we have a setup function
(defn setup []
  (let [in-img  (load-image "resources/silhouette.gif")
        max-x   (.width in-img)
        max-y   (.height in-img)]
      (set-state! :in in-img :max-x max-x :max-y max-y))
  (frame-rate 60)
  (background 255))

;; just like processing, we have a draw function
(defn draw []
  (let [x (random (state :max-x))
        y (random (state :max-y))
        p (.get (state :in) x y)]
      (set-pixel x y p)))

;; this will start the sketch
(defsketch example
  :title "Monte Carlo Simulation"
  :setup setup
  :draw draw
  :size [300 215])

;; use this function to stop it
(sketch-stop example)
