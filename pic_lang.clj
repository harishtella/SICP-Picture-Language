(ns pic-lang
  (:use rosado.processing)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet)))


(defn add-vec
  ([v1] v1)
  ([v1 v2] (map + v1 v2))
  ([v1 v2 & vs] (add-vec (add-vec v1 v2) (apply add-vec vs))))

(defn scale-vec 
  [factor v]
  (map #(* factor %) v))


(defn new-canvas 
  [origin x y]
  {:origin-vec origin :x-vec x :y-vec y})

(defn map-to-canvas
   [{:keys [origin-vec x-vec y-vec]}
    [point-x point-y]]
  (add-vec 
    origin-vec
    (scale-vec point-x x-vec)
    (scale-vec point-y y-vec)))

(defn x-split 
  [painter_left painter_right split_point]
  (let [left-canvas (new-canvas '(0 0) (list split_point 0) '(0 1))
        right-canvas (new-canvas (list split_point 0) (list (- 1 split_point) 0) '(0 1))]
    (new-painter (concat (painter_left left-canvas) (painter_right right-canvas)))))

(defn right-push
  [painter iterations split-point]
  (if (= iterations 0) 
    painter
    (x-split 
      painter 
      (right-push painter (- iterations 1) split-point) 
      split-point)))

(defn paint 
  [lines]
  (doseq [line_ lines]
    (let [[[x1 y1] [x2 y2]] line_]
      (line x1 y1 x2 y2))))

(defn new-painter 
  [lines]
  (fn [canvas]
    (let [c-lines (doall (map 
                           #(doall (map (partial map-to-canvas canvas) % ))
                           lines))
          c-lines-border (add-canvas-border c-lines canvas)]
      c-lines-border)))


(defn add-canvas-border 
  [lines 
   {:keys [origin-vec x-vec y-vec]}]
  (let [p1 origin-vec
        p2 (add-vec origin-vec x-vec)
        p3 (add-vec origin-vec x-vec y-vec)
        p4 (add-vec origin-vec y-vec)]
    (conj lines (list p1 p2) (list p2 p3) (list p3 p4) (list p4 p1))))
  


(defn rand-point
  [] (take 2 (repeatedly rand)))
(defn rand-line
  [] (take 2 (repeatedly rand-point)))
(defn rand-lines 
  [x] (take x (repeatedly rand-line)))


(def my-canvas (new-canvas '(0 0) '(500 50) '(50 500)))
(def my-painter (new-painter (rand-lines 10)))
(def my-painter2 (right-push my-painter 15 0.2))
                  

(defn setup [dst]
  (size 500 500)
  (smooth)
  (framerate 10))

(defn draw [dst]
  (background-float 150 150 150)
  (fill-float 100 100 100)
  (stroke-float 10)
  (paint (pushed-pic my-canvas)))



(defn stop-p-app [dst]
  )
  ; do I need to call superclass stop method? 
  ; below line doesn't work 
  ; (.. dst super stop))


(def swing-frame (JFrame. "Pretty Picture"))
(def p-app
     (proxy [PApplet] []
       (setup []
              (binding [*applet* this]
                (setup this)))
       (draw []
             (binding [*applet* this]
               (draw this)))
       (stop []
             (binding [*applet* this]
               (stop-p-app this)))))

(defn start 
  []
  (.init p-app)
  (doto swing-frame
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setSize 500 500)
    (.add p-app)
    (.pack)
    (.show)))

(defn stop 
  []
  (let [closing-fn (fn []
                     (.destroy p-app)
                     (doto swing-frame
                       (.hide)
                       (.dispose)))]
    (javax.swing.SwingUtilities/invokeAndWait closing-fn)))

(start)

(stop)
  


