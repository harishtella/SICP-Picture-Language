(ns pic-lang
  (:use rosado.processing)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet PImage PGraphics3D))
  (:import (processing.opengl PGraphicsOpenGL)))


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

(defn vertex-v 
  [[x y] u v]
  (vertex x y u v))

(defn paint 
  [lines]
  (doseq [[[x1 y1] [x2 y2]] (partition 2 lines)]
    (line x1 y1 x2 y2)))
  
(defn paint-pic
  [vertex-list img]
  (doall
    (map 
      (fn [[v1 v2 v3 v4]]
        (begin-shape :quads)
        (texture img)
        (vertex-v v1 0 0)
        (vertex-v v2 380 0)
        (vertex-v v3 380 380)
        (vertex-v v4 0 380)
        (end-shape))
      (partition 4 vertex-list))))

(defn add-canvas-verts
  [{:keys [origin-vec x-vec y-vec]}]
  (let [p1 origin-vec
        p2 (add-vec origin-vec x-vec)
        p3 (add-vec origin-vec x-vec y-vec)
        p4 (add-vec origin-vec y-vec)]
    (list p1 p2 p3 p4)))

(defn new-painter 
  ([lines]
   (fn [canvas]
     (doall (map 
              (fn [x] (map-to-canvas canvas x))
              lines))))
  ([]
   (fn [canvas]
     (add-canvas-verts canvas))))


(defn besides 
  [lp rp lc rc]
  (new-painter (concat (lp lc) (rp rc))))

(def on-top besides)

(defn y-split 
  [painter-top painter-bot split-point]
  (let [top-canvas (new-canvas '(0 0) 
                               '(1 0) 
                               (list 0 split-point))
        bot-canvas (new-canvas (list 0 split-point) 
                               '(1 0) 
                               (list 0 (- 1 split-point)))]
    (on-top painter-top painter-bot top-canvas bot-canvas)))

(defn x-split 
  [painter_left painter_right split_point]
  (let [left-canvas (new-canvas '(0 0) 
                                (list split_point 0) 
                                '(0 1))
        right-canvas (new-canvas (list split_point 0) 
                                 (list (- 1 split_point) 0) 
                                 '(0 1))]
    (besides painter_left painter_right left-canvas right-canvas)))

(defn split-3
  [split-fn p1 p2 p3 sp1 sp2]
  (assert (> sp2 sp1))
  (let [sp2-adj (/ (- sp2 sp1) (- 1 sp1))]
    (split-fn p1 
             (split-fn p2 p3 sp2-adj)
             sp1)))

(defn right-push
  [painter iterations split-point]
  (if (= iterations 0) 
    painter
    (x-split 
      painter 
      (right-push painter (- iterations 1) split-point) 
      split-point)))


(defn rand-point
  [] (take 2 (repeatedly rand)))
(defn rand-line
  [] (take 2 (repeatedly rand-point)))
(defn rand-lines 
  [x] 
  (loop [iter x lines ()]
    (if (> iter 0)
      (recur (- iter 1) (concat lines (rand-line)))
      lines)))
    


(def my-canvas0 (new-canvas '(0 0) '(500 0) '(0 500)))

(def my-canvas1 (new-canvas '(0 0) '(500 100) '(100 500)))
(def my-canvas2 (new-canvas '(0 0) '(500 0) '(500 100)))
(def my-canvas3 (new-canvas '(0 0) '(100 500) '(0 500)))

(def my-painter (new-painter (rand-lines 10)))
(def my-painter2 (right-push my-painter 5 0.5))

(def my-painter3 (new-painter))
(def my-painter4 (right-push my-painter3 5 0.5))
(def my-painter5 (y-split my-painter3 my-painter3 0.75))


(def harish-y-side (split-3 y-split my-painter3 my-painter3 my-painter3 0.2 0.8))
(def harish-y-split-center (split-3 y-split my-painter3 my-painter3 my-painter3 0.2 0.8))
(def p6 (split-3 x-split harish-y-side harish-y-side harish-y-side 0.2 0.8))
                  

(defn setup [dst]
  (size 500 500 OPENGL )
  (smooth)
  (framerate 10)
  (def harish-pic (load-image "harish.jpg")))

(defn draw [dst]
  (background-float 150 150 150)
  (fill-float 100 100 100)
  (stroke-float 10)
  (paint-pic (p6 my-canvas0) harish-pic))
  ;(paint (my-painter2 my-canvas0)))
  
(defn stop-p-app [dst]
  )
  ; do I need to call superclass stop method? 
  ; below line doesn't work 
  ; (.. dst super stop))



(defn start 
  []
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
; stop when called after the gui thread crashes 
; causes repl to hang due to .dispose call
; so I just call (start) again to reboot the app

(start)

