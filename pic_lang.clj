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
  [vertex-list img img-x img-y]
  (doall
    (map 
      (fn [[v1 v2 v3 v4]]
        (begin-shape :quads)
        (texture img)
        (vertex-v v1 0 0)
        (vertex-v v2 img-x 0)
        (vertex-v v3 img-x img-y)
        (vertex-v v4 0 img-y)
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

(defn split-squared
  [p1 p2 p3 p4]
  (y-split 
    (x-split p1 p2 0.5)
    (x-split p3 p4 0.5)
    0.5))
  
(defn split-squared-r
  [p depth corners]
  (if (> depth 0)
    (let [r-fn (partial split-squared-r p (dec depth) corners)
          args (map 
                 #(if (corners %) (r-fn) p) 
                 '(:tl :tr :bl :br))]
      (apply split-squared args)) 
    p))





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
    


(def simple-canvas (new-canvas '(0 0) '(500 0) '(0 500)))

(def simple_p (new-painter (rand-lines 10)))
(def simple-p-rp (right-push simple_p 5 0.5))

(def harish-p (new-painter))
(def harish-p-rp (right-push harish-p 5 0.5))

;experimenting with a different recursive image
(def harish-y-side (split-3 y-split harish-p harish-p harish-p 0.2 0.8))
(def harish-y-split-center (split-3 y-split harish-p harish-p harish-p 0.2 0.8))
(def p6 (split-3 x-split harish-y-side harish-y-side harish-y-side 0.2 0.8))

(def harish-squared 
  (let [p harish-p
        n 7]
    (split-squared 
      (split-squared-r p n #{:tl :tr :bl})
      (split-squared-r p n #{:tl :tr :br})
      (split-squared-r p n #{:tl :bl :br})
      (split-squared-r p n #{:tr :bl :br}))))


(defn setup [dst]
  (size 500 500 OPENGL )
  (smooth)
  (framerate 10)
  (no-loop)
  (def harish-pic (load-image "harish.tga"))
  (def harish-pic-2 (load-image "harish_2.tga")))

(defn draw [dst]
  (background-float 150 150 150)
  (fill-float 100 100 100)
  (stroke-float 10)
  (paint-pic (harish-squared simple-canvas) harish-pic-2 384 384)
  (save "harish_out_2.tif" ))
  
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
