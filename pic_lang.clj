(ns pic-lang
  (:use rosado.processing)
  (:import (javax.swing JFrame))
  (:import (processing.core PApplet)))

(defn setup [dst]
  (size 500 500)
  (smooth)
  (no-stroke)
  (fill 226)
  (framerate 60))

(defn draw [dst]
  (background-float (rand-int 256) (rand-int 256) (rand-int 256))
  (fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  (ellipse 100 100 (rand-int 90) (rand-int 90))
  (stroke-float 10)
  (line 10 10 (rand-int 150) (rand-int 150))
  (no-stroke)
  (filter-kind INVERT))

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
  


