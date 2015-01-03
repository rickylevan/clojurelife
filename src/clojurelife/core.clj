(ns clojurelife.core
  (:import [javax.swing JButton JFrame JPanel])
  (:import java.awt.event.ActionListener)
  (:import java.awt.Color))

;; http://stackoverflow.com/questions/3636364/can-i-clean-the-repl
;; trying to purge existing state for a fresh run with a new -main call
;; (map #(ns-unmap *ns* %) (keys (ns-interns *ns*))) 

(defrecord Square [glow color])
(def nrows 30) 
(def ncols 20)
(def cool-green (Color. 30 200 90))

;; helper from StackOverflow
(defn in? 
  [seq elm]  
  (some #(= elm %) seq))

(defn rand-bool []
  (if (>= (Math/random) 0.5)
    true
    false))

(def spaceship-vals [[1 1] [3 1] [2 2] [3 2] [2 3]])

(defn random-start-squares []
  (vec (for [i (range nrows)] 
         (vec (for [j (range ncols)]
                (Square. (rand-bool) cool-green))))))

(defn spaceship-start-squares []
  (vec (for [i (range nrows)] 
       (vec (for [j (range ncols)]
              (Square. 
                (in? spaceship-vals [i j]) 
                cool-green))))))
                                     
(def squares (atom (random-start-squares)))
  
(defn assign-squares! []
  (swap! squares (fn [_] (random-start-squares))))

(defn kill! [square]
  (swap! square assoc :glow false))
(defn revive! [square]
  (swap! square assoc :glow true))
(defn stay-alive! [square])
(defn stay-dead! [square])

(defn alive? [square]
  (true? (:glow square)))
(defn dead? [square]
  (not (alive? square)))

;; time primitives
(def timeflow-bool (atom false))
(defn time-flowing? [] @timeflow-bool)
(defn resume-timeflow! [] (swap! timeflow-bool (fn [_] true)))
(defn stop-timeflow! [] (swap! timeflow-bool (fn [_] false)))
(defn start-timeflow! [] (resume-timeflow!))
(defn switch-timeflow! [] (swap! timeflow-bool (fn [b] (not b))))

(def square-size 20)
(def gap-size 2)
(defn paint-square [square i j g]
  (.fillRoundRect g 
    (+ 20 gap-size (* square-size j))
    (+ gap-size (* square-size i))
    (- square-size gap-size)
    (- square-size gap-size)
    (* 4 gap-size)
    (* 4 gap-size)))

(def start-button
  (doto (JButton. "Start")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e] 
        (do
          (resume-timeflow!)
          (assign-squares!)))))))

(def pause-button
  (doto (JButton. "Pause")
    (.addActionListener (proxy [ActionListener] []
      (actionPerformed [e]
        (if (time-flowing?)
          (stop-timeflow!)
          (resume-timeflow!)))))))

(def control-panel
  (doto (JPanel.)
    (.setBackground Color/lightGray)
    (.add start-button)
    (.add pause-button)))

(def main-panel (proxy [JPanel] []
         (paintComponent [g]
           (proxy-super paintComponent g)
           (doseq [i (range nrows)]
             (doseq [j (range ncols)]
               (let [square (get (get @squares i) j)]
                 (if (:glow square)
                   (.setColor g (:color square))
                   (.setColor g Color/black))
                 (paint-square square i j g)))))))

(doto main-panel
  (.setBackground Color/lightGray))

;; having trouble getting this dynamically
(def control-height 39)
(def main-frame (JFrame. "Game of Life"))
(doto main-frame
  (.add control-panel java.awt.BorderLayout/NORTH)
  (.add main-panel java.awt.BorderLayout/CENTER)
  (.setSize 
    (+ (* ncols square-size) (* (inc ncols) gap-size))
    (+ control-height (* nrows square-size) (* (inc nrows) gap-size))))

(defn score [i j squares]
  (let [adjs 
        [(get (get squares (inc i)) j)
         (get (get squares (inc i)) (inc j))
         (get (get squares i) (inc j))
         (get (get squares (dec i)) (inc j))
         (get (get squares (dec i)) j)
         (get (get squares (dec i)) (dec j))
         (get (get squares i) (dec j))
         (get (get squares (inc i)) (dec j))]]
    (reduce (fn [a b]
              ;; (:glow nil) ==> nil, off the board
              (if (:glow b) (inc a) a)) 0 adjs)))

(defn compute-new-state [squares]
  (vec (for [i (range nrows)] 
         (vec (for [j (range ncols)]
                (Square. 
                  (let [score-val (score i j squares)]
                    (if (:glow (get (get squares i) j))
                      (cond (< score-val 2) false
                            (> score-val 3) false
                            :else           true)
                      (cond (= score-val 3) true
                            :else           false)))
                  cool-green))))))

(defn update! [squares]
  (swap! squares compute-new-state))

;; main loops
(defn refresh [] 
  (javax.swing.SwingUtilities/invokeLater #(.repaint main-panel)))
(defn start-game-loop [] 
  (loop []
    (Thread/sleep 200)
    (if (time-flowing?) 
      (update! squares))
    (refresh)
    (recur)))

(defn -main []
  (.show main-frame)
  (future (start-game-loop)))
