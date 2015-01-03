(ns clojurelife.core
  (:import [javax.swing JButton JFrame JPanel])
  (:import java.awt.event.ActionListener)
  (:import java.awt.Color))

;; http://stackoverflow.com/questions/3636364/can-i-clean-the-repl
;; trying to purge existing state for a fresh run with a new -main call
;; (map #(ns-unmap *ns* %) (keys (ns-interns *ns*))) 

(defrecord Square [i j glow color])
(def nrows 30) 
(def ncols 40)
(def cool-green (Color. 30 200 90))

(defn rand-bool []
  (if (>= (Math/random) 0.5)
    true
    false))

(def start-vals #{[2 2] [2 3] [3 2] [3 3]})

(defn random-start-squares []
  (map atom (set (for [i (range nrows) j (range ncols)]
                         (Square. i j (rand-bool) cool-green)))))

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
(defn paint-square [square g]
  (.fillRoundRect g 
    (+ 20 gap-size (* square-size (:j square)))
    (+ gap-size (* square-size (:i square)))
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

(def control-panel
  (doto (JPanel.)
    (.setBackground Color/lightGray)
    (.add start-button)))

(def main-panel (proxy [JPanel] []
         (paintComponent [g]
           (proxy-super paintComponent g)
           (doseq [square @squares]
             (if (true? (:glow @square))
               (.setColor g (:color @square))
               (.setColor g Color/black))
             (paint-square @square g)))))

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


;; check if one square is adjacent to another. In this function, 
;; a square is adjacent to itself by definition
(defn is-adjacent? [this-square that-square] 
  (if (and (<= (:i that-square) (inc (:i this-square)))
           (>= (:i that-square) (dec (:i this-square)))
           (<= (:j that-square) (inc (:j this-square)))
           (>= (:j that-square) (dec (:j this-square))))
    true 
    false))


;; determining what a square should do next, given board 
;; inputs here are pure values
(defn next-action [square squares]
  (let [adjacent-squares
         (filter #(and (is-adjacent? square %) (alive? %)) squares)]
    (let [score-count (count adjacent-squares)]
      (if (alive? square)
        (cond 
          (< score-count 3) kill!
          (> score-count 4) kill!
          :else             stay-alive!)
        (cond
          (= 4 score-count) revive!
          :else             stay-dead!)))))


(defn update! [squares]
  (let [squares2actions
         (into {} (for [square @squares] 
                    [square (next-action @square (map deref @squares))]))]
    (dosync [square @squares]
      ((squares2actions square) square))))


;; main loops
(defn refresh [] (javax.swing.SwingUtilities/invokeLater #(.repaint main-panel)))
;; (defn start-gui-refresh-loop [] (loop [] (refresh) (Thread/sleep 40) (recur)))
(defn start-game-loop [] 
  (loop []
    (Thread/sleep 100)
    (if (time-flowing?) 
      (update! squares))
    (refresh)
    (recur)))

(defn -main []
  (.show main-frame)
  ;;(future (start-gui-refresh-loop))
  ;;(future (start-action-loop)))
  (future (start-game-loop)))
