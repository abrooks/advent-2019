(ns advent-2019.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn line-comma-input [s]
  (map #(mapv str/trim (str/split % #","))
       (str/split-lines s)))

(defn inputs [fname]
  (-> (io/resource fname) slurp line-comma-input))

(defn mass->fuel [m]
  (-> m (/ 3) Math/floor long (- 2) (max 0)))

(defn rocketeq [mass]
   (let [fuel (mass->fuel mass)]
     (when (pos? fuel)
       (cons fuel (lazy-seq (rocketeq fuel))))))

;; Intcode Computer

(defn hlt-op [prog & _]
  [prog nil])

(defn tri-op [prog ip [a b out] op]
  [(assoc prog out (op (prog a) (prog b))) (+ 4 ip)])

(def intcode-instr
  {1  [tri-op +]
   2  [tri-op *]
   99 [hlt-op]})

;; Make transducer?
(defn intcode-step [prog ip]
  (let [[inst & prog-rest] (nthrest prog ip)
        [opfn & args] (intcode-instr inst)]
    ;(prn :ip ip :inst ({1 '+ 2 '* 99 :hlt} inst) :args (take 3 prog-rest))
    (apply opfn prog ip prog-rest args)))

(defn intcode-run [prog ip]
  (let [[prog' ip'] (intcode-step prog ip)]
    (if ip'
      (recur prog' ip')
      [prog ip])))

;; Problems

;; 3336985
(defn aoc-1a []
  (let [masses (map #(Long/parseLong (first %)) (inputs "aoc-1.txt"))]
    (prn (reduce + (map mass->fuel masses)))))

;; 5002611
(defn aoc-1b []
  (let [masses (map #(Long/parseLong (first %)) (inputs "aoc-1.txt"))]
    (prn (reduce + (mapcat rocketeq masses)))))

;; 4930687
(defn aoc-2a []
  (let [prog (mapv #(Long/parseLong %) (first (inputs "aoc-2.txt")))
        prog (-> prog (assoc 1 12) (assoc 2 2))
        [prog' ip'] (intcode-run prog 0)]
    (first prog')))

;; 5336
(defn aoc-2b []
  (let [prog (mapv #(Long/parseLong %) (first (inputs "aoc-2.txt")))]
    (first (for [noun (range 99)
                 verb (range 99)
                 :let [prog (-> prog (assoc 1 noun) (assoc 2 verb))
                       [prog' ip'] (intcode-run prog 0)]
                 :when (= 19690720 (prog' 0))]
             (+ verb (* 100 noun))))))

(def wire-dir
  {\U [ 0  1]
   \D [ 0 -1]
   \L [-1  0]
   \R [ 1  0]})

(defn route-wire [segments [dir & digits :as path]]
  (let [start (:start (meta segments))
        length (Long/parseLong (str/join digits))
        delta (mapv #(* % length) (wire-dir dir))
        finish (mapv + start delta)
        last-dist (or (:dist (meta (last segments))) 0)
        new-seg (with-meta [start finish]
                           {:dist (+ length last-dist) :start start})]
    (vary-meta (conj segments new-seg) assoc :start finish)))

(defn between [a x b]
  (or (< a x b)
      (> a x b)))

;; The problem description seems to only indicate orthogonal intersections.
;; So that's what we're doing...
(defn intersection [[[x1 y1][x1' y1'] :as seg1] [[x2 y2][x2' y2'] :as seg2]]
   (cond
     (and (= x1 x1') (between x2 x1 x2')
          (= y2 y2') (between y1 y2 y1')) [x1 y2]
     (and (= x2 x2') (between x1 x2 x1')
          (= y1 y1') (between y2 y1 y2')) [x2 y1]
     :otherwise false))

(defn manhattan-dist [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn split-segment [[[x y] [x' y'] :as seg] [xl yl :as loc]]
  (let [[[x y] [x' y'] :as seg] seg [xl yl :as loc] loc
        [xs ys] (:start (meta seg))
        [[x y] [x' y']] (if (and (= x xs) (= y ys)) seg (reverse seg))]
    (cond
      (= x xl x') [(Math/abs (- yl y)) (Math/abs (- y' yl))]
      (= y yl y') [(Math/abs (- xl x)) (Math/abs (- x' xl))])))

;; 855
(defn aoc-3a []
  (let [[wire1 wire2] (inputs "aoc-3.txt")
        seed (with-meta [] {:start [0 0]})
        segments1 (reduce route-wire seed wire1)
        segments2 (reduce route-wire seed wire2)
        intersections (doall (for [seg1 segments1
                                   seg2 segments2
                                   :let [loc (intersection seg1 seg2)]
                                   :when loc]
                               loc))]
    (->> intersections (sort-by manhattan-dist) first manhattan-dist)))

;; 11238
(defn aoc-3b []
  (let [[wire1 wire2] (inputs "aoc-3.txt")
        seed (with-meta [] {:start [0 0]})
        segments1 (reduce route-wire seed wire1)
        segments2 (reduce route-wire seed wire2)
        intersections (doall (for [seg1 segments1
                                   seg2 segments2
                                   :let [loc (intersection seg1 seg2)]
                                   :when loc
                                   :let [[to1 from1] (split-segment seg1 loc)
                                         [to2 from2] (split-segment seg2 loc)
                                         overshoot (+ (:dist (meta seg1))
                                                      (:dist (meta seg2)))
                                         dist (- overshoot (+ from1 from2))]]
                               (vary-meta loc assoc :dist dist)))]
    (->> intersections (sort-by #(:dist (meta %))) first meta :dist)))

(defn -main [test]
  ((ns-resolve (the-ns 'advent-2019.core) (symbol (str "aoc-" test)))))
