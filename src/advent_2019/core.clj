(ns advent-2019.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

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

(defn mode-read [state base parm mode]
  (case mode
    0 (get-in state [:prog parm])
    1 parm))

(defn mode-write [state base parm mode val]
  (case mode
    0 (assoc-in state [:prog parm] val)))

(defn jmp-true-op [{:keys [ip] :as state} [mode-a mode-b] [a b]]
  (if (not= 0 (mode-read state nil a mode-a))
    (assoc state :ip (mode-read state nil b mode-b))
    (assoc state :ip (+ 3 ip))))

(defn jmp-false-op [{:keys [ip] :as state} [mode-a mode-b] [a b]]
  (if (= 0 (mode-read state nil a mode-a))
    (assoc state :ip (mode-read state nil b mode-b))
    (assoc state :ip (+ 3 ip))))

(defn <-op [a b]
  ({true 1 false 0} (< a b)))

(defn =-op [a b]
  ({true 1 false 0} (= a b)))

(defn hlt-op [state & _]
  (assoc state :running false))

(defn tri-op [{:keys [ip] :as state} [mode-a mode-b mode-c] [a b c] op]
  (let [A (mode-read state nil a mode-a)
        B (mode-read state nil b mode-b)
        val (op A B)
        state' (mode-write state nil c mode-c val)]
    (assoc state' :ip (+ 4 ip))))

(defn input [{:keys [ip in] :as state} [_ _ mode-c] [c]]
  (let [val (async/<!! in)
        _ (prn :input :val val)
        state (mode-write state (inc ip) c mode-c val)]
    (assoc state :ip (+ 2 ip))))

(defn output [{:keys [ip out] :as state} [mode-a _ _] [a]]
  (let [val (mode-read state (inc ip) a mode-a)]
    (prn :out val a mode-a)
    (async/>!! out val))
  (assoc state :ip (+ 2 ip)))

(def intcode-instr
  {1  [#'tri-op +]
   2  [#'tri-op *]
   3  [#'input]
   4  [#'output]
   5  [#'jmp-true-op]
   6  [#'jmp-false-op]
   7  [#'tri-op #'<-opp]
   8  [#'tri-op #'=-op]
   99 [#'hlt-op]})

;; Make transducer?
(defn intcode-step [{:keys [prog ip] :as state}]
  (let [[instmode & prog-rest] (nthrest prog ip)
        inst (mod instmode 100)
        modes (map #(mod (quot instmode %) 10) [100 1000 10000])
        [opfn & args] (intcode-instr inst)]
    (prn :step instmode inst (take 3 prog-rest) modes args)
    (apply opfn state modes prog-rest args)))

(defn intcode-run [state]
  (let [{:keys [running] :as state'}  (intcode-step state)]
    (if running
      (recur state')
      state')))

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
        state (intcode-run {:running true :prog prog :ip 0})]
    (get-in state [:prog 0])))

;; 5335
(defn aoc-2b []
  (let [prog (mapv #(Long/parseLong %) (first (inputs "aoc-2.txt")))]
    (first (for [noun (range 99)
                 verb (range 99)
                 :let [prog (-> prog (assoc 1 noun) (assoc 2 verb))
                       state (intcode-run {:running true :prog prog :ip 0})]
                 :when (= 19690720 (get-in state [:prog 0]))]
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

(def aoc-4-input (map #(Long/parseLong %) (str/split "158126-624574" #"-")))

;; 1665
(defn aoc-4a []
  (let [[low high] aoc-4-input]
    (prn :low low :high high)
    (count
     (for [a (range 0 (inc 9))
           b (range a (inc 9))
           c (range b (inc 9))
           d (range c (inc 9))
           e (range d (inc 9))
           f (range e (inc 9))
           :when (< (count (set [a b c d e f])) 6)
           :let [n (Long/parseLong (str a b c d e f))]
           :when (<= low n high)]
       n))))

;; 1131
(defn aoc-4b []
  (let [[low high] aoc-4-input]
    (prn :low low :high high)
    (count
     (for [a (range 0 (inc 9))
           b (range a (inc 9))
           c (range b (inc 9))
           d (range c (inc 9))
           e (range d (inc 9))
           f (range e (inc 9))
           :when (some #(= 2 %) (map count (partition-by identity [a b c d e f])))
           :let [n (Long/parseLong (str a b c d e f))]
           :when (<= low n high)]
       n))))

;; 13294380
(defn aoc-5a []
  (let [prog (mapv #(Long/parseLong %) (first (inputs "aoc-5.txt")))
        in (async/chan 10)
        out (async/chan 10)
        state {:running true :in in :out out :prog prog :ip 0}]
    (async/<!! (async/go
                 (async/>! in 1)
                 (intcode-run state)
                 (async/<! out)))))

(defn aoc-5b []
  (let [prog (mapv #(Long/parseLong %) (first (inputs "aoc-5.txt")))
        in (async/chan 10)
        out (async/chan 10)
        state {:running true :in in :out out :prog prog :ip 0}]
    (async/<!! (async/go
                 (async/>! in 5)
                 (intcode-run state)
                 (async/<! out)))))

(defn -main [test]
  ((ns-resolve (the-ns 'advent-2019.core) (symbol (str "aoc-" test)))))

(comment
  (require '[advent-2019.core :reload]))
