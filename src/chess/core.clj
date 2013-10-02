(ns chess.core
  ;;  (:use [clojure.tools logging])
  (:require [clojure.core [reducers :as r]])
  (:import [java.util ArrayList BitSet HashSet Collection])
  (:gen-class))

;; for emacs:
;; (setenv "PATH" (concat "/usr/local/java/jdk1.7.0_40/:" (getenv "PATH")))
;; (setenv "LEIN_JVM_OPTS" "-Xms8G -Xmx8G -server")

;; Reflective calls are 10x slower than compiled ones. We are using
;; native Java type. Ensure that we are warned about any use of
;; reflection.

(set! *warn-on-reflection* true)

;;------------------------------------------------------------------------------
;; high-level definitions of how pieces move/cover
;;------------------------------------------------------------------------------

(defn on-board? [[x y] [w h]]
  "is a location at [x y] on a board of w*h ?"
  (and (>= x 0) (< x w)
       (>= y 0) (< y h)))

(defn up    [[x y]] [x (inc y)])
(defn down  [[x y]] [x (dec y)])
(defn left  [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(def up-right   (comp up   right))
(def up-left    (comp up   left))
(def down-right (comp down right))
(def down-left  (comp down left))

(def up-up-right      (comp up   up    right))
(def up-right-right   (comp up   right right))
(def down-right-right (comp down right right))
(def down-down-right  (comp down down  right))
(def down-down-left   (comp down down  left))
(def down-left-left   (comp down left  left))
(def up-left-left     (comp up   left  left))
(def up-up-left       (comp up   up    left))

;;------------------------------------------------------------------------------
;; calculating covers: simple - king/knight - and traversal - queen/bishop/rook
;;------------------------------------------------------------------------------

;; when a piece is placed on the board, we also place its "covers"
;; i.e. we record on the board all the squares covered by that piece
;; as well as its own location. This enables us to quickly check a
;; location to see if it is occupied or covered.

;; "simple" covers are those that always extend over a fixed area -
;; e.g. king and knight. "traversal" covers are those that extend from
;; the piece to the edge of the board - e.g. queen, rook, bishop...

(defn simple-covers [dimensions location directions]
  "generate a lazy sequence of all the locations covered on a board of
given dimensions by a piece of given location using simple given
directions"
  (filter
   (fn [location] (on-board? location dimensions))
   (map (fn [direction] (direction location)) directions)))

(defn traverse [direction location dimensions]
  "generate a lazy sequence of locations by traversing from a given
location in a given direction on a board of given dimensions"
  (rest (take-while (fn [location] (on-board? location dimensions)) (iterate direction location))))

(defn traversal-covers [dimensions location directions]
  "generate a lazy sequence of all the locations covered on a board of
given dimensions by a piece of given location using traversals of
given directions"
  (mapcat (fn [direction] (traverse direction location dimensions)) directions))

;;------------------------------------------------------------------------------
;; defining how each piece "covers" the board
;;------------------------------------------------------------------------------

(defmulti find-covers
  "generate a lazy sequence of all the locations covered on a board of
given dimensions by a given piece of given location"
  (fn [piece dimensions location] piece))

(defmethod find-covers \Q [piece dimensions location]
  (traversal-covers dimensions location [up up-right right down-right down down-left left up-left]))

(defmethod find-covers \B [piece dimensions location]
  (traversal-covers dimensions location [up-right down-right down-left up-left]))

(defmethod find-covers \R [piece dimensions location]
  (traversal-covers dimensions location [up right down left]))

(defmethod find-covers \K [piece dimensions location]
  (simple-covers dimensions location [up up-right right down-right down down-left left up-left]))

(defmethod find-covers \N [piece dimensions location]
  (simple-covers dimensions location
                 [up-up-right up-right-right down-right-right down-down-right
                  down-down-left down-left-left up-left-left up-up-left]))

;;------------------------------------------------------------------------------
;; The board is the most important abstraction in this program. It
;; must be as small as possible - we are going to be dealing with many
;; millions and checks and alterations to it must be as quick as
;; possible - we will be performing many millions of these. Finally,
;; interactions with it must be as simple as possible and it should
;; have solid equality semantics so a list of boards may be easily
;; deduplicated. After trying a map of location:piece and a string
;; (char-array - one char per location), I've settled on a BitSet
;; since it best fulfils these criteria.

;; A board is represented by a BitSet containing 4*w*h bits - i.e. 4
;; bits for each square on the chessboard. Squares are addressed from
;; bottom-left to top-bright. Their format is CPPP, where:

;; The C bit indicates whether the location is covered by an existing
;; piece or not.

;; The three P bits encode the type of piece occupying the location or
;; none.

;; The following c[har]->b[its] table provides the PPP values for thee
;; different pieces as well as ordering them such that the pieces with
;; the highest coverage are given lower values. This allows us to
;; order the pieces prior to playing them in order to keep the game
;; tree's branching factor to a minimum.

(def c->b {
           \Q 2r001                     ;queen
           \R 2r010                     ;rook
           \B 2r011                     ;bishop
           \N 2r100                     ;knight
           \K 2r101                     ;king
           })

;; Inverted mapping of b->c - used for printing board...

(def b->c (reduce (fn [r [k v]] (assoc r v k)) {} c->b))

;; Since the board is represented as a BitSet, we may now represent
;; all potential moves upon it as a pair of BitSets of the same size.

;; one bitset which may be used to check (by bitwise-or-ing with the
;; board) whether the intended move is valid - i.e. it does not
;; attempt to cover a location that it already occupied or to
;; introduce a new piece onto a covered location.

;; one bitset which actually encodes the intended move's coverage and
;; occupation details. This may be bitwise-or-ed with the board to
;; produce a new board - the result of making this move.

(defn translate [x y w] (* (+ (* y w) x) 4))

(defn encode-check [[w h] piece [x y] covers]
  "given board dimensions, a piece, a location and a seq of covers
return a BitSet that may be bitwise-or-ed against a board to decide
whether a potential move is valid"
  (let [^BitSet bs (BitSet. (* w h 4))
        ^long offset (translate x y w)
        set-cover-bit
        (fn [offset]
          (doto bs (.set (+ offset 0))))
        set-occupied-bits
        (fn [offset]
          (doto bs
            (.set (+ offset 1))
            (.set (+ offset 2))
            (.set (+ offset 3))))]
    ;; we want to occupy this location - we may not if it already
    ;; covered...
    (set-cover-bit offset)
    ;; or occupied
    (set-occupied-bits offset)
    ;; we will be covering these locations - they should not already
    ;; be occupied.
    (reduce (fn [^BitSet bs [x y]] (set-occupied-bits (translate x y w))) bs covers)))

(defn encode-move [[w h] piece [x y] covers]
  "return a new bitset that may be bitewise-or-ed with another
representing a board state to produce a new board state including the
placement of a given piece at a given location along with its given
covers"
  (let [^BitSet bs (BitSet. (* w h 4))
        ^long offset (translate x y w)
        bits (c->b piece)]
    ;; encode the piece
    (.set bs (+ offset 0))              ;this location is covered
    (.set bs (+ offset 1) (> (bit-and bits 2r001) 0))
    (.set bs (+ offset 2) (> (bit-and bits 2r010) 0))
    (.set bs (+ offset 3) (> (bit-and bits 2r100) 0))
    ;; encode the covers
    (reduce (fn [^BitSet bs [x y]] (.set bs (translate x y w)) bs) bs covers)))

;;------------------------------------------------------------------------------
;; functions for rendering board as a human friendly string
;; e.g.

;; +R++
;; N+N+
;; +++R
;; -+N+

;; where:
;; - : empty
;; + : covered (useful for checking program logic)
;; K/Q/B/N/R : corresponding piece

(defn decode-location [[x y] w ^BitSet board]
  "decode a given board location from a given board of given width, returning the associated character"
  (let [offset (translate x y w)]
    (get
     b->c
     (reduce (fn [c i] (+ (bit-shift-left c 1) (if (.get board (+ offset i)) 1 0))) 0 [3 2 1])
     (if (.get board offset) \+ \-))))

(defn print-row [w y board]
  "print the row at a given y coordinate from a given board of given width"
  (println (apply str (map (fn [x] (decode-location [x y] w board)) (range w)))))

(defn print-board [[w h] board]
  "print a given board of given dimensions"
  (doseq [y (reverse (range h))] (print-row w y board)))

(defn print-boards [dimensions boards]
  "print a given seq of boards of given dimensions"
  (doseq [board boards] (print-board dimensions board) (println)))

;;------------------------------------------------------------------------------

;; I want a map of piece:vector of a mask pair for each board location
(defn init [[w h :as dimensions] pieces]
  (reduce
   (fn [result piece]
     (assoc
         result
       piece
       (into 
        []
        (mapv
         (fn [[x y :as location]]
           (let [covers (find-covers piece dimensions location)]
             [(encode-check dimensions piece location covers)
              (encode-move dimensions piece location covers)]))
         (for [y (range h) x (range w)] [x y])))
       ))
   {}
   (into #{} pieces)))

;;------------------------------------------------------------------------------

(defn place [[^BitSet check ^BitSet move] ^BitSet board]
  "given a potential move and a board, check whether the move is valid
and if it is apply it returning the new board, or nil of not."
  (if (not (.intersects board check))
    (doto ^BitSet (.clone board) (.or move))))

;;------------------------------------------------------------------------------

;; TODO - avoiding copying collections with .addAll() saves 30% of time
(defn breadth-first-search [parents generate-children [head & tail] done]
  (if head
    (do
      (println "processing:" head "on" (count parents) "node[s]")
      (breadth-first-search
       (reduce
        (fn [^Collection result parent]
         (doto result (.addAll (generate-children parent head))))
        (if (contains? done head) (HashSet.) (ArrayList.))
        parents)
       generate-children
       tail
       (conj done head)))
    parents))
  
(defn depth-first-search [parent generate-children [head & tail] done]
  (if head
    (reduce
     (fn [^Collection result child]
       (doto result (.addAll (depth-first-search child generate-children tail (conj done head)))))
     (ArrayList.)
    (generate-children parent head))
    [parent]))

(defn generate-children-fn [model]
  "try given piece in every position on given board, returning only valid boards"
  (fn [board piece]
    (reduce
     (fn [^Collection result move]
       (if-let [new-board (place move board)]
         (doto result (.add new-board))
         result))
     (ArrayList.)
     (model piece))))

;;------------------------------------------------------------------------------

(defn run [w h pieces]
  (let [b true
        board (BitSet. (* w h 4))]
    ((if b breadth-first-search depth-first-search)
     (if b [board] board)
     (generate-children-fn (init [w h] (into [] (distinct pieces))))
     (sort-by c->b pieces)              ;get the queens out first etc...
     #{}
     )))

;;------------------------------------------------------------------------------

(defn n-pieces [n piece] (run n n (repeat n piece)))

;;------------------------------------------------------------------------------

(defn -main [& args]
  (println "count:" (count (run 9 6 [\Q \B \R \N \K \K]))))

;;------------------------------------------------------------------------------
;; TODO
;; tests
;; how can I check my answer - exhaustive testing
;; document
;; run on big iron
;; use reducers library ?
;; get some numbers for kings and rooks
;; use core.reducers
;; consider use of transient / persistent!
;; should we be using multimethods ?
;; use clojure-test mode
;; faster !
;; concurrent
;; revisit logging later
