(ns chess.core
  (:use [clojure.tools.cli :only [cli]])
  (:require [clojure.core [reducers :as r]])
  (:import [java.util ArrayList BitSet HashSet Collection])
  (:gen-class))

;;------------------------------------------------------------------------------
;; Reflective calls are 10x slower than compiled ones. We are using
;; native Java types. Ensure that we are warned about any use of
;; reflection.
(set! *warn-on-reflection* true)

;;------------------------------------------------------------------------------
;; utils - mainly eager replacements for map (and related functions),
;; which is lazy in Clojure and uses Futures and therefore
;; synchronization which we don't want slowing us down...

;; "optional code" allowing us to parallelize efficiently...

(defn count-seqs
  "return the aggregate count of a given sequence of sequences"
  [seqs]
  (reduce (fn [r s] (+ r (count s))) 0 seqs))

(defn emap
  "like map but eager and requiring an additional combinator (used to
combine output of function with return value) and initial value for
return value."
  [function combinator result collection]
  (reduce (fn [result item] (combinator result (function item))) result collection))

;;------------------------------------------------------------------------------
;; map related utils using clojure vector / hash-set - 4x slower than
;; java.util.Collection based fns below.
;; uncomment these and comment those below to enable

;; (defn if-conj!
;;   "add a given item to a given collection if it is non-nil"
;;   [collection item]
;;   (if item (conj! collection item) collection))

;; (defn emapseq
;;   "like map, but eager and expects a given initial return value which must be a sequence"
;;   [function result sequence]
;;   (persistent! (emap function if-conj! (transient result) sequence)))

;; (def g-vec vector)
;; (def g-set hash-set)
;; (def g-emap emapseq)

;; (defn emapcatseq
;;   "like map, but eager and expects a given initial return value which must be a sequence"
;;   [function result sequence]
;;   (emap function into result sequence)) ;why is there no into! :-(

;; (defn emapv
;;   "like mapv but eager"
;;   [function sequence]
;;   (emapseq function [] sequence))

;; (defn emapcatv
;;   "like mapv but eager"
;;   [function sequence]
;;   (emapcatseq function [] sequence))

;;------------------------------------------------------------------------------
;; map related utils using java.util.ArrayList / HashSet - 4x faster than
;; native Clojure [] and #{} based fns above.
;; uncomment these and comment those above to enable

(defn if-add
  "add a given item to a given collection if it is non-nil"
  [^Collection collection item]
  (if item (doto collection (.add item)) collection))

(defn emapc
  "like map but eager and expecting an additional parameter giving the
initial return value. This must be a java.util.Collection as results
will be add()-ed to it."
  [function ^Collection result collection]
  (emap function if-add result collection))

(defn emapcatc
  "like emapc, but concatenating results together"
  [function ^Collection result collection]
  (emap function (fn [^Collection c ^Collection i] (.addAll c i) c) result collection))

(defn g-vec [& args] (let [^Collection c (or args [])] (ArrayList. c)))
(defn g-set [& args] (let [^Collection c (or args [])] (HashSet. c)))
(def g-emap emapc)

(defn emapv
  "like mapv but eager"
  [function sequence]
  (emapc function (g-vec) sequence))

(defn emapcatv
  "like mapcat but eager and returns a vector"
  [function sequence]
  (emapcatc function (g-vec) sequence))

;;------------------------------------------------------------------------------
;; define the ways that pieces may move
;;------------------------------------------------------------------------------

(defn on-board?
  "is a location at [x y] on a board of w*h ?"
  [[x y] [w h]]
  (and (>= x 0) (< x w)
       (>= y 0) (< y h)))

;; I'm not going to doc these - they are self explanatory...

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
;; functions used to calculate sets of covers by combining above moves
;; ------------------------------------------------------------------------------

;; when a piece is placed on the board, we also place its "covers"
;; i.e. we record on the board all the squares covered by that piece
;; as well as its own location. This enables us to quickly check a
;; location to see if it is occupied or covered.

;; "simple" covers are those that always extend over a fixed area -
;; e.g. king and knight. "traversal" covers are those that extend from
;; the piece to the edge of the board - e.g. queen, rook, bishop...

(defn simple-covers
  "generate a lazy sequence of all the locations covered on a board of
given dimensions by a piece of given location using simple given
directions"
  [dimensions location directions]
  (filter
   (fn [location] (on-board? location dimensions))
   (emapv (fn [direction] (direction location)) directions)))

(defn traverse
  "generate a lazy sequence of locations by traversing from a given
location in a given direction on a board of given dimensions"
  [direction location dimensions]
  (rest (take-while (fn [location] (on-board? location dimensions)) (iterate direction location))))

(defn traversal-covers
  "generate a lazy sequence of all the locations covered on a board of
given dimensions by a piece of given location using traversals of
given directions"
  [dimensions location directions]
  (emapcatv (fn [direction] (traverse direction location dimensions)) directions))

;;------------------------------------------------------------------------------
;; attach the moves and cover functions to the pieces defining how
;; they cover the board.
;; ------------------------------------------------------------------------------

(def char->cover-fn
  "a map of char to function that can generate a sequence of all the locations covered on a board of
given dimensions by a given piece of given location"
  {\Q (fn [dimensions location]
        (traversal-covers dimensions location [up up-right right down-right down down-left left up-left]))

   \B (fn [dimensions location]
        (traversal-covers dimensions location [up-right down-right down-left up-left]))

   \R (fn [dimensions location]
        (traversal-covers dimensions location [up right down left]))

   \K (fn [dimensions location]
        (simple-covers dimensions location [up up-right right down-right down down-left left up-left]))

   \N (fn [dimensions location]
        (simple-covers dimensions location
                       [up-up-right up-right-right down-right-right down-down-right
                        down-down-left down-left-left up-left-left up-up-left]))})

;;------------------------------------------------------------------------------
;; define the boards representation and the game's interaction with it.
;;------------------------------------------------------------------------------

;; The board is the most important abstraction in this program. It
;; must be as small as possible - we are going to be dealing with many
;; millions - and checks and alterations to it must be as simple/fast
;; as possible - we will be performing many millions of
;; these. Finally, it should have solid equality semantics so a
;; sequence of boards may be easily deduplicated.

;; After trying to represent the board as a map of location:piece and
;; a string (char-array - one char per location) - both too big and
;; slow, I've settled on a BitSet based representation - more complex
;; than I would like but space and time dictate implementation here.

;; So, a board is represented by a BitSet containing 4*w*h bits -
;; i.e. 4 bits for each square on the chessboard. Squares are
;; addressed from bottom-left to top-bright. Their bit format is CPPP,
;; where:

;; The C bit indicates whether the location is covered by an existing
;; piece or not.

;; The three P bits encode the type of any piece occupying the
;; location or none.

;; The following char->bits table provides the PPP values for the
;; different pieces as well as ordering them such that the pieces with
;; the highest coverage are given lower values. This allows us to
;; order the pieces in the same way prior to playing them in order to
;; keep the game tree's branching factor to a minimum.

(def char->bits
  "lookup table for translation of a char to the bits used to encode
its piece on a board."
  {
   \Q 2r001                             ;queen
   \R 2r010                             ;rook
   \B 2r011                             ;bishop
   \N 2r100                             ;knight
   \K 2r101                             ;king
   })

;; Inverted mapping of char->bits - used for printing board...

(def bits->char
  "lookup table for translation of the bits used to encode its piece
on a board to the char that they represent."
  (reduce (fn [r [k v]] (assoc r v k)) {} char->bits))

;; Since the board is represented as a BitSet, we may now represent
;; all potential moves upon it as a pair of BitSets of the same size:

;; A 'check' bitset which may be used to check (by bitwise-or-ing with
;; the board) whether the intended move is valid - i.e. it does not
;; attempt to cover an already occupied location or to occupy an
;; already covered location.

;; An 'action' bitset which actually encodes the intended move's
;; coverage and occupation details. This may be bitwise-or-ed with the
;; board to produce a new board - the result of making this move.

(defn coords->offset
  "given the x,y location and board width, return the corresponding
offset into a board's bitset representation"
  [x y w]
  (* (+ (* y w) x) 4))

(defn encode-check
  "given board dimensions, a piece, a location and a seq of covers
return a BitSet that may be bitwise-or-ed against a board to decide
whether a potential move is valid"
  [[w h] piece [x y] covers]
  (let [^BitSet bs (BitSet. (* w h 4))
        ^long offset (coords->offset x y w)
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
    (reduce (fn [^BitSet bs [x y]] (set-occupied-bits (coords->offset x y w))) bs covers)))

(defn encode-action
  "return a new bitset that may be bitewise-or-ed with another
representing a board state to produce a new board state including the
placement of a given piece at a given location along with its given
covers"
  [[w h] piece [x y] covers]
  (let [^BitSet bs (BitSet. (* w h 4))
        ^long offset (coords->offset x y w)
        bits (char->bits piece)]
    ;; encode the piece
    (.set bs (+ offset 0))              ;this location is covered
    (.set bs (+ offset 1) (> (bit-and bits 2r001) 0))
    (.set bs (+ offset 2) (> (bit-and bits 2r010) 0))
    (.set bs (+ offset 3) (> (bit-and bits 2r100) 0))
    ;; encode the covers
    (reduce (fn [^BitSet bs [x y]] (.set bs (coords->offset x y w)) bs) bs covers)))

;;------------------------------------------------------------------------------

(defn encode-move
  "return the vector of pairs of all encoded check and action BitSets
for a given piece on a board of given w[idth] and h[eight] ordered
bottom-left to top-right."
  [[w h :as dimensions] piece]
  (emapv
   (fn [[x y :as location]]
     (let [covers ((char->cover-fn piece) dimensions location)]
       [(encode-check dimensions piece location covers)
        (encode-action dimensions piece location covers)]))
   (for [y (range h) x (range w)] [x y])))

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

(defn decode-location
  "decode a given board location from a given board of given width, returning the associated character"
  [[x y] w ^BitSet board]
  (let [offset (coords->offset x y w)]
    (get
     bits->char
     (reduce (fn [c i] (+ (bit-shift-left c 1) (if (.get board (+ offset i)) 1 0))) 0 [3 2 1])
     (if (.get board offset) \+ \-))))

(defn print-row
  "print the row at a given y coordinate from a given board of given width"
  [w y board]
  (println (apply str (map (fn [x] (decode-location [x y] w board)) (range w)))))

(defn print-board
  "print a given board of given dimensions"
  [[w h] board]
  (doseq [y (reverse (range h))] (print-row w y board)))

(defn print-boards
  "print a given seq of boards of given dimensions"
  [dimensions boards]
  (doseq [board boards] (print-board dimensions board) (println)))

;;------------------------------------------------------------------------------

(defn place-on-board
  "given a potential move (check and action) and board, check whether
the move is valid and if it is apply it, returning the new board, or
nil of not."
  [[^BitSet check ^BitSet action] ^BitSet board]
  (if (not (.intersects board check))
    (doto ^BitSet (.clone board) (.or action))))

;;------------------------------------------------------------------------------

(defn generate-children
  "on a given board, try a given seq of moves, adding valid resulting
boards to a given seq, and returning it"
  [board moves result]
  (g-emap (fn [move] (place-on-board move board)) result moves))

(defn serial-breadth-first-search
  "starting with a given set of parent boards and a plan, execute a
  breadth first search for solutions in the problem space, in serial
  mode returning these solutions"
 [parents [[move result-fn piece :as head] & tail]]
  (if head
    (do
      ;;(println "serial processing:" piece "over" (count parents) "node[s]")
      (serial-breadth-first-search
       (reduce
        (fn [result parent]
          (generate-children parent move result))
        (result-fn)
        parents)
       tail))
    parents))

;;------------------------------------------------------------------------------
;; this is "optional" code to support a parallel tree search

(defn parallel-breadth-first-search
  "starting with a given set of parent boards and a plan, execute a
  breadth first search for solutions in the problem space, in parallel
  mode returning these solutions"
  [parent-seqs [head & _ :as moves]]
  (if head
    (do
      (println "parallel processing:" (emapv count parent-seqs) "nodes")
      (pmap
       (fn [parents] (serial-breadth-first-search parents moves))
       parent-seqs))
    parent-seqs))

;; should trade off speed for space, but have not had time to try properly

;; (defn depth-first-search [parent [head & tail] done]
;;   (if head
;;     (reduce
;;      (fn [^Collection result child]
;;        (depth-first-search child tail (conj done head) result))
;;      (ArrayList.)
;;      (generate-children parent head))
;;     [parent]))

;;------------------------------------------------------------------------------
;; this is "optional" code to allow partitioning of the data to
;; support a parallel tree search

(defn partition-sizes
  "return a lazy sequence of partition sizes to be used to partition a
sequence of given size into a given number of partitions"
  [size num-partitions]
  (take
   num-partitions
   (map
    (fn [q m] (+ q m))
    (repeat (quot size num-partitions))
    (concat (repeat (mod size num-partitions) 1) (repeat 0)))))

(defn partition-indeces
  "given a sequence and a list of partition sizes return a sequence of
  the indeces to be used to slice the sequence into partitions"
  [sequence num-partitions]
  (reduce
   (fn [r i] (if (and (number? r)(zero? r)) [[r i]] (conj r [(apply + (last r)) i])))
   0
   (partition-sizes (count sequence) num-partitions)))

(defmulti slice
  "partition a given sequence/collection into a given number of
sequences/collections for parallel processing - agenerif function
which knows how to do this efficiently for a number of types"
  (fn [collection num-partitions] (type collection)))

(defmethod slice clojure.lang.PersistentVector [c n]
  (emapv (fn [[i l]] (subvec c i (+ i l))) (partition-indeces c n)))

;; these methods support slicing of java.util.Collections and should
;; be uncommented if you are running with these - see above.

(defmethod slice ArrayList [^ArrayList c n]
  (emapv (fn [[i l]] (.subList c i (+ i l))) (partition-indeces c n)))

(defmethod slice HashSet [^Collection c n]
  (slice (ArrayList. c) n))

;; these methods support slicing of idiomatic clojure seqs and should
;; be uncommented if you are running with these - see above.

;; (defmethod slice clojure.lang.PersistentHashSet [c n]
;;   (slice (into [] c) n))

;; (defmethod slice clojure.lang.LazySeq [c n]
;;   (slice (into [] c) n))

;;------------------------------------------------------------------------------

(defn plan
  "given board dimensions and a set of pieces to play, return a
two-phase plan. The first phase contains all duplicate pieces ordered
by their frequency and impact. We want to play the most frequent first
as we will have to deduplicate our results (an expensive undertaking)
after each one so it is better to do this at an early stage in the
search when there will be fewer boards. This phase is run in serial
mode as the overhead or partitioning and collapsing (for
deduplication) the data outweighs the benefit of running in
parallel. The second phase plays out the singleton pieces in order of
impact in parallel mode. Both phases contain a sequence of tuples of
<move deduplicate? piece>."
  [[w h :as dimensions] pieces]
  (let [piece->frequency (frequencies pieces)]
    ;; split resulting list as it reaches first non-duplicate piece...
    (split-with
     (fn [[_ _ piece]]
       (> (piece->frequency piece) 1))
     ;; expand each piece into a tuple of:
     ;; [move return-fn piece]
     (emapcatv
      (fn [[piece frequency]]
        (into
         []
         (map         ;should be emapv but is processing multiple seqs
          (fn [piece return-fn]
            [(encode-move dimensions piece) return-fn piece])
          (repeat frequency piece)
          (concat [g-vec] (repeat g-set)))))
      ;; sort pieces by the inverse of their frequency and their impact on the board
      (sort-by
       (fn [[piece frequency]] [(/ 1 frequency) (char->bits piece)])
       piece->frequency)))))

(defn run
  "given board dimensions and a set of pieces to be played, make and
  execute a two phase plan, playing the first phase in serial and the
  second in parallel to find all solutions to the problem outlined"
  [w h pieces]
  (println "problem: place" pieces "on a" (str w "x" h) "board")
  (let [start (System/currentTimeMillis)
        [part1 part2] (plan [w h] pieces)
        boards (parallel-breadth-first-search
                (slice 
                 (serial-breadth-first-search
                  [(BitSet. (* w h 4))]
                  part1)
                 (.availableProcessors (Runtime/getRuntime)))
                part2)]
    (println "solutions:" (count-seqs boards) "found in" (- (System/currentTimeMillis) start) "millis\n")
    boards
    ))

;;------------------------------------------------------------------------------

(defn -main [& args]
  (let [[options _ documentation]
        (cli
         args
         "chess: find all non-attacking solutions for given pieces on a board of given dimensions"
         ["-w" "--width"   "board width"       :parse-fn #(Integer/parseInt %) :default 0]
         ["-h" "--height"  "board height"      :parse-fn #(Integer/parseInt %) :default 0]
         ["-k" "--kings"   "number of kings"   :parse-fn #(Integer/parseInt %) :default 0]
         ["-q" "--queens"  "number of queens"  :parse-fn #(Integer/parseInt %) :default 0]
         ["-b" "--bishops" "number of bishops" :parse-fn #(Integer/parseInt %) :default 0]
         ["-r" "--rooks"   "number of rooks"   :parse-fn #(Integer/parseInt %) :default 0]
         ["-n" "--knights" "number of knights" :parse-fn #(Integer/parseInt %) :default 0]
         ["-p" "--[no-]printing" :default true])
        {:keys [width height kings queens bishops rooks knights printing]} options]
    (println documentation)
    (let [result (run width height (mapcat repeat [kings queens bishops rooks knights] [\K \Q \B \R \N]))]
      (if printing
        (do
          (println "boards:\n")
          (print-boards [width height] (apply concat result))
          (println "done")))
      ;; pmap seems to use non-daemon threads which don't shut down by themselves...
      (System/exit 0))))
  
;;------------------------------------------------------------------------------
;; FUTURE DIRECTIONS
;;------------------------------------------------------------------------------

;; A specific board type generated at runtime, after the board
;; dimensions become known would be able to be less generic and
;; therefore smaller and faster than BitSet - investigate.

;; Proper profiling might highlight some interesting areas....
