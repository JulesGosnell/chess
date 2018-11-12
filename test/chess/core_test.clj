(ns chess.core-test
  (:import [java.util BitSet ArrayList HashSet])
  (:require [clojure.test :refer :all]
            [chess.core :refer :all]))

;;------------------------------------------------------------------------------
;; test utils

(deftest test-count-seqs
  (is (= 10 (count-seqs [[][0][1 2][3 4 5][6 7 8 9]]))))

(deftest test-emap-vector
  (let [actual (emap inc conj [] '(0 1 2))]
    (is (and (vector? actual) (= [1 2 3] actual)))))

(deftest test-emap-set
  (let [actual (emap identity conj #{} '(0 0 0 2 2 1))]
    (is (and (set? actual) (= #{0 1 2} actual)))))

;;------------------------------------------------------------------------------

(deftest test-if-add
  (is (= [1 2 3] (doto (ArrayList.) (if-add nil) (if-add 1) (if-add nil) (if-add 2) (if-add nil) (if-add 3)))))

(deftest test-emapc-array-list
  (let [result (emapc inc (ArrayList.) [0 1 2])]
    (is (= [1 2 3] result))
    (is (instance? ArrayList result))))

(deftest test-emapc-hash-set
  (let [result (emapc inc (HashSet.) [0 1 2])]
    (is (= #{1 2 3} result))
    (is (instance? HashSet result))))

(deftest test-emapcatc-array-list
  (let [result (emapcatc (fn [n] [(inc n)]) (ArrayList.) [0 1 2])]
    (is (= [1 2 3] result))
    (is (instance? ArrayList result))))

(deftest test-emapcatc-hash-set
  (let [result (emapcatc (fn [n] [(inc n)]) (HashSet.) [0 1 2])]
    (is (= #{1 2 3} result))
    (is (instance? HashSet result))))

(deftest test-g-vec
  (let [result (g-vec 0 1 2)]
    (is (= [0 1 2] result))
    (is (= (type result) ArrayList))))

(deftest test-g-set
  (let [result (g-set 0 1 2)]
    (is (= #{0 1 2} result))
    (is (= (type result) HashSet))))

(deftest test-emapv
  (let [result (emapv inc '(0 1 2))]
    (is (= [1 2 3] result))
    (is (= (type result) ArrayList))))

(deftest test-emapcatv
  (let [result (emapcatv (fn [n] [(inc n)]) '(0 1 2))]
    (is (= [1 2 3] result))
    (is (= (type result) ArrayList))))

;;------------------------------------------------------------------------------
;; test directions

(deftest test-on-board?
  (is (on-board? [0 0] [1 1]))
  (is (not (on-board? [1 0] [1 1])))
  (is (not (on-board? [-1 0] [1 1])))
  (is (not (on-board? [0 1] [1 1])))
  (is (not (on-board? [0 -1] [1 1])))
  (is (not (on-board? [-1 -1] [1 1])))
  (is (not (on-board? [1 -1] [1 1])))
  (is (not (on-board? [-1 1] [1 1])))
  (is (not (on-board? [1 1] [1 1])))
  )

(deftest test-up (is (= [0 1] (up [0 0]))))
(deftest test-down (is (= [0 -1] (down [0 0]))))
(deftest test-right (is (= [1 0] (right [0 0]))))
(deftest test-left (is (= [-1 0] (left [0 0]))))

;; I'm not going to test the compound direction fns - they are pretty
;; straightforward and simply use comp (a builtin) to build on the
;; primitive ones..

;; ------------------------------------------------------------------------------
;; cover functions

(deftest test-simple-covers
  (is (= [[1 0][0 1][2 1][1 2]] (simple-covers [3 3][1 1][down left right up up-up-right]))))

(deftest test-traversal-covers
  (is (= [[1 1][2 2][3 3]] (traversal-covers [4 4][0 0][up-right]))))

;; ------------------------------------------------------------------------------

(deftest test-coords->offset
  (is (= 144 (coords->offset 4 4 8))))

;; test the encoding of a king in the middle of a 3x3 board
;; i.e. ('-' = free, '+' = covered)
;;
;;  -+-
;;  +K+
;;  -+-

(deftest test-encode-check 
  (is (=
       ;; the king and his covers
       (encode-check [3 3] \K [1 1] [[1 0][0 1][2 1][1 2]])
       ;; the expected 'check' bitset
       (reduce (fn [^BitSet bitset location] (doto bitset (.set location)))
               (BitSet. 36)
               [5 6 7 13 14 15 16 17 18 19 21 22 23 29 30 31]))))

(deftest test-encode-action
  (is (=
       ;; the king and his covers
       (encode-action [3 3] \K [1 1] [[1 0][0 1][2 1][1 2]])
       ;; the expected 'action' bitset
       (reduce (fn [^BitSet bitset location] (doto bitset (.set location)))
               (BitSet. 36)
               [4 12 16 17 19 20 28]))))

(deftest test-encode-move
  ;; "mock out" functions used by encode-move
  (with-redefs [char->cover-fn (fn [piece]
                                 (is (= \P piece))
                                 (constantly [piece :covers]))
                encode-check (fn [dimensions piece location covers]
                               (is (= [1 2] dimensions))
                               (is (= \P piece))
                               (is (contains? #{[0 0][0 1]} location))
                               (is (= [piece :covers] covers))
                               [:check piece location covers])
                encode-action (fn [dimensions piece location covers]
                                (is (= [1 2] dimensions))
                                (is (= \P piece))
                                (is (contains? #{[0 0][0 1]} location))
                                (is (= [piece :covers] covers))
                                [:action piece location covers])]
    
    (is (= [[[:check \P [0 0] [\P :covers]][:action \P [0 0] [\P :covers]]]
            [[:check \P [0 1] [\P :covers]][:action \P [0 1] [\P :covers]]]]
           (encode-move [1 2] \P)))
    
    ))

;;------------------------------------------------------------------------------

;; these tests used a simplified 2-bit layout - CO - Covered, Occupied

(deftest test-place-piece-on-empty-board
  (is (=
       ;; a board with 1 square, which is occupied
       (doto (BitSet. 2)(.set 1))
       (place-on-board
        ;; a move to occupy the square on a board of 1
        [(doto (BitSet. 2) (.set 0)(.set 1))(doto (BitSet. 2) (.set 1))] 
        ;; an empty board of 1 square
        (BitSet. 2)))))

(deftest test-place-cover-on-empty-board
  (is (=
       ;; a board with 1 square which is covered
       (doto (BitSet. 2)(.set 0))
       (place-on-board
        ;; a move to cover the square of a 1 square board
        [(doto (BitSet. 2) (.set 0))(doto (BitSet. 2) (.set 0))]
        ;; an empty board
        (BitSet. 2)))))

(deftest test-place-piece-on-covered-space
  (is (nil?
       (place-on-board
        ;; a move to occupy the square of a 1 square board
        [(doto (BitSet. 2) (.set 0)(.set 1))(doto (BitSet. 2) (.set 1))]
        ;; a one square board on which the square is covered
        (doto (BitSet. 2) (.set 0))))))

(deftest test-place-cover-on-occupied-space
  (is (nil?
       (place-on-board
        ;; a move to cover the square on a one square board
        [(doto (BitSet. 2) (.set 0))(doto (BitSet. 2) (.set 0))]
        ;; a one square board on which the square is occupied
        (doto (BitSet. 2) (.set 0)(.set 1))))))

;;------------------------------------------------------------------------------

(deftest test-generate-children
  ;; "mock out" functions used by generate-children - we are only unit-testing
  (with-redefs [place-on-board (fn [move board] move)]

    (is (= #{1 :a "b" \c} (generate-children :board [nil 1 nil :a nil "b" nil] (g-set \c))))
    
    ))

;;------------------------------------------------------------------------------
;; test our tree traversal functions - define a tree and check that
;; functions traverse the nodes in the expected order and find the
;; same set of solutions...


(def tree {:a [:b :c]
           :b [:d :e]
           :c [:f :g]
           :d [:h]
           :e [:i]
           :f [:j]
           :g [:k]
           :h []
           :i []
           :j []
           :k []})

(deftest test-serial-breadth-first-search
  
  ;; use atom to hold state - I want to remember which nodes in the tree have been visited
  (let [nodes (atom [])]
    ;; replace generate-children with a function that remembers where it has been
    (with-redefs [generate-children (fn [parent move result]
                                      (swap! nodes (fn [r n] (conj r n)) parent)
                                      (doto result (.addAll (tree parent))))]

      (is (= [:h :i :j :k]
             (serial-breadth-first-search [:a] [[nil g-vec :p1] [nil g-vec :p2] [nil g-vec :p3]])))

      (is (= [:a :b :c :d :e :f :g]
             @nodes))

      )))

;;; test parallel-breadth-first-search

(deftest test-parallel-breadth-first-search
  (let [parent-seqs [[:a][:b][:c]]
        moves nil]
    (with-redefs [serial-breadth-first-search
                  (fn [p m] (is (contains? #{[:a][:b][:c]} p)) (is (nil? m)) p)]

      (is (= [[:a][:b][:c]] (parallel-breadth-first-search parent-seqs moves)))

      )))

;;------------------------------------------------------------------------------

(deftest test-partition-sizes
  (is (= '(1 1 1 0 0 0) (partition-sizes 3 6)))
  (is (= '(2 2 2 1 1 1) (partition-sizes 9 6)))
  (is (= '(2 2 2 2 2 2) (partition-sizes 12 6)))
  (is (= '(0 0 0 0 0 0) (partition-sizes 0 6))))

;; I'm going to black-box this one...
(deftest test-partition-indeces
  ;;; clean fit
  (is (= [[0 3] [3 3] [6 3]] (partition-indeces (range 9) 3)))
  ;; too many
  (is (= [[0 4] [4 4] [8 4]] (partition-indeces (range 12) 3)))
  ;; too few
  (is (= [[0 1] [1 1] [2 1] [3 0] [3 0]] (partition-indeces (range 3) 5))))
  
;; and this one
(deftest test-slice
  (is (= [[0 1 2 3] [4 5 6] [7 8 9]] (slice (into [] (range 10)) 3)))
  (is (= [[0 1 2 3] [4 5 6] [7 8 9]] (slice (apply g-vec (range 10)) 3)))
  (is (= [[0 1 2 3] [4 5 6] [7 8 9]] (slice (apply g-set (range 10)) 3))))

;;------------------------------------------------------------------------------

(deftest test-plan
;;; "stub out" encode-move
  (with-redefs [encode-move (fn [dimensions piece][dimensions piece])]

    ;;no duplicates - therefore no phase1
    (let [[p1 p2] (plan [1 2] [\K \Q])]
      (is (empty? p1))
      (is (= [[[[1 2] \Q] g-vec \Q] [[[1 2] \K] g-vec \K]] p2)) ;N.B. Q reordered before K)

      ;; all duplicates - therefore no phase2
      (let [[p1 p2] (plan [1 2] [\Q \Q])]
        (is (= [[[[1 2] \Q] g-vec \Q] [[[1 2] \Q] g-set \Q]] p1)) ;N.B. vec used for 1st move, set for 2nd
        (is (empty? p2)))

      ;; duplicates ordered by frequency, impact
      ;; non-duplicates ordered by impact
      (let [[p1 p2] (plan [1 2] [\N \R \Q \Q \K \K \K])]
        (is (= [[[[1 2] \K] g-vec \K]
                [[[1 2] \K] g-set \K]
                [[[1 2] \K] g-set \K]
                [[[1 2] \Q] g-vec \Q]
                [[[1 2] \Q] g-set \Q]]
               p1))
        (is (= [[[[1 2] \R] g-vec \R]
                [[[1 2] \N] g-vec \N]]
               p2)))
      )
    
    ))

(deftest test-run
  (let [boards [(range 1)(range 2)(range 3)]]
    (with-redefs [plan (constantly [[][]])
                  parallel-breadth-first-search (constantly boards)
                  slice (constantly nil)
                  serial-breadth-first-search (constantly nil)]

      (is (= boards (run 1 2 [\A \B \C])))
      )))

;;------------------------------------------------------------------------------
;; Black box testing - entire software stack - answers gleaned from
;; Trafigura examples and e.g.
;; - http://en.wikipedia.org/wiki/N_queens
;; - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.174.7944&rep=rep1&type=pdf

(defn n-pieces [n piece] (run n n (repeat n piece)))

(deftest test-black-box

  ;; homogeneous:
  ;; queens
  (is (= (count-seqs (n-pieces 1 \Q)) 1))
  (is (= (count-seqs (n-pieces 2 \Q)) 0))
  (is (= (count-seqs (n-pieces 3 \Q)) 0))
  (is (= (count-seqs (n-pieces 4 \Q)) 2))
  (is (= (count-seqs (n-pieces 5 \Q)) 10))
  (is (= (count-seqs (n-pieces 6 \Q)) 4))
  (is (= (count-seqs (n-pieces 7 \Q)) 40))
  (is (= (count-seqs (n-pieces 8 \Q)) 92))
  (is (= (count-seqs (n-pieces 9 \Q)) 352))
  ;; bishops
  (is (= (count-seqs (n-pieces 1 \B)) 1))
  (is (= (count-seqs (n-pieces 2 \B)) 4))
  (is (= (count-seqs (n-pieces 3 \B)) 26))
  (is (= (count-seqs (n-pieces 4 \B)) 260))
  (is (= (count-seqs (n-pieces 5 \B)) 3368))
  (is (= (count-seqs (n-pieces 6 \B)) 53744))
  (is (= (count-seqs (run 7 7 (repeat 6 \B))) 692320))
  (is (= (count-seqs (run 8 8 (repeat 6 \B))) 5599888))
  ;; knights
  (is (= (count-seqs (n-pieces 2 \N)) 6))
  (is (= (count-seqs (run 3 3 (repeat 2 \N)))  28))
  (is (= (count-seqs (run 4 4 (repeat 2 \N)))  96))
  (is (= (count-seqs (run 5 5 (repeat 2 \N)))  252))
  (is (= (count-seqs (run 5 5 (repeat 5 \N)))  9386))
  (is (= (count-seqs (run 6 6 (repeat 15 \N))) 2560))
  ;; rooks
  (is (= (count-seqs (n-pieces 1 \R)) 1))

  ;; heterogeneous:
  ;; from the program description
  (is (= (count-seqs (run 3 3 [\K \K \R])) 4)) ;example1
  (is (= (count-seqs (run 4 4 [\R \R \N \N \N \N])) 8)) ;example 2
  ;; made up - not 100% sure that these are right, but a regression should be looked into...
  (is (= (count-seqs (run 5 5 [\Q \B \R \N \K \K])) 1088))
  (is (= (count-seqs (run 6 6 [\Q \B \R \N \K \K])) 180568))
  (is (= (count-seqs (run 9 6 [\Q \B \R \N \K \K])) 20136752)) ;this is the final answer that we are looking for...

  )

;;------------------------------------------------------------------------------
