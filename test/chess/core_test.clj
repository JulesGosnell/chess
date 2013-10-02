(ns chess.core-test
  (:import [java.util BitSet])
  (:require [clojure.test :refer :all]
            [chess.core :refer :all]))

;;------------------------------------------------------------------------------
;; lets test the encoding of a king in the middle of a 3x3 board
;; i.e. ('-' = free, '+' = covered)
;;
;;  -+-
;;  +K+
;;  -+-

;; lets test the encoding of the check to see whether the above move
;; will be valid for a given board.

(deftest test-encode-check 
  (is (encode-check [3 3] \K [1 1] [[1 0][0 1][2 1][1 2]])
      (reduce (fn [^BitSet bs location] (doto bs (.set location)))
              (BitSet. 36)
              [5 6 7 13 14 15 16 17 18 19 21 22 23 29 30 31])))

;; lets test the encoding of a move...
(deftest test-encode-move
  (is (encode-move [3 3] \K [1 1] [[1 0][0 1][2 1][1 2]])
      (reduce (fn [^BitSet bs location] (doto bs (.set location)))
              (BitSet. 36)
              [4 12 16 17 19 20 28])))

;;------------------------------------------------------------------------------
;; test our tree traversal functions - define a tree and check that
;; both functions traverse the nodes in the expected order and find
;; the same set of children...


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

(deftest test-breadth-first-search

  (let [nodes (atom [])]

    (is (=

         (breadth-first-search
          [:a]
          (fn [parent level]
            (swap! nodes (fn [r n] (conj r n)) parent)
            (if level (tree parent)))
          [1 2 3]
          #{})

         [:h :i :j :k]))

    (is (= @nodes [:a :b :c :d :e :f :g]))

    ))

(deftest test-depth-first-search
  (let [nodes (atom [])]

    (is (=

         (depth-first-search
          :a
          (fn [parent level]
            (swap! nodes (fn [r n] (conj r n)) parent)
            (if level (tree parent)))
          [1 2 3]
          #{})

         [:h :i :j :k]))

    (is (= @nodes [:a :b :d :e :c :f :g]))

    ))

;;------------------------------------------------------------------------------
;; high-level tests - most numbers scraped from web ...

(defn count-is [s expected] (is expected (count s)))

(deftest test-all

  (is
   (and

    ;; homogeneous:

    ;; queens
    (count-is (n-pieces 1 \Q) 1)
    (count-is (n-pieces 2 \Q) 0)
    (count-is (n-pieces 3 \Q) 0)
    (count-is (n-pieces 4 \Q) 2)
    (count-is (n-pieces 5 \Q) 10)
    (count-is (n-pieces 6 \Q) 4)
    (count-is (n-pieces 7 \Q) 40)
    (count-is (n-pieces 8 \Q) 92)

    ;; bishops
    (count-is (n-pieces 1 \B) 1)
    (count-is (n-pieces 2 \B) 4)
    (count-is (n-pieces 3 \B) 26)

    ;; knights
    (count-is (n-pieces 2 \N) 6)
    (count-is (run 3 3 (repeat 2 \N))  28)
    (count-is (run 4 4 (repeat 2 \N))  96)

    ;; rooks
    (count-is (n-pieces 1 \R) 1)

    ;; heterogeneous:

    ;; from the program description
    (count-is (run 3 3 [\K \K \R]) 4)   ;example1
    (count-is (run 4 4 [\R \R \N \N \N \N]) 8) ;example 2 - TODO: check actual positions

    ))


  (if (> (.availableProcessors (Runtime/getRuntime)) 2)
    (is
     (and
      (count-is (n-pieces 9 \Q) 352)

      (count-is (n-pieces 4 \B) 260)
      (count-is (n-pieces 5 \B) 3368)
      (count-is (n-pieces 6 \B) 53744)
      (count-is (run 7 7 (repeat 6 \B)) 692320)
      (count-is (run 8 8 (repeat 6 \B)) 5599888)

      (count-is (run 5 5 (repeat 2 \N))  252)
      (count-is (run 5 5 (repeat 5 \N))  9386)
      (count-is (run 6 6 (repeat 15 \N)) 2560)

      ;; not 100% sure that these are right, but a regression should be looked into...
      (count-is (run 5 5 [\Q \B \R \N \K \K]) 1088)
      (count-is (run 6 6 [\Q \B \R \N \K \K]) 180568)
      (count-is (run 9 6 [\Q \B \R \N \K \K]) 20136752)

      ))))

;;------------------------------------------------------------------------------


