;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |NQueens SOLVED|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment bank/search-p3)

(@problem 1)
;; This project involves the design of a program to solve the n queens puzzle.
;;
;; This starter file explains the problem and provides a few hints you can use
;; to help with the solution.
;;
;; The key to solving this problem is to follow the recipes! It is a challenging
;; problem, but if you understand how the recipes lead to the design of a Sudoku
;; solve then you can follow the recipes to get to the design for this program.
;;  
;;
;; The n queens problem consists of finding a way to place n chess queens
;; on a n by n chess board while making sure that none of the queens attack each
;; other. 
;;
;; The BOARD consists of n^2 individual SQUARES arranged in 4 rows of 4 columns.
;; The colour of the squares does not matter. Each square can either be empty
;; or can contain a queen.
;;
;; A POSITION on the board refers to a specific square.
;;
;; A queen ATTACKS every square in its row, its column, and both of its
;; diagonals.
;;
;; A board is VALID if none of the queens placed on it attack each other.
;;
;; A valid board is SOLVED if it contains n queens.
;;
;;
;; There are many strategies for solving nqueens, but you should use the
;; following:
;;  
;;   - Use a backtracking search over a generated arb-arity tree that
;;     is trying to add 1 queen at a time to the board. If you find a
;;     valid board with 4 queens produce that result.
;;
;;   - You should design a function that consumes a natural - N - and
;;     tries to find a solution.
;;    
;;    
;;    
;; NOTE 1: You can tell whether two queens are on the same diagonal by comparing
;; the slope of the line between them. If one queen is at row and column
;; (r1, c1) and another queen is at row and column (r2, c2) then the slope of
;; the line between them is: (/ (- r2 r1) (- c2 c1)).
;; If that slope is 1 or -1 then the queens are on the same diagonal.


;(@problem 1)
;; This version has data definitions, signature, purpose, tests and a couple
;; helpers.


;; =================
;; Data definitions:

(@htdd Position)
;; Position is Natural
;; interp. positions on the board
;;         if    N is the number of queens
;;         then  (sqr N) is the number of positions on the board
;;         so    this number should be in [0, (- (sqr N) 1)]
(define P1 0)        ;upper left corner of board
(define P2 (- 16 1)) ;lower left corner of 4x4 board


(@htdd Board)
;; Board is (listof Position)  up to N elements long
;; interp. the positions of the queens that have been placed on the board
(define BD1 empty)           ;no queens placed
(define BD2 (list 0))        ;one queen in upper left corner
(define BD3 (list 14 8 7 1)) ;a solution to 4x4 puzzle 



;; =================
;; Functions:

;; NOTE about following function. It could have been designed with N as a
;; top-level constant and all the locally defined functions as top-level
;; functions. But doing it the way we have done below, makes it easy to make
;; the top-level nqueens function consume N which is kind of nice.  The
;; trampoline starts the actual search out by calling fn-for-bd with an empty
;; board.

(@htdf nqueens)
(@signature Natural -> Board or false)
;; produce first found solution for n queens of size N; or false if none exists
(check-expect (nqueens 1) (list 0))
(check-expect (nqueens 2) false)
(check-expect (nqueens 3) false)
(check-expect (nqueens 4) (list 14 8 7 1))
(check-expect (nqueens 5) (list 23 16 14 7 0))
(check-expect (nqueens 6) (list 34 26 18 17 9 1))
(check-expect (nqueens 7) (list 47 38 29 27 18 9 0))
(check-expect (nqueens 8) (list 59 49 46 34 29 23 12 0))


(define (nqueens N)
  (local [(define (fn-for-bd bd)
            (if (solved? bd)
                bd
                (fn-for-lobd (next-boards bd))))

          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else (local [(define try (fn-for-bd (first lobd)))]
                          (if (not (false? try))
                              try
                              (fn-for-lobd (rest lobd))))]))

        



          
          (define (solved? bd) (= (length bd) N))
          ;; Are there N queens placed on the board? Only valid boards exist
          ;; So there is no Board that is invalid that could be fed to this
          ;; function.

          ;(@signature Board -> Boolean)


          (define (next-boards bd)
            (map (lambda (p) (cons p bd))
              (filter (lambda (p) (not-attacks-existing-queen? p bd))
                    (build-list (sqr N) identity))))

          ;; First we create all the possible positions
          ;; We remove pos that has attacking queens so only valid pos exist
          ;; We add valid pos to bd, new board has the new queen


          (define (not-attacks-existing-queen? p bd)
            (andmap (lambda (eq)
                      (not (attack? eq p)))
                    bd))

          

          
                        
      
          
          ;; Position Position -> Boolean
          ;; produce true if queens at position a and b attack each other
          (define (attack? pa pb)
            (local [(define x1 (pos-x pa))
                    (define y1 (pos-y pa))
                    (define x2 (pos-x pb))
                    (define y2 (pos-y pb))]
              (or (= x1 x2)                           ;same row
                  (= y1 y2)                           ;same column
                  (= (/ (- y2 y1) (- x2 x1))  1)      ;same slope  1 diagonal
                  (= (/ (- y2 y1) (- x2 x1)) -1))))   ;same slope -1 diagonal
          
          
          ;; Pos -> Natural
          ;; produce the row or column number in [0, N) for the given position
          (define (pos-x p) (remainder p N))
          (define (pos-y p) (quotient  p N))]
    
    (fn-for-bd empty)))
















