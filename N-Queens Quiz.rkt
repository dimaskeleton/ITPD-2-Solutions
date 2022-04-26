;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Short-Dimitri N-Queens Problem Bonus Quiz--LATE|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/abstraction)

; Data Defintions
; A board position, bpos, is a structure, (make-posn a b), where a and be are natnums
; Examples
(define P1(make-posn 3 2))
(define P2(make-posn 0 0))

; natnum --> (listof bpos)
; Purpose: To returns a list of all the positions in a nxn board
(define(total-board n)
  (for*/list [(i n)(j n)](make-posn i j)))

; natnum --> (listof bpos) or false
; Purpose: To place n queens on a nxn board; otherwise return false
(define(place-queens n)
  (place-them(total-board n) n))

; (listof bpos) natnum --> (listof bpos) or false
; Purpose: To place n queens in the given list of unthreatened positions
; This will terminate because we will eventually run out of queens or places to put the first queen.
(define (place-them valid-positions n)
  (cond [(and(empty? valid-positions)(> n 0))#false]
        [(= n 0) '()]
        [else
         (local((define sol-other-queens (place-them
                                           (rm-threatened(first valid-positions)
                                                         (rest valid-positions))
                                          (sub1 n))))
           (cond[(false? sol-other-queens)(place-them (rest valid-positions) n)]
                [else(cons (first valid-positions)sol-other-queens)]))]))

; (listof bpos) natnum --> (listof bpos) or false
; Purpose: To place n queens in the given list of unthreatened positions
; Algorithm: ...
; Termination: At each step, either or both the number of queens is
; reduced by 1 and the remaining number of unthreatened
; positions is reduced. Eventually, either the number of
; queens becomes 0 or the unthreatened positions becomes
; empty and the program terminates

; bpos (listof bpos) --> (listof bpos)
; Purpose: To remove the positions in the given list that are
; threatened by the given bpos.;;;;;;;;;;;;FIX THIS
(define(rm-threatened a-bpos lob)
  (local(
          ; bpos --> boolean
          ; Purpose: To determine if the given bpos is threatened by a-bpos
          ; Note: A bpos, target, is threatened by a bpos B if target and B
          ; are in the same column, in the same row, or if the difference
          ; between the xs is equal to the difference between the ys
          ; (i.e., target is on a diagonal from B).
          (define(threatened? target)
            (or(=(posn-x target)(posn-x a-bpos))
               (=(posn-y target)(posn-y a-bpos))
               (=(abs(-(posn-x target)(posn-x a-bpos)))
                  (abs(-(posn-y target)(posn-y a-bpos)))))))
    (filter(lambda(p)(not(threatened? p)))lob)))

;valid-sol?: to see if a postition is a valid solution to the board
;Input: lop -> Output: boolean
;This will terminate because it will eventually run out of valid solutions and the recursion stops
(define(valid-sol? a-lop)
  (local[(define(threatened posn1 posn2)
            (or (=(posn-x posn1)(posn-x posn2))
                (=(posn-y posn1)(posn-y posn2))
                (=(abs (-(posn-x posn1)(posn-x posn2)))
                  (abs (-(posn-y posn1)(posn-y posn2))))))
         ;Any-threantened: Checks if any queen pieces threantened by other pieces
         ;Input posn, lop -> Output: boolean
         (define (any-threatened? posn1 lop)
          (ormap (lambda (p)(threatened posn1 p))lop))]
    (cond[(empty? a-lop)#true]
         [else(and(not (any-threatened?(first a-lop)(rest a-lop)))
                  (valid-sol? (rest a-lop)))])))
    
         

; Tests to ensure the program works to spec 
(check-expect (valid-sol? (place-queens 0)) #true)
(check-expect (valid-sol? (place-queens 1)) #true)
(check-expect (place-queens 2) #false)
(check-expect (place-queens 3) #false)
(check-expect (valid-sol? (place-queens 4)) #true)
(check-expect (valid-sol? (place-queens 5)) #true)
(check-expect (valid-sol? (place-queens 8)) #true)