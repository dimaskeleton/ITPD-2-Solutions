;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Short-Dimitri, Diskant-Derek, Presinal-Gregori, Attalla-Maryam, Dunn-Kyle, Loop Exercise 16 (quiz)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require while)

; [+ natnum natnum] -> natnum
; Purpose: Add two natural numbers using repetetive addition
; Termination: Program always terminates because when a reaches 0, the loop stops
(define (plus a b)
  (local [
          #| Signatures? I do not understand your purpose statements. |#
          
          ;Purpose: Calculates distance from 0 from the given natural number
          (define x (void))
          ;Purpose: Calculates the addition being made within the while loop
          (define accum (void))]

    ;Accumulator invariant accum = sum of numbers, x - 1 (until = 0)
    (begin
      (set! x a)
      (set! accum b)
      (while (not (= x 0))
             (set! accum (+ accum 1))
             (set! x (- x 1)))
      accum)))


; Tests for the plus function
(check-expect(plus 5 5) 10)
(check-expect(plus 2 3) 5)
(check-expect(plus 0 0) 0)
(check-expect(plus 258 258) 516)
(check-expect(plus 42 86) 128)
(check-expect(plus 8162002 8162002) 16324004)
(check-expect(plus 1403 9137) 10540)
(check-expect(plus 30 50) 80)