;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
;;Refactor the following function to be imperative:
;;lon → number Purpose: Find the max of the given list

;;Termination: Program always terminates because it will always reach the end of the list
(define (max-lon a-lon)
  (local [
          (define ACUMM (void))
          (define lst   (void))
          ;; lon number → number
          ;; Purpose: Return max(max of given list, given number)

          ;; Accumulator Invariant: accum = maximum in L - lst
          (define (max-helper)
            (if(empty? lst)
                ACUMM
                (begin
                  (set! ACUMM (largest? ACUMM (first lst))) 
                  (set! lst (rest lst)) 
                  (max-helper))))]
     
    (if (empty? a-lon)
        (error 'max-lon "An empty lon does not have a maximum.") 
        
        (begin
          (set! ACUMM(first a-lon))
          (set! lst (rest a-lon))
          (max-helper))))) 



; Purpose: Return the highest of two numbers recieved
; number number -> number (highest)
; Input: Two numbers from a list passed through max-lon above
(define (largest? a b)
  (if (> a b) a b))


; Tests for the largest? function
(check-expect (largest? 1 2) 2)
(check-expect (largest? 1 56) 56)
(check-expect (largest? 0 0) 0)
(check-expect (largest?  832 -9837) 832)
(check-expect (largest? 83 77 ) 83)
(check-expect (largest? 258 852) 852)
(check-expect (largest? 5 -21049) 5)
(check-expect (largest? 2000000002 200000000000000001) 200000000000000001)

; Tests for the max-lon function
(check-error (max-lon '()) "max-lon: An empty lon does not have a maximum.")
(check-expect (max-lon '(0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 1)) 1)
(check-expect (max-lon '(-10 -20 -30 -40 -50 -60 -70 -80 -90 -100)) -10)
(check-expect (max-lon '(10 20 30 40 50 60 70 80 90 100)) 100) 
(check-expect (max-lon '(10 6873 258 852 1000 100 200)) 6873)
(check-expect (max-lon '(90 63 258 44 10 50 1)) 258)
(check-expect (max-lon '(64 14 84 41 85 763 427 323 4)) 763)
(check-expect (max-lon '(5 64 92 12 43 66)) 92)

