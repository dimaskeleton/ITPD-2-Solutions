;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Purpose: Check if an interval is empty or not
; Input: Vector -> True or False if the vector is empty 
(define (empty-VINT? low high) (> low high))

;-------------------------------------------------------------------------------------------------------------------
; Question 1

; Purpose: To find the number with the highest value in a vector
; (vectorof number) -> number
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop
(define (Max V)
  (local [; Contract: (max-help(vectorof number)) -> max
          ; Purpose: To find the highest valued number in a vector
          ; max is the highest valued number in the processed
          ; part of the vector [0...low]
          (define (max-help low high max)
            (cond[(empty-VINT? low high) max] 
                 [(> (vector-ref V low) max)
                     (max-help (add1 low) high(vector-ref V low))]
                  [else (max-help (add1 low) high max)]))]
    (max-help 0 (sub1 (vector-length V)) (vector-ref V 0)))) 

; Check-expects for the Max function 
(check-expect (Max (vector 1 2 3))  3)
(check-expect (Max (vector 0)) 0)
(check-expect (Max (vector -1 4 5 6))  6)
(check-expect (Max (vector -1 4000 6000 -15)) 6000)
(check-expect (Max (vector -30 -736 -14 -84 -1234)) -14)
(check-expect (Max (vector -5 912 9731 643 64 92 64 8237 6473)) 9731)

;-------------------------------------------------------------------------------------------------------------------
; Question 2

; Purpose: To find the number with the lowest value in a vector
; (vectorof number) -> number 
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop
(define (Lowest V)
  (local [; Contract (low-help(vectorof number)) -> lowest
          ; Purpose: To find the lowest valued number in a vector
          ; lowest is the lowest number in the processed
          ; part of the vector [0...low]
          (define (low-help low high lowest)
            (cond[(empty-VINT? low high)lowest]
                 [(< (vector-ref V low)lowest)
                     (low-help (add1 low) high(vector-ref V low))]
                 [else (low-help (add1 low) high lowest)]))]
    (low-help 0 (sub1 (vector-length V)) (vector-ref V 0)))) 

; Check-expects for the Lowest function 
(check-expect (Lowest (vector 1 2 3)) 1)
(check-expect (Lowest (vector 0)) 0)
(check-expect (Lowest (vector -1 4 5 6)) -1)
(check-expect (Lowest (vector -1 4000 -15 6000)) -15)
(check-expect (Lowest (vector -30 -736 -14 -84 -1234)) -1234)
(check-expect (Lowest (vector -5 912 9731 643 -645 64 92 64 8237 6473)) -645)

;-------------------------------------------------------------------------------------------------------------------
; Optional (question 1 and 2 combined)

; Purpose: To find the lowest or highest value of a vector with the greater than or lower than sign (< >)
; Input: (max-and-min(vectorof number)) (> or <) -> number
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop
(define (max-and-min V sign)
  (local [
          ; Contract: (vectorof number) (> or <) -> value 
          ; Purpose: To find the lowest or highest valued number in a vector 
          ; value is the lowest or highest valued number in the processed part of the vector [0...low]
         (define (low-help low high value)
          (cond[(empty-VINT? low high)value]
                 [(sign (vector-ref V low)value)
                   (low-help (add1 low) high(vector-ref V low))]
                 [else (low-help (add1 low) high value)]))]
    (low-help 0 (sub1 (vector-length V)) (vector-ref V 0))))


; Check-expects for the max-and-min function finding the number with the lowest value 
(check-expect (max-and-min (vector 1 2 3) <)  1)
(check-expect (max-and-min (vector 0) <) 0)
(check-expect (max-and-min (vector -1 4 5 6)<) -1)
(check-expect (max-and-min (vector -1 4000 -15 6000)<) -15)
(check-expect (max-and-min (vector -30 -736 -14 -84 -1234)<) -1234)
(check-expect (max-and-min (vector -5 912 9731 643 -645 64 92 64 8237 6473)<) -645)


; Check-expects for the max-and-min function finding the number with the highest value 
(check-expect (max-and-min (vector 1 2 3)>)  3)
(check-expect (max-and-min (vector 0)>) 0)
(check-expect (max-and-min (vector -1 4 5 6)>)  6)
(check-expect (max-and-min (vector -1 4000 6000 -15)>) 6000)
(check-expect (max-and-min (vector -30 -736 -14 -84 -1234)>) -14)
(check-expect (max-and-min (vector -5 912 9731 643 64 92 64 8237 6473)>) 9731)

;-------------------------------------------------------------------------------------------------------------------
; Question 3

; Purpose: To count the number of elements that are a multiple of 10 in a vector
; (vectorof number) -> number
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop
(define (multiple-ten V) 
  (local [; Contract: (ten-help(vectorof number)) -> value
          ; Purpose: return the number of values in a vector that are multiples of 10
          ; Value is the holder of values in the processed part of the vector [0...low]
          (define(ten-help low high value) 
            (cond[(empty-VINT? low high) value]
                 [(= (remainder (vector-ref V low) 10) 0) 
                  (ten-help (add1 low) high (add1 value))] 
                 [else (ten-help (add1 low) high value)]))] 
    (ten-help 0 (sub1 (vector-length V)) 0)))


; Check-expects for the multiple-ten function 
(check-expect (multiple-ten (vector 10 5 30 9 50 60 70 80 100)) 7) 
(check-expect (multiple-ten (vector 5 7 1 3 9 7 15)) 0)
(check-expect (multiple-ten (vector 1 2 3 4 50 6)) 1) 
(check-expect (multiple-ten (vector 10 2 130)) 2)
(check-expect (multiple-ten (vector 7 46 92 110 74 -170 5 730)) 3)
(check-expect (multiple-ten (vector 875 321 678 948 2578)) 0)

;-------------------------------------------------------------------------------------------------------------------
; Question 4

; Purpose: a symbol and that returns true if the given symbol is in the vector and false otherwise
; (vectorof symbol) symbol -> boolean
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop
(define (match-symbol V sym)
  (local [; Contract: (match-help(vectorof symbol)) symbol -> boolean
          ; Purpose: return true or false if the given symbol matches any of the symbols in the vector
          (define(match-help low high)
            (cond[(empty-VINT? low high) #false]
                 [(equal?(vector-ref V low) sym) #true]
                 [else(match-help (add1 low) high)]))]
    (match-help 0 (sub1(vector-length V)))))

; Check-expects for the match-symbol function
(check-expect (match-symbol (vector 'smiling 'stars 'rocketship ) 'smiling) #true) 
(check-expect (match-symbol (vector "" "" '"" ) 'test) #false)  
(check-expect (match-symbol (vector 'smiling 'frowning 'friends ) 'smiling) #true) 
(check-expect (match-symbol (vector 'enchanted 'forest 'quest ) 'smiling) #false) 
(check-expect (match-symbol (vector 'compsci 'racket 'rocketship 'cars 'plane) 'car) #false) 
(check-expect (match-symbol (vector 'compsci 'racket 'rocketship 'cars 'plane) 'racket) #true)
