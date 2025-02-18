; Question
; 
; Design and implement a function using a while loop that takes as
; input a natural number n and that returns the product of the
; first n integers.
; 
; Example:
;     whhen n = 4 the function returns 1 * 3 * 5 * 7 = 105
;     when n = 6 the function returns 1 * 3 * 5 * 7 * 9 * 11 = 10395
; 
; Follow all steps of the design recipe 


(require while) 

; Purpose: return the product of the amount of odd numbers with a set given number 
; Input: number of odd numbers to multiply 
; Termination: program terminates because the accumulator will always reach the input number and terminate  
(define (product n) 
  (local [ 
          ; Counter: The placeholder for the odd numbers to multiply | Starts at 1 to get odd numbers 
          (define counter 1)  
          ; Accum: Accumulator for the while loop to know when to stop  
          (define accum 0) 
          ; Total: The product total after the loop finishes | Starts at 1 to avoid multiplication by 0  
          (define total 1)] 
    ; Invariant: accum is the index so the while-loop terminates 
    ; Invariant: counter is the index of each odd number that will be multiplied 
    ; Invariant: total is the final result and is multiplied by itself along with the new odd number 
    (begin 
      (while (< accum n) 
             (set! accum (+ accum 1)) 
             (set! total (* total counter)) 
             (set! counter (+ counter 2))) 
      total)))   


; Tests for the product function
(check-expect (product 4) 105)  
(check-expect (product 6) 10395) 
(check-expect (product 10) 654729075) 
