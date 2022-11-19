;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Final Exam |) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
; Purpose: Check if an interval is empty or not
; Input: Vector -> True or False if the vector is empty 
(define (empty-VINT? low high) (> low high))

; Exercise 7 (Integer Length)-----------------------------------------------------------------------------------

; Purpose: To find the number with the highest integer length in a vector
; (vectorof number) -> number
; Termination: This terminates because the function will reach the end of the vector and the recursion will stop.
; digitnum will read if n is less then 10 and will stop dividing and return the amount of times it divided to find the length.
(define (longest-integer V)
  (local [ 
          
          ;Contract:(digitnum (vectorof numbers)-> amount of digits in highest number in the vector
          ;Purpose: digitnum, finds the digits of the inputed number 
          ;accumulator invariant: Temp is the current amount of times n has been divided
          ;by 10 until it is equal to or less then 10 and returns that amount
          (define (digitnum n temp)
            (cond [(< n 10)temp]
                  [else (digitnum (/ n 10) (add1 temp))]))
          
          ; Contract: (integer-help (vectorof number)) -> longest integer length
          ; Purpose: To find the highest valued number in a vector        
          ; max is the highest valued number in the processed
          ; part of the vector [0...low]
          (define (integer-help low high max)
            (cond[(empty-VINT? low high) max] 
                 [(> (vector-ref V low) max)
                  (integer-help (add1 low) high(vector-ref V low))]
                 [else (integer-help (add1 low) high max)]))]
    (digitnum (integer-help 0 (sub1 (vector-length V)) (vector-ref V 0)) 1))) 

;check-expects for longest-integer function
(check-expect (longest-integer (vector 123 1234)) 4)
(check-expect (longest-integer (vector 8166923)) 7)
(check-expect (longest-integer (vector 1 10 100 1000 500 20 674)) 4)
(check-expect (longest-integer (vector 0 0 0 0 0)) 1)
(check-expect (longest-integer (vector 1 5 9 3 23 4 7)) 2)
(check-expect (longest-integer (vector 3003 3002 3001 30003)) 5)
(check-expect (longest-integer (vector 1000000 -2000000)) 7)
(check-expect (longest-integer (vector 3765 3934 9212)) 4)



; Exercise 8 (Interface of Buckets)-----------------------------------------------------------------------------------

;Bucket-D is a vector
;Bucket-V is a bucket
;Contract: (vector)-> (vector)
;Purpose: an interface for a bucket of integers. A bucket of
;integers offers the following services: add!, dump!, size and elems.
(define (make-bucket num) 
  (local [
          ;bucket-V holds numbers in bucket currently
          ;Contract: number-> bucket
          ;num: is the number inputted
          (define bucket-V (build-vector num (lambda (i) (void))))
          
          ;index: keeps track of next available slot in the bucket 
          (define index 0)
          
          ;Contract: add!-bucket: vector->bucket
          ;Purpose: Purpose: mutates a bucket to add a given integer
          (define (add!-bucket num)
            (begin
              (vector-set! bucket-V index num)
              (set! index (add1 index))))
          
          ;set!: adds one to the index which starts at 0 to 
          ;Contract: vector->bucket
          ;Purpose: dump!-bucket: dumps the bucket elements into the given vector starting; bucket-d
          ;dump-index: Dumps numbers before Num to vector
          (define (dump!-bucket D dump-index)
            (local[
                   (define (dump-helper dlow dhigh vlow vhigh)
                     (cond [(empty-VINT? vlow vhigh) (void)]              ; check if low is greater than high of bucket V 
                           [(empty-VINT? dlow dhigh) (error "It's Empty")]; check if low is greater than high of bucket D
                           [else  
                            (begin
                              (vector-set! D dlow (vector-ref bucket-V vlow))
                              ;set!  sets Bucket-V to the lowest integer of the Bucket          
                              (vector-set! bucket-V vlow (void))
                              ;set! sets bucket-D to the lowest integer of the vector
                              (dump-helper (add1 dlow) dhigh (add1 vlow) vhigh))]))]
              (begin
                (dump-helper dump-index (sub1 (vector-length D)) 0 (sub1 index)) 
                (set! index 0))))
          
          ;Purpose: Manager: Manages the add!, dump!, size,
          ;and  elems of a bucket
          (define (manager a-mess)
            (cond [(equal? a-mess 'add) add!-bucket]
                  [(equal? a-mess 'dump) dump!-bucket]
                  [(equal? a-mess 'size) index]
                  [(equal? a-mess 'elems)  bucket-V]
                  [else (error "bucket unkown")]))]
    manager))

;Wrapper functions
;Purpose:  mutates a bucket to add a given integer
;Contract: Interger bucket -> (void)
;Effect: Bucket-V at the Index recieved value of n and the index index is increased
;value = V number we add
;Bucket = B the bucket with the numbers in it 
(define (bucket-add! B V)
  ((B 'add) V))

;Contract: bucket-> (void)
;Purpose: Move all bucket numbers into the vector
;Effect: moves digits to the vector to be dumped 
(define (bucket-dump! dump-index D index)
  ((dump-index 'dump) D index))
  
; bucket-size!: bucket -> number
; Purpose: gives next spot in the bucket
(define (bucket-size! B)
  (B 'size))


; bucket-elems: the bucket -> vectorof num
; Purpose: gives the vector with bucket elements that are inside it
(define (bucket-elems! B)
  (B 'elems))

;-----------------Following are test cases for the make-bucket function----------------
(define Buck1 (make-bucket 10))
(define Buck2 (build-vector 10 (lambda (i) 0)))
(define Buck3 (build-vector 10 (lambda (i) (make-bucket 10))))


(check-expect (begin
                (bucket-add! Buck1 40)
                (bucket-add! Buck1 97)
                (bucket-dump! Buck1 Buck2 0)
                Buck2)
              (vector 40 97 0 0 0 0 0 0 0 0))

(check-expect (bucket-size! Buck1) 0)
(check-expect (bucket-elems! Buck1) (vector
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)
                                     (void)))

; Exercise 9 (Radix Sort)-----------------------------------------------------------------------------------

; Purpose: To sort a vector using radix algorithm based on taking in digits
; Termination: Once the vector is sorted out, the function terminates 
(define (radix-sort! V)
  (local [
          (define buc-vec (build-vector 10 (lambda (i) (make-bucket (vector-length V)))))
          
          ; Radix sort takes n steps, the length of the vector is the amount
          ; of steps that it will take to sort it completley 
          (define n (void))
          
          ; Purpose: To count the amount of steps so far
          (define counter (void))

          ; Purpose: To keep track of the digits to bucketize
          (define digit (void))
          
          ; bucketize: takes a number in order in order to bucketize it (vectorof number) number -> (void)
          ; Purpose: To bucketize the elements of a vector based on the current digit
          (define (bucketize low dig)
            (if  (> low (sub1 (vector-length V))) (void) 
                 (begin (bucket-add!
                         (vector-ref buc-vec (modulo (floor (/ (vector-ref V low) dig)) 10))
                         (vector-ref V low))
                        (bucketize (add1 low) dig))))

          
          ; debucketize: dumps the bucket of numbers into the vector in order (vectorof number) (vectorof bucket) -> (void) 
          ; Purpose: To dump each bucket of the vector of buckets into the vector of number in order 
          (define (debucketize delow index)
            (if (empty-VINT? delow (sub1 (vector-length buc-vec))) (void)
                (local [
                        (define new-index (+ (bucket-size! (vector-ref buc-vec delow)) index))]
                  (begin
                    (bucket-dump! (vector-ref buc-vec delow) V index)
                    (debucketize (add1 delow) new-index)))))

          ; longest-length: finds the integer with the longest length (vectorof number) -> number
          ; Purpose: To find the length of the integer in a vector
          (define (longest-length V)
            (local [
                    ; integer-help: natnum natnum number -> number
                    ; Purpose: To find the highest number in the vector
                    ; accum is the largest number found
                    (define (integer-help low high accum)
                      (cond [(empty-VINT? low high) accum] 
                            [(> (vector-ref V low) accum) (integer-help (add1 low) high (vector-ref V low))]
                            [else (integer-help (add1 low) high accum)]))
                    ; digit-counter: num -> num
                    ; Purpose: Count the number of digits in a number
                    (define(digit-counter num accum)
                      (if (< num 10) (+ 1 accum) (digit-counter (/ num 10) (+ 1 accum))))]
              (digit-counter (integer-help 0 (sub1 (vector-length V)) 0) 0)))

          ; Purpose: To actually sort out the vector based on all of its digits hence it
          ; being radix sort
          (define (sort! counter n digits)
            (cond [(> counter n) (void)]
                  [else (begin 
                          (bucketize 0 digits)
                          (debucketize 0 0)
                          (sort! (add1 counter) n (* digits 10)))]))]
    (sort! 1 (longest-length V) 1)))

;-----------------Following are test cases for radix-sort! function----------------

(define shovel1 (vector 989 87 69 58 111 9))
(define shovel2 (vector 0))
(define shovel3 (vector 19 95 21 9 10 8 -1))
(define shovel4 (vector -19 -95 -21 -9 -10 -8 -1))
(define shovel5 (vector 1287436 912637123 045401 27328 340102 293 9238203 19823))

(check-expect (begin (radix-sort! shovel1) shovel1) (vector 9 58 69 87 111 989))
(check-expect (begin (radix-sort! shovel2) shovel2) (vector 0))
(check-expect (begin (radix-sort! shovel3) shovel3) (vector 8 9 10 19 21 95 -1))
(check-expect (begin (radix-sort! shovel4) shovel4) (vector -10 -19 -9 -8 -95 -21 -1))
(check-expect (begin (radix-sort! shovel5) shovel5) (vector 293 19823 27328 45401 340102 1287436 9238203 912637123))

; Exercise 11 (Insertion Sort)-----------------------------------------------------------------------------------

; To sort a vector by going through it and sorting it each time a lower number is found
; and placing it from lowest to highest based on the vector of numbers
; Termination: Once the vector is fully sorted, the function stops
(define (insertion-sort! V)
  (local [
          ; Purpose: To switch places with two given numbers in the vector
          (define (switch x y)
            (local [
                    (define holder (vector-ref V x))]
              (begin
                (vector-set! V x (vector-ref V y))
                (vector-set! V y holder)))) 
          
          ; Purpose: To sort the elements in a vector in increasing order
          ; based on whats being given an passed through 
          (define (insert! low high)
            (cond [(empty-VINT? low high) (void)]
                  [(<= (vector-ref V low) 
                       (vector-ref V (add1 low)))
                   (void)] 
                  [else (begin (switch low (add1 low)) 
                               (insert! (add1 low) high))]))

          ; Purpose: To sort the vector in entiritey in the given vector
          ; getting help from the functions above 
          (define (sort! low high)
            (cond [(empty-VINT? low high) (void)]
                  [else (begin
                          (sort! (add1 low) high)
                          (insert! low (sub1 high)))]))] 
    (sort! 0 (sub1 (vector-length V))))) 

;-----------------Following are test cases for insertion-sort! function----------------

(define test1 (vector 5 1 3 7 2))
(define test2 (vector 486 358 124 123 894 265 579 218))
(define test3 (vector 12 897 6544 15456 231 789 8921))
(define test4 (vector 6218 7319 9148 2637 4962 5413 4217)) 
(define test5 (vector -1287 -762 -359 -2385 -8423 -624))


(check-expect (begin
                (insertion-sort! test1)
                test1)
              (vector 1 2 3 5 7))

(check-expect (begin
                (insertion-sort! test2)
                test2)
              (vector 123 124 218 265 358 486 579 894))

(check-expect (begin
                (insertion-sort! test3)
                test3)
              (vector 12 231 789 897 6544 8921 15456)) 

(check-expect (begin
                (insertion-sort! test4)
                test4)
              (vector 2637 4217 4962 5413 6218 7319 9148))

(check-expect (begin
                (insertion-sort! test5)
                test5)
              (vector -8423 -2385 -1287 -762 -624 -359))

; Exercise 12 (Imperical Study (using radix and insertion sort from above))-------------------------------------------

; Radix Sorting Times
(define (radix-timing n) 
  (cond [(> n 20000) (void)] 
        [else (begin
                (display (format "Radix Sort ~a" n))
                (newline)
                (time (radix-sort! (build-vector n (lambda (i) (random 10000)))))
                (newline)
                (radix-timing (+ n 500)))]))

(radix-timing 500)

; Insertion Sorting Times
(define (insertion-timing n) 
  (cond [(> n 20000) (void)] 
        [else (begin
                (display (format "Insertion Sort ~a" n))
                (newline)
                (time (insertion-sort! (build-vector n (lambda (i) (random 10000)))))
                (newline)
                (insertion-timing (+ n 500)))]))

(insertion-timing 500) 