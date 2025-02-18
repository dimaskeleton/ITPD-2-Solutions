; The 32 first lines are given in the problem

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
(define M ; an SOE (system of equations)
  (list (list 2 2 3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2 1)))

(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(define (lhs e)
  (reverse (rest (reverse e))))

(check-expect (lhs (first M)) '(2 2 3))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(define (rhs e)
  (first (reverse e)))

(check-expect (rhs (first M)) 10)

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 462

; lhs --- (3 3 10) 3x + 3y + 3z
; sol --- (2 4 6) x = 2, y = 4, z = 6
; Purpose: Plugs in the solution to the left hand side of the system of equations
; Input: lhs and a solutions -> Output: NUM
(define (plug-in lhs sol)
  (cond[(empty? lhs) 0] ; Returns 0 if lhs (first list) is empty as theres nothing to plug in
       [else (+ (*(first lhs)(first sol)) (plug-in (rest lhs) (rest sol)))] ; calls the list (lhs) and multiplies each value by the value 
       ))                                                                   ; in the same position in the second list (sol) and adds it all together

; Tests for the plug-in function 
(check-expect (plug-in '(1 2 3) '(4 5 6)) 32)
(check-expect (plug-in '(8 11 4) '(2 5 9)) 107)


; checks the solution of SOE SOL
; consumes an SOE and a solution (SOL) and returns true if the the numbers from the
; solution equal the left hand side values through the plug-in function, if not it returns false
(define(check-solutions SOE SOL)
  (andmap (lambda (x) (= (plug-in (lhs x) SOL) (rhs x))) SOE))

; Tests for the check-solutions function
(check-expect (check-solutions M S) #true)
(check-expect (check-solutions M '(2 1 1)) #false)
(check-expect (check-solutions M '(0 0 0)) #false)

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 463

; M2 is a given test equations in the problem for the check-solution function
; Purpose: to hand check the function check-solutions from the previous question 
(define M2
  '((2 2 3 10)
    (0 3 9 21)
    (4 1 -2 1)))

; Test for the M2 variable being passed through the check-solutions function
(check-expect (check-solutions M2 S) #true)

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 464

; M3 is a given test equations in the problem for the check-solution function
; Purpose: to also hand check the function check-solutions from exercise 462 with different values
; mostly being with more negative numbers and lists starting with 0
(define M3
  '((2 2 3 10)
    (0 3 9 21)
    (0 -3 -8 -19)))

; Test for the M3 variable being passed through the check-solutions function 
(check-expect (check-solutions M3 S) #true)

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 465

; Purpose: Subtracts two equations until first number in new equation is 0 by substracting and dividing
; Input: two equations -> Output: one equation with the first number being 0
(define (subtract row1 row2)
  (rest 
   (map (lambda(R1 R2) (- R2 (* R1 (/ (first row2) (first row1))))) row1 row2)))

; Test for the subtract function
(check-expect (subtract (list 2 2 3 10) (list 2 5 12 31)) (list 3 9 21))
(check-expect (subtract '(2 3 -2 3) '(4 -2 2 4)) '(-8 6 -2))

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 466-468 (Modified triangulation function)

; Problem notes:
; ; A TM is an [NEList-of Equation]
; ; such that the Equations are of decreasing length: 
; ;   n + 1, n, n - 1, ..., 2. 
; ; interpretation represents a triangular matrix


; Purpose: to triangulate the given SOE (system of equations)
; triangulate takes in SOE gives out matrix
(define (triangulate M)
  (local[(define top (first M))] ; local variable to manage the list thats being passed through
  (cond [(empty? (rest M))
         (list top)] ; checks to make sure that the list is not empty
        [(andmap (lambda (X) (zero? (first X)))M)(error "all coefficients are 0")] ; returns this error if all the values in a list are zero
        [(zero? (caar M)) (triangulate (append (rest M) (list (first M))))] ; rotates through the list to find a list where a leading coefficient is not zero
        [else
         (cons top 
               (triangulate 
                (map(lambda(x) (subtract top x)) (rest M))))]))) ; triangulates the list of lists subtracting one value from each list after applying the subtract function to it 

; Test variable which is the original matrix triangulated
(define MT
  (list (list 2 2 3 10)
        (list 3 9 21)
        (list 1 2 )))

; Test for the triangulation function 
(check-expect (triangulate M) MT)
(check-error (triangulate errorcheck) "all coefficients are 0")
(check-expect (triangulate rotatecheck)(list (list 5 6 7 4) (list 1.8 7.6 3.2) (list 0 0)))

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 469

; Purpose: to consume a triangular SOE (system of equations) and produces a solution based on the given values
(define (solve-helper eq sol)
  (cons (/ (- (rhs eq)(plug-in (rest (lhs eq)) sol)) ; does the math based off the pre-given functions for each value which is
           (first eq))                               ; called by the function below 
           sol))
(define (solve tri)
  (foldr solve-helper '() tri)) ; uses folder to add the solution values to a list through the solve-helper function

; Test for the solve function
(check-expect(solve MT)S)
;-------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 470

; Purpose: to combine the triangulate function and solve function from Exercises 466 and 469
(define (gauss M)
  (solve(triangulate M)))

; Test for the gauss function
(check-expect(gauss M)S)
(check-expect(gauss M2)S)
(check-expect(gauss M3)S)

;-------------------------------------------------------------------------------------------------------------------------------------------------------
; The following are just test variables for the triangulate function under different conditions

(define errorcheck
  (list (list 0 0 0 0)
        (list 0 0 0 0)
        (list 0 0 0 0 )))

(define rotatecheck
  (list (list 0 0 0 0)
        (list 5 6 7 4)
        (list 1 3 9 4 )))
