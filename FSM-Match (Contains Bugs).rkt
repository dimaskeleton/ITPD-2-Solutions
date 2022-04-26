;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Short-Dimitri, Diskant-Derek, Presinal-Gregori, Attalla-Maryam, Dunn-Kyle|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;Purpose design fsm-match, whichs takes data representation of finite state machine and a string
;Contract: Input: data representation of a finite state machine and a string -> Output: gives #true thencauses the finite state machine to a final state.

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

(define fsm-contains-abc
  (make-fsm
   "Q0"
   (list (make-transition "Q0" "a" "Q1")
         (make-transition "Q0" "b" "Q0")
         (make-transition "Q0" "c" "Q0")
         (make-transition "Q1" "a" "Q1")
         (make-transition "Q1" "c" "Q0")
         (make-transition "Q1" "b" "Q2")
         (make-transition "Q2" "a" "Q0")
         (make-transition "Q2" "b" "Q0")
         (make-transition "Q2" "c" "Q3")
         (make-transition "Q3" "a" "Q3")
         (make-transition "Q3" "b" "Q3")
         (make-transition "Q3" "c" "Q3"))
   "Q3"))

(define fsm-even-num-b
  (make-fsm
   "Q0"
   (list (make-transition "Q0" "a" "Q0")
         (make-transition "Q0" "b" "Q1")
         (make-transition "Q1" "a" "Q1")
         (make-transition "Q1" "b" "Q0"))
   "Q0"))
         

;fsm-match: an fsm string ->  boolean value 
;Purpose: produces #true if the sequence of characters in the string causes the finite state machine
;to transition from an initial state to a final state
;This terminates because it will always reach the end of the string and the recursion will stop
(define (fsm-match an-fsm a-string)
  (local [(define transition(fsm-transitions an-fsm))
          
          ;fsm-match?: state string -> boolean
          ;Purpose: To filter out the string if its empty or not before its passed through transition2 
          (define (fsm-match? current-state s)
            (cond [(string=? s "") (equal? current-state (fsm-final an-fsm))]
                  [else (fsm-match? (transition2 current-state (substring s 0 1))
                                    (substring s 1))]))
          
          ;transition2: Input: state string -> Output: the next-state until it reaches the final state
          ;Purpose: To process the string and go through each letter to see if the string will return true or false 
          (define (transition2 current-state str)
            (local [;next-state: Input: fsm-string -> Output: Finale State Of Machine Or Termintion
                    ;Purpose: Reads String Input Letter By Letter To Determine If Input Is True Or False based on the letters
                    (define next-state
                      (filter (lambda (x)(equal? str (transition-key x)))
                              (filter (lambda (y)(equal? current-state(transition-current y)))
                                      transition)))]
              (if (empty? next-state)
                  current-state
                  (transition-next (first next-state)))))]
    (fsm-match? (fsm-initial an-fsm) a-string)))


;Check-expects:True
(check-expect (fsm-match fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match fsm-a-bc*-d "abccccccd") #true)
(check-expect (fsm-match fsm-a-bc*-d "abbbbbbbbbbbbccccccccccd") #true)
(check-expect (fsm-match fsm-a-bc*-d "abbbcd") #true)
(check-expect (fsm-match fsm-even-num-b "abab")#true)
(check-expect (fsm-match fsm-even-num-b "bb")#true)
(check-expect (fsm-match fsm-contains-abc "abc")#true)
(check-expect (fsm-match fsm-contains-abc "aabcc")#true)

;Check-expects:False
(check-expect (fsm-match fsm-a-bc*-d "aaaa")#false)
(check-expect (fsm-match fsm-a-bc*-d "dbca") #false)
(check-expect (fsm-match fsm-a-bc*-d "") #false)
(check-expect (fsm-match fsm-a-bc*-d "da") #false)
(check-expect (fsm-match fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match fsm-a-bc*-d "d") #false)
(check-expect (fsm-match fsm-a-bc*-d "iwfuwhfewhg") #false)
(check-expect (fsm-match fsm-even-num-b "aba")#false)
(check-expect (fsm-match fsm-contains-abc "")#false)
(check-expect (fsm-match fsm-contains-abc "aba")#false)