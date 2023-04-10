#lang racket

; struct for custom calculator monad
(struct calc (stack log))

(define (empty-calc)
  (calc '() '()))

; if c is a calculator, return it
; if its a value, return a calculator with that value as the stack
; if its null, return and empty calculator
(define(unit-calc c)
  (cond
    [(null? c) (empty-calc)]
    [(calc? c) c]
    [else (calc c '())]))

(define (push input c)
  (parse
   (rest input)
   (calc (append (list (string->number(first input))) (calc-stack c)) (calc-log c))))

(define (calc-bind f input c)
  (parse (rest input)
         (calc
          (append (list (f (second (calc-stack c)) (first (calc-stack c)))) (list-tail (calc-stack c) 2))
          (append (list (format "~a ~a ~a = ~a\n" (second (calc-stack c)) (first input) (first (calc-stack c)) (f (second (calc-stack c)) (first (calc-stack c)))))
                  (calc-log c)))))

;clear the stack on first call
;clears the log on the second call or if the stack is already empty
(define (clear input c)
  (if (empty? (calc-stack c))
      (clear-log input c)
      (parse (rest input)
             (calc
              '()
              (append (list "Cleared Stack\n") (calc-log c))))))

;continue parsing input even after clearing the log
(define (clear-log input c)
  (parse (rest input) (empty-calc)))

(define (show input c)
  (printf "~a\n" (calc-stack c))
  (parse (rest input)
         (calc
          (calc-stack c)
          (append (list "Display full stack\n") (calc-log c)))))

(define (show-top input c)
  (printf "~a\n" (first (calc-stack c)))
  (parse (rest input)
         (calc
          (calc-stack c)
          (append (list "Display top of stack\n") (calc-log c)))))

(define (get-size input c)
  (printf "~a\n" (length (calc-stack c)))
  (parse (rest input)
         (calc
          (calc-stack c)
          (append (list "Display stack size\n") (calc-log c)))))

(define (duplicate input c)
  (append (first (calc-stack c)) (calc-stack c))
  (parse (rest input) c))

; added this function to the list to show the output of the log
(define (show-log input c)
  (printf "~a\n" (calc-log c))
  (parse (rest input)
         (calc
          (calc-stack c)
          (append (list "Display log\n") (calc-log c)))))

(define (parse input c)
  (cond
    [(empty? input) c]
    [(string->number (first input)) (push input c)]
    [(equal? "+" (first input)) (calc-bind + input c)]
    [(equal? "-" (first input)) (calc-bind - input c)]
    [(equal? "*" (first input)) (calc-bind * input c)]
    [(equal? "/" (first input)) (calc-bind / input c)]
    [(equal? "CLR" (first input)) (clear input c)]
    [(equal? "SHOW" (first input)) (show input c)]
    [(equal? "TOP" (first input)) (show-top input c)]
    [(equal? "SIZ" (first input)) (get-size input c)]
    [(equal? "DUP" (first input)) (duplicate input c)]
    [(equal? "END" (first input)) (exit 0)]
    [(equal? "LOG" (first input)) (show-log input c)]))

(define (main) 
  (let loop ([input (string-split (read-line (current-input-port)))]
             [user-calc (empty-calc)])
    (let ([new-calc (parse input user-calc)])
      (loop (string-split (read-line (current-input-port))) new-calc))))

(main)