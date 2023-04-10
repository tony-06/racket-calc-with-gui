; please forgive, errors are handled more gracefully in the gui than ending the program immediately
; error message is displayed, stack left unchanged for further use

#lang racket/gui

(define user-calc '())

;either monad definitions
(struct either (success? result))

(define (failure err)
  (calc-error err)
  (either #f err))

(define (failure? x)
  (if (either-success? x)
      #f
      #t))

(define (success x)
  (either #t x))

(define (success? x)
  (if (either-success? x)
      #t
      #f))

(define (unit-either x)
  (if (null? x)
      (failure null)
      (success x)))

(define (from-success x)
  (either-result x))

;check for safe division
(define (safe-div stack)
  (if (eq? (first stack) 0)
      (failure "divide by zero")
      (success (/ (second stack) (first stack)))))

;check stack size for commands
(define (safe-op stack len)
  (if (< (length stack) len)
      (failure "not enough items on stack")
      (success stack)))

;event for splitting input and parsing
(define (user-input key event)
  (if (equal? (send event get-event-type) 'text-field-enter)
      (for-each parse (string-split (string-upcase (send input-field get-value))))
      null)
  (if (equal? (send event get-event-type) 'text-field-enter)
      (send input-field set-value "")
      null))

(define (button-input)
  (for-each parse (string-split (string-upcase (send input-field get-value))))
  (send input-field set-value ""))

;update the display area of calculator
(define (update-display input)
  (send display-message set-label input))

;lots of annoying definitions for the gui
(define main-frame
  (new frame%
       [label "Calculator"]
       [width 335]
       [height 540]
       [alignment '(center bottom)]))

(define spacing-panel
  (new panel%
       [parent main-frame]
       [min-height 50]))

(define display-message
  (new message%
       [parent main-frame]
       [min-height 50]
       [min-width 325]
       [horiz-margin 10]
       [label "CALC GUI"]))

(define main-panel
  (new vertical-panel%
       [parent main-frame]))

(define text-input-panel
  (new horizontal-panel%
       [parent main-panel]
       [horiz-margin 10]))

(define input-field
  (new text-field%
       [parent text-input-panel]
       [label "Input"]
       [horiz-margin 5]
       [style '(single)]
       [callback (lambda (key event) (user-input key event))]))

(define button-panel1
  (new horizontal-panel%
       [parent main-panel]
       [spacing 10]
       [alignment '(center top)]))

(define button-panel2
  (new horizontal-panel%
       [parent main-panel]
       [spacing 10]
       [alignment '(center top)]))

(define button-panel3
  (new horizontal-panel%
       [parent main-panel]
       [spacing 10]
       [alignment '(center top)]))

(define button-panel4
  (new horizontal-panel%
       [parent main-panel]
       [spacing 10]
       [alignment '(center top)]))

(define button-panel5
  (new horizontal-panel%
       [parent main-panel]
       [spacing 10]
       [alignment '(center top)]))

(define (button-maker lst)
  (new button%
       [parent (first lst)]
       [label (second lst)]
       [min-height 40]
       [callback (third lst)]))

;error function
(define (calc-error err)
  (update-display (format "ERROR- ~a\nCurrent Stack: ~a" err user-calc)))

;perform operations on the stack
(define (stack-op op)
  (if (success? (safe-op user-calc 2))
      (set! user-calc (append (list (op (second user-calc) (first user-calc))) (list-tail user-calc 2)))
      null))

(define (stack-div)
  (cond
    [(failure? (safe-op user-calc 2)) (stack-op /)]
    [(success? (safe-div user-calc)) (set! user-calc (append (list (from-success (safe-div user-calc))) (list-tail user-calc 2)))]))

;parse user input
(define (parse input)
        (cond
          [(empty? input) user-calc]
          [(string->number input) (push (string->number input))]
          [(equal? "+" input) (stack-op +)]
          [(equal? "-" input) (stack-op -)]
          [(equal? "*" input) (stack-op *)]
          [(equal? "/" input) (stack-div)]
          [(equal? "CLR" input) (clear)]
          [(equal? "SHOW" input) (show-stack)]
          [(equal? "TOP" input) (show-top)]
          [(equal? "SIZ" input) (get-size)]
          [(equal? "DUP" input) (duplicate)]
          [(equal? "END" input) (exit 0)]
          [else (calc-error "unknown command")]))

(define (textbox in)
  (send input-field set-value (string-append (send input-field get-value) in)))

;required end function
(define (end-program)
  (lambda (button event) (exit 0)))

;require show function
(define (show-stack)
  (if (success? (safe-op user-calc 0))
      (update-display (format "~a\n" user-calc))
      null))

;required top function
(define (show-top)
  (if (success? (safe-op user-calc 1))
      (update-display (format "~a" (first user-calc)))
      null))

(define (get-size)
  (length user-calc))

;required size function
(define (show-size)
  (update-display (number->string (get-size))))

;required dup function
(define (duplicate)
  (if (success? (safe-op user-calc 1))
      (push (first user-calc))
      null))

;required clear function
(define (clear)
  (set! user-calc '())
  (update-display ""))

(define (push x)
  (set! user-calc (append (list x) user-calc)))

;combined input functions, calculator auto calculates when you press a function (+ - * /) button
(define (text-in x)
  (textbox x)
  (button-input))

;list of calculator buttons and their callbacks to map over with the above buttonmaker function
(define buttons
  (list
   (list button-panel1 "+" (lambda (button event) (text-in " +")))
   (list button-panel1 "-" (lambda (button event) (text-in " -")))
   (list button-panel1 "*" (lambda (button event) (text-in " *")))
   (list button-panel1 "/" (lambda (button event) (text-in " /")))
   (list button-panel2 "7" (lambda (button event) (textbox " 7")))
   (list button-panel2 "8" (lambda (button event) (textbox " 8")))
   (list button-panel2 "9" (lambda (button event) (textbox " 9")))
   (list button-panel2 "TOP" (lambda (button event) (show-top)))
   (list button-panel3 "4" (lambda (button event) (textbox " 4")))
   (list button-panel3 "5" (lambda (button event) (textbox " 5")))
   (list button-panel3 "6" (lambda (button event) (textbox " 6")))
   (list button-panel3 "SIZE" (lambda (button event) (show-size)))
   (list button-panel4 "1" (lambda (button event) (textbox " 1")))
   (list button-panel4 "2" (lambda (button event) (textbox " 2")))
   (list button-panel4 "3" (lambda (button event) (textbox " 3")))
   (list button-panel4 "DUP" (lambda (button event) (duplicate)))
   (list button-panel5 "CLR" (lambda (button event) (clear)))
   (list button-panel5 "0" (lambda (button event) (textbox " 0")))
   (list button-panel5 "SHOW" (lambda (button event) (show-stack)))
   (list button-panel5 "END" (lambda (button event) (end-program)))))

  (define (main)
    (map button-maker buttons)
    (send main-frame show #t))

  (main)