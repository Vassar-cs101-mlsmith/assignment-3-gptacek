;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; Gabor Ptacek
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (img x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 2) "solid" "teal")
             (+ RADIUS 2) (/ HEIGHT 2) -4 4))

(define BALL-AT-RIGHT
  (make-ball (circle (+ RADIUS 3) "solid" "VioletRed")
             (- WIDTH (+ RADIUS 3)) (/ HEIGHT 2) 2 -3))

(define BALL-AT-TOP
  (make-ball (circle (+ RADIUS 4) "solid" "Midnight Blue")
             (/ WIDTH 2) (+ RADIUS 4) -2 -4))

(define BALL-AT-BOTTOM
  (make-ball (circle (+ RADIUS 5) "solid" "Crimson")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 5)) -1 3))


; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB (list BALL-AT-LEFT BALL-AT-RIGHT BALL-AT-TOP BALL-AT-BOTTOM)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
;(define (fun-for-ball b) 
;  (...(ball-im b)...
;   ...(ball-x b)...(ball-y b)...
;   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
;(define (fun-for-list-of-balls lob) 
;  (cond
;    [(empty? lob)...] 
;    [else (...(fun-for-ball (first lob))...
;           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/ (image-width (ball-img b)) 2))

(check-expect (ball-radius BALL-AT-TOP) 29)
(check-expect (ball-radius BALL-AT-BOTTOM) 30)
(check-expect (ball-radius BALL-AT-RIGHT) 28)
(check-expect (ball-radius BALL-AT-LEFT) 27)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
    (<= (- (ball-y b) (ball-radius b)) 0))

(check-expect (top-edge? BALL-AT-TOP) #true)
(check-expect (top-edge? BALL-AT-RIGHT) #false)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (>= (+ (ball-y b) (ball-radius b)) HEIGHT))

(check-expect (bottom-edge? BALL-AT-BOTTOM) #true)
(check-expect (bottom-edge? BALL-AT-RIGHT) #false)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (<= (- (ball-x b) (ball-radius b)) 0))

(check-expect (left-edge? BALL-AT-LEFT) #true)
(check-expect (left-edge? BALL-AT-TOP) #false)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (>= (+ (ball-x b) (ball-radius b)) WIDTH))

(check-expect (right-edge? BALL-AT-RIGHT) #true)
(check-expect (right-edge? BALL-AT-TOP) #false)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-img b) (ball-x b) (ball-y b) (ball-dx b) (* -1 (ball-dy b))))

(check-expect (reverse-up-down BALL-AT-TOP) (make-ball (circle (+ RADIUS 4)
                                                               "solid" "Midnight Blue")
             (/ WIDTH 2) (+ RADIUS 4) -2 4))
(check-expect (reverse-up-down BALL-AT-BOTTOM) (make-ball (circle (+ RADIUS 5)
                                                                  "solid" "Crimson")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 5)) -1 -3))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
  (make-ball (ball-img b) (ball-x b) (ball-y b) (* -1 (ball-dx b)) (ball-dy b)))

(check-expect (reverse-left-right BALL-AT-TOP) (make-ball (circle (+ RADIUS 4)
                                                                  "solid" "Midnight Blue")
             (/ WIDTH 2) (+ RADIUS 4) 2 -4))
(check-expect (reverse-left-right BALL-AT-BOTTOM) (make-ball (circle (+ RADIUS 5)
                                                                     "solid" "Crimson")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 5)) 1 3))

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (top-edge? b) (bottom-edge? b)) (reverse-up-down b)]
    [else b]))

(check-expect (bounce-up-down BALL-AT-TOP) (make-ball (circle (+ RADIUS 4)
                                                              "solid" "Midnight Blue")
             (/ WIDTH 2) (+ RADIUS 4) -2 4))
(check-expect (bounce-up-down BALL-AT-BOTTOM) (make-ball (circle (+ RADIUS 5)
                                                                 "solid" "Crimson")
             (/ WIDTH 2) (- HEIGHT (+ RADIUS 5)) -1 -3))
(check-expect (bounce-up-down BALL-AT-RIGHT) BALL-AT-RIGHT)

; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(or (right-edge? b) (left-edge? b)) (reverse-left-right b)]
    [else b]))

(check-expect (bounce-left-right BALL-AT-RIGHT) (make-ball (circle (+ RADIUS 3)
                                                                   "solid" "VioletRed")
             (- WIDTH (+ RADIUS 3)) (/ HEIGHT 2) -2 -3))
(check-expect (bounce-left-right BALL-AT-LEFT) (make-ball (circle (+ RADIUS 2)
                                                                  "solid" "teal")
             (+ RADIUS 2) (/ HEIGHT 2) 4 4))
(check-expect (bounce-left-right BALL-AT-TOP) BALL-AT-TOP)

; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-img b) (+ (ball-dx b) (ball-x b)) (+ (ball-dy b) (ball-y b))
             (ball-dx b) (ball-dy b)))

(check-expect (move-ball BALL-AT-TOP) (make-ball (circle (+ RADIUS 4) "solid"
                                                         "Midnight Blue")
             (- (/ WIDTH 2) 2) RADIUS -2 -4))
(check-expect (move-ball BALL-AT-RIGHT) (make-ball (circle (+ RADIUS 3) "solid" "VioletRed")
             (+ (- WIDTH (+ RADIUS 3)) 2) (- (/ HEIGHT 2) 3) 2 -3))

; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (move-ball (bounce-left-right (bounce-up-down (first lob))))
                       (move-list-of-balls (rest lob)))]))

(check-expect (move-list-of-balls '()) '())
(check-expect (move-list-of-balls (list BALL-AT-TOP BALL-AT-RIGHT))
              (list (make-ball (circle (+ RADIUS 4) "solid" "Midnight Blue")
             (- (/ WIDTH 2) 2) (+ RADIUS 8) -2 4)
                (make-ball (circle (+ RADIUS 3) "solid" "VioletRed")
             (- WIDTH (+ RADIUS 5)) (- (/ HEIGHT 2) 3) -2 -3)))

; ball image -> image
; renders given ball b on given background bg
(define (render-ball ball-ex bg)
  (place-image (ball-img ball-ex) (ball-x ball-ex) (ball-y ball-ex) bg))

(check-expect (render-ball BALL-AT-TOP BACKGROUND) (place-image (circle (+ RADIUS 4) "solid" "Midnight Blue")
             (/ WIDTH 2) (+ RADIUS 4) BACKGROUND))
  

; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob1) 
  (cond [(empty? lob1) BACKGROUND]
        [else (render-ball (first lob1)
                           (render-balls (rest lob1)))]))

(check-expect (render-balls '()) BACKGROUND)
(check-expect (render-balls (list BALL-AT-TOP BALL-AT-RIGHT)) (place-images (list (circle (+ RADIUS 4) "solid" "Midnight Blue")
                                                                                  (circle (+ RADIUS 3) "solid" "VioletRed"))
                                                                            (list (make-posn (/ WIDTH 2) (+ RADIUS 4))
                                                                                  (make-posn (- WIDTH (+ RADIUS 3)) (/ HEIGHT 2)))
                                                                            BACKGROUND))

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)