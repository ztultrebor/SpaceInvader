;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

(define-struct war-objects (tank invader missile))
;; A WarObjects is a make-war-objects [Weapon Weapon Weapon]
;;     a collection of war weapons and vehicles:
;;        includes a heroic tank, a spooky invader and a missle
;;        to shoot it down
#;
(define (fn-with-war-objects objs)
  (... (war-objects-tank objs) ...)
  (... (war-objects-invader objs) ...)
  (... (war-objects-missile objs) ...))


;; A Weapon is one of
;;     - #false
;;     - Parameters
;; contains the kinematc parameters of the weapon if it's active, else #false
#;
(define (fn-with-weapon weap)
  (cond
    [(false? weap)  ...]
    [else  (... weap ...)]))


(define-struct parameters (position velocity))
;; A Parameters is a make-parameters [Vector Vector]
;; gives the position in pixels and velocity in pixels/tick
;;     of a weapon
#;
(define (fn-with-parameters params)
  (... (parameters-position params) ...)
  (... (parameters-velocity params) ...))


(define-struct vector (x y))
;; A Vector is a make-vector [Number Number]
;;     a mathematical object in 2D cartesian space
#;
(define (fn-with-vector vec)
  (... (vector-x vec) ...)
  (... (vector-y vec) ...))



; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define ALTITUDE 100)
(define GROUNDLEVEL (- HEIGHT ALTITUDE))
(define INITTANKPARAMS (make-parameters (make-vector (/ WIDTH 2) GROUNDLEVEL)
                                        (make-vector 0 0)))
(define INITINVADERPARAMS (make-parameters (make-vector (/ WIDTH 2) 50)
                                           (make-vector 0 1)))
(define BACKGROUND
  (overlay/align "left" "bottom"
                 (rectangle WIDTH ALTITUDE "solid" "light green")
                 (empty-scene WIDTH HEIGHT "midnight blue")))
(define TANK (overlay/align "center" "bottom"
                            (rectangle 50 20 "solid" "red")
                            (rectangle 20 50 "solid" "red")))
(define INVADER (overlay (circle 20 "solid" "green")
                         (rectangle 80 20 "solid" "green")))
(define MISSILE (rectangle 16 40 "solid" "red"))



; functions

(define (main objs)
  ;; WarObjects -> WarObjects
  ;; run the pocket universe
  (big-bang objs
    [on-tick deploy]
    [to-draw render]
    ; [on-key impulse)) !!!
    ;[stop-when crashed? render])) ;; be sure to show explosion
    ))

(define (deploy objs)   
  ;; WarObjects -> WarObjects
  ;; war objects move around in accordance with user input and hard wiring
  (make-war-objects  (move (war-objects-tank objs))
                     (move (war-objects-invader objs))
                     (move (war-objects-missile objs))))


(define (render objs)
  ;; WarObjects -> Img
  ;; display the scene with current positions of all war objects
  (insert-image (war-objects-tank objs) TANK
                (insert-image (war-objects-invader objs) INVADER
                              (insert-image (war-objects-missile objs) MISSILE
                                            BACKGROUND))))

(define (insert-image weap weap-img background)  
  ;; Weapon, Img, Img -> Img
  ;; takes a weapon and an image for that weapon and places the
  ;;     image into the image for the background
  (cond
    [(false? weap) background]
    [else (place-image/align weap-img (vector-x (parameters-position weap))
                             (vector-y (parameters-position weap))
                             "center" "bottom" background)]))
; checks
(check-expect (insert-image (make-parameters (make-vector (/ WIDTH 2) GROUNDLEVEL)
                                             (make-vector 0 0)) TANK BACKGROUND)
              (place-image/align TANK (/ WIDTH 2) GROUNDLEVEL
                                 "center" "bottom" BACKGROUND))
(check-expect (insert-image #f TANK BACKGROUND) BACKGROUND)


(define (move w)
  ;; Weapon -> Weapon
  ;; add update position vector with velocity
  (cond
    [(false? w)  w]
    [else   (make-parameters
             (make-vector (+ (vector-x (parameters-position w))
                             (vector-x (parameters-velocity w)))
                          (+ (vector-y (parameters-position w))
                             (vector-y (parameters-velocity w))))
             (parameters-velocity w))]))
;; checks
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 12 5)))
               (make-parameters (make-vector 24 10) (make-vector 12 5)))
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 0 0)))
              (make-parameters (make-vector 12 5) (make-vector 0 0)))
(check-expect (move (make-parameters (make-vector 0 0) (make-vector 12 5)))
              (make-parameters (make-vector 12 5) (make-vector 12 5)))


  random
;; actions!

(main (make-war-objects INITTANKPARAMS INITINVADERPARAMS #f))