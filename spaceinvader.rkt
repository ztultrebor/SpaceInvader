;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

(define-struct war-objects [tank invader missile])
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


(define-struct parameters [position velocity])
;; A Parameters is a make-parameters [Vector Vector]
;; gives the position in pixels and velocity in pixels/tick
;;     of a weapon
#;
(define (fn-with-parameters params)
  (... (parameters-position params) ...)
  (... (parameters-velocity params) ...))


(define-struct vector [x y])
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
(define MISSILEVELOCITY -10)
(define BLASTRADIUS 75)
(define TANKSPEED (make-vector 3 0))
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
(define DESTRUCTION (radial-star 12 50 100 "solid" "red"))
(define DETONATION (radial-star 8 20 50 "solid" "red"))
(define HIT (radial-star 12 50 100 "solid" "green"))


; functions

(define (main objs)
  ;; WarObjects -> WarObjects
  ;; run the pocket universe
  (big-bang objs
    [on-key control]
    [on-tick deploy]
    [to-draw render]
    [stop-when victory-or-defeat? explosion])) ;; be sure to show explosions


(define (control objs ke)
  ;; WarObjects -> WarObjects
  ;; move tank with left- and right-arrows, and fire missile on spacebar
  (make-war-objects
   (tank-control (war-objects-tank objs) ke)
   (war-objects-invader objs)
   (missile-control (war-objects-missile objs) (war-objects-tank objs) ke)))


(define (deploy objs)
  ;; WarObjects -> WarObjects
  ;; war objects move around in accordance with user input and hard wiring
  (make-war-objects  (move (war-objects-tank objs))
                     (move (jitter (war-objects-invader objs)))
                     (move (war-objects-missile objs))))


(define (render objs)
  ;; WarObjects -> Img
  ;; display the scene with current positions of all war objects
  (insert-image (war-objects-tank objs) TANK
                (insert-image (war-objects-invader objs) INVADER
                              (insert-image (war-objects-missile objs) MISSILE
                                            BACKGROUND))))


(define (victory-or-defeat? objs)
  ;; WarObjects -> Bool
  ;; ends when missile destroys invader, missile misses invader,
  ;;     or invader successfully lands
  (cond
    [(parameters? (war-objects-missile objs))
     (or
      (target-eliminated? (war-objects-invader objs) (war-objects-missile objs))
      (misfire? (war-objects-invader objs) (war-objects-missile objs)))]
    [(alien-invasion? (war-objects-tank objs) (war-objects-invader objs)) #t]
    [else #f]))
; checks
(check-expect (victory-or-defeat?
               (make-war-objects INITTANKPARAMS INITINVADERPARAMS #f)) #f)
(check-expect (victory-or-defeat?
               (make-war-objects INITTANKPARAMS INITINVADERPARAMS
                                 INITTANKPARAMS)) #f)
(check-expect (victory-or-defeat?
               (make-war-objects INITTANKPARAMS INITINVADERPARAMS
                                 INITINVADERPARAMS)) #t)
(check-expect (victory-or-defeat?
               (make-war-objects INITINVADERPARAMS INITTANKPARAMS
                                 INITINVADERPARAMS)) #t)


(define (explosion objs)
  ;; WarObjects -> Img
  ;; shows game-ending explosions!
  (cond
    [(alien-invasion? (war-objects-tank objs) (war-objects-invader objs))
     (insert-image (war-objects-tank objs) DESTRUCTION (render objs))]
    [(target-eliminated? (war-objects-invader objs) (war-objects-missile objs))
     (insert-image (war-objects-missile objs) DETONATION
                   (insert-image (war-objects-invader objs) HIT 
                                 (render objs)))]
    [(misfire? (war-objects-invader objs) (war-objects-missile objs))
     (insert-image (war-objects-missile objs) DETONATION
                   (render objs))]))


(define (tank-control tank ke)
  ;; Weapon, Key Event -> Weapon
  ;; send tank tank left with left arrow or right with right
  (cond
    [(key=? "left" ke) (make-parameters
                        (parameters-position tank)
                        (-vec (parameters-velocity tank) TANKSPEED))]
    [(key=? "right" ke) (make-parameters
                         (parameters-position tank)
                         (+vec (parameters-velocity tank) TANKSPEED))]
    [else tank]))
;; checks
(check-expect (tank-control
               (make-parameters (make-vector 0 0) (make-vector 0 0)) "left")
              (make-parameters (make-vector 0 0)
                               (-vec (make-vector 0 0) TANKSPEED)))
(check-expect (tank-control
               (make-parameters (make-vector 0 0) (make-vector 0 0)) "right")
              (make-parameters (make-vector 0 0) TANKSPEED))
(check-expect (tank-control
               (make-parameters (make-vector 0 0) (make-vector 0 0)) " ")
              (make-parameters (make-vector 0 0) (make-vector 0 0)))


(define (missile-control missile tank ke)
  ;; Weapon, Weapon, Key Event -> Weapon
  ;; fire missile on spacebar; missile takes x-position anf velocity of tank
  (cond
    [(and (false? missile) (key=? " " ke))
     (make-parameters (parameters-position tank)
                      (make-vector (vector-x (parameters-velocity tank))
                                   MISSILEVELOCITY))]
    [else missile]))
;; checks
(check-expect (missile-control
               #f (make-parameters (make-vector 27 42)
                                   (make-vector TANKSPEED 0)) " ")
              (make-parameters (make-vector 27 42)
                               (make-vector TANKSPEED MISSILEVELOCITY)))
(check-expect (missile-control
               #f (make-parameters (make-vector 27 42)
                                   (make-vector TANKSPEED 0)) "left") #f)
(check-expect (missile-control
               (make-parameters (make-vector 39 222)
                                (make-vector 0  MISSILEVELOCITY))
               (make-parameters (make-vector 27 42)
                                (make-vector TANKSPEED 0)) " ")
              (make-parameters (make-vector 39 222)
                               (make-vector 0 MISSILEVELOCITY)))


(define (move w)
  ;; Weapon -> Weapon
  ;; update position vector with velocity
  (cond
    [(false? w)  w]
    [else   (make-parameters
             (+vec (parameters-position w) (parameters-velocity w))
             (parameters-velocity w))]))
;; checks
(check-expect (move #f) #f)
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 12 5)))
              (make-parameters (make-vector 24 10) (make-vector 12 5)))


(define (jitter w)
  ;; Weapon -> Weapon
  ;; update velocity vector with some randomness
  (cond
    [(false? w)  #f]
    [else (make-parameters
           (parameters-position w)
           (make-vector (+ (random 51) -25)
                        (vector-y (parameters-velocity w))))]))
;; checks
(check-expect (jitter #f) #f)
(check-within (jitter (make-parameters
                       (make-vector 0 0) (make-vector 0 0)))
              (make-parameters (make-vector 0 0) (make-vector 0 0)) 25)
; how do you properly test a function that yields randomized output?



(define (target-eliminated? invader missile)
  ;; Parameters, Parameters -> Bool
  ;; direct hit on landing craft!
  (< (normalize (-vec (parameters-position invader)
                      (parameters-position missile)))
     BLASTRADIUS))
;; checks
(check-expect (target-eliminated?
               INITTANKPARAMS
               (make-parameters (make-vector 700 600) (make-vector 0 0))) #t)
(check-expect (target-eliminated?
               INITTANKPARAMS
               (make-parameters (make-vector 700 100) (make-vector 0 0))) #f)


(define (misfire? invader missile)
  ;; Parameters, Paramerters -> Bool
  ;; hey man, bad shot
  (<= (vector-y (parameters-position missile))
      (vector-y (parameters-position invader))))
;; checks
(check-expect (misfire?
               (make-parameters (make-vector 700 100) (make-vector 0 0))
               (make-parameters (make-vector 400 300) (make-vector 0 0))) #f)
(check-expect (misfire?
               (make-parameters (make-vector 700 100) (make-vector 0 0))
               (make-parameters (make-vector 400 100) (make-vector 0 0))) #t)


(define (alien-invasion? tank invader)
  ;; Paramerters, Parameters -> Bool
  ;; aliens have landed!
  (>= (vector-y (parameters-position invader))
      (vector-y (parameters-position tank))))
; checks
(check-expect (alien-invasion?
               (make-parameters (make-vector 500 600) (make-vector 0 0))
               (make-parameters (make-vector 700 600) (make-vector 0 0))) #t)
(check-expect (alien-invasion?
               (make-parameters (make-vector 500 600) (make-vector 0 0))
               (make-parameters (make-vector 700 100) (make-vector 0 0))) #f)


(define (+vec v1 v2)
  ;; Vector, Vector -> Vector
  ;; add one vector to another
  (make-vector (+ (vector-x v1) (vector-x v2))
               (+ (vector-y v1) (vector-y v2))))
;; checks
(check-expect (+vec (make-vector 12 5) (make-vector 12 5)) (make-vector 24 10))


(define (-vec v1 v2)
  ;; Vector, Vector -> Vector
  ;; subtract one vector from another
  (make-vector (- (vector-x v1) (vector-x v2))
               (- (vector-y v1) (vector-y v2))))
;; checks
(check-expect (-vec (make-vector 12 5) (make-vector 12 5)) (make-vector 0 0))


(define (normalize vec)
  ;; Vector -> Vector
  ;; calculates norm of a vector
  (sqrt (+ (sqr (vector-x vec)) (sqr (vector-y vec)))))
;; checks
(check-expect (normalize (make-vector 30 40 )) 50)


(define (insert-image weap weap-img background)  
  ;; Weapon, Img, Img -> Img
  ;; takes a weapon and an image for that weapon and places the
  ;;     image into the image for the background at the weapons x/y-position
  (cond
    [(false? weap) background]
    [else (place-image weap-img (vector-x (parameters-position weap))
                       (vector-y (parameters-position weap)) background)]))



;; actions!

(main (make-war-objects INITTANKPARAMS INITINVADERPARAMS #f))