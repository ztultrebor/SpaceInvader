;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

;; A WarObjects is one of:
;;     - '()
;;     - (cons Parameters WarObjects)
;;     a collection of war weapons and vehicles:
;;        starts with a spooky invader and a heroic tank.
;;        invader is the last (earliest) one on the list. Tank is next
;;        can add as many missles as you like to the front of the list
(define (war-objects? objs)
  (or
   (empty? objs)
   (and (parameters? (first objs)) (war-objects? (rest objs)))))
#;
(define (fn-with-war-objects objs)
  (cond
    [(empty? objs) ... '()]
    [(empty? (rest objs)) ... (cons ... (first objs) '())]
    [(empty? (rest (rest objs))) ... (cons ... (first objs)
                                           (fn-with-war-objects (rest objs)))]
    [else ...(cons ... (first objs) ... (fn-with-war-objects (rest objs)))]))


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
(define WAROBJECTS (cons INITTANKPARAMS (cons INITINVADERPARAMS '())))
(define MISSILEVELOCITY (make-vector 0 -10))
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
    [stop-when victory-or-defeat? render])) ;; be sure to show explosions


(define (control objs ke)
  ;; WarObjects -> WarObjects
  ;; move tank with left- and right-arrows, and fire missile on spacebar
  (cond
    [(empty? (rest objs)) objs]
    [(key=? ke " ")
     (cons (fire-missile (penultimate objs)) objs)]
    [(empty? (rest (rest objs)))
     (cons (tank-control (first objs) ke) (rest objs))]
    [else (cons (first objs) (control (rest objs) ke))]))
; checks
(check-expect (control
               (cons INITTANKPARAMS (cons INITINVADERPARAMS '())) "left")
              (cons (make-parameters (parameters-position INITTANKPARAMS)
                                     (+vec (parameters-velocity INITTANKPARAMS)
                                           (make-vector -3 0)))
                    (cons INITINVADERPARAMS '())))
(check-expect (control
               (cons INITTANKPARAMS (cons INITINVADERPARAMS '())) "right")
              (cons (make-parameters (parameters-position INITTANKPARAMS)
                                     (+vec (parameters-velocity INITTANKPARAMS)
                                           (make-vector 3 0)))
                    (cons INITINVADERPARAMS '())))
(check-expect (control
               (cons INITTANKPARAMS (cons INITINVADERPARAMS '())) "\r")
              (cons INITTANKPARAMS (cons INITINVADERPARAMS '())))
(check-expect (control
               (cons INITTANKPARAMS (cons INITINVADERPARAMS '())) " ")
              (cons (make-parameters (parameters-position INITTANKPARAMS)
                                     (+vec (parameters-velocity INITTANKPARAMS)
                                           MISSILEVELOCITY))
                    (cons INITTANKPARAMS (cons INITINVADERPARAMS '()))))


(define (deploy objs)
  ;; WarObjects -> WarObjects
  ;; war objects move around in accordance with user input and hard wiring
  (cond
    [(empty? objs) objs]
    [(empty? (rest objs)) (cons (move (jitter (first objs))) '())]
    [(empty? (rest (rest objs))) (cons (move (first objs))
                                       (deploy (rest objs)))]
    [else (cons (move (first objs)) (deploy (rest objs)))]))


(define (render objs)
  ;; WarObjects -> Img
  ;; display the scene with current positions of all war objects
  (cond
    [(empty? objs) BACKGROUND]
    [(empty? (rest objs)) (insert-image (first objs) INVADER
                                        (render (rest objs)))]
    [(empty? (rest (rest objs)))
     (insert-image (first objs)
                   (if (alien-invasion? (first objs) (first (rest objs)))
                       DESTRUCTION TANK) (render (rest objs)))]
    [(target-eliminated? (first objs) (last objs))
     (insert-image (first objs) DETONATION
                   (insert-image (last objs) HIT 
                                 (render (rest objs))))]
    [else (insert-image (first objs) MISSILE  (render (rest objs)))]))


(define (victory-or-defeat? objs)
  ;; WarObjects -> Bool
  ;; ends when missile destroys invader or invader successfully lands
  (and
   (not (empty? (rest objs)))
   (or
    (alien-invasion? (penultimate objs) (last objs))
    (target-eliminated? (first objs) (last objs))
    (victory-or-defeat? (rest objs)))))
; checks
(check-expect (victory-or-defeat?
               (cons INITTANKPARAMS (cons INITINVADERPARAMS '()))) #f)
(check-expect (victory-or-defeat?
               (cons INITINVADERPARAMS (cons INITTANKPARAMS '()))) #t)
(check-expect (victory-or-defeat?
               (cons INITTANKPARAMS (cons INITTANKPARAMS 
                                          (cons INITINVADERPARAMS '())))) #f)
(check-expect (victory-or-defeat?
               (cons INITINVADERPARAMS (cons INITTANKPARAMS
                                             (cons INITINVADERPARAMS '())))) #t)
(check-expect (victory-or-defeat?
               (cons INITINVADERPARAMS (cons INITTANKPARAMS
                                             (cons INITINVADERPARAMS '())))) #t)


(define (tank-control tank ke)
  ;; Parameters, Key Event -> Parameters
  ;; send tank left with left arrow or right with right
  (make-parameters
   (parameters-position tank)
   (cond
     [(key=? ke "left") (-vec (parameters-velocity tank) TANKSPEED)]
     [(key=? ke "right") (+vec (parameters-velocity tank) TANKSPEED)]
     [else (parameters-velocity tank)])))
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


(define (fire-missile tank)
  ;; WeaponObjects -> Parameters
  ;; fire missile on spacebar; missile takes x-position and velocity of tank
  (make-parameters (parameters-position tank)
                   (+vec (parameters-velocity tank) MISSILEVELOCITY)))
;; checks
(check-expect (fire-missile
               (make-parameters (make-vector 27 42) TANKSPEED))
              (make-parameters (make-vector 27 42)
                               (+vec TANKSPEED MISSILEVELOCITY)))


(define (move w)
  ;; Weapon -> Weapon
  ;; update position vector with velocity
  (make-parameters
   (+vec (parameters-position w) (parameters-velocity w))
   (parameters-velocity w)))
;; checks
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 12 5)))
              (make-parameters (make-vector 24 10) (make-vector 12 5)))


(define (jitter invader)
  ;; Weapon -> Weapon
  ;; update velocity vector with some randomness
  (make-parameters
   (parameters-position invader)
   (make-vector (+ (random 51) -25)
                (vector-y (parameters-velocity invader)))))
;; checks
(check-range (vector-x (parameters-velocity
                        (jitter (make-parameters
                                 (make-vector 0 0) (make-vector 0 0))))) -25 25)


(define (target-eliminated? missile invader)
  ;; Parameters, Parameters -> Bool
  ;; direct hit on landing craft!
  (< (normalize (-vec (parameters-position invader)
                      (parameters-position missile)))
     BLASTRADIUS))
;; checks
(check-expect (target-eliminated? INITINVADERPARAMS INITINVADERPARAMS) #t)
(check-expect (target-eliminated? INITINVADERPARAMS INITTANKPARAMS) #f)


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



;; WarObjects -> Parameters
;; returns parameters for the last object in the list, the invader
(define (last objs)
  (cond
    [(empty? (rest objs)) (first objs)]
    [else (last (rest objs))]))
; check
(check-expect (last (cons "a" '())) "a")
(check-expect (last (cons "b" (cons "a" '()))) "a")


;; WarObjects -> Parameters
;; returns parameters for the last object in the list, the invader
(define (penultimate objs)
  (cond
    [(empty? (rest (rest objs))) (first objs)]
    [else (penultimate (rest objs))]))
; check
(check-expect (penultimate (cons "b" (cons "a" '()))) "b")
(check-expect (penultimate (cons "c" (cons "b" (cons "a" '())))) "b")


(define (insert-image weap weap-img background)  
  ;; Weapon, Img, Img -> Img
  ;; takes a weapon and an image for that weapon and places the
  ;;     image into the image for the background at the weapons x/y-position
  (place-image weap-img (vector-x (parameters-position weap))
               (vector-y (parameters-position weap)) background))



;; actions!

(main WAROBJECTS)