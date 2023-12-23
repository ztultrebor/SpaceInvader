;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;; data definitions

(define-struct war-objects [tank invaders missiles explosions])
;; A WarObjects is a [Parameters ListOfParameters
;; ListOfParameters ListOfParameters]
;; A single tank, a swarm of invaders, a hail of missiles and
;; hopefully, lots of enemies exploding
#;
(define (fn-with-war-objects objs)
  (make-war-objects
   (fn-with-tank (war-objects-tank objs))
   (fn-with-invaders (war-objects-invaders objs))
   (fn-with-missiles (war-objects-missiles objs)))
  (fn-with-explosions (war-objects-explosions objs)))


;; A ListOfParameters is one of
;;   - '()
;;   (cons Parameters ListOfParameters)
#;
(define (fn-with-lop lop)
  (cond
    [(empty? lop) ...]
    [else (fn-on-parameters (first lop)) ... (fn-with-lop (rest lop))]))


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
(define HIT (overlay
               (radial-star 20 32 48 "solid" "purple")
               (radial-star 10 48 72 "solid" "green")))
(define GAMEOVERTEXTCOLOR "white")
(define NUMINVADERS 20)
(define WAROBJECTS (make-war-objects
                    INITTANKPARAMS
                    (make-list NUMINVADERS INITINVADERPARAMS) '() '()))



; functions

(define (main objs)
  ;; WarObjects -> WarObjects
  ;; run the pocket universe
  (big-bang objs
    [on-tick deploy]
    [on-key control]
    [to-draw render]
    [stop-when victory-or-defeat? render-game-over]))


(define (deploy objs)
  ;; !!! wrap around pacman style
  ;; WarObjects -> WarObjects
  ;; war objects move around in accordance with user input and hard wiring
  (make-war-objects
   (move (war-objects-tank objs))
   (move-stuff (jitter-stuff (hit
                              (war-objects-invaders objs)
                              (war-objects-missiles objs))))
   (move-stuff (delete-misses (hit (war-objects-missiles objs)
                                   (war-objects-invaders objs))))
   (move-stuff (delete-misses (append
                                (war-objects-explosions objs)
                               (detonation (war-objects-missiles objs)
                                          (war-objects-invaders objs)))))))


(define (delete-misses lop)
  ;; ListOfParameters -> ListOfParametrs
  ;; delete missiles that exit stage top
  (cond
    [(empty? lop) '()]
    [(< (vector-y (parameters-position (first lop))) 0)
     (delete-misses (rest lop))]
    [else (cons (first lop) (delete-misses (rest lop)))]))


(define (control objs ke)
  ;; WarObjects -> WarObjects
  ;; move tank with left- and right-arrows, and fire missile on spacebar
  (cond
    [(key=? ke " ")
     (make-war-objects
      (war-objects-tank objs)
      (war-objects-invaders objs)
      (cons (fire-missile (war-objects-tank objs))
            (war-objects-missiles objs))
      (war-objects-explosions objs))]
    [else (make-war-objects
           (tank-control (war-objects-tank objs) ke)
           (war-objects-invaders objs)
           (war-objects-missiles objs)
           (war-objects-explosions objs))]))
; checks
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '()) "left")
              (make-war-objects
               (make-parameters (parameters-position INITTANKPARAMS)
                                (+vec (parameters-velocity INITTANKPARAMS)
                                      (make-vector -3 0)))
               (list INITINVADERPARAMS) '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '()) "right")
              (make-war-objects
               (make-parameters (parameters-position INITTANKPARAMS)
                                (+vec (parameters-velocity INITTANKPARAMS)
                                      (make-vector 3 0)))
               (list INITINVADERPARAMS) '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '()) "\r")
              (make-war-objects
               INITTANKPARAMS (list INITINVADERPARAMS) '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '()) " ")
              (make-war-objects
               INITTANKPARAMS (list INITINVADERPARAMS)
               (list (make-parameters
                      (parameters-position INITTANKPARAMS)
                      (+vec (parameters-velocity INITTANKPARAMS)
                            MISSILEVELOCITY))) '()))


(define (render objs)
  ;; WarObjects -> Img
  ;; display the scene with current positions of all war objects
  ;; !!! invader expolsions!
  ;; !!! tank explosion
  (render-list-of-stuff
   (war-objects-explosions objs) HIT
   (render-list-of-stuff
    (war-objects-missiles objs) MISSILE
    (render-list-of-stuff
     (war-objects-invaders objs) INVADER
     (render-list-of-stuff
      (list (war-objects-tank objs)) TANK BACKGROUND)))))


(define (victory-or-defeat? objs)
  ;; WarObjects -> Bool
  ;; ends when all invaders are destroyed or one invader successfully lands
  (or
   (empty? (war-objects-invaders objs))
   (alien-invasion? (war-objects-tank objs) (war-objects-invaders objs))))
; checks
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS (list INITINVADERPARAMS) '() '())) #f)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITINVADERPARAMS (list INITTANKPARAMS) '() '())) #t)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS (list INITINVADERPARAMS)
                (list INITTANKPARAMS) '())) #f)
(check-expect (victory-or-defeat?
               (make-war-objects INITINVADERPARAMS (list INITTANKPARAMS)
                                 (list INITINVADERPARAMS) '())) #t)


(define (render-game-over objs)
  ;; WarObjects -> Img
  ;; display game over screen
  (overlay
   (above
    (text "Game Over!" 48 GAMEOVERTEXTCOLOR)
    (beside
     (text "you destroyed "  16 GAMEOVERTEXTCOLOR)
     (text (number->string (- NUMINVADERS
                              (length (war-objects-invaders objs))))
           24 GAMEOVERTEXTCOLOR)
     (text " invaders" 16 GAMEOVERTEXTCOLOR)))
   (render objs)))
  

(define (hit swarm1 swarm2)
  ;; ListOfParameters ListOfParameter -> ListOfParameters
  ;; delete parameters of an element of swarm1 that has
  ;; made contact with an element of swarm1
  (cond
    [(empty? swarm1) '()]
    [(contact? (first swarm1) swarm2)
     (hit (rest swarm1) swarm2)]
    [else (cons (first swarm1) (hit (rest swarm1) swarm2))]))


(define (detonation missiles invaders)
  ;; ListOfParameters Parameter -> ListOfParameters
  ;; move parameters of detonated missile to explosion list
  (cond
    [(empty? missiles) '()]
    [(contact? (first missiles) invaders)
     (cons (first missiles) (detonation (rest missiles) invaders))]
    [else (detonation (rest missiles) invaders)]))


(define (move params)
  ;; Parameters -> Parameters
  ;; update position vector with velocity
  (make-parameters
   (+vec (parameters-position params) (parameters-velocity params))
   (parameters-velocity params)))
;; checks
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 12 5)))
              (make-parameters (make-vector 24 10) (make-vector 12 5)))


(define (move-stuff loprms)
  ; ListOfParameters  -> ListOfParameters
  ; update parameters that are organized into lists
  (cond
    [(empty? loprms) '()]
    [else (cons (move (first loprms)) (move-stuff (rest loprms)))]))


(define (jitter-stuff loprms)
  ; ListOfParameters  -> ListOfParameters
  ; apply jitter to a list of parameters
  (cond
    [(empty? loprms) '()]
    [else (cons (jitter (first loprms)) (jitter-stuff (rest loprms)))]))


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
               INITTANKPARAMS "left")
              (make-parameters
               (parameters-position INITTANKPARAMS)
               (-vec (parameters-velocity INITTANKPARAMS) TANKSPEED)))
(check-expect (tank-control
               INITTANKPARAMS "right")
              (make-parameters
               (parameters-position INITTANKPARAMS)
               (+vec (parameters-velocity INITTANKPARAMS) TANKSPEED)))
(check-expect (tank-control INITTANKPARAMS "p") INITTANKPARAMS)


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


(define (contact? loner swarm)
  ;; Parameter, Parameters -> Bool
  ;; if contact between loner and any element of swarm, return #t
  (and
   (not (empty? swarm))
   (or
    (< (normalize (-vec (parameters-position loner)
                        (parameters-position (first swarm))))
       BLASTRADIUS)
    (contact? loner (rest swarm)))))


(define (alien-invasion? tank invaders)
  ;; Paramerters, Parameters -> Bool
  ;; aliens have landed! (maybe)
  (and
   (not (empty? invaders))
   (or
    (>= (vector-y (parameters-position (first invaders)))
        (vector-y (parameters-position tank)))
    (alien-invasion? tank (rest invaders)))))
; checks
(check-expect (alien-invasion?
               INITTANKPARAMS (list INITINVADERPARAMS)) #f)
(check-expect (alien-invasion?
               INITTANKPARAMS (list INITTANKPARAMS)) #t)


(define (normalize vec)
  ;; Vector -> Vector
  ;; calculates norm of a vector
  (sqrt (+ (sqr (vector-x vec)) (sqr (vector-y vec)))))
;; checks
(check-expect (normalize (make-vector 30 40 )) 50)


(define (render-list-of-stuff loprms img bkgd)
  ; ListOfParameters Img -> Img
  ; render a list of objects as the given image onto the given background
  (cond
    [(empty? loprms) bkgd]
    [else (place-image img
                       (vector-x (parameters-position (first loprms)))
                       (vector-y (parameters-position (first loprms)))
                       (render-list-of-stuff (rest loprms) img bkgd))]))



;; actions!

(main WAROBJECTS)