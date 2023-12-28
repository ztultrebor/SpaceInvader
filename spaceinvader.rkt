;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; ===============================
; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define ALTITUDE 100)
(define GROUNDLEVEL (- HEIGHT ALTITUDE))

(define BLASTRADIUS 75)

(define NUMINVADERS 100)
(define COOLDOWN 14)
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
             (radial-star 20 12 48 "solid" "white")
             (radial-star 10 32 72 "solid" "light purple")))
(define GAMEOVERTEXTCOLOR "white")



; =============================
;; data definitions

; !!! add bombs dropped by invaders

(define-struct war-objects [tank invaders missiles explosions cooldown])
;; A WarObjects is a [Parameters [ListOf Parameter]
;; [ListOf Parameters] [ListOf Parameters] Number]
;; A single tank, a swarm of invaders, a hail of missiles and
;; hopefully, lots of enemies exploding
#;
(define (fn-with-war-objects objs)
  (make-war-objects
   (fn-with-tank (war-objects-tank objs))
   (fn-with-invaders (war-objects-invaders objs))
   (fn-with-missiles (war-objects-missiles objs)))
  (fn-with-explosions (war-objects-explosions objs)))


;; A [ListOf Parameters] is one of
;;   - '()
;;   (cons Parameters [ListOf Parameters])
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
  ;; WarObjects -> WarObjects
  ;; war objects move around in accordance with user input and hard wiring
  (make-war-objects
   (move (war-objects-tank objs))
   (move-invaders objs)
   (move-missiles objs)
   (move-explosions objs)
   (sub1 (war-objects-cooldown objs))))


(define (control objs ke)
  ;; WarObjects -> WarObjects
  ;; move tank with left- and right-arrows, and fire missile on spacebar
  (cond
    [(and (key=? ke " ") (<= (war-objects-cooldown objs) 0))
     (make-war-objects
      (war-objects-tank objs)
      (war-objects-invaders objs)
      (cons (fire-missile (war-objects-tank objs))
            (war-objects-missiles objs))
      (war-objects-explosions objs)
      COOLDOWN)]
    [else (make-war-objects
           (tank-control (war-objects-tank objs) ke)
           (war-objects-invaders objs)
           (war-objects-missiles objs)
           (war-objects-explosions objs)
           (war-objects-cooldown objs))]))
; checks
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() 0) "left")
              (make-war-objects
               (make-parameters (parameters-position INITTANKPARAMS)
                                (+vec (parameters-velocity INITTANKPARAMS)
                                      (make-vector -3 0)))
               (list INITINVADERPARAMS) '() '() 0))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() 0) "right")
              (make-war-objects
               (make-parameters (parameters-position INITTANKPARAMS)
                                (+vec (parameters-velocity INITTANKPARAMS)
                                      (make-vector 3 0)))
               (list INITINVADERPARAMS) '() '() 0))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() 0) "\r")
              (make-war-objects
               INITTANKPARAMS (list INITINVADERPARAMS) '() '() 0))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() 0) " ")
              (make-war-objects
               INITTANKPARAMS (list INITINVADERPARAMS)
               (list (make-parameters
                      (parameters-position INITTANKPARAMS)
                      (+vec (parameters-velocity INITTANKPARAMS)
                            MISSILEVELOCITY))) '() COOLDOWN))


(define (render objs)
  ;; WarObjects -> Img
  ;; display the scene with current positions of all war objects
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
                INITTANKPARAMS (list INITINVADERPARAMS) '() '() 0)) #f)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITINVADERPARAMS (list INITTANKPARAMS) '() '() 0)) #t)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS (list INITINVADERPARAMS)
                (list INITTANKPARAMS) '() 0)) #f)
(check-expect (victory-or-defeat?
               (make-war-objects INITINVADERPARAMS (list INITTANKPARAMS)
                                 (list INITINVADERPARAMS) '() 0)) #t)


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


(define (move object)
  ;; Parameters -> Parameters
  ;; update position vector with velocity
  (make-parameters
   (+vec-w/modulo (parameters-position object) (parameters-velocity object))
   (parameters-velocity object)))
; checks
(check-expect (move (make-parameters (make-vector 12 5) (make-vector 12 5)))
              (make-parameters (make-vector 24 10) (make-vector 12 5)))


(define (move-invaders objs)
  ; WarObjects -> (ListOf Parameters]
  ; delete destroyed invaders, jitter the survivors around
  ; and move them purposefully downward
  (local (
          (define invaders (war-objects-invaders objs))
          (define missiles (war-objects-missiles objs))
          (define survivors (cull-missile-hits invaders missiles))
          (define jittery-survivors (jitter survivors)))
    ; - IN -
    (move-stuff jittery-survivors)))


(define (move-missiles objs)
  ; WarObjects -> [ListOf Parameters]
  ; delete exploded missiles, delete those that leave screen
  ; and move the rest purposefully upward
  (local (
          (define missiles (war-objects-missiles objs))
          (define invaders (war-objects-invaders objs))
          (define dry-powder (cull-missile-hits missiles invaders))
          (define in-play (delete-misses dry-powder)))
    ; - IN -
    (move-stuff in-play)))


(define (move-explosions objs)
; WarObjects -> [ListOf Parameters]
; conver destroved invaders into fireballs, propel them upward,
; delete those outside field of play and move the rest purposefully upward
(local (
        (define explosions (war-objects-explosions objs))
        (define invaders (war-objects-invaders objs))
        (define missiles (war-objects-missiles objs))
        (define fresh-explosions (detonation invaders missiles))
        (define rising-fireballs (rise fresh-explosions))
        (define remaining-faders (delete-misses explosions))
        (define total-carnage (append remaining-faders rising-fireballs)))
  ; -IN -
   (move-stuff  total-carnage)))
                               

(define (move-stuff loprms)
  ; [ListOf Parameters]  -> [ListOf Parameters]
  ; update parameters that are organized into lists
  (map move loprms))


(define (jitter invaders)
  ; [ListOf Parameters] -> [ListOf Parameters]
  ; jitter a group of invaders
  (local (
          (define (jitter invader)
            (make-parameters
             (parameters-position invader)
             (make-vector (+ (random 51) -25)
                          (vector-y (parameters-velocity invader))))))
    ; - IN -
    (map jitter invaders)))


(define (rise explosions)
  ;; [ListOf Parameters] -> [ListOf Parameters]
  ;; move parameters of detonated missile to explosion list
  (local (
          (define (flip-velo pars)
            (make-parameters
             (parameters-position pars)
             (+vec (parameters-velocity pars) MISSILEVELOCITY))))  
    (map flip-velo explosions)))


; !!! abstract these next two functions

(define (cull-missile-hits swarm1 swarm2)
  ;; [ListOf Parameters] [ListOf Parameter] -> [ListOf Parameters]
  ;; delete parameters of an element of swarm1 that has
  ;; made contact with an element of swarm2
  (filter (lambda (i)
            (andmap (lambda (j)
                      (avoid-flak? i j))
                    swarm2)) swarm1))


(define (detonation swarm1 swarm2)
  ;; [ListOf Parameters] [ListOf Parameters] -> [ListOf Parameters]
  ;; move parameters of detonated missile to explosion list
  (filter (lambda (i)
            (ormap (lambda (j)
                     (not (avoid-flak? i j)))
                   swarm2)) swarm1))


(define (delete-misses lop)
  ;; [ListOf Parameters] -> [ListOf Parameters]
  ;; delete missiles and fireballs that exit stage top
  (local (
          (define (overshot? p)
            (> (vector-y (parameters-position p)) 0)))
    ; - IN -
    (filter overshot? lop)))
          

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


(define (avoid-flak? obj1 obj2)
  ;; Parameter, Parameters -> Bool
  ;; if distance between obj1 and obj2 is greater than radius, return #t
  (local (
          (define delta-p (-vec (parameters-position obj1)
                                (parameters-position obj2)))
          (define dist (normalize delta-p)))
    ; - IN-
    (> dist BLASTRADIUS)))


; !!! abstract vector arithmetic function

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


(define (+vec-w/modulo v1 v2)
  ;; Vector, Vector -> Vector
  ;; add one vector to another
  (make-vector (modulo (+ (vector-x v1) (vector-x v2)) WIDTH)
               (+ (vector-y v1) (vector-y v2))))
;; checks
(check-expect (+vec-w/modulo (make-vector 12 5) (make-vector 12 5))
              (make-vector 24 10))
(check-expect (+vec-w/modulo (make-vector WIDTH 5) (make-vector 12 5))
              (make-vector 12 10))
(check-expect (+vec-w/modulo (make-vector 0 5) (make-vector -12 5))
              (make-vector (- WIDTH 12) 10))


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


; ===========================
;; actions!

(define INITTANKPARAMS (make-parameters (make-vector (/ WIDTH 2) GROUNDLEVEL)
                                        (make-vector 0 0)))
(define INITINVADERPARAMS (make-parameters (make-vector (/ WIDTH 2) 50)
                                           (make-vector 0 1)))
(define MISSILEVELOCITY (make-vector 0 -10))
(define TANKSPEED (make-vector 3 0))
(define WAROBJECTS (make-war-objects
                    INITTANKPARAMS
                    (make-list NUMINVADERS INITINVADERPARAMS) '() '() 0))

(main WAROBJECTS)