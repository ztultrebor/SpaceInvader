;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A space-invaders-like game

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
(define BOMB (circle 10 "solid" "green"))
(define DESTRUCTION (radial-star 12 50 100 "solid" "red"))
(define DETONATION (radial-star 8 20 50 "solid" "red"))
(define HIT (overlay
             (radial-star 20 12 48 "solid" "white")
             (radial-star 10 32 72 "solid" "light purple")))
(define GAMEOVERTEXTCOLOR "white")


; =============================
; data definitions

; !!! add bombs dropped by invaders

(define-struct war-objects [tank invaders missiles bombs explosions])
; A WarObjects is a [Unit [ListOf Unit]
; [ListOf Unit] [ListOf Unit] [ListOf Unit]]
; A single tank, a swarm of scary invaders, a hail of missiles,
; a cannonade of enemy fire and hopefully, lots of enemies exploding
; also has a cooldown for player's shooting
#;
(define (fn-with-war-objects objs)
  (make-war-objects
   (fn-with-tank (war-objects-tank objs))
   (fn-with-invaders (war-objects-invaders objs))
   (fn-with-missiles (war-objects-missiles objs)))
  (fn-with-bombs (war-objects-bombs objs))
  (fn-with-explosions (war-objects-explosions objs)))


; A [ListOf Unit] is one of
;   - '()
;   (cons Unit [ListOf Unit])
#;
(define (fn-with-lop lop)
  (cond
    [(empty? lop) ...]
    [else (fn-on-unit (first lop)) ... (fn-with-lop (rest lop))]))


(define-struct unit [position velocity cooldown])
; A Unit is a [Vector Vector Number]
; gives the position in pixels and velocity in pixels/tick
;     of a weapon
#;
(define (fn-with-unit params)
  (... (unit-position params) ...)
  (... (unit-velocity params) ...))


(define-struct vector [x y])
; A Vector is a make-vector [Number Number]
;     a mathematical object in 2D cartesian space
#;
(define (fn-with-vector vec)
  (... (vector-x vec) ...)
  (... (vector-y vec) ...))



; functions

(define (main objs)
  ; WarObjects -> WarObjects
  ; run the pocket universe
  (big-bang objs
    [on-tick deploy]
    [on-key control]
    [to-draw render]
    [stop-when victory-or-defeat? render-game-over]))


(define (deploy objs)
  ; WarObjects -> WarObjects
  ; war objects move around in accordance with user input and hard wiring
  (make-war-objects
   (drawdown (move (war-objects-tank objs)))
   (map drawdown (move-invaders objs))
   (move-missiles objs)
   (move-bombs objs)
   (move-explosions objs)))
   

(define (drawdown obj)
  (make-unit
   (unit-position obj)
   (unit-velocity obj)
   (sub1 (unit-cooldown obj))))


(define (control objs ke)
  ; WarObjects -> WarObjects
  ; move tank with left- and right-arrows, and fire missile on spacebar
  (cond
    [(and (key=? ke " ") (<= (unit-cooldown (war-objects-tank objs)) 0))
     (make-war-objects
      (make-unit
       (unit-position (war-objects-tank objs))
       (unit-velocity (war-objects-tank objs))
       COOLDOWN)
      (war-objects-invaders objs)
      (cons (fire-missile (war-objects-tank objs))
            (war-objects-missiles objs))
      (war-objects-bombs objs)
      (war-objects-explosions objs))]
    [else (make-war-objects
           (tank-control (war-objects-tank objs) ke)
           (war-objects-invaders objs)
           (war-objects-missiles objs)
           (war-objects-bombs objs)
           (war-objects-explosions objs))]))
; checks
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() '()) "left")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (+vec (unit-velocity INITTANKPARAMS)
                                (make-vector -3 0)) 0)
               (list INITINVADERPARAMS) '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() '()) "right")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (+vec (unit-velocity INITTANKPARAMS)
                                (make-vector 3 0)) 0)
               (list INITINVADERPARAMS) '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() '()) "\r")
              (make-war-objects
               INITTANKPARAMS (list INITINVADERPARAMS) '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 (list INITINVADERPARAMS) '() '() '()) " ")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (unit-velocity INITTANKPARAMS)
                          COOLDOWN)
               (list INITINVADERPARAMS)
               (list (make-unit
                      (unit-position INITTANKPARAMS)
                      (+vec (unit-velocity INITTANKPARAMS)
                            MISSILEVELOCITY) 0)) '() '()))


(define (render objs)
  ; WarObjects -> Img
  ; display the scene with current positions of all war objects
  ; !!! tank explosion
  (render-list-of-stuff
   (war-objects-explosions objs) HIT
   (render-list-of-stuff
    (war-objects-explosions objs) BOMB
    (render-list-of-stuff
     (war-objects-missiles objs) MISSILE
     (render-list-of-stuff
      (war-objects-invaders objs) INVADER
      (render-list-of-stuff
       (list (war-objects-tank objs)) TANK BACKGROUND))))))


; !!! tank destroyed by bomb?
(define (victory-or-defeat? objs)
  ; WarObjects -> Bool
  ; ends when all invaders are destroyed or one invader successfully lands
  (or
   (empty? (war-objects-invaders objs))
   (alien-invasion? (war-objects-tank objs) (war-objects-invaders objs))))
; checks
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS (list INITINVADERPARAMS) '() '() '())) #f)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITINVADERPARAMS (list INITTANKPARAMS) '() '() '())) #t)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS (list INITINVADERPARAMS)
                (list INITTANKPARAMS) '() '())) #f)
(check-expect (victory-or-defeat?
               (make-war-objects INITINVADERPARAMS (list INITTANKPARAMS)
                                 (list INITINVADERPARAMS) '() '())) #t)


(define (render-game-over objs)
  ; WarObjects -> Img
  ; display game over screen
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
  ; Unit -> Unit
  ; update position vector with velocity
  (make-unit
   (vec-modulo (unit-position object) (unit-velocity object))
   (unit-velocity object) (unit-cooldown object)))
; checks
(check-expect (move (make-unit (make-vector 12 5) (make-vector 12 5) 0))
              (make-unit (make-vector 24 10) (make-vector 12 5) 0))


(define (move-invaders objs)
  ; WarObjects -> (ListOf Unit]
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
  ; WarObjects -> [ListOf Unit]
  ; delete exploded missiles, delete those that leave screen
  ; and move the rest purposefully upward
  (local (
          (define missiles (war-objects-missiles objs))
          (define invaders (war-objects-invaders objs))
          (define dry-powder (cull-missile-hits missiles invaders))
          (define in-play (delete-misses dry-powder)))
    ; - IN -
    (move-stuff in-play)))


(define (move-bombs objs)
  ; WarObjects -> [ListOf Unit]
  ; delete exploded bombs, delete those that leave screen
  ; and move the rest purposefully downward
  (war-objects-bombs objs))


(define (move-explosions objs)
  ; WarObjects -> [ListOf Unit]
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
  ; [ListOf Unit]  -> [ListOf Unit]
  ; update Unit that are organized into lists
  (map move loprms))


(define (jitter invaders)
  ; [ListOf Unit] -> [ListOf Unit]
  ; jitter a group of invaders
  (local (
          (define (jitter invader)
            (make-unit
             (unit-position invader)
             (make-vector (+ (random 51) -25)
                          (vector-y (unit-velocity invader))) 0)))
    ; - IN -
    (map jitter invaders)))


(define (rise explosions)
  ; [ListOf Unit] -> [ListOf Unit]
  ; move unit of detonated missile to explosion list
  (local (
          (define (flip-velo pars)
            (make-unit
             (unit-position pars)
             (+vec (unit-velocity pars) MISSILEVELOCITY) 0)))  
    (map flip-velo explosions)))


(define (second-order-filter fmap fpred subj-lst obj-lst)
  ; [[Unit Unit -> Boolean] [ListOf Unit] -> Boolean]
  ; [Unit Unit -> Boolean] [ListOf Unit]
  ; [ListOf Unit] -> [ListOf Unit]
  ;;; abstract function that enables a filtration that requires
  ; recursion over two lists
  (filter (lambda (i)
            (fmap (lambda (j)
                    (fpred i j))
                  obj-lst)) subj-lst))


(define (cull-missile-hits swarm1 swarm2)
  ; [ListOf Unit] [ListOf Parameter] -> [ListOf Unit]
  ; delete unit of an element of swarm1 that has
  ; made contact with an element of swarm2
  (second-order-filter andmap avoid-flak? swarm1 swarm2))


(define (detonation swarm1 swarm2)
  ; [ListOf Unit] [ListOf Unit] -> [ListOf Unit]
  ; move unit of detonated missile to explosion list
  (local (
          (define (smack-flak? i j)
            (not (avoid-flak? i j))))
    ; - IN -
    (second-order-filter ormap smack-flak? swarm1 swarm2)))


(define (delete-misses lop)
  ; [ListOf Unit] -> [ListOf Unit]
  ; delete missiles and fireballs that exit stage top
  (local (
          (define (overshot? p)
            (< 0 (vector-y (unit-position p)) HEIGHT)))
    ; - IN -
    (filter overshot? lop)))
          

(define (alien-invasion? tank invaders)
  ; Paramerters, Unit -> Bool
  ; aliens have landed! (maybe)
  (and
   (not (empty? invaders))
   (or
    (>= (vector-y (unit-position (first invaders)))
        (vector-y (unit-position tank)))
    (alien-invasion? tank (rest invaders)))))
; checks
(check-expect (alien-invasion?
               INITTANKPARAMS (list INITINVADERPARAMS)) #f)
(check-expect (alien-invasion?
               INITTANKPARAMS (list INITTANKPARAMS)) #t)


(define (fire-missile tank)
  ; WeaponObjects -> Unit
  ; fire missile on spacebar; missile takes x-position and velocity of tank
  (make-unit (unit-position tank)
             (+vec (unit-velocity tank) MISSILEVELOCITY) 0))
; checks
(check-expect (fire-missile
               (make-unit (make-vector 27 42) TANKSPEED 0))
              (make-unit (make-vector 27 42)
                         (+vec TANKSPEED MISSILEVELOCITY) 0))


(define (tank-control tank ke)
  ; Unit, Key Event -> Unit
  ; send tank left with left arrow or right with right
  (make-unit
   (unit-position tank)
   (cond
     [(key=? ke "left") (-vec (unit-velocity tank) TANKSPEED)]
     [(key=? ke "right") (+vec (unit-velocity tank) TANKSPEED)]
     [else (unit-velocity tank)])
   (unit-cooldown tank)))
; checks
(check-expect (tank-control
               INITTANKPARAMS "left")
              (make-unit
               (unit-position INITTANKPARAMS)
               (-vec (unit-velocity INITTANKPARAMS) TANKSPEED) 0))
(check-expect (tank-control
               INITTANKPARAMS "right")
              (make-unit
               (unit-position INITTANKPARAMS)
               (+vec (unit-velocity INITTANKPARAMS) TANKSPEED) 0))
(check-expect (tank-control INITTANKPARAMS "p") INITTANKPARAMS)


(define (avoid-flak? obj1 obj2)
  ; Parameter, Unit -> Bool
  ; if distance between obj1 and obj2 is greater than radius, return #t
  (local (
          (define delta-p (-vec (unit-position obj1)
                                (unit-position obj2)))
          (define dist (normalize delta-p)))
    ; - IN-
    (> dist BLASTRADIUS)))


(define (vector-arithmetic op v1 v2)
  ; [Number Number -> Number] Vector Vector -> Vector
  ;;; abstract function for vector arithmetic
  (make-vector (op (vector-x v1) (vector-x v2))
               (op (vector-y v1) (vector-y v2))))


(define (+vec v1 v2)
  ; Vector, Vector -> Vector
  ; add one vector to another
  (vector-arithmetic + v1 v2))
; checks
(check-expect (+vec (make-vector 12 5) (make-vector 12 5)) (make-vector 24 10))


(define (-vec v1 v2)
  ; Vector, Vector -> Vector
  ; subtract one vector from another
  (vector-arithmetic - v1 v2))
; checks
(check-expect (-vec (make-vector 12 5) (make-vector 12 5)) (make-vector 0 0))


(define (vec-modulo v1 v2)
  ; Vector, Vector -> Vector
  ; add one vector to another
  (local (
          (define (mod x y)
            (modulo (+ x y) WIDTH)))
    ; -IN -
    (vector-arithmetic mod v1 v2)))
; checks
(check-expect (vec-modulo (make-vector 12 5) (make-vector 12 5))
              (make-vector 24 10))
(check-expect (vec-modulo (make-vector WIDTH 5) (make-vector 12 5))
              (make-vector 12 10))
(check-expect (vec-modulo (make-vector 0 5) (make-vector -12 5))
              (make-vector (- WIDTH 12) 10))


(define (normalize vec)
  ; Vector -> Vector
  ; calculates norm of a vector
  (sqrt (+ (sqr (vector-x vec)) (sqr (vector-y vec)))))
; checks
(check-expect (normalize (make-vector 30 40 )) 50)


(define (render-list-of-stuff loprms img bkgd)
  ; ListOfUnit Img -> Img
  ; render a list of objects as the given image onto the given background
  (cond
    [(empty? loprms) bkgd]
    [else (place-image img
                       (vector-x (unit-position (first loprms)))
                       (vector-y (unit-position (first loprms)))
                       (render-list-of-stuff (rest loprms) img bkgd))]))


; ===========================
; actions!

(define INITTANKPARAMS (make-unit (make-vector (/ WIDTH 2) GROUNDLEVEL)
                                  (make-vector 0 0) 0))
(define INITINVADERPARAMS (make-unit (make-vector (/ WIDTH 2) 50)
                                     (make-vector 0 1) (random 56)))
(define MISSILEVELOCITY (make-vector 0 -10))
(define TANKSPEED (make-vector 3 0))
(define WAROBJECTS (make-war-objects
                    INITTANKPARAMS
                    (make-list NUMINVADERS INITINVADERPARAMS) '() '() '()))

(main WAROBJECTS)