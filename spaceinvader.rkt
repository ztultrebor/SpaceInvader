;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname spaceinvader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

; A space-invaders-like game


; =============================
; data definitions

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
   (fn-with-missiles (war-objects-missiles objs))
   (fn-with-bombs (war-objects-bombs objs))
   (fn-with-explosions (war-objects-explosions objs))))


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
;     of a weapon, along with a cooldown timefor firing a weapon in ticks
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



; ===============================
; constants

(define WIDTH 1400)
(define HEIGHT 750)
(define ALTITUDE 100)
(define GROUNDLEVEL (- HEIGHT ALTITUDE))
(define BLASTRADIUS 75)
(define BOMBBLASTRADIUS 35)
(define NUMINVADERS 20)
(define COOLDOWN 10)
(define INVADERCOOLDOWN 112)
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
(define BOMB (overlay (circle 10 "outline" "white")
                      (circle 10 "solid" "black")))
(define DESTRUCTION (radial-star 12 50 100 "solid" "red"))
(define DETONATION (radial-star 8 20 50 "solid" "red"))
(define HIT (overlay
             (radial-star 10 8 72 "solid" "white")
             (radial-star 10 32 72 "solid" "light purple")))
(define GAMEOVERTEXTCOLOR "white")
(define INITTANKPARAMS
  (make-unit (make-vector (/ WIDTH 2) GROUNDLEVEL)
             (make-vector 0 0) 0))
(define INITINVADERS
  (build-list NUMINVADERS
              (lambda (x)
                (make-unit (make-vector (random WIDTH) 50)
                           (make-vector 0 1) (random INVADERCOOLDOWN)))))
(define MISSILEVELOCITY (make-vector 0 -10))
(define TANKSPEED (make-vector 3 0))
(define WAROBJECTS (make-war-objects
                    INITTANKPARAMS INITINVADERS '() '() '()))



; ==================================
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
  (local (
          (define invaders (war-objects-invaders objs))
          (define missiles (war-objects-missiles objs)))
    ; - IN- 
    (make-war-objects
     (move-tank (war-objects-tank objs))
     (move-invaders invaders missiles)
     (move-missiles missiles invaders)
     (move-bombs (war-objects-bombs objs) invaders)
     (move-explosions (war-objects-explosions objs) invaders missiles))))


(define (control objs ke)
  ; WarObjects -> WarObjects
  ; move tank with left- and right-arrows, and fire missile on spacebar
  (match ke
    [" " (fire-missile objs)]
    ["left" (accel-tank objs ke)]
    ["right" (accel-tank objs ke)]
    [key-event? objs]))
; checks
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 INITINVADERS '() '() '()) "left")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (+vec (unit-velocity INITTANKPARAMS)
                                (make-vector -3 0)) 0)
               INITINVADERS '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS
                                 INITINVADERS '() '() '()) "right")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (+vec (unit-velocity INITTANKPARAMS)
                                (make-vector 3 0)) 0)
               INITINVADERS '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS INITINVADERS '() '() '()) "\r")
              (make-war-objects
               INITTANKPARAMS INITINVADERS '() '() '()))
(check-expect (control
               (make-war-objects INITTANKPARAMS INITINVADERS '() '() '()) " ")
              (make-war-objects
               (make-unit (unit-position INITTANKPARAMS)
                          (unit-velocity INITTANKPARAMS)
                          COOLDOWN)
               INITINVADERS
               (list (make-unit
                      (unit-position INITTANKPARAMS)
                      (+vec (unit-velocity INITTANKPARAMS)
                            MISSILEVELOCITY) 0)) '() '()))


(define (render objs)
  ; WarObjects -> Img
  ; display the scene with current positions of all war objects
  (render-list-of-stuff
   (war-objects-explosions objs) HIT
   (render-list-of-stuff
    (war-objects-bombs objs) BOMB
    (render-list-of-stuff
     (war-objects-missiles objs) MISSILE
     (render-list-of-stuff
      (war-objects-invaders objs) INVADER
      (render-list-of-stuff
       (list (war-objects-tank objs))
       (cond
         [(tank-destroyed? (war-objects-tank objs) (war-objects-bombs objs))
          DESTRUCTION]
         [else TANK])
       BACKGROUND))))))


(define (victory-or-defeat? objs)
  ; WarObjects -> Bool
  ; ends when all invaders are destroyed or one invader successfully lands
  (or
   (empty? (war-objects-invaders objs))
   (alien-invasion? (war-objects-tank objs) (war-objects-invaders objs))
   (tank-destroyed? (war-objects-tank objs) (war-objects-bombs objs))))
; checks
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS INITINVADERS '() '() '())) #f)
(check-expect (victory-or-defeat?
               (make-war-objects
                INITTANKPARAMS INITINVADERS
                (list INITTANKPARAMS) '() '())) #f)


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


(define (move-tank tank)
  ; Unit -> Unit
  ; update tank location and cooldown
  (move (drawdown tank)))


(define (move-invaders invaders missiles)
  ; WarObjects -> (ListOf Unit]
  ; delete destroyed invaders, jitter the survivors around
  ; and move them purposefully downward
  (local (
          (define survivors (cull-strikes invaders missiles))
          (define jittery-survivors (map jitter survivors))
          (define cooldown-resets (map reset-cooldown jittery-survivors))
          (define cooled-down-jitterers (map drawdown cooldown-resets)))
    ; - IN -
    (move-stuff cooled-down-jitterers)))
; checks
(check-expect (unit-cooldown
               (first (move-invaders INITINVADERS (list INITTANKPARAMS))))
              (sub1 (unit-cooldown (first INITINVADERS))))


(define (move-missiles missiles invaders)
  ; WarObjects -> [ListOf Unit]
  ; delete exploded missiles, delete those that leave screen
  ; and move the rest purposefully upward
  (local (
          (define dry-powder (cull-strikes missiles invaders))
          (define in-play (delete-misses dry-powder)))
    ; - IN -
    (move-stuff in-play)))


(define (move-bombs bombs invaders)
  ; WarObjects -> [ListOf Unit]
  ; release fresh bombs, delete exploded bombs,
  ; delete those that leave screen and move the rest purposefully downward
  (local (
          (define new-releases (drop-bombs bombs invaders))
          (define in-play (delete-misses bombs))
          (define all-bombs (append in-play new-releases)))
    ; - IN -
    (move-stuff all-bombs)))


(define (move-explosions explosions invaders missiles)
  ; WarObjects -> [ListOf Unit]
  ; conver destroved invaders into fireballs, propel them upward,
  ; delete those outside field of play and move the rest purposefully upward
  (local (
          (define fresh-explosions (detonation invaders missiles))
          (define rising-fireballs (rarify fresh-explosions))
          (define remaining-faders (delete-misses explosions))
          (define total-carnage (append remaining-faders rising-fireballs)))
    ; -IN -
    (move-stuff total-carnage)))


(define (fire-missile objs)
  ; WeaponObjects -> Unit
  ; fire missile on spacebar; missile takes x-position and velocity of tank
  (local (
          (define tank (war-objects-tank objs)))
    ; - IN -
    (cond
      [(<= (unit-cooldown tank) 0)
       (make-war-objects
        (make-unit
         (unit-position tank)
         (unit-velocity tank)
         COOLDOWN)
        (war-objects-invaders objs)
        (cons
         (make-unit (unit-position tank)
                    (+vec (unit-velocity tank) MISSILEVELOCITY) 0)
         (war-objects-missiles objs))
        (war-objects-bombs objs)
        (war-objects-explosions objs))]
      [else objs])))


(define (accel-tank objs ke)
  ; WarObjects KeyEvent -> Unit
  ; send tank left with left arrow or right with right
  (local (
          (define tank (war-objects-tank objs)))
    ; - IN -
    (make-war-objects
     (make-unit
      (unit-position tank)
      (match ke
        ["left" (-vec (unit-velocity tank) TANKSPEED)]
        ["right" (+vec (unit-velocity tank) TANKSPEED)])
      (unit-cooldown tank))
     (war-objects-invaders objs)
     (war-objects-missiles objs)
     (war-objects-bombs objs)
     (war-objects-explosions objs))))


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
               INITTANKPARAMS (list INITTANKPARAMS)) #t)
(check-expect (alien-invasion?
               INITTANKPARAMS INITINVADERS) #f)


(define (tank-destroyed? tank bombs)
  ; Unit [ListOf Unit] -> Bool
  ; tank destroyed by bomb
  (ormap (lambda (bomb) ((catch-flak? tank BOMBBLASTRADIUS) bomb )) bombs))


(define (render-list-of-stuff loprms img bkgd)
  ; ListOfUnit Img -> Img
  ; render a list of objects as the given image onto the given background
  (match loprms
    ['() bkgd]
    [(cons pre post) (place-image img
                       (vector-x (unit-position pre))
                       (vector-y (unit-position pre))
                       (render-list-of-stuff post img bkgd))]))


(define (move object)
  ; Unit -> Unit
  ; update position vector with velocity
  (make-unit
   (vec-modulo (unit-position object) (unit-velocity object))
   (unit-velocity object) (unit-cooldown object)))
; checks
(check-expect (move (make-unit (make-vector 12 5) (make-vector 12 5) 0))
              (make-unit (make-vector 24 10) (make-vector 12 5) 0))


(define (move-stuff loprms)
  ; [ListOf Unit]  -> [ListOf Unit]
  ; update Unit that are organized into lists
  (map move loprms))


(define (drawdown obj)
  ; Unit -> Unit
  ; decrement the unit's reaining cooldown time by 1
  (make-unit
   (unit-position obj)
   (unit-velocity obj)
   (sub1 (unit-cooldown obj))))


(define (reset-cooldown invader)
  ; [ListOf Unit] -> [ListOf Unit]
  ; reset invader cooldowns after bomb release
  (make-unit
   (unit-position invader)
   (unit-velocity invader)
   (match (unit-cooldown invader)
     [0 (random INVADERCOOLDOWN)]
     [cd cd])))


(define (jitter invader)
  ; Unit -> Unit
  ; add some jitter to an invader's horizontal velocity
  (make-unit
   (unit-position invader)
   (make-vector (+ (random 51) -25)
                (vector-y (unit-velocity invader)))
   (unit-cooldown invader)))


(define (drop-bombs bombs invaders)
  ; [ListOf Unit] [ListOf Unit] -> [ListOf Unit]
  ; release bombs when invader cooldown reaches zero
  (local (
          (define (fire-when-ready inv)
            (= (unit-cooldown inv) 0))
          (define bombers (filter fire-when-ready invaders))
          (define (make-bomb-unit b)
            (make-unit (unit-position b)
                       (+vec (unit-velocity b) (make-vector 0 10)) 0)))
    ; - IN -
    (map make-bomb-unit bombers)))


(define (rarify explosions)
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
  (filter (lambda (i) (fmap (lambda (j) (fpred i j)) obj-lst)) subj-lst))
; This lambdized version is equivalent to the following. The inner function
; must be defined inside the scope of the outer function in order to access
; the variable i--which exists only in the scope of the outer function--
; without taking it as an argument
#;
(local (
        (define (outer-func1 i)
          (local (
                  (define (inner-func j)
                    (fpred i j)))
            ; - IN -
            (fmap inner-func obj-lst))))
  ; - IN -
  (filter outer-func1 subj-lst))


(define (cull-strikes subj-swarm obj-swarm)
  ; [ListOf Unit] [ListOf Parameter] -> [ListOf Unit]
  ; delete unit of an element of swarm1 that has
  ; made contact with an element of swarm2
  (second-order-filter andmap
                       (lambda (subj obj)
                         (not ((catch-flak? subj BLASTRADIUS) obj)))
                       subj-swarm obj-swarm))


(define (detonation invaders missiles)
  ; [ListOf Unit] [ListOf Unit] -> [ListOf Unit]
  ; move unit of destroyed invader to explosion list
  (second-order-filter ormap
                       (lambda (invader missile)
                         ((catch-flak? invader BLASTRADIUS) missile))
                       invaders missiles))


(define (delete-misses swarm)
  ; [ListOf Unit] -> [ListOf Unit]
  ; delete missiles, bombs and fireballs that exit stage top/bottom
  (filter (lambda (swarm-obj)
            (< 0 (vector-y (unit-position swarm-obj)) HEIGHT)) swarm))


(define (catch-flak? subj radius)
  ; Unit Number -> [Unit -> Boolean]
  ; if distance between obj1 and obj2 is less than radius, return #t
  (lambda (obj)
    (local (
            (define delta-p (-vec (unit-position subj)
                                  (unit-position obj)))
            (define dist (normalize delta-p)))
      ; - IN-
      (< dist radius))))


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
  (vector-arithmetic (lambda (x y) (modulo (+ x y) WIDTH)) v1 v2))
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


; ===========================
; actions!

(main WAROBJECTS)