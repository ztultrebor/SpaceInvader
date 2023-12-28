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
(define BOMBBLASTRADIUS 35)
(define NUMINVADERS 20)
(define COOLDOWN 14)
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
(define BOMB (circle 10 "solid" "green"))
(define DESTRUCTION (radial-star 12 50 100 "solid" "red"))
(define DETONATION (radial-star 8 20 50 "solid" "red"))
(define HIT (overlay
             (radial-star 20 12 48 "solid" "white")
             (radial-star 10 32 72 "solid" "light purple")))
(define GAMEOVERTEXTCOLOR "white")


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
   (move-invaders objs)
   (move-missiles objs)
   (move-bombs objs)
   (move-explosions objs)))


(define (control objs ke)
  ; WarObjects -> WarObjects
  ; move tank with left- and right-arrows, and fire missile on spacebar
  (cond
    [(key=? ke " ") (fire-missile objs)]
    [else (accel-tank objs ke)]))
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
          (define jittery-survivors (map jitter survivors))
          (define cooldown-resets (map reset-cooldown jittery-survivors))
          (define cooled-down-jitterers (map drawdown cooldown-resets)))
    ; - IN -
    (move-stuff cooled-down-jitterers)))
; checks
(check-expect (unit-cooldown
               (first (move-invaders (make-war-objects INITTANKPARAMS
                                                       INITINVADERS
                                                       '() '() '()))))
              (sub1 (unit-cooldown (first INITINVADERS))))


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
  ; release fresh bombs, delete exploded bombs,
  ; delete those that leave screen and move the rest purposefully downward
  (local (
          (define bombs (war-objects-bombs objs))
          (define invaders (war-objects-invaders objs))
          (define new-releases (drop-bombs bombs invaders))
          (define in-play (delete-misses bombs))
          (define all-bombs (append in-play new-releases)))
    ; - IN -
    (move-stuff all-bombs)))


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
    (move-stuff total-carnage)))


(define (move-stuff loprms)
  ; [ListOf Unit]  -> [ListOf Unit]
  ; update Unit that are organized into lists
  (map move loprms))


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
          (define bombers (filter
                           (lambda (x) (< (unit-cooldown x) 0))
                           invaders))
          (define (make-bomb-unit b)
            (make-unit (unit-position b)
                       (+vec (unit-velocity b) (make-vector 0 10)) 0)))
    ; - IN -
    (map make-bomb-unit bombers)))


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
   (cond
     [(< (unit-cooldown invader) 0) (random INVADERCOOLDOWN)]
     [else (unit-cooldown invader)])))

    
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
  (local (
          (define (got-flak? i j)
            (avoid-flak? i j BLASTRADIUS)))
    ; - IN -
    (second-order-filter andmap got-flak? swarm1 swarm2)))


(define (detonation swarm1 swarm2)
  ; [ListOf Unit] [ListOf Unit] -> [ListOf Unit]
  ; move unit of detonated missile to explosion list
  (local (
          (define (smack-flak? i j)
            (not (avoid-flak? i j BLASTRADIUS))))
    ; - IN -
    (second-order-filter ormap smack-flak? swarm1 swarm2)))


(define (delete-misses lop)
  ; [ListOf Unit] -> [ListOf Unit]
  ; delete missiles, and bombs fireballs that exit stage top/bottom
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
               INITTANKPARAMS (list INITTANKPARAMS)) #t)
(check-expect (alien-invasion?
               INITTANKPARAMS INITINVADERS) #f)


(define (tank-destroyed? tank bombs)
  ; Unit [ListOf Unit] -> Bool
  ; tank destroyed by bomb
  (local (
          (define (tank-smack? b)
            (not (avoid-flak? tank b BOMBBLASTRADIUS))))
    ; - IN -
    (ormap tank-smack? bombs)))


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
      (cond
        [(key=? ke "left") (-vec (unit-velocity tank) TANKSPEED)]
        [(key=? ke "right") (+vec (unit-velocity tank) TANKSPEED)]
        [else (unit-velocity tank)])
      (unit-cooldown tank))
     (war-objects-invaders objs)
     (war-objects-missiles objs)
     (war-objects-bombs objs)
     (war-objects-explosions objs))))


(define (avoid-flak? obj1 obj2 radius)
  ; Unit Unit -> Bool
  ; if distance between obj1 and obj2 is greater than radius, return #t
  (local (
          (define delta-p (-vec (unit-position obj1)
                                (unit-position obj2)))
          (define dist (normalize delta-p)))
    ; - IN-
    (> dist radius)))


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

(define INITTANKPARAMS
  (make-unit (make-vector (/ WIDTH 2) GROUNDLEVEL)
             (make-vector 0 0) 0))
(define INITINVADERS
  (build-list NUMINVADERS
              (lambda (x)
                (make-unit (make-vector (/ WIDTH 2) 50)
                           (make-vector 0 1) (random INVADERCOOLDOWN)))))
(define MISSILEVELOCITY (make-vector 0 -10))
(define TANKSPEED (make-vector 3 0))
(define WAROBJECTS (make-war-objects
                    INITTANKPARAMS INITINVADERS '() '() '()))

(main WAROBJECTS)