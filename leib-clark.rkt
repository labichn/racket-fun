#lang racket

;; Racket is a batteries-included lisp, including the tools to build a couple
;; of universes.
(require 2htdp/universe)
(require 2htdp/image)

;; Our universe is going to be pretty sparse. Let's say it's two dimensional
;; and the only substances in it are a couple of spheres with radii of one
;; unit and two units, and the center of the sphere with the radius of one
;; is three units above the sphere with the radius of two. Hardly the best of
;; all possible worlds, but then again, I've never been called benevolent.

;; Clarke, as Newton's proxy, holds the view that space has some form of
;; existence, an implicit Cartesian space.
(define-struct fnu (width height substs))
;; A finite Newtonian universe is a (make-fnu width height substances)
;; where width and height are natural numbers and substances is a list of
;; pairs of locations and substances. Width and height are the bounds of
;; space, and the locations of all the substances must be within the bounds set
;; by width and height.
(define-struct inu (substs))
;; An infinite Newtonian universe is a (make-inu substances)
;; where substs is a list of pairs of locations and substances.
;; A location is a pair of natural numbers.

(define-struct circ (loc radius))
;; A substance in Clarke's universe is a (make-circ location radius)
;; where location is a pair of natural numbers and radius is a natural number.

;; The universe described above can be implemented as an finite Newtonian
;; universe with space that spans fifty units by fifty units as follows:
(define x 20)
(define y 20)
(define scal 3)
(define subst1 (make-circ (cons 5 5) 1))
(define subst2 (make-circ (cons 5 8) 2))
(define simple-fnu (make-fnu x y (list subst1 subst2)))

;; Now lets flex our omniscience a bit. As God, we can see our universe.

;; perceive : fnu -> image
;; Creates an image representation of a finite Newtonian universe.
(define (scale-it x) (* scal x))
(define (perceive u)
  (foldl (λ (n a) (place-image (circle (scale-it (circ-radius n)) 'solid 'blue)
                               (scale-it (car (circ-loc n)))
                               (scale-it (cdr (circ-loc n)))
                               a))
         (empty-scene (scale-it (fnu-width u)) (scale-it (fnu-height u)))
         (fnu-substs u)))

;; Now, the Clarkean God is a tinkerer.

;; tinker : fnu -> fnu
;; Planned maintenance. Some would say that planned maintenance makes God look
;; like he doesn't know what he's doing. That's just silly. A wizard is never
;; late, nor is he early; he arrives precisely when he means to. Either way,
;; there's no tinkering that needs to be done in a universe so simple, so we'll
;; just leave the universe as is, making big-bang superfluous. If, however, we
;; had the desire to do some tinkering, this is where we would do it.
(define (tinker u) u)

;; Now, let's have at it!
;; (big-bang simple-fnu (on-tick tinker) (to-draw perceive))
(perceive simple-fnu)

;; Of course, you probably already noticed I picked some arbitrary values for
;; the absolute coordinates of the substances in our universe. Since all we
;; care about is that the center of the circle with the radius of one is three
;; units above the circle with the radius of two, there are a whole bunch of
;; different locations in the universe that satisfy those conditions. In fact, we
;; can generate all pairs of locations that satisfy those conditions, and all of
;; the resulting universes.

;; cart-prod : (listof X) (listof Y) -> (listof (pairof X Y))
;; Generates the Cartesian product of the given lists.
(define (cart-prod lox loy)
  (foldl (λ (x a1)
            (append (foldl (λ (y a2) (cons (cons x y) a2)) '() loy) a1))
         null
         lox))

;; gen-locs : (location location -> bool) nat nat ->
;;            (listof (pairof location location))
;; Generates the list of pairs of locations that satisfy the predicate for a
;; universe of the given width and height.
(define (gen-locs pred width height)
  (let* [(ident (λ (x) x))
         (xs (build-list width ident))
         (ys (build-list height ident))
         (possible-locs (append (cart-prod xs ys)
                                (cart-prod ys xs)))]
    (remove-duplicates (filter (λ (x) (pred (car x) (cdr x)))
                               (cart-prod possible-locs possible-locs)))))

;; possible-fnus : nat nat -> (listof fnu)
;; Generates all possible finite Newtonian universes which satisfy the
;; conditions of our universe, namely, that a circle with the radius of one
;; unit is three units above a circle with the radius of two units.
(define (possible-fnus width height)
  (define (within-bounds r loc)
    (and (>= (-  width r) (car loc) r)
         (>= (- height r) (cdr loc) r)))
  (foldl (λ (n a)
            (if (and (within-bounds 1 (car n))
                     (within-bounds 2 (cdr n)))
                (cons (make-fnu width
                                height
                                (list (make-circ (car n) 1)
                                      (make-circ (cdr n) 2)))
                      a)
                a))
         null
         (gen-locs (λ (x y) (and (zero? (- (car x) (car y)))
                                 (= 3 (- (cdr y) (cdr x)))))
                   width
                   height)))

;; Now, if we were on Leibniz's side, we would point out the arbitrary
;; nature of an FNU's absolute coordinates, by translating them to our
;; understanding of monads. A monad contains within it the predicates
;; which denote relational distance between it and other monads. If we
;; take our example universe, `simple-fnu', in Leibniz's framework, it
;; would look as follows:

;; TODO: Add Leibniz's universe and leib->clarke / clarke->leib
