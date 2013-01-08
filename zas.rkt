#lang htdp/asl

(require 2htdp/image)
(require 2htdp/universe)
(require racket/base)

;; Poorly coded zombie apocalypse simulator. First time I was ever able to do a
;; 'creative project' for a class assignment. Lots of list abuse going on.

#| ZAS initial planning... some or all is inaccurate.
 | 
 | GUI
 | - humans
 |   - small circles on canvas
 |   - blue, intensity based on remaining health
 |   - limited endurance; speed ranges from sprint to standard zombie
 |     speed
 |   - some start with projectile weapons, all fall back to melee
 | - zombies
 |   - small circles on canvas
 |   - green, intensity based on remaining health
 |   - health for zombies - headshot/non-headshot; non-headshots effect
 |     speed temporarily (1-2 seconds)
 | - terrain
 |
 |   - base-zombie-deterioration-rate => .01/tick
 |   - a multiplier is a float between 0 and 2
 |   - a percentage is a float between 0 and 1
 |   - a Terrain is a 5-tuple:
 |     '(Color Speed-Multiplier Resource-Multiplier Zombie-Deterioration-Rate
 |       Zombie-Start-Rate)
 |     where Color is one of {'grey, 'green, 'yellow, 'white, 'blue}
 |       and          Speed-Multiplier is a 2-tuple '(multiplier multiplier)
 |       and       Resource-Multiplier is a multiplier
 |       and Zombie-Deterioration-Rate is a percentage
 |       and         Zombie-Start-Rate is a percentage
 |     and one of
 |     - Mountain
 |     - Flatland
 |     - Desert
 |     - Water
 |     - Suburb
 |     - City
 |     Where Mountain is: '(white (.5 .5) 1 (+ b-z-d-r .25) 0)
 |       and Flatland is: '(green (1 1) 1.1 b-z-d-r .1)
 |       and   Desert is: '(yellow (.75 1) 1 (+ b-z-d-r .25) .1)
 |       and    Water is: '(blue (0 .75) 1 (+ b-z-d-r .25) 0)
 |       and   Suburb is: '(green (1 1) 1.25 b-z-d-r .25)
 |       and     City is: '(grey (.75 1) 1.5 b-z-d-r .75)
 |
 |   - {mountainous, flatland, desert, water, suburb, city}
 |     - mountainous
 |       - white
 |       - zombies and people move slower (50%), zombies freeze eventually
 |         (considered dead)
 |       - 0% resource rate (speed at which human resources are refilled)
 |       - 0% zombie start rate
 |     - flatland
 |       - dark green
 |       - full speed for both
 |       - 10% resource rate
 |       - 10% zombie start rate
 |     - desert
 |       - grey/yellow
 |       - people move slower (25%), zombies deteriate faster (25%)
 |       - 0% resource rate
 |       - 0% zombie start rate
 |     - water
 |       - blue
 |       - humans cannot cross, zombies move slower (25%)
 |       - 0% resource rate
 |       - 0% zombie start rate
 |     - suburb
 |       - light  green with small house images?
 |       - full speed for both
 |       - 25% resource rate
 |       - 25% zombie start rate
 |     - city
 |       - grey with large building images?
 |       - people move slower (25%)
 |       - 50% resource rate
 |       - 75% zombie start rate
 | - a terrain-marker is one of {'mountainous, 'flatland, 'desert, 'water, 'suburb, 'city}
 | - a map is an M x N list of lists of terrain-markers
 | - a SNat is a Nat from 0 to 99
 | - a location is a 2-tuple '(SNat SNat)
 | - a person is a 3-tuple '(Resources Location)
 |   Where Resources is a SNat
 | - a zombie is a 2-tuple '(Health Location)
 |   Where Health is an SNat
 |#

;; World constants
(define width0 50)
(define height0 50)

;; Drawing constants
(define pixel-offset 16)

;; A Nat is an integer in the range [0, inf)
;; A City is:
(define     city '(    "dim gray" (1 2) 1 6))
;; A Suburb is:
(define   suburb '(        "gray" (1 2) 1 5))
;; A Flatland is:
(define flatland '("yellow green" (1 2) 0 4))
;; A Desert is:
(define   desert '(       "khaki" (4 2) 0 3))
;; A Mountain is:
(define mountain '(       "white" (4 2) 0 2))
;; A Water is:
(define    water '(  "steel blue" (4 4) 0 1))
;; A Terrian is a 3-tuple: '(color traversal-time resource-rate rank)
;; Where color is one of {'white, 'green, 'yellow, 'blue, 'green, 'grey},
;;       traversal-time is a Nat, the number of ticks it takes to leave the Terrain,
;;       resource-rate is the number of resources gained when entering the Terrain,
;;   and rank is the relative attractiveness of location to humans
;; and the union of:
;; - Mountain
;; - Flatland
;; - Desert
;; - Water
;; - Suburb
;; - City
;; A Terrain-Marker is the union of:
;; - 'mountain
;; - 'flatland
;; - 'desert
;; - 'water
;; - 'suburb
;; - 'city
;; A Map is a m-length list of n-length lists of Terrains where m and n are Nats

;; marker->terrain : Terrain-Marker => Terrain
(define (marker->terrain m)
  (cond [(symbol=? 'mountain m) mountain]
        [(symbol=? 'flatland m) flatland]
        [(symbol=? 'desert m) desert]
        [(symbol=? 'water m) water]
        [(symbol=? 'suburb m) suburb]
        [(symbol=? 'city m) city]))

;; draw-terrain : Terrain => Image
;; Converts a Terrain to an Image
(define (draw-terrain t)
  (rectangle pixel-offset pixel-offset 'solid (car t)))
(equal? (draw-terrain mountain) (rectangle pixel-offset pixel-offset 'solid "white"))
(equal? (draw-terrain desert) (rectangle pixel-offset pixel-offset 'solid "khaki"))

;; draw-map : Map => Scene
;; Converts a Map to a Scene
(define (draw-map m)
  (draw-map* m 0 (empty-scene (* (length m) pixel-offset)
                             (* (length (car m)) pixel-offset))))
(define (draw-map* m row-n acc)
  (cond [(empty? m) acc]
        [else (draw-map* (cdr m)
                         (+ row-n 1)
                         (place-image (draw-map-row (car m))
                                      (/ (* (length (car m)) pixel-offset) 2)
                                      (+ (* row-n pixel-offset) (/ pixel-offset 2))
                                      acc))]))
(define (draw-map-row r) (draw-map-row* (cdr r) (draw-terrain (marker->terrain (car r)))))
(define (draw-map-row* r acc)
  (cond [(empty? r) acc]
        [else (draw-map-row* (cdr r) (beside acc (draw-terrain (marker->terrain (car r)))))]))

;; random-list : Nat => [Listof Terrain-Marker]
;; produces a random n-length list of terrain markers
(define (random-list n) (random-list* n '()))
(define (random-list* n acc)
  (cond [(zero? n) acc]
        [else (random-list* (- n 1)
                            (cons (let [(ran (random 6))]
                                    (cond [(= ran 0) 'mountain]
                                          [(= ran 1) 'flatland]
                                          [(= ran 2) 'desert]
                                          [(= ran 3) 'water]
                                          [(= ran 4) 'suburb]
                                          [else 'city]))
                                  acc))]))
(equal? (random-list 0) '())
(equal? (length (random-list 1)) 1)
(equal? (length (random-list 5)) 5)

;; random-map : Nat Nat => Map
;; produces a random map with the given height and width
(define (random-map h w) (random-map* h w '()))
(define (random-map* h w acc)
  (cond [(zero? h) acc]
        [else (random-map* (- h 1) w (cons (random-list w) acc))]))
(equal? (length (random-map 0 0)) 0)
(equal? (let [(res (random-map 1 1))]
          (and (= (length res) 1) (= (length (car res)) 1))) true)
(equal? (let [(res (random-map 10 10))]
          (and (= (length res) 10) (= (length (car res)) 10))) true)

;; A Location is a pair of Nats that do not exceed a world's Map's atomic dimensions

;; random-location : Map => Location
;; Returns a random location inside the given Map
(define (random-location m)
  (list (random (length m)) (random (length (car m)))))

;; A Human is a 3-tuple
;; '(location time-at-location resources)
;; Where location is the person's current Location,
;;       time-at-location is the number of ticks a person has been at that location
;;       resources is a Nat representing the current amount of resources (bullets)
;;   and a person has

;; draw-human : Human => Image
;; Draws a human, either teal or dark tourquoise, depending on personality
(define (draw-human h)
  (let* [(quarter-offset (/ pixel-offset 4))
        (one-more (+ quarter-offset 1))]
    (place-image (circle quarter-offset 'solid 'purple)
                 one-more
                 one-more
                 (circle one-more 'solid 'black))))

;; random-humans : Nat Map => [Listof Human]
;; Given a number of humans to generate and a Map, returns a randomized list of
;; humans along the Map
(define (random-humans n m) (random-humans* n m '()))
(define (random-humans* n m acc)
  (cond [(zero? n) acc]
        [else (random-humans* (- n 1)
                              m
                              (cons (list (random-location m) 0 0) acc))]))
(equal? (random-humans 0 (random-map 10 10)) '())
(equal? (length (random-humans 10 (random-map 10 10))) 10)
(equal? (length (random-humans 100 (random-map 10 10))) 100)

;; A Zombie is an pair
;; '(location health time-at-location)
;; Where location is the zombie's current Location,
;;   and health is a Nat representing the Zombie's remaining health.

;; draw-zombie : Zombie => Image
;; Draws a zombie, red
(define (draw-zombie z)
  (let* [(quarter-offset (/ pixel-offset 4))
        (one-more (+ quarter-offset 1))]
    (place-image (circle quarter-offset 'solid 'red)
                 one-more
                 one-more
                 (circle one-more 'solid 'black))))

;; random-zombies : Nat Map => [Listof Zombie]
;; Given a number of zombies to generate and a Map, returns a randomized list of
;; zombies along the Map
(define (random-zombies n m) (random-zombies* n m '()))
(define (random-zombies* n m acc)
  (cond [(zero? n) acc]
        [else (random-zombies* (- n 1)
                               m
                               (cons (list (random-location m)
                                           0
                                           (+ 200 (random 20)))
                                     acc))]))
(equal? (random-zombies 0 (random-map 10 10)) '())
(equal? (length (random-zombies 10 (random-map 10 10))) 10)
(equal? (length (random-zombies 100 (random-map 10 10))) 100)

;; A World is an 3-tuple:
;; '(map humans zombies)
;; Where map is a Map of the simulated world,
;;       humans is a [Listof Human],
;;   and zombies is a [Listof Zombie]

;; place-x-on-map-image : X Func Image => Image
;; Places the given X (either human or zombie) on the given image on a map, drawn with the given func
(define (place-x-on-map-image h func mi)
  (let [(half-offset (/ pixel-offset 2))]
    (place-image (func h)
                 (+ half-offset (* pixel-offset (caar h)))
                 (+ half-offset (* pixel-offset (cadar h)))
                 mi)))

;; draw-world : World => Image
;; draws a world
(define (draw-world w)
  (cond [(empty? (cadr w)) (place-image (text "The zombies have taken control!" 36 'black)
                                        (* pixel-offset (/ width0 2))
                                        (* pixel-offset (/ height0 2))
                                        (empty-scene (* pixel-offset width0)
                                                     (* pixel-offset height0)))]
        [(empty? (caddr w)) (place-image (text "The humans defeated the zombie hoard!" 36 'black)
                                         (* pixel-offset (/ width0 2))
                                         (* pixel-offset (/ height0 2))
                                         (empty-scene (* pixel-offset width0)
                                                      (* pixel-offset height0)))]
        [else (foldr (lambda (next acc) (place-x-on-map-image next draw-zombie acc))
                     (foldr (lambda (next acc) (place-x-on-map-image next draw-human acc))
                            (draw-map (car w))
                            (cadr w))
                     (caddr w))]))

;; next-world : World => World
;; The fourth dimension (well, third in this sim). Takes the current world to
;; the next world.
;; Things that happen:
;; 1. Humans that can move, move
;;    - priority is not dying
;;      - can't move to a spot with a zombie
;;      - terrain rank + 2^distance to nearest zombie = dynamic terrain attractiveness
;;      - move to the adjacent terrain that has the lowest DTA
;;    - resources added to those who move (and time reset)
;;    - time added to those who can't
;; 2. Humans shoot
;;    - resources depleted for those who shoot
;;    - zombies get shot (some die, some modified)
;; 3. Zombies get shot
;; 4. Zombies that can move, move to nearest human
(define (next-world w)
  (next-humans (next-zombies w)))

;; next-humans : World => World
;; Produces the next world after the humans move and shoot
(define (next-humans w)
  (move-humans (shoot-zombies w)))

;; human-can-move : Human Map => Boolean
;; Determines if the given human can move, given the map
(define (human-can-move x m)
  (>= (cadr x) (caadr (get-terrain (car x) m))))

;; zombie-can-move : Zombie Map => Boolean
;; Determines if the given zombie can move, given the map
(define (zombie-can-move x m)
  (>= (cadr x) (caadr (get-terrain (car x) m))))

;; adjacent-locations : Location => [Listof Location]
;; Returns the adjacent locations to this spot
(define (adjacent-locations l)
  (filter (lambda (x) (and (>= (car x) 0)
                           (>= (cadr x) 0)
                           (< (car x) width0)
                           (< (cadr x) height0)))
          (list (list (- (car l) 1) (- (cadr l) 1))
                (list (car l) (- (cadr l) 1))
                (list (+ (car l) 1) (- (cadr l) 1))
                (list (- (car l) 1) (cadr l))
                (list (+ (car l) 1) (cadr l))
                (list (- (car l) 1) (+ (cadr l) 1))
                (list (car l) (+ (cadr l) 1))
                (list (+ (car l) 1) (+ (cadr l) 1)))))
(equal? (adjacent-locations '(0 0))
        (list '(1 0) '(0 1) '(1 1)))
(equal? (adjacent-locations '(1 1))
        (list '(0 0) '(1 0) '(2 0) '(0 1) '(2 1) '(0 2) '(1 2) '(2 2)))

;; dyn-rank-terrain : Location Map [Listof Zombie] => Nat
;; Dynamicly ranks the given Location's Terrain based on:
;; terrain rank * (3 ^ distance to nearest zombie) = dynamic terrain attractiveness
(define (dyn-rank-terrain l m zs)
  (let [(t (get-terrain l m))]
    (* (cadddr t) (expt 4 (foldr min 100 (map (lambda (x) (distance-to l (car x))) zs))))))

;; distance-to : Location Location => Float
;; Calculates the distance between the two locations
(define (distance-to l1 l2)
  (sqrt (+ (sqr (- (car l1) (car l2))) (sqr (- (cadr l1) (cadr l2))))))

;; get-terrain : Location Map => Terrain
;; Gets the Terrain that matches the given Location on the Map.
(define (get-terrain l m)
  (marker->terrain (list-ref (list-ref m (cadr l)) (car l))))
(equal? (get-terrain '(1 1) (list '(city city) '(city mountain)))
        mountain)
(equal? (get-terrain '(0 1) (list '(city city) '(mountain city)))
        mountain)
(equal? (get-terrain '(1 0) (list '(city mountain) '(city city)))
        mountain)
(equal? (get-terrain '(0 0) (list '(mountain city) '(city city)))
        mountain)
  
;; best-location : Human Map [Listof Zombie] => Location
;; Returns the best dynamically ranked location
(define (best-location h m zs)
  (let [(current (car h))]
    (cadr (foldr (lambda (next acc) (if (> (car next) (car acc)) next acc))
                 (list -1 (list -1 -1))
                 (map (lambda (x) (list (dyn-rank-terrain x m zs) x))
                      (cons current (adjacent-locations current)))))))
(equal? (best-location (list '(1 1) 0 0)
                       (list '(water water water)
                             '(water flatland water)
                             '(water water flatland))
                       '())
        '(2 2))
(equal? (best-location (list '(1 1) 0 0)
                       (list '(water water water)
                             '(water flatland water)
                             '(water water flatland))
                       (list (list '(2 2) 0 100)))
        '(0 0))

;; move-humans : World => World
;; Moves the humans of the given World that can and should move.
(define (move-humans w) (list (car w) (move-humans* (car w) (cadr w) (caddr w) '()) (caddr w)))
(define (move-humans* m hs zs acc)
  (cond [(empty? hs) acc]
        [else (move-humans* m (cdr hs) zs (cons (move-human (car hs) m zs) acc))]))

;; move-human : Human Map [Listof Zombie] => Human
;; Moves the given human, if they can move and they should move
(define (move-human h m zs)
  (let [(best-location-tmp (best-location h m zs))]
  (if (and (human-can-move h m) (not (equal? best-location-tmp (car h))))
      (list best-location-tmp 0 (+ (caddr h) (caddr (get-terrain best-location-tmp m))))
      (list (car h) (+ 1 (cadr h)) (caddr h)))))
(equal? (move-human (list '(1 1) 1 10)
                    (list '(flatland flatland flatland)
                          '(flatland flatland flatland)
                          '(flatland flatland flatland))
                    (list (list '(2 2) 0 100)))
        (list '(0 0) 0 10))

;; remove-ref : Nat List => List
;; Removes the given list reference from the list
(define (remove-ref n l) (if (>= n 0) (remove-ref* n l '()) l))
(define (remove-ref* n l acc)
  (cond [(zero? n) (append acc (cdr l))]
        [else (remove-ref* (- n 1) (cdr l) (cons (car l) acc))]))
(equal? (remove-ref 0 '(1)) '())
(equal? (remove-ref 0 '(1 2 3)) '(2 3))
(equal? (remove-ref 1 '(1 2 3)) '(1 3))
(equal? (remove-ref -1 '(1 2 3)) '(1 2 3))

(define (shoot-zombie z)
  (if (= 0 (random 50))
      (let [(health (- (caddr z) 5))]
        (if (<= health 0) '() (list (list (car z) (cadr z) health))))
      '()))

(define (find-ref z zs) (find-ref* z zs 0))
(define (find-ref* z zs n)
  (cond [(empty? zs) n]
        [(equal? (caar zs) (car z)) n]
        [else (find-ref* z (cdr zs) (+ n 1))]))
(equal? (find-ref (list '(1 1) 100) (list (list '(1 2) 0 100)
                                          (list '(1 1) 0 100)))
        1)
(equal? (find-ref (list '(1 1) 100) (list (list '(1 2) 0 100)
                                          (list '(0 3) 0 100)
                                          (list '(1 1) 0 50)
                                          (list '(1 1) 0 100)
                                          (list '(1 3) 0 100)))
        2)

;; distance-map-zombies : Location [Listof Zombie] => [Listof [Pair Nat Zombie]]
(define (distance-map-zombies l zs) (distance-map-zombies* l zs 0 '()))
(define (distance-map-zombies* l zs n acc)
  (cond [(empty? zs) acc]
        [else (distance-map-zombies* l
                                     (cdr zs)
                                     (+ n 1)
                                     (cons (list (distance-to l (caar zs)) (car zs)) acc))]))
(equal? (distance-map-zombies '(1 1) (list (list '(1 2) 0 100)
                                     (list '(1 3) 0 100)
                                     (list '(1 4) 0 100)))
        (list (list 3 (list '(1 4) 0 100)) (list 2 (list '(1 3) 0 100)) (list 1 (list '(1 2) 0 100))))
                      

;; human-shoot-zombie : Human [Listof Zombie] => [Pair Human [Pair Integer [Listof Zombie]]
(define (human-shoot-zombie h zs)
  (if (> (caddr h) 0)
      (let* [(distance-mapped (distance-map-zombies (car h) zs))
             (filtered (map (lambda (x) (cadr x))
                            (filter (lambda (x) (< (car x) 2)) distance-mapped)))]
        (if (empty? filtered)
            (list h (list -1 '()))
            (list (list (car h) (cadr h) (- (caddr h) 1))
                  (list (find-ref (car filtered) zs) (shoot-zombie (car filtered))))))
      (list h (list -1 '()))))
(let [(tmp (human-shoot-zombie (list '(1 1) 0 10)
                               (list (list '(1 2) 0 100)
                                     (list '(1 6) 0 100))))]
  (or (equal? (list (list '(1 1) 0 9)
                    (list 0 (list (list '(1 2) 0 75))))
              tmp)
      (equal? (list (list '(1 1) 0 9)
                    (list 0 '()))
              tmp)))
(equal? (human-shoot-zombie (list '(1 1) 0 0)
                            (list (list '(1 2) 0 100)
                                  (list '(1 6) 0 100)))
        (list (list '(1 1) 0 0) (list -1 '())))
(equal? (human-shoot-zombie (list '(1 1) 0 10)
                            (list (list '(2 3) 0 100)
                                  (list '(1 6) 0 100)))
        (list (list '(1 1) 0 10) (list -1 '())))

;; shoot-zombies : World => World
;; Humans take shots at the nearest zombie less than two units away
(define (shoot-zombies w)
  (shoot-zombies* (cadr w) (car w) (caddr w) '() '()))
(define (shoot-zombies* hs m zs hacc zacc)
  (cond [(empty? hs) (list m hacc (append zs zacc))]
        [else (let [(modified (human-shoot-zombie (car hs) zs))]
                (shoot-zombies* (cdr hs)
                                m
                                (remove-ref (caadr modified) zs)
                                (cons (car modified) hacc)
                                (append (cadr (cadr modified)) zacc)))]))
(let [(tmp (shoot-zombies (list (list '(flatland flatland)
                                      '(flatland flatland))
                                (list (list '(0 0) 0 10))
                                (list (list '(1 1) 0 100)))))]
  (or (equal? (list (list '(flatland flatland)
                          '(flatland flatland))
                    (list (list '(0 0) 0 9))
                    (list (list '(1 1) 0 90)))
              tmp)
      (equal? (list (list '(flatland flatland)
                          '(flatland flatland))
                    (list (list '(0 0) 0 9))
                    '())
              tmp)))
(let [(tmp (shoot-zombies (list (list '(flatland flatland flatland flatland)
                                      '(flatland flatland flatland flatland)
                                      '(flatland flatland flatland flatland)
                                      '(flatland flatland flatland flatland))
                                (list (list '(0 0) 0 10)
                                      (list '(0 1) 0 10)
                                      (list '(2 2) 0 10))
                                (list (list '(3 2) 0 100)
                                      (list '(3 3) 0 100)))))]
  (or (equal? (list (list '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland))
                    (list (list '(2 2) 0 9)
                          (list '(0 1) 0 10)
                          (list '(0 0) 0 10))
                    (list (list '(3 2) 0 100)
                          (list '(3 3) 0 90)))
              tmp)
      (equal? (list (list '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland)
                          '(flatland flatland flatland flatland))
                    (list (list '(2 2) 0 9)
                          (list '(0 1) 0 10)
                          (list '(0 0) 0 10))
                    (list (list '(3 2) 0 100)))
              tmp)))
                                                   
;; next-zombies : World => World
;; Zombies (that can move) move to the nearest human
(define (next-zombies w) (next-zombies* (caddr w) (car w) (cadr w) '()))
(define (next-zombies* zs m hs zacc)
  (cond [(empty? zs) (list m hs zacc)]
        [else (let [(tmp (next-zombie (car zs) m hs))]
                (next-zombies* (cdr zs)
                               m
                               (filter (lambda (h) (not (equal? (car h) (caar tmp)))) hs)
                               (cons (car tmp) (append (cadr tmp) zacc))))]))

;; next-zombie : Zombie Map [Listof Human] => [Pair Zombie [Listof Zombie]]
;; returns the next version of the given zombie
(define (next-zombie z m hs)
  (if (zombie-can-move z m)
      (move-zombie z hs)
      (list (list (car z) (+ (cadr z) 1) (caddr z)) '())))

;; nearest-human : Location [Listof Human] => Human
(define (nearest-human l hs)
  (cadr (foldr (lambda (next acc) (if (< (car next) (car acc)) next acc))
               (list 100 (list '(-1 -1) -1 -1))
               (map (lambda (h) (list (distance-to l (car h)) h)) hs))))

;; move-zombie : Zombie [Listof Human] => [Pair Zombie [Listof Zombie]]
(define (move-zombie z hs)
  (let [(zombie (list (cadr (let* [(nearest (nearest-human (car z) hs))
                                   (adjacents (adjacent-locations (car z)))]
                              (foldr (lambda (next acc) (if (< (car next) (car acc)) next acc))
                                     (list 100 '(-1 -1))
                                     (map (lambda (x) (list (distance-to (car nearest) x) x)) adjacents))))
                      0
                      (caddr z)))]
    (list zombie (map human->zombie (filter (lambda (h) (equal? (car h) (car zombie))) hs)))))

;; human->zombie : Human => Zombie
;; Turns infected humans into zombies
(define (human->zombie h)
  (begin (print "turned") (list (car h) 10 (+ (random 20) 400))))


(define map0 (random-map width0 height0))
(define num-of-humans 200)
(define humans0 (random-humans num-of-humans map0))
(define num-of-zombies 200)
(define zombies0 (random-zombies num-of-zombies map0))
(define world0 (list map0 humans0 zombies0))

(define (random-world)
  (let* [(the-map (random-map width0 height0))
         (the-humans (random-humans num-of-humans the-map))
         (the-zombies (random-zombies num-of-zombies the-map))]
    (list the-map the-humans the-zombies)))

(define (simulate n hw zw)
  (cond [(zero? n) (begin (displayln (string-append "Humans won " (number->string hw)))
                          (displayln (string-append "Zombies won " (number->string zw))))]
        [else (let [(sim (run-sim (random-world)))]
                (simulate (- n 1) (if sim (+ hw 1) hw) (if (not sim) (+ zw 1) zw)))]))
(define (run-sim w)
  (cond [(empty? (cadr w)) false]
        [(empty? (caddr w)) true]
        [else (run-sim (next-world w))]))
                         

;(big-bang world0 (on-tick next-world) (to-draw draw-world))























