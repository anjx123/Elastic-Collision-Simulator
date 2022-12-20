;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |shoddy physics sim|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)


;; My world program (make this more specific)
(@htdw Ball)
;; =================
;; Constants:

(define WIDTH 1000)
(define HEIGHT 400)
(define MTS (rectangle WIDTH HEIGHT "solid" "white"))
;; =================
;; Data definitions:
(@htdd Ball)
(define-struct ball (x dx m))
;ball is (make-ball Number Number)
;parameters of right ball
(define SPEED1 -12)
(define MASS1 3)

;parameters of left ball
(define SPEED2 2)
(define MASS2 5)


(define B1 (make-ball WIDTH SPEED1 MASS1)) ;will only work if its on the right
(define B2 (make-ball 0 SPEED2 MASS2))


(@htdd LOB)
;one of
;empty
;- (cons Ball ListOfBall)
(define LOB1 (list B1 B2))

;; WS is ... (give WS a better name)
;; =================
;; Functions:
(@htdf main)
(@signature LOB -> LOB)
;; start the world with (main LOB1) ;;

(define (main ws)
  (big-bang ws ;WS
    (on-tick tock) ;WS -> WS
    (to-draw render) ;WS -> Image
    ;   (stop-when ...) ;WS -> Boolean
    ; (on-mouse ...) ;WS Integer Integer MouseEvent -> WS
    (on-key handle-key))
  ;press "o" to switch to reference velocity

  
  ) ;WS KeyEvent -> WS
(@htdf tock)
(@signature LOB -> LOB)
;; produce the next ...
;; !!!


(check-expect (tock (list B1 B2)) (list
                                   (make-ball (+ (ball-dx B1)
                                               (ball-x B1))
                                               (ball-dx B1)
                                               (ball-m B1))
                                   (make-ball (+ (ball-dx B2)
                                               (ball-x B2))
                                               (ball-dx B2)
                                               (ball-m B2))))

(define (tock lob)
  (local [(define (tick b) (make-ball (+ (ball-x b) (ball-dx b))
                                      (ball-dx b)
                                      (ball-m b)))]
    (cond [(balls-touch? lob) (bounce lob)]
          [else
           (map tick lob)])))

(@htdf balls-touch?)
(@signature LOB -> Boolean)

(define (balls-touch? lob)
  (<= (- (ball-x (first lob)) (* 10 (ball-m (first lob))))
     (+ (ball-x (second lob)) (* 10 (ball-m (second lob))))))
(@htdf bounce)
(@signature LOB -> LOB)
;(check-expect (bounce (list (make-ball 240 -3 3) (make-ball 200 5 1)))
 ;             (list (make-ball 243 3 3) (make-ball 195 -5 1)))


   


(define (bounce lob)
  (local [
           (define (reference-velocity lob)
            (local [(define (sum-of-momentum lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (* (ball-m (first lob))
                                   (ball-dx (first lob)))
                                (sum-of-momentum (rest lob)))]))
                    (define (sum-of-masses lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (ball-m (first lob))
                                (sum-of-masses (rest lob)))]))]
    
              (/ (sum-of-momentum lob) (sum-of-masses lob))))

          (define (reverse b) (make-ball (- (ball-x b) (ball-dx b))
                                         (+ (- (- (ball-dx b)
                                                  (reference-velocity lob)))
                                            (reference-velocity lob))
                                         (ball-m b)))]
    (map reverse lob)))

;purpose: press "o" to switch to the reference velocity POV
(define (handle-key g ke)
  (cond [(key=? ke "r") (un-mod g)]
        [(key=? ke "o")
         (ref-mod g)]
        [else g]
        ))

(@htdf ref-mod)
(@signature LOB -> LOB)
;modifies the list of ball velocity to follow reference frame 


(define (ref-mod lob)
  (local [
          (define (reference-velocity lob)
            (local [(define (sum-of-momentum lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (* (ball-m (first lob))
                                   (ball-dx (first lob)))
                                (sum-of-momentum (rest lob)))]))
                    (define (sum-of-masses lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (ball-m (first lob))
                                (sum-of-masses (rest lob)))]))]
    
              (/ (sum-of-momentum lob) (sum-of-masses lob))))
          (define (ref-v b)
            (make-ball (ball-x b)
                       (- (ball-dx b) (reference-velocity lob))
                       (ball-m b)))]
  
    (map ref-v lob)))

(define (un-mod lob)
  (local [
           (define (reference-velocity lob)
            (local [(define (sum-of-momentum lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (* (ball-m (first lob))
                                   (ball-dx (first lob)))
                                (sum-of-momentum (rest lob)))]))
                    (define (sum-of-masses lob)
                      (cond [(empty? lob) 0]
                            [else
                             (+ (ball-m (first lob))
                                (sum-of-masses (rest lob)))]))]
               (/ (sum-of-momentum lob) (sum-of-masses lob))))

          (define (return b)
            (make-ball (ball-x b)
                       (+ (ball-dx b) (reference-velocity lob))
                       (ball-m b)))]

    (map return lob)))

          

  (@htdf render)
  (@signature Ball LOB -> Image)
  ;; render ...
  ;; !!!
  ;(define (render ws) empty-image)
  (check-expect (render (list B1 B2)) (render (list B1 B2)))

  (define (render ws)
    (local [(define (fn-for-ball b lob)
              (place-image (circle (* 10 (ball-m b)) "solid" "red")
                           (ball-x b) (/ HEIGHT 2) lob))]
      (foldr fn-for-ball MTS ws)))
  