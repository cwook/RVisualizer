;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname visualizer) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
(require rsound)
(require rsound/draw)
(require 2htdp/universe)
(require racket/base)

;(define SONG-LOCATION "C:/tmp/rct2theme.wav")
(define SONG-LOCATION "songs/The Intro.wav")
(define SONG (rs-read/clip SONG-LOCATION 0 (* 44100 30)))
(define SONGLEN (rs-frames SONG))

(define-struct world[t a c1now c1go slide-h drag? INT Radius])
;; a world is (make-world Num Num Num X-coord Boolean)

(define ps (make-pstream))
(define current-frame (pstream-current-frame ps))
(define (both a b) b)

(define INITIAL-WORLD (make-world 0 0 300 300 900 false 1.0 0))
(define world-box (box (world-INT INITIAL-WORLD)))
(define cur-frame (box (world-Radius INITIAL-WORLD))) 

;; increment "old" by "incr", and wrap around if necessary
;; number number number -> number
(define (maybe-wrap old incr len)
  (local [(define new (+ old incr))]
    (cond [(<= len new) (- new len)]
          [(< new 0) (+ len new)]
          [else new])))

;; a network that smoothly moves through a sound
(define (flexloop len)
  (network (incr)
           [ctr =
                (+ (prev ctr 0) incr)]))
                #;(maybe-wrap 
                   (prev ctr 0)
                   incr
                   len)

(define (playing?)
  (unbox world-box))

(define (netwrk) 
  (network ()
           [ctr <= (flexloop SONGLEN) (playing?)]
           [out = (begin
                    (set-box! cur-frame (floor ctr))
                    (rs-ith/left SONG (floor ctr)))]
           ))

(signal-play (netwrk))

 
(define (tock w)
  (cond
    [(= 0 (remainder (world-t w) 2))
     (begin
      (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (unbox cur-frame))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (* (world-a w) 400)(world-slide-h w)(world-drag? w)(world-INT w)(world-Radius w))
      w)
     ]
    [else
     (begin
     (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (unbox cur-frame))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (world-c1go w)(world-slide-h w)(world-drag? w)(world-INT w)(world-Radius w))
     w)
     ]))


#;(define (tock w)
  w)
(define (draw w)
  (place-image
   (circle (world-c1now w) "solid" "red")
   600 320
   (place-image
    (square 20 "solid" "green")
    (world-slide-h w) 650
    (place-image
     (rectangle 1100 500 "outline" "black")
     600 320
     (place-image
      (square 50 "solid" "green")
      100 650
      (place-image
       (square 50 "solid" "red")
       200 650
       (place-image
        (rectangle 500 20 "outline" "black")
        900 650
        (empty-scene 1200 720))))))))

;; World X-coord Y-coord Mouse-Event -> World
(define (mouse-event w x y event)
  (cond
    ;; handles button events
    [(mouse=? event "button-down")
     (cond
       ;;stops pstream
       [(and (and (> x (- 200 25)) (< x (+ 200 25))) (> y (- 650 25)) (< y (+ 650 25)))
        (begin
          (set-box! world-box 0)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 0.0 (world-Radius w))
          
        )]
       ;;play pstream
       [(and (and (> x (- 100 25)) (< x (+ 100 25))) (> y (- 650 25)) (< y (+ 650 25)))
        (begin
          (set-box! world-box 1)
          (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-drag? w) 1.0 (world-Radius w))
          
        )] 
       [else w])]
    ;; makes drag? false when button is not held down
    [(mouse=? event "button-up")
     (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) false (world-INT w) (world-Radius w))
     ]
    [(mouse=? event "drag")
     (cond
       [(boolean=? true (world-drag? w))
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250)))
           ;(make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x (world-slide-v w) true (round (* 255 (/ (- (world-slide-h w) 660) 480))) (world-c1g w)(world-c1b w))]
           (begin
             (pstream-set-volume! ps (/ (- (world-slide-h w) 660) 480))
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x true (world-INT w)(world-Radius w))
             )]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) true (world-INT w)(world-Radius w) )]
          )]
       ;; When mouse is within the boundaries of a slider, then drag? is true
       [else 
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250)))
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w)
                       x true (world-INT w)(world-Radius w))]
          [else w])])
     ]
    [else w])
  )
(define (input w key)
    (if (= (world-INT w) 1) 0.0 1.0)
  )

#;(define (key-handler w ke)
    (begin (print (pstream-current-frame ps)) w))

;(pstream-queue ps SONG 0)
(big-bang INITIAL-WORLD
          ;[on-tick tock]
          [to-draw draw]
          [on-mouse mouse-event]
          [on-key input]
          [state true])


