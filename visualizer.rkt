;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname visualizer) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp")))))
(require rsound)
(require rsound/draw)
(require 2htdp/universe)
(require racket/base)

;(define SONG-LOCATION "C:/tmp/rct2theme.wav")
(define SONG-LOCATION "songs/The Intro.wav")

(define SONG (rs-read/clip SONG-LOCATION 0 (* 44100 120)))

(define-struct world[t a c1now c1go slide-h slide-v drag? c1r c1g c1b cframe])



(define ps (make-pstream))
(define current-frame (pstream-current-frame ps))

(define INITIAL-WORLD (make-world 0 0 300 300 900 650 false 100 100 100 current-frame))

(define (both a b) b)

(define (tock w)
  (cond
    [(= 0 (remainder (world-t w) 5))
     (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (pstream-current-frame ps))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (* (world-a w) 400)(world-slide-h w)(world-slide-v w)(world-drag? w)(world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))
     ]
    [else
     (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (pstream-current-frame ps))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (world-c1go w)(world-slide-h w)(world-slide-v w)(world-drag? w)(world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))
     ]
    ))


(define (draw w)
  (place-image
   (circle (world-c1now w) "solid" (make-color (world-c1r w)(world-c1g w)(world-c1b w)))
   600 320
   (place-image
    (square 20 "solid" "green")
    (world-slide-h w) (world-slide-v w)
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


(define (mouse-event w x y event)
  (cond
    [(mouse=? event "button-down")
     (cond
       [(and (and (> x (- 200 25)) (< x (+ 200 25))) (> y (- 650 25)) (< y (+ 650 25)));;stops
        (begin (stop) (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-slide-v w) false (world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w)))]
       [(and (and (> x (- 100 25)) (< x (+ 100 25))) (> y (- 650 25)) (< y (+ 650 25)));;play
        (begin (stop) 
               (pstream-queue (make-pstream) (clip SONG (world-cframe w) (rs-frames SONG)) 0) w)]
       [else w])]
    [(mouse=? event "button-up")
     (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-slide-v w) false (world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))
     ]
    [(mouse=? event "drag")
     (cond
       [(boolean=? true (world-drag? w))
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250)))
           ;(make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x (world-slide-v w) true (round (* 255 (/ (- (world-slide-h w) 660) 480))) (world-c1g w)(world-c1b w))]
           (begin
             (pstream-set-volume! ps (/ (- (world-slide-h w) 660) 480))
             (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) x (world-slide-v w) true (world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))
             )]
          [else 
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w) (world-slide-h w) (world-slide-v w) true (world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))]
          )]
       [else 
        (cond
          [(and (> x (- 910 250)) (< x (+ 890 250)))
           (make-world (world-t w) (world-a w) (world-c1now w) (world-c1go w)
                       x (world-slide-v w) true (world-c1r w)(world-c1g w)(world-c1b w)(world-cframe w))]
          [else w])])
     ]
    [else w])
  )


#;(define (key-handler w ke)
    (begin (print (pstream-current-frame ps)) w))

(pstream-queue ps SONG 0)
(big-bang INITIAL-WORLD
          [on-tick tock]
          [to-draw draw]
          [on-mouse mouse-event]
          ;[on-key key-handler]
          [state true])


