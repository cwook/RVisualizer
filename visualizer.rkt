;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname visualizer) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp")))))

(require rsound)
(require rsound/draw)
(require 2htdp/universe)
(require racket/base)

(define SONG-LOCATION "C:/tmp/rct2theme.wav")
;(define SONG-LOCATION "songs/The Intro.wav")

(define SONG (rs-read/clip SONG-LOCATION 0 (* 44100 10)))

(define-struct world[t a c1now c1go])

(define INITIAL-WORLD (make-world 0 0 300 300))

(define ps (make-pstream))

(define (both a b) b)

(define (tock w)
  (cond
    [(= 0 (remainder (world-t w) 5)) 
     (begin
       (print "0")
     (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (pstream-current-frame ps))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (* (world-a w) 300)))
     ]
    [else 
     (begin
       (print "1")
     (make-world
      (add1 (world-t w))
      (abs (inexact->exact (rs-ith/left SONG (pstream-current-frame ps))))
      (/ (+ (world-c1now w) (world-c1go w)) 2)
      (world-c1go w)))
     ]
  ))

(define (draw w)
  (place-image
   (circle (world-c1now w) "solid" (make-color 0 0 0))
   600 200
   (empty-scene 1200 400)))


(pstream-queue ps SONG 0)
(big-bang INITIAL-WORLD [on-tick tock] [to-draw draw] [state true])



