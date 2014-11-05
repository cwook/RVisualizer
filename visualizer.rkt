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

(define ps (make-pstream))

(define (both a b) b)

(define (tock w)
  (* 4 (abs (inexact->exact (rs-ith/left SONG (pstream-current-frame ps)))))
  )

(define (draw w)
  (place-image
   (circle (* 30 w) "solid" (make-color (random 255) (random 255) (random 255)))
   (random 600) 100
   (place-image
    (circle (* 120 w) "solid" (make-color (random 255) (random 255) (random 255)))
    (random 600) 50
    (place-image
    (circle (* 45 w) "solid" (make-color (random 255) (random 255) (random 255)))
    (random 600) 100
     (place-image
    (circle (* 180 w) "solid" (make-color (random 255) (random 255) (random 255)))
    (random 600) 100
    (place-image
    (circle (* 20 w) "solid" (make-color (random 255) (random 255) (random 255)))
    (random 600) 50
   (empty-scene 600 200)))))))
  

(pstream-queue ps SONG 0)
(big-bang 0 [on-tick tock] [to-draw draw])



