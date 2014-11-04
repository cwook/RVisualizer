;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname visualizer) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "arrow.rkt" "teachpack" "htdp")))))

(require rsound)
(require rsound/draw)
(require 2htdp/universe)
(require racket/base)


(define SONG-LOCATION "C:/tmp/rct2theme.wav")

(define SONG (rs-read/clip SONG-LOCATION 0 (* 44100 10)))

(define ps (make-pstream))

(define (both a b) b)

(define (tock w)
  (both
   (if (= 0 w) (pstream-queue ps SONG 0) w)
   (add1 w)))

(define (draw w)
  (overlay
   (beside
    (draw-helper 0)
    (draw-helper 1)
    (draw-helper 2)
    (draw-helper 3)
    (draw-helper 4)
    (draw-helper 5)
    (draw-helper 6)
    (draw-helper 7)
    (draw-helper 8)
    (draw-helper 10)
    (draw-helper 11)
    (draw-helper 12)
    (draw-helper 13)
    (draw-helper 14)
    (draw-helper 15)
    (draw-helper 16)
    (draw-helper 17)
    (draw-helper 18)
    (draw-helper 19)
    (draw-helper 20)
    (draw-helper 21)
    (draw-helper 22)
    (draw-helper 23)
    (draw-helper 24)
    (draw-helper 25)
    (draw-helper 26)
    (draw-helper 27)
    (draw-helper 28)
    (draw-helper 29)
    (draw-helper 30)
   )
   (empty-scene 20 200))
  )

(define (draw-helper n)
  (rectangle 1 (* 1000 (abs (rs-ith/left SONG (+ n (pstream-current-frame ps))))) "solid" "black")
  )


(big-bang 0 [on-tick tock] [to-draw draw])