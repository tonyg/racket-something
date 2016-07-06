#lang something // -*- sth -*-

require
  something/infix
  for-syntax something/base
  only-in racket/math pi
  racket/class
  racket/draw
  racket/gui/base

def-operator -> 900 left { receiver message: message receiver }

def-syntax def-protocol stx
  syntax-case stx [block]
    _ protocol-name (block (method arg ...) ...)
      syntax (begin (def method arg ... : { receiver : send receiver method arg ... }) ...)

///////////////////////////////////////////////////////////////////////////

def-operator ->> #f prefix-macro ->>
def-syntax ->> stx parse
  syntax-case stx [block]
    _ p ... (block body ...)
      syntax (let:
                def v: '#%rewrite-infix' p ...
                (begin (('#%rewrite-infix' body) v) ...)
                v)

def-protocol window-management
  show show?

def-protocol drawing
  draw-path path
  draw-bitmap source dest-x dest-y

def-protocol path-construction
  arc left top width height start-angle end-angle
  move-to x y
  line-to x y

def-protocol bitmap-control
  set-smoothing smoothing
  set-pen color width style
  set-brush color style

def quarter-circle cx cy quarter radius
  (arc (cx - radius)
       (cy - radius)
       (radius * 2)
       (radius * 2)
       (pi * quarter * 0.5)
       (pi * (quarter + 1) * 0.5))

def draw-it dc
  def corner-radius: 20
  def top: 20
  def left: 20
  def bottom: 100
  def right: 200
  dc -> draw-path (->> new 'dc-path%':
                     move-to left (top + corner-radius)
                     line-to left bottom
                     line-to right bottom
                     quarter-circle (right - corner-radius) (top + corner-radius) 0 corner-radius
                     line-to (left + corner-radius) top
                     quarter-circle (left + corner-radius) (top + corner-radius) 1 corner-radius)

def logo: let
  def bm: make-bitmap 300 300
  draw-it (->> new 'bitmap-dc%' (bitmap bm):
             set-smoothing `smoothed
             set-pen "black" 1 `solid
             set-brush "yellow" `solid)
  bm

let
  def frame: new 'frame%' (label "Shape") (width 340) (height 340)
  def canvas: new 'canvas%' (parent frame) (paint-callback { _ dc: dc -> draw-bitmap logo 0 0 })
  frame -> show #t
