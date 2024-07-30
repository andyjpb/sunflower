#!/usr/bin/env -S csi -s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; write-back-silkscreen.scm
;;; Tools for designing and generating sunflower LED PCBs.
;;;
;;; The sunflower PCB and the driver PCB have connections between them for each
;;; SEG and GRID net for each LED driver. This program takes the silkscreen
;;; markings from the driver PCB and transfers them to the back of the
;;; sunflower PCB so that it is easy to see which LEDs leads to leave intact so
;;; that the boards can be joined together.
;;;
;;;
;;;  Copyright (C) 2024, Andy Bennett
;;;  All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining a
;;;  copy of this software and associated documentation files (the "Software"),
;;;  to deal in the Software without restriction, including without limitation
;;;  the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;  and/or sell copies of the Software, and to permit persons to whom the
;;;  Software is furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be included in
;;;  all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;  DEALINGS IN THE SOFTWARE.
;;;
;;; Andy Bennett <andyjpb@ashurst.eu.org>, 2027/07/29 21:38
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use numbers srfi-1)
(use matchable irregex)

; Process command line arguments

(if (not (= 2 (length (command-line-arguments))))
  (begin
    (fprintf (current-error-port) "Usage: write-back-silkscreen.scm <driver-board.kicad_pcb> <sunflower-board.kicad_pcb>\n")
    (exit 1)))

(define driver-pcb    (first  (command-line-arguments)))
(define sunflower-pcb (second (command-line-arguments)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pcbnew stuff

; Reader for pcbnew specific symbol format.
; ${KISYS3DMOD}/LED_THT.3dshapes/LED_D5.0mm.wrl
; Reads the symbol in as a string because Chicken would escape a symbol
; containing these characters, symbol-escape doesn't seem to help (as it only
; affects the reader) and pcbnew seems to accept it formatted as a string.
(set-read-syntax! #\$
  (lambda (port)
    (let loop ((c (peek-char port))
               (l '(#\$)))
      (if (or (eof-object? c)
              (eq? #\) c)
              (char-whitespace? c))
        (list->string (reverse l))
        (let ((c (read-char)))
          (loop (peek-char) (cons c l)))))))

; Finds the G.* and S.* pads in driver-pcb and "transfers" the silkscreen
; markings that enclose the pad to sunflower-pcb so that they enclose the
; corresponding pad there..
(define (transform-pcb driver-pcb sunflower-pcb)
  (let* ((silkscreen
	   ; Silkscreen annotations to add to sunflower-pcb
	   '())
	 (pads
	   ; All the pads we're interested in, where they are, what they're
	   ; connected to and how many times we've found it on the sunflower
	   ; board.
	   ; '((x y ref net found?) ...)
	   (fold
	     (lambda (expr pads)
	       (match expr
		 (('module type . properties)
		  (if (or (eqv? type 'sunflower:Link_Round)
			  (eqv? type 'sunflower:Link_Square))
		    (let ((x   #f)
			  (y   #f)
			  (ref #f)
			  (net #f))
		      (for-each
			(match-lambda
			  (('at a b)
			   (set! x a)
			   (set! y b))
			  (('fp_text 'reference a . _)
			   (if (irregex-match
				 '(: (or "G" "S") (+ numeric))
				 (symbol->string a))
			     (set! ref a)))
			  (('pad '1 'thru_hole _ . rest)
			   (set! net (second (alist-ref 'net rest))))
			  (else #f))
			properties)
		      (if (and x y ref net)
			(begin
			  (set! silkscreen
			    (cons
			      `(gr_circle (center ,x ,y) (end ,(- x 1) ,y) (layer B.SilkS) (width 0.12))
			      silkscreen))
			  (cons (list x y ref net #f) pads))
			pads))
		    pads))
		 (else pads)))
	     '()
	     driver-pcb)))

    (append
      sunflower-pcb
      (reverse silkscreen))))

(define (write-kicad-pcb driver-pcb sunflower-pcb)
  (pp
    (transform-pcb driver-pcb sunflower-pcb)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Program

(write-kicad-pcb
  (with-input-from-file driver-pcb    read)
  (with-input-from-file sunflower-pcb read))

