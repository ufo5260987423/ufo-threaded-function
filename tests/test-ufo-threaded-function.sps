#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2022 Guy Q. Schemer
;; SPDX-License-Identifier: MIT
#!r6rs

(import (rnrs (6)) (srfi :64 testing) (ufo-threaded-function))

(define GAMMA 0.577215664901532860606512090082)
(define C_LIMIT 49) 
(define S_LIMIT 1e-5) 

(define (sum-stack stack item)
    (apply + (reverse (append stack (list item)))))

(define (trigamma x)
    (let loop ((stack '()) (iterator x))
        (if (or (nan? iterator) (infinite? iterator))
            (sum-stack stack iterator)
            (let ((inv (/ 1 (* iterator iterator))))
                (if (and (> iterator 0) (<= iterator S_LIMIT))
                    (sum-stack stack inv)
                    (if (>= iterator C_LIMIT)
                        (sum-stack stack (+ (/ 1 iterator) (/ inv 2) (* (/ inv iterator) (- 1/6 (* inv (+ 1/30 (/ inv 42)))))))
                        (loop (append stack (list inv)) (+ iterator 1))))))))

(define (digamma x)
    (let loop ((stack '()) (iterator x))
        (if (or (nan? iterator) (infinite? iterator))
            (sum-stack stack iterator)
            (if (and (> iterator 0) (<= iterator S_LIMIT))
                (sum-stack stack  (- 0 GAMMA (/ 1 iterator)))
                (let ((inv (/ 1 (* iterator iterator))))
                    (if (>= iterator C_LIMIT)
                        (sum-stack stack (- (log iterator) (/ 0.5 iterator) (* inv (+ 1/12 (* inv (- 1/120 (/ inv 252)))))))
                        (loop (append stack (list (/ -1 iterator))) (+ iterator 1))))))))

(define (t m proc l)
    (let ((begin (time-nanosecond (current-time))))
        (m proc l)
        (- (time-nanosecond (current-time)) begin )))

(test-begin "Test 1: threaded-map")
(let ((l (list 1 2 3 4 5 6)))
    (test-equal (map sqrt l) (threaded-map sqrt l))
    (test-equal (map trigamma l) (threaded-map trigamma l))
    (test-equal (map digamma l) (threaded-map digamma l))
    (test-equal #t (> (t map digamma l) (t threaded-map digamma l))))
(test-end)

(test-begin "Test 2: vector-threaded-map")
(let ((l (vector 1 2 3 4 5 6)))
    (test-equal (vector-map sqrt l) (threaded-vector-map sqrt l))
    (test-equal (vector-map trigamma l) (threaded-vector-map trigamma l))
    (test-equal (vector-map digamma l) (threaded-vector-map digamma l))
    (test-equal #t (> (t vector-map digamma l) (t threaded-vector-map digamma l))))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
