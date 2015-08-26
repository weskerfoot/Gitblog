#! /usr/bin/racket
#lang racket

(require "api.rkt")

(define argv
  (current-command-line-arguments))

(displayln
  (add-tag-env
    (vector-ref argv 0)
  (vector-ref argv 1)))

(displayln (environment-variables-names (current-environment-variables)))
