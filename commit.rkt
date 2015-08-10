#! /usr/bin/racket
#lang racket
(require "api.rkt")


(parameterize ([current-commits (get-commits)])
  (commit-posts))
