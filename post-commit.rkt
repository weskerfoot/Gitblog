#lang racket

; Runs after a commit (to analyze the commit message)

(require "api.rkt")

(displayln
 (format
  "The commit message was ~a"
  (get-commit-msg)))