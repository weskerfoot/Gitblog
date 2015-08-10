#! /usr/bin/racket
#lang racket
(require "api.rkt")

(parameterize ([current-commits (get-commits)]
               [password (xstring (your-config 'password))]
               [username (xstring (your-config 'username))])
  (commit-posts))
