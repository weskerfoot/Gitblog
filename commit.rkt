#! /usr/bin/racket
#lang racket
(require "api.rkt")

(if (in-blog?)
    (parameterize ([current-commits (get-commits)]
               [password (xstring (your-config 'password))]
               [username (xstring (your-config 'username))])
  (commit-posts))
    (displayln "Not in the blog directory right now so quitting."))