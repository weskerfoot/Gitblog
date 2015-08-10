#lang racket

(define/contract (categorize path-string)
  (-> absolute-path? any)
  (let-values ([(upper bottom not-root?)
                (split-path path-string)])
    (cond
      [(not upper) '()]
      [else
       (cons (path->string bottom)
             (categorize (path->string upper)))])))