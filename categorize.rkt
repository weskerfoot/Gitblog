#lang racket

(define blog-location (make-parameter #f))

(define/contract (categorize path-string)
  (-> path-string? any)
  (let-values ([(upper bottom not-root?)
                (split-path path-string)])
    (cond
      [(not (path? upper)) (list (path->string
                                  bottom))]
      [(not upper) '()]
      [else
       (cons (path->string bottom)
             (categorize (path->string upper)))])))

;(define (prefix-of 

(define (get-categories path-string)
  (let ([non-categories (categorize (blog-location))])
    (filter
     (lambda (cat)
       (not
        (member cat non-categories)))
     (cdr
      (categorize path-string)))))

(provide get-categories blog-location)