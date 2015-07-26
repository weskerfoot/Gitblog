#! /usr/bin/racket
#lang racket
(require xml)
(require xml/path)
(require net/url)
(require racket/string)

; Run a command and get the string written to stdout
(define (system-result command)
  (match
      (process command)
    [(list out in pid errport _)
     (let ([result (port->string out)])
       (close-input-port out)
       (close-output-port in)
       (close-input-port errport)
       result)]))

(define posts (make-hash))

(define (store-post-id title post-id)
  (hash-set! posts title post-id))

(define (retrieve-post-id title)
  (hash-ref posts title))

; XML-RPC string
(define (xstring str)
  `(value
    (string ,str)))

; XML-RPC int
(define (xint int)
  `(value
    (int ,(number->string int))))

; XML-RPC array
(define (xarray members)
  `(array
    ,(cons 'data members)))

; XML-RPC struct
(define (xstruct members)
  (cons 'struct
     (for/list ([member members])
         `(member
            (name ,(car member))
            ,(cadr member)))))

(define password
  (xstring "youtpass"))

(define username
   (xstring "yourname"))

; Puts the tags and categories into a terms_names struct
(define (terms-names tags categories)
  (xstruct
   `(("post_tag" ,(xarray (map xstring tags)))
     ("category" ,(xarray (map xstring categories))))))

; Calls an arbitrary wordpress XML-RPC method
(define (method-call name args)
   `(methodCall
    (methodName ,(symbol->string name))
     ,(cons 'params
       (for/list ([arg args])
        `(param ,arg)))))

; Creates a new post
(define (new-post post-title post-content terms_names)
  (method-call 'wp.newPost
               (list
                (xint 1)
                username
                password
                (xstruct
                 `(("post_title"
                   ,(xstring post-title))
                  ("post_status"
                   ,(xstring "draft"))
                  ("post_author"
                   ,(xint 1))
                  ("post_excerpt"
                   ,(xstring (substring post-content 20)))
                  ("post_content"
                   ,(xstring post-content))
                  ("terms_names"
                   ,terms_names))))))

; Updates an existing post
(define (edit-post post-title
                   post-content
                   post-id)
  (method-call 'wp.editPost
               (list
                (xint 1)
                username
                password
                (xint post-id)
                (xstruct
                 `(("post_title"
                   ,(xstring post-title))
                  ("post_excerpt"
                   ,(xstring (substring post-content 20)))
                  ("post_content"
                   ,(xstring post-content)))))))

(define (is-new? title)
  (not
   (hash-has-key? posts title)))

(define (get-post-id result)
  (se-path* '(string)
             (string->xexpr
              result)))

; Writes a post to the blog
(define (write-post title content tags categories)
  (get-post-id
   (port->string
   (post-pure-port
    (string->url "https://yourblog.com/xmlrpc.php")
   (string->bytes/utf-8
     (xexpr->string
      (cond
        [(is-new? title) (new-post title content (terms-names tags categories))]
        [else (edit-post title content (retrieve-post-id title))])))))))

; Returns a list of all modified post files in this commit
(define (get-files)
  (string-split
   (with-output-to-string
    (lambda ()
     (system
      "git status --short | grep -E '^(A|M)' | awk '{ print $2 }' | grep -E '\\.post$'")))
   "\n"))

; Parses a post file and returns the components
(define (parse-post text)
  (let ([lines (string-split text "\n")])
    (values
     (car lines)
     (string-join
      (cdr lines)
      "\n")
     '("test" "firstpost") '("Introduction" "Tests"))))

; Writes a new post and returns its post id
(define (handle-post post)
     (call-with-values
      (lambda ()
        (parse-post
         (port->string
         (open-input-file post))))
      write-post))

(define (get-commits)
  (string-split
   (system-result "git rev-list master")
   "\n"))

(define (tracked? post-name)
   (memf
    (compose1 number? string->number string-trim)
    (for/list ([commit (get-commits)])
    (system-result
     (format "git notes --ref=~a show ~a" post-name commit)))))

(for ([post (get-files)])
  (displayln (length (get-commits)))
  (let* ([post-id (system-result
                 (format
                  "git notes --ref=~a show ~a" post (cadr (get-commits))))])
  (match (tracked? post)
    [#f (displayln "new post!")
        (let ([post-id (handle-post post)])
         (system
          (format
           "git notes --ref=~a add HEAD -m \"~a\"" post post-id)))]
    [(list-rest post-id _) (displayln (format "updating post ~a" post-id))
       (system (format
                "git notes --ref=~a add HEAD -m \"~a\"" post post-id))])))
