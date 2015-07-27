#! /usr/bin/racket
#lang racket
(require xml)
(require xml/path)
(require net/url)
(require racket/string)

; The current list of commits (as a dynamically scoped name)
(define current-commits (make-parameter (list)))

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
  (xstring ""))

(define username
   (xstring ""))

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

(define (get-post-id result)
  (se-path* '(string)
             (string->xexpr
              result)))

; Writes a post to the blog
(define (write-post post-id title content tags categories)
  (get-post-id
   (port->string
   (post-pure-port
    (string->url "https://primop.me/blog/xmlrpc.php")
   (string->bytes/utf-8
     (xexpr->string
      (cond
        [(not post-id) (new-post title content (terms-names tags categories))]
        [else (edit-post title content post-id)])))))))

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
(define (handle-post post post-id)
        (call-with-values
         (lambda ()
           (parse-post
            (port->string
             (open-input-file post))))
         (curry write-post post-id)))

; Get a list of all commit refs
(define (get-commits)
  (string-split
   (system-result "git rev-list master")
   "\n"))

; Convert a commit ref into its post ID number (if it exists)
(define (commit->post-id post-name)
  (compose1
   string->number
   string-trim
   system-result
   (curry format "git notes --ref=~a show ~a" post-name)))

; Grab a post id given a post name
; Return false if it does not exist
(define (git-href post-name)
  (let ([get-post-id (commit->post-id post-name)])
  (match
   (get-post-id
    (memf
     get-post-id
     (current-commits)))
    [(list-rest post-id _) post-id]
    [_ #f])))

; Add or refresh a post id associated with that post name
(define (git-set! post-name post-id)
  (system
   (format
    "git notes --ref=~a add HEAD -fm \"~a\""
    post-name
    post-id)))

(parameterize ([current-commits (get-commits)])
  (for ([post (get-files)])
    (match (git-href post)
      [#f (displayln "new post!")
          (let ([post-id (handle-post post #f)])
            (git-set! post post-id))]
      [post-id
       (displayln (format "updating post ~a" post-id))
       (git-set! post post-id)
       (handle-post post post-id)])))
