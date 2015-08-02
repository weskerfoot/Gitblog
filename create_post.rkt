#! /usr/bin/racket
#lang racket
(require xml)
(require xml/path)
(require net/url)
(require racket/string)
(require readline/readline)

(define (create-config path ex)
  (let ([password (readline "Your wordpress password? ")]
        [username (readline "Your wordpress username? ")]
        [conf (open-output-file path)])
    (displayln
     (format "password = ~a" password) conf)
    (displayln
     (format "username = ~a" username) conf)
    (close-output-port conf)
    (new-config path)))

(define (new-config path)
  (with-handlers
    ([exn:fail:filesystem:errno? (curry create-config path)])
  (let* ([config-file (open-input-file path)]
         [lines (string-split
                 (port->string config-file)
                 "\n")]
         [config (make-hash)])
    (for ([line lines]
          #:unless (string=? line ""))
      (match (map string-trim
                  (string-split line "="))
        [(list "password" password)
         (hash-set! config 'password password)]
        [(list "username" username)
         (hash-set! config 'username username)]
        [(list "url" url)
         (hash-set! config 'url url)]
        [val (error
              (format "Invalid configuration line: ~a" val))]))
    (curry hash-ref config))))

; The current list of commits (as a dynamically scoped name)
(define current-commits (make-parameter (list)))

; The current configuration
(define your-config (new-config "/home/wes/.config/gitblog.conf"))

; Run a command and get the string written to stdout
(define (system-result command)
  (match
      (process command)
    [(list out in pid errport _)
     (let ([result (port->string out)]
           [err-result (port->string out)])
       (close-input-port out)
       (close-output-port in)
       (close-input-port errport)
       (cond
         [(not (string=? "" (string-trim result))) result]
         [else #f]))]))

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
  (xstring (your-config 'password)))

(define username
   (xstring (your-config 'username)))

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
    (string->url (your-config 'url))
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
   (match (system-result "git rev-list master")
     [#f ""]
     [str str])
   "\n"))

; Convert a commit ref into its post ID number (if it exists)
(define (commit->post-id post-name commit-ref)
   (string->number
    (string-trim
        (system-result
          (format "git notes --ref=~a show ~a" post-name commit-ref)))))

(define (git-notes-ref post-name commit-ref)
  (let ([result
          (string-trim
    (match
      (system-result
      (format "git notes --ref=~a show ~a"
            post-name
            commit-ref))
      [#f ""]
      [str str]))
    
    ])
    (displayln (format "git notes ref result: ~a" result))
    result))

; Grab a post id given a post name
; Return false if it does not exist
(define git-href
  (let ([def-val (gensym)])
    (lambda (post-name [default def-val])
      (match
        (memf
        (lambda (commit-ref)
          (displayln (format "checking commit ~a" commit-ref))
          (let ([notes (git-notes-ref post-name commit-ref)])
          (match notes
            ["" (displayln (format "commit ~a did not have anything" commit-ref)) #f]
            [result (displayln (format "found ~a in commit ~a" result commit-ref)) #t])))
        (current-commits))
        [(list-rest commit-id _) commit-id]
        [_
          (cond
            [(eq? def-val default)
            (raise
              (exn:fail
                (format
                  "git-href: no value found for key\n\t~a" post-name)
                (current-continuation-marks)))]
            [else default])]))))

; Add or refresh a post id associated with that post name
(define (git-set! post-name post-id)
  ;(displayln (format "git setting ~a ~a" post-name post-id))
  (system
   (format
    "git notes --ref=~a add HEAD -fm \"~a\""
    post-name
    post-id)))

 (parameterize ([current-commits (get-commits)])
   (displayln (format "these are the current commits: ~a" (current-commits)))
   (for ([post (get-files)])
        ;(call-with-values (lambda () (split-path (string->path post))) (compose1 displayln list))
     (match (git-href post #f)
       [#f (displayln "new post!")

           (when (empty? (current-commits))
             ; Add a first commit if there are none so it can store the note properly!
             (system "git commit -n --allow-empty -m \"bootstrap blog\""))

             (git-set! post (handle-post post #f))]

       [commit-id
        (displayln (format "updating post ~a" post))
        (displayln (format "the commit ref I got is ~a" commit-id))
        (let ([post-id (commit->post-id post commit-id)])
          (displayln post-id)
          (displayln (format "the commit ref is ~a" commit-id))
          (git-set! post post-id)
          (handle-post post post-id))])))

