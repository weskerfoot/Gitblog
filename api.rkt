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

(define valid-config-keys
  ; The list of all valid configuration keys
  ; i.e. what you can put on the left side of an = in your config
  (list
    "password"
    "username"
    "url"
    "blog-location"))

(define (valid-key? key)
  ; Check if a key is in the list of valid ones
  (memf
    (curry equal? key)
    valid-config-keys))

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
        [(list key val)
         (cond
           [(valid-key? key)
            (hash-set! config
                       (string->symbol key)
                       val)]
           [else
             (error
               (format "Invalid configuration line: ~a" val))])]))
    
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

(define (delete-post post-id)
  (method-call 'wp.deletePost
               (list
                (xint 1)
                username
                password
                (xint post-id))))

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

; Deletes a post
(define (rm-post post-id)
  (port->string
   (post-pure-port
    (string->url (your-config 'url))
    (string->bytes/utf-8
     (xexpr->string
      (delete-post post-id))))))

; Returns a list of all modified post files in this commit
(define (get-files)
  (map
   (lambda (file)
     (string-split
      file
      ":"))
   (string-split
   (with-output-to-string
    (lambda ()
     (system
      "git status --short | grep -E '^(A|M|D)' | awk '{ printf \"%s:%s\\n\",$1,$2 }' | grep -E '\\.post$'")))
   "\n")))

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
(define (handle-post status post post-id)
   (match status

     [(? (lambda (x)
           (ormap
            (curry equal? x)
            (list "A" "M"))))
      (call-with-values
         (lambda ()
           (parse-post
            (port->string
             (open-input-file post))))
         (curry write-post post-id))]
     
     ["D" (rm-post post-id)]

     [m (error
         (format "Unimplemented mode ~a" m))]))

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
      
      [str str]))])
    result))

; Grab a value given a key
; Return the default value if it is set
; otherwise raise an exception
(define git-href
  (let ([def-val (gensym)])
    (lambda (post-name [default def-val])
      (match
        (memf
        (lambda (commit-ref)
          (let ([notes (git-notes-ref post-name commit-ref)])
          (match notes
            ["" #f]
            [result #t])))
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

; Add or refresh a key associated with that value
(define (git-set! key val)
  (system
   (format
    "git notes --ref=~a add HEAD -fm \"~a\""
    key
    val)))

;(parameterize ([current-commits (get-commits)])

; Run when a commit of one or more posts occurs
(define (commit-posts)
  (for ([post-file (get-files)])

    (let ([post-status (car post-file)]
           [post (cadr post-file)])
      
      (match (git-href post #f)

        [#f
        (when (empty? (current-commits))
          ; Add a first commit if there are none so it can store the note properly!
          (system "git commit -n --allow-empty -m \"bootstrap blog\""))
        
          (git-set! post (handle-post post-status post #f))]
        
       [commit-id
        (let ([post-id (commit->post-id post commit-id)])
          (git-set! post post-id)
          (handle-post post-status post post-id))]))))

(provide
  git-href
  git-set!
  commit-posts
  new-config
  get-commits
  current-commits
  your-config)