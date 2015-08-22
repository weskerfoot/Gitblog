#! /usr/bin/racket
#lang racket
(require xml)
(require xml/path)
(require net/url)
(require racket/string)
(require readline/readline)
(require "categorize.rkt")

(define (create-config path ex)
  (let ([password (readline "Your wordpress password? ")]
        [username (readline "Your wordpress username? ")]
        [blog-location (readline "Where is the blog directory? ")]
        [conf (open-output-file path)])
    (displayln
     (format "password = ~a" password) conf)
    (displayln
     (format "username = ~a" username) conf)
    (displayln
     (format "blog-location = ~a" blog-location) conf)
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

(define (in-blog?)
  (equal?
    (string-trim
     (system-result "pwd"))
   (path->string
    (path->directory-path
    (string->path
    (your-config 'blog-location))))))

; The current list of commits (as a dynamically scoped name)
(define current-commits (make-parameter (list)))

; The current configuration
(define your-config (new-config "/home/wes/.config/gitblog.conf"))

(define password (make-parameter #f))
(define username (make-parameter #f))

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
                (username)
                (password)
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
                (username)
                (password)
                (xint post-id))))

; Updates an existing post
(define (edit-post post-title
                   post-content
                   post-id
                   terms-names)
  (method-call 'wp.editPost
               (list
                (xint 1)
                (username)
                (password)
                (xint post-id)
                (xstruct
                 `(("post_title"
                   ,(xstring post-title))
                  ("post_excerpt"
                   ,(xstring (substring post-content 20)))
                  ("post_content"
                   ,(xstring post-content))
                  ("terms_names"
                   ,terms-names))))))

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
        [(not post-id) (new-post
                        title
                        content
                        (terms-names tags categories))]
        
        [else (edit-post title content post-id
                         (terms-names tags categories))])))))))

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
   string-split
   (string-split
   (with-output-to-string
    (lambda ()
     (system
      "git status --short | grep -E '\\.post$'")))
   "\n")))

; Returns the latest commit message
; Intended to run in the post-commit hook
; since you have to actually write the commit msg first
(define (get-commit-msg)
  (system-result
   "git log -1 HEAD | tail -n 1 | sed s/^[[:space:]]*// | tr -d '\\n'"))

; Parses a post file and returns the components
(define (parse-post categories tags text)
  (let ([lines (string-split text "\n")])
    (values
     (car lines)
     (string-join
      (cdr lines)
      "\n")
     tags categories)))

; Writes a new post and returns its post id
(define (handle-post categories tags status post post-id)
   (match status

     [(? (lambda (x)
           (ormap
            (curry equal? x)
            (list "A" "M"))))
      (call-with-values
         (lambda ()
           (parse-post
            categories
            tags
            (port->string
             (open-input-file post))))
         (curry write-post post-id))]
     
     ["D" (displayln post-id)
          (rm-post post-id)]

     [m (displayln
         (format "Untracked file ~a" m))]))

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
          (commit->value
           post-name
           commit-ref)))

(define (commit->value key ref)
  (string-trim
   (system-result
    (format
     "git notes --ref=~a show ~a"
     key ref))))

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

; Add tags associated with a post
(define (add-tag! post tag)
  (git-set!
   (format
    "~a.tags" post)
   tag))

(define (get-tags post)
  (let*
      ([key (format "~a.tags" post)]
       [ref (git-href
             key
             #f)])
    (cond
      [(not ref) (list "default")]
      [else
       (let ([tag-string
              (commit->value
               key
               ref)])
    (parse-tags tag-string))])))

(define (parse-tags tag)
  (map
   (lambda (st)
     (substring st 1))
   (string-split tag ":")))

(define (to-tag-string taglist)
  (string-join
   (map
    (curry format "#~a")
    taglist)
   ":"))

; Check the environment variables for any new tags
(define (new-tags)
  (let* ([environ (current-environment-variables)]
         [names (environment-variables-names environ)]
         [names*
          (filter
             (compose1
              (curry
               regexp-match
               #rx"^.+_tags$")
              string-foldcase
              bytes->string/utf-8)
             names)]
         [names-hash (make-hash)])
    (for ([name names*])
      (let ([tag-string (bytes->string/utf-8
                        (environment-variables-ref environ name))])
      (hash-set! names-hash
                 (bytes->string/utf-8 name)
                  (parse-tags tag-string))))
    names-hash))

(define (check-tags post new-tags)
  (match (hash-ref new-tags
               (format "~a_tags"
                       (regexp-replace* #px"\\/|\\." post "_"))
               #f)
    [#f
     (match (get-tags post)
       [#f (list "default")]
       [tags
        (add-tag! post (to-tag-string tags))
        tags])]
    [tags
     (add-tag! post (to-tag-string tags))
     tags]))
  

(define (add-tag-alias)
  ; Should only run when the repo is first made
  (system
   "git config alias.tag !() { git notes --ref=$1 add HEAD -fm \"${*:2}\"}")) 
    

;(parameterize ([current-commits (get-commits)])

; Run when a commit of one or more posts occurs
(define (commit-posts)
  (parameterize
      ([blog-location (your-config 'blog-location)])
    (for ([post-file (get-files)])

    (let* ([post-status (car post-file)]
           [post (cadr post-file)]
           [categories (get-categories post)]
           [tags (check-tags post (new-tags))])
      (displayln tags)
      
      (match (git-href post #f)

        [#f
        (when (empty? (current-commits))
          ; Add a first commit if there are none so it can store the note properly!
          (system "git commit -n --allow-empty -m \"bootstrap blog\""))
        
          (git-set! post
                    (handle-post
                     categories
                     tags
                     post-status
                     post #f))]
        
       [commit-id
        (let ([post-id
               (commit->post-id post commit-id)])
          (git-set! post post-id)
          (handle-post
           categories
           tags
           post-status
           post
           post-id))])))))

(provide
  git-href
  git-set!
  commit-posts
  new-config
  get-commits
  get-commit-msg
  current-commits
  your-config
  password
  username
  xstring
  in-blog?)