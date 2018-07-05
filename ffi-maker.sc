(import (irregex irregex))

(define (read-file file-name)
  ;; 读取文件
  (let ((p (open-input-file file-name)))
      (let loop ((lst '()) (c (read-char p)))
          (if (eof-object? c)
              (begin 
                  (close-input-port p)
                  (list->string (reverse lst)))
              (loop (cons c lst) (read-char p))))))

(define (write-file file-name content)
  ;; 写文件
  (delete-file file-name)
  (let ([p (open-output-file file-name)] [len (string-length content)])
    (let loop ([idx 0])
      (when (< idx len)
          (write-char (string-ref content idx) p)
          (loop (add1 idx))))
    (close-output-port p)
  )
)

(define find-functions
  (lambda (str)
    (define pattern "(?<rst>\\S+?)\\s+?(?<sym>\\S+?)\\((?<args>[^()]*?)\\);")
    (define len (string-length str))
    (let loop ([match (irregex-search pattern str 0 len)]
               [func-list '()])
      (if match
        (loop
          (irregex-search pattern str (irregex-match-end-index match) len)
          (cons
            (reg-func match)
            func-list
          )
        )
        (reverse func-list)
      )
    )
  ))  


(define reg-func
  (lambda (match)
    (let ([c-def (irregex-match-substring match)]
          [sym (irregex-match-substring match 'sym)]
          [args (irregex-match-substring match 'args)]
          [rst (irregex-match-substring match 'rst)])
      (list
        (cons "c-def" (irregex-replace/all "[\\r\\n]" c-def ""))
        (cons "sym" sym)  
        (cons "args" (reg-args (ffi-replace args)))
        (cons "rst" (ffi-replace rst))
        (cons "name" (sym->name sym))
      )
    )
  ))

(define reg-args
  (lambda (str)
    (define pattern "(?<type>[^,\\s]+?)\\s+?(?<arg>[^,\\s]+?)")
    (define len (string-length str))
    (let loop ([match (irregex-search pattern str 0 len)]
               [rst '()])
      (if match
        (loop
          (irregex-search pattern str (irregex-match-end-index match) len)
          (cons (reg-arg match) rst)
        )
        (string-join (reverse rst) " "))
    )
  ))

(define reg-arg
  (lambda (match)
    (let ([type (irregex-match-substring match 'type)]
          [arg (irregex-match-substring match 'arg)])
      (if (char=? (string-ref arg 0) #\*)
        (format "(* ~a)" type)
        type
      )
    )
  ))

(define sym->name
  (lambda (sym)
    (let loop ([n '()] [s (string->list sym)])
      (if (null? s)
        (list->string (reverse n))
        (let* ([c (car s)])
          (cond
            ((char=? c #\_) (loop (cons #\- n) (cdr s))) 
            ((char-upper-case? c) (loop (cons* (char-downcase c) #\- n) (cdr s)))
            (else (loop (cons c n) (cdr s)))
          )
        )
      )
    )
  ))

(define replace-list
  (list
    (cons "OPENBLAS_CONST" "")
    (cons "CBLAS_INDEX" "size_t")
    (cons "enum" "")
    (cons "CBLAS_ORDER" "cblas-order")
    (cons "CBLAS_TRANSPOSE" "cblas-transpose")
    (cons "CBLAS_UPLO" "cblas-uplo")
    (cons "CBLAS_DIAG" "cblas-diag")
    (cons "CBLAS_SIDE" "cblas-size")
    ))
  
(define ffi-replace
  (lambda (str)
    (let loop ([str str] [rl replace-list])
      (if (null? rl)
        str
        (loop
          (irregex-replace/all (car (car rl)) str (cdr (car rl)))
          (cdr rl))
      )
    )
  ))

(define (string-join string-list sep)
  (let loop ([new-s '()] [old-s string-list])
      (if (null? old-s)
        (if (null? new-s)
          ""  
          (apply string-append (reverse (cdr new-s))))
        (loop (cons* sep (car old-s) new-s) (cdr old-s)))
  )
)

(define make-library
  (lambda (func-list lib-name)
    (format
"(library (~a ~a)
  (export
~a  )
  (import
    (scheme)
    (chezcore ffi)
    (chezcore pkg))

  (define lib-name (pkg-prop \"~a\" \"lib\"))

  (define lib (load-shared-object lib-name))

~a)"
    lib-name
    lib-name
    ; export
    (apply
      string-append
      (map
        (lambda (func)
          (format
            "    ~a\n"
            (cdr (assoc "name" func))
          )
        )
        func-list))
    lib-name
    ; fun
    (apply
      string-append
      (map
        (lambda (func)
          (format
            "  ;; ~a\n  (define-procedure ~a \"~a\" (~a) ~a)\n\n"
            (cdr (assoc "c-def" func))
            (cdr (assoc "name" func))
            (cdr (assoc "sym" func))
            (cdr (assoc "args" func))
            (cdr (assoc "rst" func))
          )
        )
        func-list))
    )
  ))

(define main
  (lambda ()
    (let* ([args (command-line-arguments)]
           [file-name (car args)]
           [lib-name (cadr args)]
           [file (read-file file-name)]
           [func-list (find-functions file)]
           [text (make-library func-list lib-name)]
           )
      (write-file (format "~a.sc" lib-name) text)
    )
  ))

(main)