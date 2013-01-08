#lang racket
(require parser-tools/lex
         parser-tools/lex-sre
         parser-tools/yacc
         test-engine/racket-tests)

;; Just another toy lambda calculus parser and evaluator
;; to help me get familiar with racket's parser-tools.
;; Many thanks to Dan King for his calculator example:
;; https://gist.github.com/1068185 (on 2013/01/07)

(define-tokens tok (IDENT IDENTS))
(define-empty-tokens emp (OPAR CPAR DOT EOF LAM))
(define-lex-abbrevs
  (ident-chars (or (char-range "A" "z")
                   "?" "!" ":" "$" "%" "^" "&" "*" "'"))
  (ident (+ ident-chars)))

(define lc-lexer
  (lexer ("("   (token-OPAR))
         ("["   (token-OPAR))
         (")"   (token-CPAR))
         ("]"   (token-CPAR))
         ("λ"   (token-LAM))
         ("."   (token-DOT))
         (ident (token-IDENT lexeme))
         ;; discards current lexeme if it's whitespace
         (whitespace (lc-lexer input-port))
         ((eof) (token-EOF))))

;; an exp is one of {lam, app, name}
;; - a lam is a (lam name exp)
(define-struct lam (arg body))
;; - an app is a (app exp exp)
(define-struct app (left right))
;; - a name is a (name string) where
;;   the string satisfies the following pattern:
;;   (:-+ (:-or (char-range "A" "z")
;;              "?" "!" ":" "$" "%" "^" "&" "*" "'")))
(define-struct name (str))

(define lc-parser
  (parser (start exp)
          (end EOF)
          (error void)
          (tokens tok emp)
          (grammar (exp ((OPAR LAM IDENT DOT exp CPAR)
                         (make-lam (make-name $3) $5))
                        ((OPAR exp exp CPAR)
                         (make-app $2 $3))
                        ((IDENT) (make-name $1))))))

;; exp -> exp
(define (eval exp)
  (match exp
    ((app left right) (eval (β left right)))
    ((lam arg body) exp)
    ((name _) (error "agh! you tried to evaluate a name!"))))

;; exp exp -> exp
(define (β left right) (subst (lam-arg left) right (lam-body left)))

;; exp exp exp -> exp
;; substitutes v for every instance of n in exp
(define (subst n v exp)
  (define (subst* exp1) (subst n v exp1))
  (match exp
    ((lam n1 body) (if (string=? (name-str n) (name-str n1))
                       exp ; n1 shadows n
                       (lam n1 (subst* body))))
    ((app left right) (app (subst* left) (subst* right)))
    ((name str) (if (string=? str (name-str n)) v exp))))

;; exp -> string
(define (exp->string exp)
  (match exp
    ((lam arg body)
     (format "(λ~a.~a)" (exp->string arg) (exp->string body)))
    ((app left right)
     (format "(~a ~a)" (exp->string left) (exp->string right)))
    ((name str) str)))

;; string -> exp
(define (parse str)
  (let ((input (open-input-string str)))
    (lc-parser (λ () (lc-lexer input)))))

;; string -> string
;; parses and evaluates the given lc string,
;; returning its result in string form.
(define (run str) (exp->string (eval (parse str))))

(check-error (run "x") "agh! you tried to evaluate a name!")
(check-expect (run "(λx.x)") "(λx.x)")
(check-expect (run "((λx.x) (λx.x))") "(λx.x)")
;; (check-expect (run "((λx.(x x)) (λx.(x x)))") 42)
