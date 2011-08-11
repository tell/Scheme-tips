;; Copyright (c) 2011-2011 Tadanori TERUYA (tell) <tadanori.teruya@gmail.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;; @license: The MIT license <http://opensource.org/licenses/MIT>

;;; Implementation for:
;;; $ gosh -V
;;; Gauche scheme shell, version 0.9 [utf-8,pthreads], i386-apple-darwin10.4.0

(define (modpow-LSB x e m)
  (define (modmul x y)
    (remainder (* x y) m))
  (define (modsq x)
    (remainder (* x x) m))
  (let ((len (integer-length e)))
    (let iter ((result 1)
               (x x)
               (i 0))
      (if (= i len)
          result
          (iter
           (if (logbit? i e)
               (modmul result x)
               result)
           (modsq x)
           (+ i 1))))))

(modpow-LSB 1234 5678 91011)

(define (modpow-MSB x e m)
  (define (modmul x y)
    (remainder (* x y) m))
  (define (modsq x)
    (remainder (* x x) m))
  (let ((len (integer-length e)))
    (let iter ((result x)
               (i (- len 2)))
      (if (negative? i)
          result
          (let ((sq (modsq result)))
            (iter
             (if (logbit? i e)
                 (modmul sq x)
                 sq)
             (- i 1)))))))

(modpow-MSB 1234 5678 91011)
