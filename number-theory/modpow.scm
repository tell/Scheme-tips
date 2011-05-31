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
