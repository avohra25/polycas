# polycas

A small computer algebra system in portable Common Lisp, with no dependencies
beyond ASDF (which ships with every major implementation).

So far it contains the expression core and calculus:

- an infix parser (`2x^2 - 3(x + 1)/y`, decimals read as exact rationals),
- a canonical simplifier: flattening, exact rational folding, collecting like
  terms and like factors, distributing integer powers, exact roots of rationals,
- a printer whose output re-reads to the identical canonical tree,
- symbolic differentiation (sums, products, general powers `x^x`, sin, cos,
  tan, exp, log, chain rule, higher derivatives),
- simultaneous substitution, exact/floating evaluation, and full expansion.

```lisp
(polycas:to-infix (polycas:read-expr "x*y + y*x - (2x)^2/x"))
;; => "2*x*y - 4*x"

(polycas:to-infix (polycas:diff (polycas:read-expr "x^x") (polycas:var "x")))
;; => "x^x*(log(x) + 1)"

(polycas:to-infix (polycas:expand (polycas:read-expr "(x + y)^3")))
;; => "x^3 + 3*x^2*y + 3*x*y^2 + y^3"
```

## Running the tests

```sh
sbcl --script run-tests.lisp      # add -v for per-test output
```

The suite includes randomized property tests (value preservation at random
rational points, idempotence, derivatives against finite differences, print/re-read round trips, operand-order
independence) driven by a seeded generator, so failures reproduce.

## License

MIT
