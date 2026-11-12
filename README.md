# polycas

A small computer algebra system in portable Common Lisp, with no dependencies
beyond ASDF (which ships with every major implementation).

This first cut contains the expression core:

- an infix parser (`2x^2 - 3(x + 1)/y`, decimals read as exact rationals),
- a canonical simplifier: flattening, exact rational folding, collecting like
  terms and like factors, distributing integer powers, exact roots of rationals,
- a printer whose output re-reads to the identical canonical tree.

```lisp
(polycas:to-infix (polycas:read-expr "x*y + y*x - (2x)^2/x"))
;; => "2*x*y - 4*x"
```

## Running the tests

```sh
sbcl --script run-tests.lisp      # add -v for per-test output
```

The suite includes randomized property tests (value preservation at random
rational points, idempotence, print/re-read round trips, operand-order
independence) driven by a seeded generator, so failures reproduce.

## License

MIT
