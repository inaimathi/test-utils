# Test Utils

`:test-utils` provides convenience functions and macros for [:prove](https://github.com/fukamachi/prove) and [:cl-quickcheck](https://github.com/mcandre/cl-quickcheck). Specifically, its aim is to make it simple to use the two together.

`:test-utils` is still a work in progress. So, you know, careful. To be fair though, I am using this in some serious projects where testing is going to be important, so it won't be *too* much of a moving target.

## Basic Usage

```common-lisp
(tests
 (is (+ 1 2) 3 "Addition works")
 (is (+ 3 3 3 6) 15 "Addition works on more than two numbers")
 (is (+ -1 -1 -1) -3 "Addition works on negative numbers")
 (is (+ -1 2) 1 "Addition works on negative and positive numbers together")

 (for-all ((a a-number) (b a-number))
   (is= (+ a b) (+ b a))
   "Addition is commutative"))
```

This test suite will pass with flying colors.

```
; SLIME 2015-06-01
CL-USER> (ql:quickload :test-utils)
To load "test-utils":
  Load 1 ASDF system:
    test-utils
; Loading "test-utils"
..................................................
[package test-utils]
(:TEST-UTILS)
CL-USER> (defpackage :testing (:use :cl :test-utils))
#<PACKAGE "TESTING">
CL-USER> (in-package :testing)
#<PACKAGE "TESTING">
TESTING> (tests
 (is (+ 1 2) 3 "Addition works")
 (is (+ 3 3 3 6) 15 "Addition works on more than two numbers")
 (is (+ -1 -1 -1) -3 "Addition works on negative numbers")
 (is (+ -1 2) 1 "Addition works on negative and positive numbers together")

 (for-all ((a a-number) (b a-number))
   (is= (+ a b) (+ b a))
   "Addition is commutative"))
1..5

  ✓ Addition works
  ✓ Addition works on more than two numbers
  ✓ Addition works on negative numbers
  ✓ Addition works on negative and positive numbers together
  ✓ Addition is commutative

✓ 5 tests completed (3ms)
T
TESTING>
```

It combines four `prove` tests, and one `cl-quickcheck` test.

## Exported Symbols

In addition to the below, `test-utils` exports all test symbols from `prove`, and all symbols from `cl-quickcheck` **except for `cl-quickcheck:is` and `cl-quickcheck:isnt`**. Instead, you should use `is=` inside of `for-all` blocks.

### Re-exported Generators

`test-utils` exports `cl-quickcheck` symbols for all primitive generators. These are

- `a-boolean`
- `a-string`
- `a-symbol`
- `a-char`
- `an-integer`
- `a-real`
- `an-index`

They work exactly the same way as the direct `cl-quickcheck` generators, but are exported in the variable namespace so that you don't have to randomly prefix some of them with `#'` when composing or calling `generate`.

The compound generators

- `a-list`
- `a-member`
- `a-tuple`

are also re-exported, but remain in the function namespace for easier composition.

### New Generators

Lets get the obvious ones out of the way first. New primitve generators are

- `a-ratio`
- `a-number`
- `a-keyword`
- `an-atom`

They generate lisp ratios, arbitrary lisp numerics, symbols in the `:keyword` package, and arbitrary atomic values respectively.

New compound generators are

- `a-pair`
- `a-vector`
- `a-hash`

`a-pair` generates `cons` cells, `a-vector` generates vectors and `a-hash` generates hash-tables.

New generators that require some notes are

- `a-value` returns an arbitrary value tree. It can return an atom, or any of the compound forms above with atoms in all slots filled with atoms. It comes in handy when testing symbol-manipulation procedures or macros.
- `one-of` takes a list of values, and generates one of them. It works sort of like `a-member`, but for constants. `(generate (one-of :a :b :c))` will return one of `:a`, `:b` or `:c`.
- `an-alist` is an alias for `(a-list (a-pair key-generator value-generator))`.
- `a-plist` generates property lists; flat lists whose odd elements are treated as keys and even elements are treated as values.
- `an-improper-list` generates lists that aren't consed onto `NIL`. At minimum, this is a `cons` cell, and there are usually multiple values `cons`ed onto it. `(generate (an-improper-list an-integer))` will return something that looks like `(7 8 -13 0 -7 20 . -6)` or `(15 . 1)`. This might be useful in negative test cases.
- `an-array` takes a list of dimensions and a generator, and generates n-dimensional arrays with the given dimension list.
	- `(generate (an-array '() an-integer))` => `#0A0`
	- `(generate (an-array '(5) an-integer))` ~> `#(-11 -3 13 15 -14)`
	- `(generate (an-array '(5 4) an-integer))` ~> `#2A((-17 1 -8 0) (14 -7 -4 -1) (6 14 5 12) (-8 17 -18 1) (13 19 -12 -18))`
	- `(generate (an-array '(5 4 3) an-integer))` ~> #3A(((-15 16 -6) (18 -15 -14) (-16 13 -17) (3 -1 17))
    ((18 -7 4) (5 2 -4) (-3 -5 11) (9 0 -2))
    ((2 -5 8) (-2 -8 -5) (-20 5 -3) (-2 -5 -11))
    ((1 7 2) (-16 8 8) (10 1 -6) (-16 15 19))
    ((20 12 -4) (-14 5 12) (13 -7 -4) (12 6 15)))
	- ... and so forth

### `tests (&rest forms)`

A macro that counts off the given tests and calls `prove:finalize` after all tests. It automates some otherwise manual book-keeping required by `prove`. It takes any number of `prove` tests.

### `qchecks (quickcheck-test &optional message)`

Runs a `cl-quickcheck` test in the context of a `prove` test suite, with optional message.

### `quiet-check (&body body)`

Runs a `cl-quickcheck` suite, but squelches initial random seed reporting, and only sends to `*standard-output*` on failure, rather than unconditionally.

### `for-all ((&rest bindings) test &optional message)`

Shorthand for a single-clause `(qchecks (for-all (...) ...))` with optional message. This is a common enough use case for me that went ahead and put in the shortcut.

## TODOs/Thoughts

- `a-value` is mildly limited, compared to the general space of all lisp values. Specifically, it assumes that the elements of the generated `pair`s, `list`s, `vector`s, and `hash`es are themselves `atom`ic values. This is not always a good assumption; consider either re-working `a-value` so that it can generate compound values with non-`atom` components, or writing a separate generator named `a-wild-value` or something which would do the more general thing.
- Additional predicates/test-forms might be useful for `prove`. I've already found myself in one situation where I wished I had `contains`, and might have used it if it existed already.
