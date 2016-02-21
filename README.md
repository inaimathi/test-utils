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

`test-utils` makes `cl-quickcheck` exports symbols for all primitive generators. These are `a-boolean`, `a-string`, `a-symbol`, `a-char`, `an-integer`, `a-real` and `an-index`. They work exactly the same way as the direct `cl-quickcheck` generators, but are exported in the variable namespace so that you don't have to randomly prefix some of them with `#'`.

The compound generators `a-list`, `a-member` and `a-tuple` are also re-exported, but remain in the function namespace for easier composition.

### New Generators

New primitve generators are `a-ratio`, `a-number` and `an-atom`. They generate lisp ratios, arbitrary lisp numerics, and arbitrary atomic values respectively.

New compound generators are `a-pair`, `a-vector` and `a-hash`.

There is one new generator named `a-value`, which returns an arbitrary value tree. It can return an atom, or any of the compound forms above with atoms in all slots filled with atoms. It's not exactly type-sound, but comes in handy when testing symbol-manipulation procedures or macros.

There is one new generator named `one-of`. It takes a list of values, and generates one of the given values. It works sort of like `a-member`, but for constants.

### `tests (&rest forms)`

A macro that counts off the given tests and calls `prove:finalize` after all tests. It automates some otherwise manual book-keeping required by `prove`. It takes any number of `prove` tests.

### `qchecks (quickcheck-test &optional message)`

Runs a `cl-quickcheck` test in the context of a `prove` test suite, with optional message.

### `quiet-check (&body body)`

Runs a `cl-quickcheck` suite, but squelches initial random seed reporting, and only sends to `*standard-output*` on failure, rather than unconditionally.

### `for-all ((&rest bindings) test &optional message)`

Shorthand for a single-clause `(qchecks (for-all (...) ...))` with optional message. This is a common enough use case for me that went ahead and put in the shortcut.

## TODOs/Thoughts

- Considering a few additional generators, even though I don't personally need them.
	- Specifically, `a-plist`, `an-alist` and `an-improper-list` for other common Lisp structures. The last one might be useful if for no reason other than negative test-cases.
	- `an-array` might also be useful. It would take a `dimensions` list, and a generator, and return the appropriately shaped (potentially) multi-dimensional array.
	- Also, `a-value` is mildly limited, compared to the general space of all lisp values. Specifically, it assumes that the elements of the generated `pair`s, `list`s, `vector`s, and `hash`es are themselves `atom`ic values. This is not always a good assumption; consider either re-working `a-value` so that it can generate compound values with non-`atom` components, or writing a separate generator named `a-wild-value` or something which would do the more general thing.
- Additional predicates/test-forms might be useful for `prove`. I've already found myself in one situation where I wished I had `contains`, and might have used it if it existed already.
