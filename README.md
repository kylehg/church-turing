Lambda Calculus–Turing Machine Converter
========================================

For the final project in Brent Yorgey's [Art of Recursion](http://www.cis.upenn.edu/~cis39903/) course, I have undertaken to write a converter between the lambda calculus and Turing machines, in Haskell.

## Lambda calculus
For the definition of a LC term, we used the commonly-agreed-upon version described in class.

```haskell
data Term = Var Name
          | Lam Name Term
          | App Term Term
```

In the common, notation, `Var "x"` is _x_, `Lam x t` is _\x. t_, and `App t u` is _t u_.

We have also defined the normal form of a term as the beta-reduced term, which is described by `nf` in `Church.hs`. For describing the beta-reduction, I looked at Augustsson, "Lambda-calculus cooked four ways."

## Turing machines

There are many definitions of a Turing machine. For our conversion, we use the one described in Sipser, "Introduction to the Theory of Commputation," as a tuple of:

1. A set of possible states, represented here as `[Int]`.
1. A set of possible characters (the alphabet), represented as `[Char]`.
1. An initial state.
1. A terminal state.
1. A transition function `(Int, Char) -> (Int, Char, Dir)`, where `Dir = L | R`.

We represent a TM tape as a pair of lists, the characters before the head and the characters after.

## Turing machine → lambda calculus

For the transition from Turing machines to lambda calculus, I relied heavily on the proofs in Dal Lago and Martini, "The Weak Lambda Calculus as a Reasonable Machine," which ran be found in `/reading`.

There are two main "tricks" on which the conversion to lambda calculus relies: recusion through the Y-combinator and encoding symbols. The Y-combinator here is defined as:

    Y = F F
	F = \x.\f.f (\z. x x f z)

For any LC term _T_, _Y T_ beta-reduces to _T (\z.Y T z)_.

As for elements of a set, we encode an element _xi_ in that set as _\x1.\x2.[...]\xn.xi_. For a string of symbols, we string these together, so

    encode("")    = \x1.\x2.[...]\xn.\e. e
	encode(xi:xs) = \x1.\x2.[...]\xn.\e.xi encode(xs)

With these conventions, defining the `cons` operation, and reading characters, and finally encoding a Turing machine configuration becomes straightforward. Relies on enumerating a series of function applications for every possible state and character combination, and then applying it recursively to the state. This is done in `Conversion.tm`.

## Files

- `Turing.hs`: The definition of a turing machine and the tools to run it.
- `Church.hs`: The definition of lambda calculus terms and the beta reduction of them.
- `Conversion.hs`: The functions to convert TMs to the LC.

## Bibliography

Turing, A. M. 1936. "[On Computable Numbers, with an Application to the Entscheidungsproblem](www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf)." _Proceedings of the London Mathematical Society_ 2.42, pp. 230–65.

Turing, A. M. 1937. "[Computability and λ-Definability](http://www.jstor.org/stable/2268280)." _The Journal of Symbolic Logic_ 2.4 (Dec.), pp. 153–63.

Sipser, Michael. 2006. _Introduction to the Theory of Computation_. 2nd ed.
