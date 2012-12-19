Lambda Calculus–Turing Machine Converter
========================================

For the final project in Brent Yorgey's [Art of Recursion](http://www.cis.upenn.edu/~cis39903/) course, I have undertaken to write a converter between the lambda calculus and Turing machines, in Haskell.

More documentation to come as the project progresses.

## Turing machine → lambda calculus

Given a finite alphabet _S_, we encode a symbol _xi_ in that alphabet as `\x1.\x2.[...]\xn.\e.xi`, where _e_ is represents the blank symbol. Thus, for the whole tape, `w1w2w3` is encoded as

`\x1.x2.[...]\xn.\e.w1 (\x1.\x2.[...]\xn.\e.w2 (\x1.[...]\xn.\e.w3 (\x1.[...]\xn.\e.e)))`

Reading symbols is somewhat trivial, then.


Plan:
- 


### Reduction strategy

Brent on substition:

> Before performing a substitution, first rename bound variables as necessary (resulting in an α-equivalent term) so that no free variable has the same name as any bound variable.
**Option 1**: Take a set of variables and a term, and go through the term, ehc


### Turing Machine Definition

For defining a Turing Machine, I've borrowed mostly from Turing (1936) and Sipser (2006), as well as made some convenient abstrations that I deemed acceptable.

#### Tape Representation

I've defined a Turing Machine tape as a pair of lists of symbols before and after the TM head, respectively. This allows me to convenintly read forward and backward using the `:` operator in Haskell.

```
data Tape = Tape [Alphabet] [Alphabet] deriving (Eq, Show)
```

#### Insertion & Deletion

In addition to the operations of moving the TM head left and right, I've added the convenience operation of inserting and removing a symbol. This is valid because we can write a TM that performs this operation in all circumstances. To insert a symbol _a_ before a symbol _b_ in the tape:

1. At _b_, we replace _b_ with a new symbol `#` and read right, remembering _b_ with a state. (Since the alphabet is finite, we can do this.)
2. Once we reach the end of the the tape and have shifted the last symbol over one, we read left until hittin `#`.
3. We replace `#` with _a_ and continue on our merry way.

By a similar token, we can write a TM for removing a symbol from the tape and shifting everyting left. I use the combination of insertion and deletion in lieu of defining standalone print and erasure actions.



## Bibliography

Turing, A. M. 1936. "[On Computable Numbers, with an Application to the Entscheidungsproblem](www.cs.virginia.edu/~robins/Turing_Paper_1936.pdf)." _Proceedings of the London Mathematical Society_ 2.42, pp. 230–65.

Turing, A. M. 1937. "[Computability and λ-Definability](http://www.jstor.org/stable/2268280)." _The Journal of Symbolic Logic_ 2.4 (Dec.), pp. 153–63.

Sipser, Michael. 2006. _Introduction to the Theory of Computation_. 2nd ed.
