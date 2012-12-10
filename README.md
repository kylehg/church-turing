Lambda Calculus–Turing Machine Converter
========================================

For the final project in Brent Yorgey's [Art of Recursion](http://www.cis.upenn.edu/~cis39903/) course, I have undertaken to write a converter between the lambda calculus and Turing machines, in Haskell.

More documentation to come as the project progresses.


### Reduction strategy

Brent on substition:

> Before performing a substitution, first rename bound variables as necessary (resulting in an α-equivalent term) so that no free variable has the same name as any bound variable.
**Option 1**: Take a set of variables and a term, and go through the term, ehc
