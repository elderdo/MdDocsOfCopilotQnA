### Can you explain how the keyword CONTAINS is used by Fortran 2003?

In Fortran 2003, the `CONTAINS` keyword is used to separate the main body of a program, external subprogram, or module subprogram from any internal subprograms that it may contain¹. It also separates the specification part of a module from any module subprograms¹.

Here are some rules for using `CONTAINS`:

- When a `CONTAINS` statement exists, at least one subprogram must follow it¹.
- The `CONTAINS` statement cannot appear in a block data program unit or in an internal subprogram¹.
- Any label of a `CONTAINS` statement is considered part of the main program, subprogram, or module that contains the `CONTAINS` statement¹.

In the context of a derived-type definition, the `CONTAINS` statement introduces the procedure part of the definition¹. At least one procedure must follow the statement¹.

Here's an example of how `CONTAINS` is used in a module:

```fortran
MODULE A
...
CONTAINS ! Module subprogram must follow
SUBROUTINE B (X)
...
CONTAINS ! Internal subprogram must follow
FUNCTION C (Y)
...
END FUNCTION
END SUBROUTINE
END MODULE
```

And here's an example of how `CONTAINS` is used in a derived type definition:

```fortran
TYPE CUST
INTEGER :: CUST_NUMBER
CONTAINS
PROCEDURE, PASS :: GET_CUST => GET_CUST_NUMBER
END TYPE CUST
```

In these examples, the `CONTAINS` keyword is used to indicate that what follows are internal subprograms (in the first example) or procedures (in the second example)¹.

Source: Conversation with Copilot, 7/25/2024
(1) Language Reference - CONTAINS - IBM. https://www.ibm.com/docs/SS3KZ4_9.0.0/com.ibm.xlf111.bg.doc/xlflr/contain.htm.
(2) 27.1: Fortran 95/2003/2008 Keywords - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/27%3A_Appendix_7_-_Fortran_95_2003_2008_Keywords/27.01%3A_Fortran_95_2003_2008_Keywords.
(3) Implementation in Fortran 2003 - Stanford University. https://sepwww.stanford.edu/data/media/public/docs/sep142/bob1/paper_html/node8.html.
(4) fortran - When do I have to use CONTAINS in a module? - Stack Overflow. https://stackoverflow.com/questions/67636893/when-do-i-have-to-use-contains-in-a-module.
(5) Keywords in Fortran Wiki. https://fortranwiki.org/fortran/show/Keywords.
