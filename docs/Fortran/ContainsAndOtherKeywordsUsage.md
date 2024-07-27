###

Can you explain the keywords and this code and how it would be used?

```fortran
TYPE CUST
INTEGER :: CUST_NUMBER
CONTAINS
PROCEDURE, PASS :: GET_CUST => GET_CUST_NUMBER
END TYPE CUST
```

Sure, let's break down the code:

```fortran
TYPE CUST
  INTEGER :: CUST_NUMBER
  CONTAINS
  PROCEDURE, PASS :: GET_CUST => GET_CUST_NUMBER
END TYPE CUST
```

- `TYPE CUST`: This line begins the definition of a derived type named `CUST`¹. Derived types in Fortran allow you to define your own structures containing variables of different types⁵.

- `INTEGER :: CUST_NUMBER`: This line declares an integer component named `CUST_NUMBER` within the derived type `CUST`². This is a member of the `CUST` type, and it can be accessed using the `%` operator.

- `CONTAINS`: The `CONTAINS` keyword separates the main body of a derived type from any procedures that it may contain¹⁴.

- `PROCEDURE, PASS :: GET_CUST => GET_CUST_NUMBER`: This line declares a type-bound procedure⁷. `PROCEDURE` is a keyword that declares a procedure⁸. `PASS` is a keyword that indicates that the type itself should be passed as the first argument⁹. `GET_CUST` is the name that will be used to call the procedure, and `GET_CUST_NUMBER` is the actual name of the procedure⁹. This means that you can call the `GET_CUST_NUMBER` procedure on an object of type `CUST` using the syntax `object%GET_CUST()`.

- `END TYPE CUST`: This line ends the definition of the derived type `CUST`¹.

This code would be used to define a new data type `CUST` that contains an integer `CUST_NUMBER` and a procedure `GET_CUST_NUMBER`. You could then create variables of type `CUST`, access and modify their `CUST_NUMBER` component, and call their `GET_CUST_NUMBER` procedure. For example:

```fortran
TYPE(CUST) :: customer
customer%CUST_NUMBER = 123
PRINT *, customer%GET_CUST()
```

In this example, `customer` is a variable of type `CUST`. The `CUST_NUMBER` of `customer` is set to `123`, and then the `GET_CUST` procedure is called on `customer`. Note that the actual implementation of the `GET_CUST_NUMBER` procedure is not provided in your code snippet. It should be defined elsewhere in the program. The `GET_CUST_NUMBER` procedure should be a function or subroutine that takes an argument of type `CUST` (because of the `PASS` keyword) and does something with it. The specific behavior would depend on your program's requirements.

Source: Conversation with Copilot, 7/25/2024
(1) 27.1: Fortran 95/2003/2008 Keywords - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/27%3A_Appendix_7_-_Fortran_95_2003_2008_Keywords/27.01%3A_Fortran_95_2003_2008_Keywords.
(2) Difference between type and class in fortran 2003. https://stackoverflow.com/questions/29652620/difference-between-type-and-class-in-fortran-2003.
(3) Types and kinds — Fortran Programming Language. https://fortran-lang.org/en/learn/intrinsics/type/.
(4) Language Reference - CONTAINS - IBM. https://www.ibm.com/docs/en/xcafbg/9.0.0?topic=SS3KZ4_9.0.0/com.ibm.xlf111.bg.doc/xlflr/contain.html.
(5) Fortran/Fortran procedures and functions - Wikibooks. https://en.wikibooks.org/wiki/Fortran/Fortran_procedures_and_functions.
(6) PROCEDURE declaration (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/15.1.2?topic=attributes-procedure-declaration-fortran-2003.
(7) Implementation in Fortran 2003 - Stanford University. https://sepwww.stanford.edu/data/media/public/docs/sep142/bob1/paper_html/node8.html.
(8) Fortran 2003 / 2008: Elegant default arguments? - Stack Overflow. https://stackoverflow.com/questions/37723973/fortran-2003-2008-elegant-default-arguments.
(9) 17.2: Use Statement - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/17%3A_Modules/17.02%3A_Use_Statement.
(10) Type Declaration - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-type-declaration.
(11) Passing arguments by value in Fortran 95 - Stack Overflow. https://stackoverflow.com/questions/38848530/passing-arguments-by-value-in-fortran-95.
(12) VALUE (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-value-fortran-2003.
(13) Correct use of modules, subroutines and functions in Fortran. https://stackoverflow.com/questions/8412834/correct-use-of-modules-subroutines-and-functions-in-fortran.
(14) Fortran 2003 in Fortran Wiki. https://fortranwiki.org/fortran/show/Fortran+2003.
