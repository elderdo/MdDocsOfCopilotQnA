### For Fortran 2003 do you have any examples using statements FORALL and WHERE and utilizing the attribute INTENT? Are there any other attributes?

Sure, here are some examples of `FORALL`, `WHERE`, and `INTENT` in Fortran 2003:

1. **FORALL Statement**⁵⁷⁸:

```fortran
! Copy columns of array y into rows of array x
FORALL (j=1:m, k=1:n) x(k,j) = y(j,k)

! Take the reciprocal of each nonzero element of array y and assign it to the corresponding element of array x
FORALL (i=1:n, j=1:n, y(i,j).NE.0.0) x(i,j) = 1.0 / y(i,j)
```

2. **WHERE Statement**¹⁵¹⁶:

```fortran
! Given an array A and a scalar B
real, dimension(:) :: A
real :: B

! Assign B to all elements of A that are greater than zero
WHERE (A > 0) A = B
```

3. **INTENT Attribute**¹²⁴:

```fortran
! A subroutine with an INTENT(IN) argument
subroutine sub1(A)
  real, intent(in) :: A
  ! A can be used here but not modified
end subroutine sub1

! A subroutine with an INTENT(OUT) argument
subroutine sub2(A)
  real, intent(out) :: A
  ! A must be assigned a value here before use
end subroutine sub2

! A subroutine with an INTENT(INOUT) argument
subroutine sub3(A)
  real, intent(inout) :: A
  ! A can be used and modified here
end subroutine sub3
```

In addition to `INTENT`, Fortran 2003 also includes other attributes such as `PARAMETER`, `ALLOCATABLE`, `DIMENSION`, `EXTERNAL`, `INTRINSIC`, `OPTIONAL`, `POINTER`, `SAVE`, `TARGET`, `VOLATILE`, and `ASYNCHRONOUS`[^10^]¹². Each of these attributes provides specific information about how a variable or procedure should be treated by the compiler. For example, the `ALLOCATABLE` attribute indicates that a variable's memory can be dynamically allocated and deallocated during program execution[^10^].

Source: Conversation with Copilot, 7/27/2024
(1) Examples of the FORALL Statement - Rice University. http://hpff.rice.edu/versions/hpf1/hpf-v10/subsection2_5_1_3.html.
(2) Parallel Programming Features in the Fortran Standard. https://www.intel.com/content/dam/develop/external/us/en/documents/parallel-programming-features.pdf.
(3) FORALL (Fortran 95) - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/13.1.0?topic=attributes-forall-fortran-95.
(4) 8.1: Conditional Expressions - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/08%3A_Selection_Statements/8.01%3A_Conditional_Expressions.
(5) 8.3: IF Statements - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/08%3A_Selection_Statements/8.03%3A_IF_Statements.
(6) arguments - What is the explicit difference between the fortran intents .... https://stackoverflow.com/questions/1011604/what-is-the-explicit-difference-between-the-fortran-intents-in-out-inout.
(7) Argument Intent. https://hogback.atmos.colostate.edu/fortran/docs/class4.pdf.
(8) "PROCEDURE attribute conflicts with INTENT attribute" when compiling .... https://stackoverflow.com/questions/43279757/procedure-attribute-conflicts-with-intent-attribute-when-compiling-simple-fort.
(9) The New Features of Fortran 2003. https://wg5-fortran.org/N1551-N1600/N1579.pdf.
(10) Fortran 2003: the latest Fortran Standard - University of York. https://www-users.york.ac.uk/~mijp1/COL/fortran_2003.pdf.
(11) Fortran 2003 and Beyond - CUG. https://cug.org/5-publications/proceedings_attendee_lists/2005CD/S05_Proceedings/pages/Authors/Long-0517-1100/Long-0517-1100_paper.pdf.
(12) Interpreting the FORALL statement - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/13.1.0?topic=95-interpreting-forall-statement.
(13) CLASS (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-class-fortran-2003.
(14) Fortran 2003 in Fortran Wiki. https://fortranwiki.org/fortran/show/Fortran+2003.
(15) A Simple Fortran Primer - Monash University. https://users.monash.edu.au/~dprice/teaching/fortran/FortranPrimer.pdf.
(16) 8.5: SELECT CASE Statement - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/08%3A_Selection_Statements/8.05%3A_SELECT_CASE_Statement.
(17) undefined. http://www.dkuug.dk/jtc1/sc22/open/n3661.pdf.
