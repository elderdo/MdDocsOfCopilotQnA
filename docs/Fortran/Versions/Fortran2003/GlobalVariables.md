### For Fortran 2003 what is the best way to define a global variable that needs to be used by Subroutines that are in the same file as the main but the subroutines follow the end Main statement and avoid having to add as another parameter to any of the subroutines?

In Fortran 2003, the most straightforward way to define a global variable that can be used by subroutines is to use modules¹. Here's an example:

```fortran
MODULE GlobalVars
    IMPLICIT NONE
    INTEGER :: global_var
END MODULE GlobalVars

PROGRAM Main
    USE GlobalVars
    IMPLICIT NONE
    global_var = 10
    CALL Subroutine
END PROGRAM Main

SUBROUTINE Subroutine
    USE GlobalVars
    IMPLICIT NONE
    WRITE (*,*) 'Global variable value: ', global_var
END SUBROUTINE Subroutine
```

In this example, `global_var` is a global variable defined in the `GlobalVars` module. This variable is accessible in both the main program and the subroutine by using the `USE` statement¹. This way, you can avoid passing the global variable as a parameter to the subroutine. Please remember to compile this program using a Fortran 2003 compliant compiler like gfortran 8.3.0.

Let me know if you need further assistance!

Source: Conversation with Copilot, 7/25/2024
(1) Externally declared (global) variable in Fortran - Stack Overflow. https://stackoverflow.com/questions/33832803/externally-declared-global-variable-in-fortran.
(2) Fortran Modules and Global Variables - Stack Overflow. https://stackoverflow.com/questions/44218867/fortran-modules-and-global-variables.
(3) fortran90 - global variables from main program to subroutine fortran .... https://stackoverflow.com/questions/32996801/global-variables-from-main-program-to-subroutine-fortran-dimensions-at-compile.
(4) Fortran subroutine - global variable - DASSAULT: ABAQUS FEA Solver .... https://www.eng-tips.com/viewthread.cfm?qid=241391.
(5) The GNU Fortran Compiler: Interoperable Global Variables. https://gcc.gnu.org/onlinedocs/gcc-8.4.0/gfortran/Interoperable-Global-Variables.html.
(6) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.

### For Fortran 2003 how do you share global variables with a PROGRAM and SUBROUTINE's that are outside of the PROGRAM & END PROGRAM?

In Fortran 2003, you can share global variables between a PROGRAM and SUBROUTINEs that are outside of the PROGRAM & END PROGRAM using the `COMMON` block. Here is an example:

```fortran
PROGRAM main
    IMPLICIT NONE
    INTEGER :: x
    COMMON /global/ x
    x = 1
    CALL mysub
    PRINT *, x
END PROGRAM main

SUBROUTINE mysub
    IMPLICIT NONE
    INTEGER :: x
    COMMON /global/ x
    x = x + 1
END SUBROUTINE mysub
```

In this example, the variable `x` is declared in a `COMMON` block, which is shared between the main program and the subroutine `mysub`. The subroutine `mysub` modifies the value of `x`, and this change is reflected in the main program¹².

Please note that while global variables can be used to share data between the main program and subprograms, it is generally recommended to limit their use to maintain good programming practices³.

Source: Conversation with Copilot, 7/27/2024
(1) Externally declared (global) variable in Fortran - Stack Overflow. https://stackoverflow.com/questions/33832803/externally-declared-global-variable-in-fortran.
(2) Pass a common variable to a subroutine in Fortran. https://stackoverflow.com/questions/26882202/pass-a-common-variable-to-a-subroutine-in-fortran.
(3) FORTRAN 90: Functions, Modules, and Subroutines - Iowa State University. https://meteor.geol.iastate.edu/classes/mt227/lectures/Subprograms.pdf.
