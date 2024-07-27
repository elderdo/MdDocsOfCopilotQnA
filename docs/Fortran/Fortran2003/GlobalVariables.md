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
