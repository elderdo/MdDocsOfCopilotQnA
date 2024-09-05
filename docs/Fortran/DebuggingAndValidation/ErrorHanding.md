### Are there any try / catch like features for fortran 2003 so you capture exceptions and their related messages? Can user defined exceptions be thrown?

Fortran 2003 does not have built-in exception handling structures like `try/catch` found in languages such as Python or C++. However, there are ways to mimic exception handling in Fortran¹.

One common approach is to use an integer return variable to indicate an error code. For example:

```fortran
subroutine do_something(stat)
    integer :: stat
    !... Some error occurred
    if (error) stat = 1
end subroutine
```

In the main program, you can then check the status:

```fortran
call do_something(stat)
if (stat /= 0) print *, "This is an error message!"
```

Another approach is to define a dedicated derived type for exceptions that is capable of also storing an error message¹.

For user-defined exceptions, you could potentially use the final procedure of a type that stores the exceptions¹. However, this is more of a workaround and doesn't provide the full functionality of exception handling found in other languages.

There are also external libraries, such as the [Fortran Exception Library](^2^), that provide more sophisticated exception handling mechanisms for Fortran, including the common `try-except-finally` syntax known from most major languages².

Please note that these are not standard features of Fortran 2003 and may require specific compilers or additional libraries. Always ensure that your code remains portable and maintainable when adding these features.

Source: Conversation with Copilot, 7/25/2024
(1) Does exception handling exist in Fortran? - Stack Overflow. https://stackoverflow.com/questions/48212806/does-exception-handling-exist-in-fortran.
(2) Fortran Exception Library - GitHub. https://github.com/projekter/FortranExceptions.
(3) Fortran Exception Library - GitHub. https://github.com/projekter/FortranExceptions.
(4) The New Features of Fortran 2003. https://wg5-fortran.org/N1551-N1600/N1579.pdf.
(5) Catching fortran runtime errors and signals in C++ binding. https://stackoverflow.com/questions/56518283/catching-fortran-runtime-errors-and-signals-in-c-binding.
(6) EE EGuide to Fortran 2003 Programming - Springer. https://link.springer.com/content/pdf/10.1007/978-1-84882-543-7.pdf.
(7) User-defined derived-type Input/Output (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xffbg/121.141?topic=statements-user-defined-derived-type-inputoutput-fortran-2003.
(8) Fortran 2003 - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/15.1.0?topic=standards-fortran-2003.
