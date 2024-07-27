### Does Fortran 2003 have static classes, static attributes, or static methods? If so, can you show examples of each?

Fortran 2003 does not have the concept of static classes, static attributes, or static methods in the way languages like Java or C++ do. However, it does have similar concepts:

1. **Static-like Attributes**: Fortran has the `SAVE` attribute, which can be used to preserve the value of a local variable across subroutine calls[^10^]¹¹. This is similar to static variables in other languages. Here's an example:

```fortran
SUBROUTINE example_subroutine
  INTEGER, SAVE :: counter = 0
  counter = counter + 1
  PRINT *, "This subroutine has been called ", counter, " times"
END SUBROUTINE example_subroutine
```

In this example, the `counter` variable will retain its value between calls to `example_subroutine`, similar to a static variable.

2. **Static-like Classes**: Fortran doesn't have classes in the way that object-oriented languages do. However, it does have derived types, which can be used to create complex data structures⁵. Here's an example:

```fortran
MODULE my_module
  TYPE :: my_type
    INTEGER :: my_integer
  END TYPE my_type
END MODULE my_module
```

In this example, `my_type` is a derived type that contains an integer. You can create variables of this type, similar to creating instances of a class.

3. **Static-like Methods**: Fortran doesn't have static methods, but it does have module procedures, which are procedures defined inside a module⁵. These procedures can be called without creating an instance of a type, similar to static methods. Here's an example:

```fortran
MODULE my_module
  CONTAINS
  SUBROUTINE my_subroutine()
    PRINT *, "Hello, world!"
  END SUBROUTINE my_subroutine
END MODULE my_module
```

In this example, `my_subroutine` is a procedure that's part of `my_module`. You can call this procedure using `CALL my_module%my_subroutine()`.

Please note that these are just basic examples and actual usage may involve more complex data structures and procedures. For detailed information, you should refer to the Fortran 2003 standard or a comprehensive Fortran 2003 guide¹.

Source: Conversation with Copilot, 7/25/2024
(1) AUTOMATIC and STATIC attributes (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/AUTOMATIC-and-STATIC-attributes.html.
(2) fortran90 - Fortran SAVE statement - Stack Overflow. https://stackoverflow.com/questions/2893097/fortran-save-statement.
(3) Object-oriented programming in Fortran Wiki. https://fortranwiki.org/fortran/show/Object-oriented+programming.
(4) A Fortran 2003 introduction by examples - Universitetet i Oslo. https://www.uio.no/studier/emner/matnat/geofag/GEO4060/v12/undervisningsmateriale/fortran.pdf.
(5) Introduction to Programming using Fortran 95/2003/2008. http://www.egr.unlv.edu/~ed/fortranv3.pdf.
(6) Fortran/Fortran examples - Wikibooks, open books for an open world. https://en.wikibooks.org/wiki/Fortran/Fortran_examples.
(7) Object-Oriented Features in Fortran 2003 - msu.ru. http://www.elch.chem.msu.ru/tch/group/FortranBooks/Fortran_Object_Oriented.pdf.
(8) Difference between type and class in fortran 2003. https://stackoverflow.com/questions/29652620/difference-between-type-and-class-in-fortran-2003.
(9) CLASS (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-class-fortran-2003.
(10) Object-Oriented Programming in Fortran 2003 Part 1: Code Reusability. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(11) STATIC (FORTRAN 77 Language Reference) - Oracle. https://docs.oracle.com/cd/E19957-01/805-4939/6j4m0vnba/index.html.
(12) Fortran, parameters and static table inside a derived data type. https://stackoverflow.com/questions/19810506/fortran-parameters-and-static-table-inside-a-derived-data-type.
