### Does Fortran 2003 have anything similar to a class or an interface like other OOP languages? Can you utilize the interface to decouple and use like C# or Java? Do you have any good examples or github projects that use them?

Yes, Fortran 2003 does support Object-Oriented Programming (OOP) concepts similar to classes and interfaces found in other OOP languages².

1. **Classes**: In Fortran 2003, you can create classes using the `TYPE` keyword¹. These are known as derived types. You can also use the `CLASS` keyword to refer to these types or any of their extended types¹. This is similar to how classes work in other OOP languages.

```fortran
MODULE myModule
    IMPLICIT NONE
    TYPE :: myClass
        INTEGER :: myVar
    END TYPE myClass
END MODULE myModule

PROGRAM main
    USE myModule
    TYPE(myClass) :: obj
    obj%myVar = 10
    PRINT *, obj%myVar
END PROGRAM main
```

2. **Interfaces**: Fortran 2003 supports interfaces through type-bound procedures². These allow you to bind procedures (methods) to a type (class), providing functionality similar to interfaces in other OOP languages.

```fortran
MODULE myModule
    IMPLICIT NONE
    TYPE :: myClass
        INTEGER :: myVar
    CONTAINS
        PROCEDURE :: setVar
    END TYPE myClass

    CONTAINS
    SUBROUTINE setVar(this, val)
        CLASS(myClass), INTENT(INOUT) :: this
        INTEGER, INTENT(IN) :: val
        this%myVar = val
    END SUBROUTINE setVar
END MODULE myModule

PROGRAM main
    USE myModule
    TYPE(myClass) :: obj
    CALL obj%setVar(20)
    PRINT *, obj%myVar
END PROGRAM main
```

In this example, `setVar` is a method bound to the `myClass` type. It can be invoked on an object of `myClass` type, similar to how methods are invoked on objects in other OOP languages².

For more examples of OOP in Fortran 2003, you can refer to the following GitHub repositories:

- [OOP-Fortran-Examples](^1^): This repository contains examples of using the object-oriented features in Fortran¹.
- [OOP_F2003_Part_1](^2^): This repository contains examples of code reusability in Fortran 2003².
- [fortran_oop](^3^): This repository contains standalone modules or programs illustrating OOP features in Fortran³.

Let me know if you need further assistance!

Source: Conversation with Copilot, 7/25/2024
(1) Object-Oriented Programming in Fortran 2003 Part 1: Code ... - GitHub. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(2) GitHub - cmacmackin/OOP-Fortran-Examples: Examples of using the object .... https://github.com/cmacmackin/OOP-Fortran-Examples.
(3) GitHub - cmacmackin/OOP-Fortran-Examples: Examples of using the object .... https://github.com/cmacmackin/OOP-Fortran-Examples.
(4) Object-Oriented Programming in Fortran 2003 Part 1: Code ... - GitHub. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(5) GitHub - kguther/fortran_oop: Examples for OOP paradigms in Fortran. https://github.com/kguther/fortran_oop.
(6) GitHub - kguther/fortran_oop: Examples for OOP paradigms in Fortran. https://github.com/kguther/fortran_oop.
