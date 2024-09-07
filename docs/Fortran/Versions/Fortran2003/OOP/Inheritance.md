### Can you do inheritance with class or interface for Fortran 2003? Can these new OOP features of Fortran 2003 be utilitzed to update older code and to not have to rewrite a lot of the old code? Are there any good github examples of doing that?

Yes, Fortran 2003 supports inheritance, which is a key feature of Object-Oriented Programming (OOP). You can create a derived type (similar to a class in other OOP languages) that extends another type, thereby inheriting all of its components¹. Here's an example:

```fortran
MODULE myModule
    IMPLICIT NONE
    TYPE :: parentType
        INTEGER :: var1
    END TYPE parentType

    TYPE, EXTENDS(parentType) :: childType
        INTEGER :: var2
    END TYPE childType
END MODULE myModule

PROGRAM main
    USE myModule
    TYPE(childType) :: obj
    obj%var1 = 10
    obj%var2 = 20
    PRINT *, obj%var1, obj%var2
END PROGRAM main
```

In this example, `childType` is a derived type that extends `parentType`, so it inherits the `var1` component from `parentType` and also has its own `var2` component¹.

As for updating older code, yes, the OOP features of Fortran 2003 can be used to modernize and improve the structure of older Fortran code without having to rewrite it entirely. This can be achieved by encapsulating data and procedures into derived types (classes), using inheritance to reuse code, and employing polymorphism to make the code more flexible¹.

Here are some GitHub repositories that provide examples of using OOP features in Fortran 2003, including inheritance:

- [OOP-Fortran-Examples](^3^): This repository contains examples of using the object-oriented features in Fortran³.
- [fortran_oop](^6^): This repository contains standalone modules or programs illustrating OOP features in Fortran⁶.
- [OOP_F2003_Part_1](^1^) and [OOP_F2003_Part_2](^2^): These repositories contain examples of code reusability and data polymorphism in Fortran 2003¹².

These examples can serve as a guide for how to update older Fortran code with the new OOP features of Fortran 2003. However, the specifics of how to update the code would depend on the details of the existing codebase. Let me know if you need further assistance!

Source: Conversation with Copilot, 7/25/2024
(1) Object-Oriented Programming in Fortran 2003 Part 1: Code ... - GitHub. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(2) cmacmackin/OOP-Fortran-Examples - GitHub. https://github.com/cmacmackin/OOP-Fortran-Examples.
(3) cmacmackin/OOP-Fortran-Examples - GitHub. https://github.com/cmacmackin/OOP-Fortran-Examples.
(4) GitHub - kguther/fortran_oop: Examples for OOP paradigms in Fortran. https://github.com/kguther/fortran_oop.
(5) GitHub - kguther/fortran_oop: Examples for OOP paradigms in Fortran. https://github.com/kguther/fortran_oop.
(6) Object-Oriented Programming in Fortran 2003 Part 1: Code ... - GitHub. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(7) Object-Oriented Programming in Fortran 2003 Part 2: Data ... - GitHub. https://gist.github.com/n-s-k/de4af7ce6cc8f2c85e4b33cedb51fd88.
(8) Object-Oriented Programming in Fortran 2003 Part 2: Data ... - GitHub. https://gist.github.com/n-s-k/de4af7ce6cc8f2c85e4b33cedb51fd88.
(9) ISO C/C++ & Fortran Interoperability Examples - GitHub. https://github.com/adamabernathy/fortran-idl-c-interoperability.
(10) GitHub - Foadsf/FortranBLASExamples: Comprehensive examples of .... https://github.com/Foadsf/FortranBLASExamples.
(11) GitHub - arktools/fortran_tools: Tools for updating old Fortran .... https://github.com/arktools/fortran_tools.
(12) undefined. https://software.intel.com/en-us/forums/intel-fortran-compiler/topic/607462.
(13) undefined. https://github.com/Foadsf/FortranBLASExamples.git.
