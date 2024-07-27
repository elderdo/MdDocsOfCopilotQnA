### For Fortran 2003 how do you share a global instance of a type that contains an integer counter, and a boolean DEBUG indicator with the PROGRAM and SUBROUTINE's?

In Fortran 2003, you can share a global instance of a type that contains an integer counter and a boolean DEBUG indicator with the PROGRAM and SUBROUTINEs using a module. Here is an example:

```fortran
MODULE my_module
    IMPLICIT NONE
    TYPE :: my_type
        INTEGER :: counter
        LOGICAL :: DEBUG
    END TYPE my_type
    TYPE(my_type), SAVE :: global_instance
END MODULE my_module

PROGRAM main
    USE my_module
    IMPLICIT NONE
    global_instance%counter = 1
    global_instance%DEBUG = .TRUE.
    CALL my_subroutine
    PRINT *, global_instance%counter, global_instance%DEBUG
END PROGRAM main

SUBROUTINE my_subroutine
    USE my_module
    IMPLICIT NONE
    global_instance%counter = global_instance%counter + 1
    global_instance%DEBUG = .FALSE.
END SUBROUTINE my_subroutine
```

In this example, a type `my_type` is defined in the module `my_module`, and a global instance `global_instance` of this type is declared. This instance is shared between the main program and the subroutine `my_subroutine`. The subroutine `my_subroutine` modifies the values of `global_instance`, and these changes are reflected in the main program¹².

Please note that while global variables can be used to share data between the main program and subprograms, it is generally recommended to limit their use to maintain good programming practices³.

Source: Conversation with Copilot, 7/27/2024
(1) Externally declared (global) variable in Fortran - Stack Overflow. https://stackoverflow.com/questions/33832803/externally-declared-global-variable-in-fortran.
(2) fortran - saving program inputs to a module for "global" use - Stack .... https://stackoverflow.com/questions/12288377/saving-program-inputs-to-a-module-for-global-use.
(3) Correct use of modules, subroutines and functions in Fortran. https://stackoverflow.com/questions/8412834/correct-use-of-modules-subroutines-and-functions-in-fortran.
