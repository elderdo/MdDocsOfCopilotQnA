### For Fortran 2003 what constitutes a header file?

In Fortran, the concept of a "header file" as it exists in languages like C or C++ does not exist¹. However, Fortran does have an `include` statement which operates very simply. It effectively tells the compiler to copy the contents of the included file and paste them into the Fortran source file (without modification) over the `include` line¹.

For example, if you have a file named `parameters.f90` that contains some common parameters, you can include this file in another Fortran program using the `include` statement:

```fortran
program main
  implicit none
  include 'parameters.f90'
  ! Rest of the code
end program main
```

In this case, `parameters.f90` acts similarly to a header file in C or C++, but it's important to note that Fortran includes the entire contents of the file, not just declarations or definitions¹.

It's also worth noting that Fortran does not have a standard file extension for include files. Any file can be included, regardless of its extension¹. The `.fd` extension you mentioned is not a de facto standard in the world of Fortran¹.

In modern Fortran, the use of modules is more common. Modules can contain type definitions, constants, and subprogram interfaces that can be used in other program units, providing a functionality somewhat similar to header files in C or C++³. For example:

```fortran
module constants
  implicit none
  real, parameter :: pi = 3.14159
end module constants

program main
  use constants
  implicit none
  print*, "Value of pi: ", pi
end program main
```

In this example, the `constants` module acts like a header file, providing the definition of `pi` to the main program. However, unlike header files, modules also ensure that the compiler checks for consistency between the module and the program unit that uses it³.

Source: Conversation with Copilot, 7/27/2024
(1) Combine Fortran .for and .f90 include header files. https://stackoverflow.com/questions/30750278/combine-fortran-for-and-f90-include-header-files.
(2) c - Module "header" in Fortran - Stack Overflow. https://stackoverflow.com/questions/22477498/module-header-in-fortran.
(3) including a c/c++ header file in fortran subroutines. https://stackoverflow.com/questions/14629307/including-a-c-c-header-file-in-fortran-subroutines.
(4) Opening Binary Files in Fortran: Status, Form, Access. https://stackoverflow.com/questions/9998262/opening-binary-files-in-fortran-status-form-access.
(5) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.
