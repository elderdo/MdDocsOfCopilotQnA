### For Fortran 2003 do you have some examples of the five new features:

1. **Derived Type Enhancements**: Parameterized derived types, improved control of accessibility, improved structure constructors, and finalizers¹.
2. **Object-Oriented Programming Support**: Type extension and inheritance, polymorphism, dynamic type allocation, and type-bound procedures¹.
3. **Procedure Pointers**
4. **Interoperability with C**.
5. **Exception Handling**

Sure, here are some examples for each of the new features in Fortran 2003:

1. **Derived Type Enhancements**: Parameterized derived types, improved control of accessibility, improved structure constructors, and finalizers¹⁶¹⁷¹⁸¹⁹[^20^].

   ```fortran
   TYPE general_point (k, dim)
       INTEGER, KIND :: k = selected_real_kind(6,70)
       INTEGER, LEN :: dim
       REAL(k) :: coordinates(dim)
   END TYPE general_point
   ```

2. **Object-Oriented Programming Support**: Type extension and inheritance, polymorphism, dynamic type allocation, and type-bound procedures¹².

   ```fortran
   module class_Circle
       implicit none
       private
       real :: pi = 3.1415926535897931d0 ! Class-wide private constant
       type, public :: Circle
           real :: radius
           contains
           procedure :: area => circle_area
           procedure :: print => circle_print
       end type Circle
       contains
       function circle_area(this) result(area)
           class(Circle), intent(in) :: this
           real :: area
           area = pi * this%radius**2
       end function circle_area
       subroutine circle_print(this)
           class(Circle), intent(in) :: this
           real :: area
           area = this%area() !
       end subroutine circle_print
   end module class_Circle
   ```

3. **Procedure Pointers**¹¹¹²¹³¹⁴¹⁵.

   ```fortran
   PROCEDURE (PROC), POINTER :: PTR
   ```

4. **Interoperability with C**²¹²²²³²⁴²⁵.

   ```fortran
   use, intrinsic :: iso_c_binding
   integer(c_int) :: foo    ! equivalent of 'int foo' in C
   real(c_float) :: bar     ! equivalent of 'float bar' in C
   ```

5. **Exception Handling**⁶⁷⁸⁹[^10^].
   ```fortran
   try
       call do_something(stat)
   except(E)
       type is(ENotSupportedException)
           print *, "Got an exception: ", E%Message
   endExcept
   ```

Please note that these are just basic examples and the actual usage might vary based on the specific requirements of your program. Also, remember to compile your Fortran program with a compiler that supports Fortran 2003 to use these features.

Source: Conversation with Copilot, 7/27/2024
(1) The New Features of Fortran 2003. https://wg5-fortran.org/N1601-N1650/N1648.pdf.
(2) The new features of Fortran 2003 - BCS. https://fortran.bcs.org/2007/jubilee/newfeatures.pdf.
(3) Extensible derived types (Fortran 2003) - IBM - United States. https://www.ibm.com/docs/en/xl-fortran-aix/15.1.0?topic=types-extensible-derived-fortran-2003.
(4) Fortran 2003: the latest Fortran Standard - University of York. https://www-users.york.ac.uk/~mijp1/COL/fortran_2003.pdf.
(5) Derived type parameters (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=types-derived-type-parameters-fortran-2003.
(6) Object-Oriented Features in Fortran 2003 - msu.ru. http://www.elch.chem.msu.ru/tch/group/FortranBooks/Fortran_Object_Oriented.pdf.
(7) Object-oriented programming in Fortran Wiki. https://fortranwiki.org/fortran/show/Object-oriented+programming.
(8) Procedure pointers (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xffbg/121.141?topic=procedures-procedure-pointers-fortran-2003.
(9) Procedural pointer in fortran - Stack Overflow. https://stackoverflow.com/questions/67161321/procedural-pointer-in-fortran.
(10) PROCEDURE declaration (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/15.1.2?topic=attributes-procedure-declaration-fortran-2003.
(11) Function pointer arrays in Fortran - Stack Overflow. https://stackoverflow.com/questions/2521072/function-pointer-arrays-in-fortran.
(12) Fortran2003: procedure pointer to a function returning a pointer to a .... https://stackoverflow.com/questions/14899268/fortran2003-procedure-pointer-to-a-function-returning-a-pointer-to-a-polymorphi.
(13) Interoperability with C (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html.
(14) Fortran Tutorial => Calling C from Fortran. https://riptutorial.com/fortran/example/7149/calling-c-from-fortran.
(15) Mixed-Language Programming (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Mixed-Language-Programming.html.
(16) ISO C/C++ & Fortran Interoperability Examples - GitHub. https://github.com/adamabernathy/fortran-idl-c-interoperability.
(17) Interoperability in Fortran Wiki. https://fortranwiki.org/fortran/show/Interoperability.
(18) Does exception handling exist in Fortran? - Stack Overflow. https://stackoverflow.com/questions/48212806/does-exception-handling-exist-in-fortran.
(19) Fortran Exception Library - GitHub. https://github.com/projekter/FortranExceptions.
(20) GitHub - LightDA-assim/fortran_exceptions: Exception handling for .... https://github.com/LightDA-assim/fortran_exceptions.
(21) Trapping floating point exceptions in mixed C/Fortran code. https://stackoverflow.com/questions/17473304/trapping-floating-point-exceptions-in-mixed-c-fortran-code.
(22) Sample programs for exception handling - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/16.1.1?topic=exceptions-sample-programs-exception-handling.
(23) OOP - GCC Wiki. https://gcc.gnu.org/wiki/OOP.
(24) A example of Object-oriented programming implemented in Fortran.. https://github.com/yyluoyong/Fortran-OOP.
(25) Introduction – Object Oriented Programming with Fortran - GitHub Pages. https://acenet-arc.github.io/fortran_oop_as_a_second_language/introduction/index.html.
(26) undefined. http://en.wikipedia.org/wiki/Object-oriented_programming.
(27) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.
