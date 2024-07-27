### For Fortran 2003 can you show examples of derived types and associated functions and subroutines that mimic lambda expressions?

Sure, here's an example of how you can use derived types and associated functions to mimic lambda expressions in Fortran 2003.

In this example, we'll create a derived type `function_object` that contains a procedure pointer. This procedure pointer can be associated with different functions, allowing us to mimic the behavior of lambda expressions.

```fortran
module function_mod
  implicit none
  abstract interface
    function func_type(x) result(y)
      real, intent(in) :: x
      real :: y
    end function func_type
  end interface
  type :: function_object
    procedure(func_type), pointer, nopass :: func
  end type function_object
end module function_mod

program main
  use function_mod
  implicit none
  type(function_object) :: f_obj
  real :: x = 2.0, y

  ! Associate the function_object with the square function
  f_obj%func => square
  y = f_obj%func(x)
  print*, 'Square of ', x, ' is ', y

  ! Associate the function_object with the cube function
  f_obj%func => cube
  y = f_obj%func(x)
  print*, 'Cube of ', x, ' is ', y
end program main

function square(x) result(y)
  real, intent(in) :: x
  real :: y
  y = x**2
end function square

function cube(x) result(y)
  real, intent(in) :: x
  real :: y
  y = x**3
end function cube


In this code, we first define a module `function_mod` that contains an abstract interface `func_type` and a derived type `function_object`. The derived type contains a procedure pointer `func` that is associated with the abstract interface.

In the main program, we create an object `f_obj` of type `function_object`. We can then associate the `func` procedure pointer with different functions (in this case, `square` and `cube`). This allows us to change the behavior of `f_obj%func` at runtime, similar to how a lambda expression works.

Please note that this is a simple example and real-world usage may be more complex. Always ensure that your code remains portable and maintainable when adding these features. Also, remember that not all Fortran compilers may support these features. Always check the documentation of your specific compiler for details.
```
