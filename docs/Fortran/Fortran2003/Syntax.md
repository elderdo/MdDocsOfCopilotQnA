### For Fortran 2003 can you explain the syntax of this code:

```fortran
 function func_type(x) result(y)
      real, intent(in) :: x
      real :: y
    end function func_type
```

### What is significance of the ::?

In Fortran, the `::` symbol is used in variable declarations to separate the variable attributes from the variable names.

In the code snippet you provided:

```fortran
function func_type(x) result(y)
  real, intent(in) :: x
  real :: y
end function func_type
```

- `real, intent(in) :: x` declares `x` as a `real` type variable with an `intent(in)` attribute. The `intent(in)` attribute means that `x` is an input to the function `func_type` and its value cannot be changed within the function.
- `real :: y` declares `y` as a `real` type variable. The `result(y)` in the function declaration indicates that `y` is the result of the function.

So, the `::` symbol is significant as it separates the attributes of a variable (like `real, intent(in)`) from the variable name (`x` or `y`). This is a common syntax in Fortran for variable declaration. It helps in enhancing the readability of the code and allows for the specification of multiple variables with the same attributes in a single line. For example, `real :: a, b, c` declares `a`, `b`, and `c` as `real` type variables.
