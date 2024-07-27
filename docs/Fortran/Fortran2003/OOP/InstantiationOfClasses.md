### For fortran 2003 how do you make an instance of a class? Can you show an example?

In Fortran 2003, you can create an instance of a class (a derived type) by declaring a variable of that type. Here's an example using a hypothetical `Circle` class:

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
        area = this%area()
        print *, 'Circle: r = ', this%radius, ' area = ', area
    end subroutine circle_print
end module class_Circle

program circle_test
    use class_Circle
    implicit none
    type(Circle) :: c ! Declare a variable of type Circle.

    c = Circle(1.5) ! Use the implicit constructor, radius = 1.5.
    call c%print() ! Call a class subroutine
end program circle_test
```

In this example, `c` is an instance of the `Circle` class¹. The `Circle` class has two methods, `area` and `print`, which are bound to the type definition. The `circle_area` function calculates the area of the circle, and the `circle_print` subroutine prints the radius and area of the circle¹. The `circle_test` program creates an instance of the `Circle` class with a radius of 1.5 and then calls the `print` method of the `Circle` class¹.

Source: Conversation with Copilot, 7/25/2024
(1) Object-oriented programming in Fortran Wiki. https://fortranwiki.org/fortran/show/Object-oriented+programming.
(2) Object-Oriented Features in Fortran 2003 - msu.ru. http://www.elch.chem.msu.ru/tch/group/FortranBooks/Fortran_Object_Oriented.pdf.
(3) Tutorial OOP (III): Constructors and Destructors. https://dannyvanpoucke.be/oop-fortran-tut4-en/.
(4) Object-Oriented Programming in Fortran 2003 Part 1: Code Reusability. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(5) undefined. http://en.wikipedia.org/wiki/Object-oriented_programming.
(6) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.
