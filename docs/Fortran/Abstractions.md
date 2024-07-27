### Are Fortran 2003 interfaces always abstract? Does that language also have abstract classes that are inherited by other classes ?

In Fortran 2003, interfaces can be abstract¹. An abstract interface allows you to specify procedure characteristics and dummy argument names without declaring a procedure with those characteristics¹. You can use an abstract interface to declare interfaces for procedures and deferred bindings¹.

As for classes, Fortran 2003 introduced the concept of type extension, which is similar to inheritance in object-oriented programming⁵. A derived type can extend a base type, inheriting all of its components⁵. This feature allows Fortran programmers to take advantage of code reusability through an implied inheritance link in which leaf objects, known as children, reuse components from their parent and ancestor objects⁵.

Here's an example:

```fortran
type shape
  integer ::  color
  logical ::  filled
  integer ::  x
  integer ::  y
end type shape

type, extends(shape)  ::  rectangle
  integer ::  length
  integer ::  width
end type rectangle

type, extends(rectangle)  ::  square
end type square
```

In this example, `square` is a type that extends `rectangle`, which in turn extends `shape`. Therefore, `square` inherits components from both `rectangle` and `shape`⁵.

Fortran 2003 also introduced abstract types and deferred bindings⁸. An extension of an abstract type does not have to be abstract if that extension has no deferred bindings⁸. If a type definition contains or inherits a deferred binding, the `ABSTRACT` attribute must appear⁸. If `ABSTRACT` appears, the type must be extensible⁸.

Source: Conversation with Copilot, 7/25/2024
(1) Abstract interface (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xffbg/121.141?topic=procedures-abstract-interface-fortran-2003.
(2) Object-Oriented Programming in Fortran 2003 Part 1: Code Reusability. https://gist.github.com/n-s-k/522f2669979ed6d0582b8e80cf6c95fd.
(3) Abstract types and deferred bindings (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xffbg/121.141?topic=types-abstract-deferred-bindings-fortran-2003.
(4) ABSTRACT INTERFACE(Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/15.1.5?topic=attributes-abstract-interfacefortran-2003.
(5) ABSTRACT INTERFACE (Fortran 2003) - IBM. https://www.ibm.com/docs/ja/openxl-fortran-aix/17.1.1?topic=attributes-abstract-interfacefortran-2003.
(6) inheritance - Understanding Fortran extends types and override - Stack .... https://stackoverflow.com/questions/25410476/understanding-fortran-extends-types-and-override.
(7) Derived Types — Fortran Programming Language. https://fortran-lang.org/en/learn/quickstart/derived_types/.
(8) Emulating Multiple Inheritance in Fortran 2003/2008 - OSTI.GOV. https://www.osti.gov/pages/biblio/1197680.
(9) github.com. https://github.com/zodiacfireworks/course--fortran/tree/5d70133879a067f62697ecbc6095e10258477bb9/materials%2Flesson006-007%2FOOP_F2003_Part_1.md.
