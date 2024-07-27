### Does Fortran 2023 have generics?

Yes, Fortran 2023 does include features that support generic programming¹. For example, it provides syntax for declaring and accessing arrays where the syntax does not depend on the rank, but instead on the size of one or more vectors³. This size is required to be constant, so this is compile-time, not run-time, genericity³.

Additionally, Fortran 2023 has introduced features such as `typeof()` and `classof()`, and a new syntax for using integer arrays for specifying subscripts, ranks, and bounds of an array¹. These features can be helpful in enabling generic programming¹.

However, it's important to note that while these features can facilitate generic programming, they may not provide the same level of functionality as the generics feature in some other languages. Let me know if you need further information!

Source: Conversation with Copilot, 7/25/2024
(1) Fortran 2023 has been Published! - Doctor Fortran. https://stevelionel.com/drfortran/2023/11/23/fortran-2023-has-been-published/.
(2) Generic programming | Modern Fortran Explained: Incorporating Fortran .... https://academic.oup.com/book/56095/chapter/442723297.
(3) The new features of Fortran 2023. https://wg5-fortran.org/N2201-N2250/N2212.pdf.
(4) An Overview of The Fortran Standard: Fortran 2023 and Beyond. https://www.olcf.ornl.gov/wp-content/uploads/2024-04_OLCFUserCall_FortranStandard.pdf.
(5) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.
