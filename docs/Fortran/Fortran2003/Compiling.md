### Will GCC gfortran 8.3.0 compile Fortran 2003?

Yes, the GCC gfortran 8.3.0 compiler is capable of compiling Fortran 2003 code¹³. The gfortran compiler is part of the GNU Compiler Collection (GCC) and it supports a significant number of Fortran 2003 features³. It's worth noting that while it fully supports the Fortran 95 standard, there might be some Fortran 2003 features that are not implemented³. Therefore, it's recommended to check the specific Fortran 2003 features you intend to use against the gfortran documentation².

Source: Conversation with Copilot, 7/25/2024
(1) GFortran - GCC Wiki - GCC, the GNU Compiler Collection. https://gcc.gnu.org/wiki/GFortran.
(2) GNU Fortran - GNU Project - GCC, the GNU Compiler Collection. https://gcc.gnu.org/fortran/.
(3) The GNU Fortran Compiler: Top - GCC, the GNU Compiler Collection. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gfortran/.
(4) Debian -- Details of package gfortran-8 in buster. https://packages.debian.org/buster/gfortran-8.

### For the GNU Fortran 8.3.0 what are the compile options that indicate to use the 2003 features and allow statements to be written to any column?

To compile a Fortran program using GNU Fortran 8.3.0 with Fortran 2003 features, you can use the `-std=f2003` option¹. This option specifies the standard to which the program is expected to conform, which in this case is the Fortran 2003 standard¹.

To allow statements to be written to any column, you can use the `-ffree-form` option¹. This option tells the compiler to assume that the source file is in free format instead of the default fixed format¹. In free format, there are no restrictions on the column position of any statement¹.

Here is an example of how to use these options:

```bash
gfortran -std=f2003 -ffree-form myfile.f90
```

In this command, `myfile.f90` is the Fortran source file. The `-std=f2003` option enables Fortran 2003 features, and the `-ffree-form` option allows statements to be written to any column¹.

Source: Conversation with Copilot, 7/27/2024
(1) The GNU Fortran Compiler: Option Summary. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gfortran/Option-Summary.html.
(2) Option Summary (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Option-Summary.html.
(3) The GNU Fortran Compiler: Top - GCC, the GNU Compiler Collection. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gfortran/.
(4) GFortran in Fortran Wiki. https://fortranwiki.org/fortran/show/GFortran.
