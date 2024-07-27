### Is there a recommended file structure for Fortran 2003? Should classes be in their own files? Are there any naming conventions that have been adopted? How long can a variable, method, or constant name be and what characters can be used for the names?

**Fortran 2003 File Structure and Naming Conventions**

1. **File Structure**: Fortran 2003 doesn't enforce a specific file structure, but it's common to organize related procedures and data into modules¹². Each module should be in a separate file⁴. The `.f90` extension is commonly used for free-form source files⁷. If a module becomes too large, it can be split into multiple files using `include` or `submodules`, but this is not a standard practice¹².

2. **Classes**: In Fortran, the equivalent of classes are user-defined types. These can be defined in their own modules and thus in their own files¹².

3. **Naming Conventions**: Fortran names (for variables, methods, constants) must begin with a letter and can include alphanumeric characters (letters, numbers) and underscores¹¹. They are case-insensitive⁹. The maximum length is 63 characters¹⁶¹⁸. It's common to use lowercase for all Fortran constructs and to keep names to one or two syllables. If more syllables are required, underscores can be used to clarify[^10^].

4. **Variable, Method, Constant Names**: The names can be up to 63 characters long¹⁶¹⁸. They must begin with a letter and can include alphanumeric characters (letters, numbers) and underscores¹¹. They are case-insensitive⁹.

Please note that these are general guidelines and actual usage may vary depending on the project and the development team's preferences. For detailed information, you should refer to the Fortran 2003 standard or a comprehensive Fortran 2003 guide⁵.

Source: Conversation with Copilot, 7/25/2024
(1) Single fortran module in multiple files - Stack Overflow. https://stackoverflow.com/questions/11967053/single-fortran-module-in-multiple-files.
(2) class - Breaking up a Module into Multiple Files in Fortran - Stack .... https://stackoverflow.com/questions/48591767/breaking-up-a-module-into-multiple-files-in-fortran.
(3) Correct use of modules, subroutines and functions in Fortran. https://stackoverflow.com/questions/8412834/correct-use-of-modules-subroutines-and-functions-in-fortran.
(4) Correct suffix for Fortran 2003 source file - Intel Fortran compiler. https://stackoverflow.com/questions/20269076/correct-suffix-for-fortran-2003-source-file-intel-fortran-compiler.
(5) Module name requirements — Fortran Package Manager. https://fpm.fortran-lang.org/registry/naming.html.
(6) Naming conventions (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Naming-conventions.html.
(7) Routine name maximum length in Fortran 90 - Stack Overflow. https://stackoverflow.com/questions/3967247/routine-name-maximum-length-in-fortran-90.
(8) Fortran maximum name and line lengths | Scientific Computing. https://www.scivision.dev/fortran-maximum-name-length/.
(9) Fortran Style Guide — Fortran Programming Language. https://fortran-lang.org/en/learn/best_practices/style_guide/.
(10) EE EGuide to Fortran 2003 Programming - Springer. https://link.springer.com/content/pdf/10.1007/978-1-84882-543-7.pdf.
(11) Fortran Tools: FSPLIT and FPR - Universiteit Twente. https://ftp.itc.nl/pub/sardon/Fortran/CVF_STD_66A%20%28D%29/INFO/DF/DOC/CVF_HTML/VISUALFORTRAN/PG/PG4FUTIL.HTM.
(12) A Fortran 2003 introduction by examples - Universitetet i Oslo. https://www.uio.no/studier/emner/matnat/geofag/GEO4060/v12/undervisningsmateriale/fortran.pdf.
(13) File Input/Output — Fortran Programming Language. https://fortran-lang.org/en/learn/best_practices/file_io/.
(14) Fortran Coding Conventions | CLASSIC. https://cccma.gitlab.io/classic_pages/info/conventions/.
(15) fortran - Case Insensitive Naming Conventions - Stack Overflow. https://stackoverflow.com/questions/47143637/case-insensitive-naming-conventions.
(16) fortran - passing a string as an argument when the dummy has specified .... https://stackoverflow.com/questions/4780069/passing-a-string-as-an-argument-when-the-dummy-has-specified-length.
(17) fortran - Character declaration forms - Stack Overflow. https://stackoverflow.com/questions/66531967/character-declaration-forms.
(18) Character Handling in Fortran. https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html.
(19) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.
