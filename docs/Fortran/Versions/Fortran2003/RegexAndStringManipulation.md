### For Fortran 2003 are there any regex libraries that can be used to parse text or libraries for other types of string parsing and manipulation? Is there any way to do concatentation?

Yes, Fortran 2003 does support libraries for regular expressions and string manipulation, and it also provides a way to concatenate strings.

**Regular Expression Libraries:**

- `fortran-regex` is a pure Fortran implementation of regular expressions².
- `regex-fortran` and `M_match` are other Fortran-based basic RegEx packages¹.

**String Manipulation Libraries:**

- `coretran` provides routines and object-oriented classes for handling scientific data and concepts, including string operations¹.
- `M_strings` offers modules for processing strings, including parsing, tokenizing, changing case, substituting new strings for substrings, and other string manipulations¹.
- `strff` is a library of convenient string functions¹.

**String Concatenation:**
In Fortran, you can concatenate two strings using the `//` operator. Here's an example:

```fortran
character(len=12) :: str1 = "Hello"
character(len=12) :: str2 = "World"
character(len=24) :: str3
str3 = str1 // " " // str2  ! str3 now contains "Hello World"
```

In this example, `str1` and `str2` are concatenated with a space in between, and the result is stored in `str3`⁵⁶⁷⁸. Note that the length of `str3` must be long enough to hold the result of the concatenation⁵⁶⁷⁸.

Source: Conversation with Copilot, 7/27/2024
(1) GitHub - jeyemhex/fortran-regex: A pure Fortran implementation of .... https://github.com/jeyemhex/fortran-regex.
(2) Libraries in Fortran Wiki. https://fortranwiki.org/fortran/show/Libraries.
(3) printing - How to combine two strings in Fortran - Stack Overflow. https://stackoverflow.com/questions/34622995/how-to-combine-two-strings-in-fortran.
(4) Character Handling in Fortran. https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html.
(5) 11.6: Character Operators - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/11%3A_Characters_and_Strings/11.06%3A_Character_Operators.
(6) 11: Characters and Strings - Engineering LibreTexts. https://eng.libretexts.org/Bookshelves/Computer_Science/Programming_Languages/Introduction_to_Programming_using_Fortran_95_2003_2008_%28Jorgensen%29/11%3A_Characters_and_Strings.
(7) Fortran port of the tiny-regex-c library - GitHub. https://github.com/perazz/fortran-regex.
(8) Fortran libraries for general programming tasks. https://fortran-lang.org/en/packages/libraries/.
(9) Libraries for manipulating characters and strings — Fortran Programming .... https://fortran-lang.org/en/packages/strings/.

### For Fortran 2003 are there examples using one or more of the regex or string manipulation libraries that are available? How are these libraries packaged and how are they linked into the application? Are there any package managers for these libraries like pip or maven with version and dependency information?

**Examples of Using Regex and String Manipulation Libraries**

1. **fortran-regex**: Here is an example of using the `fortran-regex` library⁵:

```fortran
program re_example
  use regex
  implicit none
  character(len=3), allocatable :: list(:)
  integer :: i

  ! String matching
  print*, re_match("^Hello .* !", "Hello, world!")  ! Returns T
  print*, re_match("^Hello \\s+ world", "Hello, world!")  ! Returns F, no comma
  print*, trim(re_match_str("^Hello", "Hello, world!"))  ! Returns "Hello"

  ! String splitting
  call re_split("\\s* , \\s*", "12, 16, 2, 9.6", list)  ! Returns ["12", "16", "2", "9.6"]
  do i = 1, size(list)
    print*, "'"// list(i) //"'"
  end do
  deallocate(list)

  ! String replacing
  print*, trim(re_replace("Hello", "Goodbye", "Hello, world!"))  ! Returns "Goodbye, world!"
end program re_example
```

2. **String Manipulation**: Here is an example of string manipulation in Fortran²:

```fortran
program demo_len
  implicit none
  character(len=40) :: string
  character(len=:), allocatable :: astring
  character(len=:), allocatable :: many_strings(:)
  integer :: ii

  ! BASIC USAGE
  ii = len(string)
  write(*,*) 'length =', ii

  ! ALLOCATABLE VARIABLE LENGTH CAN CHANGE
  astring = 'How long is this allocatable string?'
  write(*,*) astring, ' LEN=', len(astring)
  astring = 'New allocatable string'
  write(*,*) astring, ' LEN=', len(astring)

  ! THE STRING LENGTH WILL BE CONSTANT FOR A FIXED-LENGTH VARIABLE
  string = 'How long is this fixed string?'
  write(*,*) string, ' LEN=', len(string)
  string = 'New fixed string'
  write(*,*) string, ' LEN=', len(string)

  ! ALL STRINGS IN AN ARRAY ARE THE SAME LENGTH
  many_strings = [character(len=7) :: 'Tom', 'Dick', 'Harry']
  write(*,*) 'length of ALL elements of array=', len(many_strings)
end program demo_len
```

**Packaging and Linking Libraries**

Libraries in Fortran can be packaged as static or dynamic libraries⁸. Static libraries become an integral part of the executable program, while dynamic libraries remain outside the executable program and can be replaced without rebuilding the entire program⁸.

Here's an example of how to link libraries using `gfortran`⁹:

```bash
gfortran filename -o outputname -I/include_libraries_here -L/link_libraries_here
```

In this command, `-I` is used to specify the path to the header files, `-L` is used to specify the path to the library files⁹.

**Package Managers for Fortran Libraries**

The Fortran Package Manager (fpm) is a package manager and build system for Fortran¹²¹³. It aims to improve the user experience of Fortran programmers by making it easier to build your Fortran program or library, run the executables, tests, and examples, and distribute it as a dependency to other Fortran projects¹⁴.

Another option is the SF Package Manager provided by Simply Fortran, which provides pre-configured libraries and development tools for Simply Fortran¹⁵.

Source: Conversation with Copilot, 7/27/2024
(1) GitHub - jeyemhex/fortran-regex: A pure Fortran implementation of .... https://github.com/jeyemhex/fortran-regex.
(2) Basic procedures for manipulating character variables — Fortran .... https://fortran-lang.org/en/learn/intrinsics/character/.
(3) Managing libraries (static and dynamic libraries) — Fortran Programming .... https://fortran-lang.org/en/learn/building_programs/managing_libraries/.
(4) compilation - Linking library syntax with gfortran - Stack Overflow. https://stackoverflow.com/questions/44591695/linking-library-syntax-with-gfortran.
(5) Installing fpm — Fortran Package Manager. https://fpm.fortran-lang.org/install/index.html.
(6) Fortran Package Manager — Fortran Package Manager. https://fpm.fortran-lang.org/.
(7) GitHub - fortran-lang/fpm: Fortran Package Manager (fpm). https://github.com/fortran-lang/fpm.
(8) Simply Fortran from Approximatrix. https://simplyfortran.com/features/packman.
(9) Character Handling in Fortran. https://fortranwiki.org/fortran/files/character_handling_in_Fortran.html.
(10) String manipulations – IO-Fortran-Library - GitHub Pages. https://acbbullock.github.io/IO-Fortran-Library/doc/page/Examples/manipulations.html.
(11) Libraries in Fortran Wiki. https://fortranwiki.org/fortran/show/Libraries.
(12) regex_module in Fortran Wiki. https://fortranwiki.org/fortran/show/regex_module.
(13) Fortran port of the tiny-regex-c library - GitHub. https://github.com/perazz/fortran-regex.
(14) How to add static library file(lib) to fortran project in VS2010 .... https://community.intel.com/t5/Intel-Fortran-Compiler/How-to-add-static-library-file-lib-to-fortran-project-in-VS2010/m-p/922527.
(15) Compiling and linking modules in Fortran Wiki. https://fortranwiki.org/fortran/show/Compiling+and+linking+modules.
(16) undefined. http://www.yolinux.com/TUTORIALS/LinuxTutorialMixingFortranAndC.html.
