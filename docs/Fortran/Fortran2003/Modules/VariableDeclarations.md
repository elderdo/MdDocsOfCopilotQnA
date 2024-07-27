### For a Fortran 2003 can you explain this variable declaration:

integer, parameter :: N = 5000 !3E5

Are there any other unique features of variable declaration and initialization?

The variable declaration `integer, parameter :: N = 5000` in Fortran 2003 is defining a constant integer named `N` with a value of `5000`. Here, `integer` is the data type, `parameter` indicates that `N` is a constant (its value cannot be changed after initialization), and `::` is used to separate the variable type and attribute from the variable name and its value².

The `!3E5` following the declaration is a comment. In Fortran, anything following an exclamation mark `!` on a line is considered a comment and is ignored by the compiler.

As for other unique features of variable declaration and initialization in Fortran:

1. **Implicit Typing**: Fortran allows implicit typing, where the type of a variable is determined by the first letter of its name. However, this is considered bad practice in modern programming, and it's recommended to always use the `implicit none` statement at the beginning of each program and procedure to require explicit declaration of all variables².

2. **Initialization implies SAVE**: In modern Fortran, initialization at the time of declaration implies the `SAVE` attribute. This means the variable retains its value between procedure calls. However, this is not allowed in `PURE` procedures¹.

3. **Derived Types**: Fortran allows for the creation of user-defined types, known as derived types. Variables of a derived type can be declared and their members can be accessed using the percentage symbol `%`⁴.

4. **Initialization at Compile Time**: When variables are initialized at the time of declaration, it happens only once, at compile time. If this takes place in a subprogram, it will not happen again upon repeated invocations³.

Remember, good practice is to initialize your variables separately from their declaration².

Source: Conversation with Copilot, 7/27/2024
(1) Variables — Fortran Programming Language. https://fortran-lang.org/en/learn/quickstart/variables/.
(2) Declaration and initialization of local variables in Fortran PURE .... https://stackoverflow.com/questions/21418576/declaration-and-initialization-of-local-variables-in-fortran-pure-functions.
(3) Derived Types — Fortran Programming Language. https://fortran-lang.org/en/learn/quickstart/derived_types/.
(4) Variable Declarations | RC Learning Portal. https://learning.rc.virginia.edu/courses/fortran-introduction/declarations/.

### For Fortran 2003 can variables in Modules be initialized upon declaration? Is this bad practice, if yes why is it a bad practice? What are PURE procedures?

Yes, in Fortran 2003, variables in modules can be initialized upon declaration¹. However, it's important to note that initialization at the time of declaration implies the `SAVE` attribute, meaning the variable retains its value between procedure calls¹. This can lead to unexpected behavior if not handled carefully.

As for whether it's a bad practice, it depends on the context. If the module variable is intended to be a constant or if its initial value is important and unchanging, then initializing upon declaration is fine. However, if the variable is meant to be reset every time a procedure is called, then initializing at declaration could lead to bugs, as the initialization only happens once at compile time¹.

Now, regarding `PURE` procedures, they are procedures in Fortran that are free of side effects⁵⁶. This means they don't modify any global data or have any I/O operations⁸. They are particularly useful in `FORALL` statements and constructs, which require that all referenced procedures be free of side effects⁵. The use of `PURE` procedures can help make the code easier to understand and debug, as you can be sure they don't change any state outside of their own scope⁵. However, `PURE` procedures have certain restrictions, such as not being able to perform I/O operations or modify any variables with the `INTENT(IN)` attribute⁶.

Source: Conversation with Copilot, 7/27/2024
(1) fortran90 - Variable Initialization in Fortran - Stack Overflow. https://stackoverflow.com/questions/27286364/variable-initialization-in-fortran.
(2) Proper use of the PURE keyword Fortran - Stack Overflow. https://stackoverflow.com/questions/30264729/proper-use-of-the-pure-keyword-fortran.
(3) Pure procedures (Fortran 95) - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/13.1.0?topic=procedures-pure-fortran-95.
(4) Attribute for "pure" procedures that do I/O - Fortran Discourse. https://fortran-lang.discourse.group/t/attribute-for-pure-procedures-that-do-i-o/2694.
(5) Old-style variable initialization (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Old-style-variable-initialization.html.
(6) make sure Fortran common variables are initialized to zero in a third .... https://stackoverflow.com/questions/49618317/make-sure-fortran-common-variables-are-initialized-to-zero-in-a-third-party-libr.
(7) Derived Types — Fortran Programming Language. https://fortran-lang.org/en/learn/quickstart/derived_types/.
(8) Pure procedures - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=procedures-pure.
(9) Getty Images. https://www.gettyimages.com/detail/illustration/fortran-programming-language-royalty-free-illustration/1144200223.

### For Fortran 2003 what are FORALL statements and what is an INTENT attribute?

In Fortran 2003:

1. **FORALL Statements**: The `FORALL` statement is used to perform assignments to groups of subobjects, especially array elements⁶. Unlike the `WHERE` statement, assignment can be performed on an elemental level rather than on an array level⁶. The `FORALL` statement is executed by first evaluating all bounds and stride expressions in the triplet specifications, giving a set of values for each subscript name. The `FORALL` assignment statement is then executed for all combinations of subscript name values for which the mask expression is true⁹.

2. **INTENT Attribute**: The `INTENT` attribute of a dummy argument in a subroutine or function declares its intended use³. The syntax is either one of `intent(IN)`, `intent(OUT)`, or `intent(INOUT)`³. The `intent(IN)` specifies that the (non-pointer) dummy argument may never be defined or become undefined throughout the function or its initialization³. If a pointer dummy argument has the attribute `intent(IN)`, this applies to its association³. The `intent(OUT)` and `intent(INOUT)` attributes indicate that the argument can be modified within the procedure¹².

Source: Conversation with Copilot, 7/27/2024
(1) Examples of the FORALL Statement - Rice University. http://hpff.rice.edu/versions/hpf1/hpf-v10/subsection2_5_1_3.html.
(2) FORALL - Canadian Institute for Theoretical Astrophysics. https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/lref_for/source_files/rfforall.htm.
(3) Fortran Tutorial => The Intent of Dummy Arguments. https://riptutorial.com/fortran/example/10042/the-intent-of-dummy-arguments.
(4) arguments - What is the explicit difference between the fortran intents .... https://stackoverflow.com/questions/1011604/what-is-the-explicit-difference-between-the-fortran-intents-in-out-inout.
(5) Fortran intent (inout) versus omitting intent - Stack Overflow. https://stackoverflow.com/questions/2880537/fortran-intentinout-versus-omitting-intent.
(6) Fortran 2003 and Beyond - CUG. https://cug.org/5-publications/proceedings_attendee_lists/2005CD/S05_Proceedings/pages/Authors/Long-0517-1100/Long-0517-1100_paper.pdf.
(7) fortran - Changing intent(in) variables through a pointer - Stack Overflow. https://stackoverflow.com/questions/69822090/changing-intentin-variables-through-a-pointer.
(8) Interpreting the FORALL statement - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/13.1.0?topic=95-interpreting-forall-statement.
(9) FORALL - IBM. https://www.ibm.com/docs/en/xl-fortran-aix/16.1.0?topic=attributes-forall.
(10) FORALL (Fortran 95) - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/13.1.0?topic=attributes-forall-fortran-95.
