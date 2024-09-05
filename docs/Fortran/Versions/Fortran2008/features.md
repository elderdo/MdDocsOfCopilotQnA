### question

What are the new features of Fortran 2008?

### answer

Fortran 2008 introduced several new features and enhancements:

1. **Coarrays**: A major extension for parallel computing, allowing for more efficient and straightforward parallel programming¹.
2. **Submodules**: These help in organizing and managing large codebases by allowing the separation of interface and implementation¹.
3. **Performance Enhancements**:
   - **DO CONCURRENT**: A construct for specifying loops that can be executed in parallel¹.
   - **Contiguous Attribute**: Ensures that arrays are stored contiguously in memory, improving performance¹.
4. **Data Declaration Enhancements**:
   - **Maximum Rank**: Increased to support larger arrays¹.
   - **Long Integers**: Support for larger integer types¹.
   - **Allocatable Components of Recursive Type**: Allows more flexible and dynamic data structures¹.
5. **Pointer Initialization**: Simplifies the initialization of pointers¹.
6. **Input/Output Enhancements**:
   - **Finding a Unit When Opening a File**: Improved file handling capabilities¹.
   - **G0 Edit Descriptor**: A new format descriptor for input/output operations¹.
7. **Execution Control**:
   - **BLOCK Construct**: Introduces a new block scope for variables¹.
   - **EXIT Statement**: Enhanced control over loop exits¹.
   - **STOP Code**: Allows for more informative program termination messages¹.

These features collectively enhance the language's capability for parallel computing, code organization, performance, and ease of use¹²³.

Source: Conversation with Copilot, 9/5/2024
(1) The new features of Fortran 2008. https://wg5-fortran.org/N1801-N1850/N1828.pdf.
(2) The new features of Fortran 2008. https://wg5-fortran.org/N1701-N1750/N1729.pdf.
(3) Guide to Fortran 2008 Programming in Fortran Wiki. https://fortranwiki.org/fortran/show/Guide+to+Fortran+2008+Programming.
(4) The new features of Fortran 2008 - BCS. https://fortran.bcs.org/2008/bcs08.pdf.
