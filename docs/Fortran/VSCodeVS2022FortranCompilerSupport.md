### Does VS Code or VS Studio support any GNU Fortran (GCC) 8.3.0 or another Open Source Fortran compiler?

Yes, both Visual Studio Code (VS Code) and Visual Studio (VS Studio) can support GNU Fortran (GCC) 8.3.0 and other open-source Fortran compilers with the help of extensions and additional configurations.

**Visual Studio Code (VS Code)**:

- VS Code supports Fortran through the Modern Fortran extension¹⁴. This extension provides features like syntax highlighting, hover support, auto-completion, and more¹⁴.
- The Fortran Language Server (fortls) can be used for advanced features like GoTo/Peek implementation, Find/Peek references, and linting¹⁴.
- You can also use the GNU Fortran compiler (gfortran) with VS Code¹⁶.

**Visual Studio (VS Studio)**:

- VS Studio doesn't natively support Fortran. However, it can be configured to work with the Intel Fortran compiler, which is part of the Intel oneAPI HPC Toolkit⁵.
- The Intel Fortran compiler is proprietary but can be used free of charge for non-commercial uses⁵.
- After installing the Intel HPC Toolkit, you need to modify the settings in VS Studio to enable Fortran⁸.

Please note that while these IDEs can support Fortran, you may need to manually configure the settings and install the necessary extensions and compilers. Also, the exact steps can vary depending on your operating system and the specific versions of the IDEs and compilers you are using. It's recommended to refer to the official documentation or community forums for the most accurate and up-to-date information.

Source: Conversation with Copilot, 7/23/2024
(1) fortran-lang/vscode-fortran-support - GitHub. https://github.com/fortran-lang/vscode-fortran-support.
(2) Fortran development in Visual Studio Code | Scientific Computing. https://www.scivision.dev/vscode-fortran/.
(3) Visual Studio 2019 for Fortran development on Windows. https://people.sissa.it/~aboiardi/post/fortran-on-windows/.
(4) Solved: FORTRAN in VS2022 - Intel Community. https://community.intel.com/t5/Intel-Fortran-Compiler/FORTRAN-in-VS2022/m-p/1345230.
(5) The GNU Fortran Compiler: Top - GCC, the GNU Compiler Collection. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gfortran/.
(6) GCC 8.3 manuals - GNU Project. https://gcc.gnu.org/onlinedocs/8.3.0/.
(7) Using the GNU Compiler Collection (GCC): Standards. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gcc/Standards.html.
(8) Using the GNU Compiler Collection (GCC): Top. https://gcc.gnu.org/onlinedocs/gcc-8.3.0/gcc/.
(9) How to use Visual Studio Community with Fortran?. https://stackoverflow.com/questions/73366185/how-to-use-visual-studio-community-with-fortran.
(10) Fortran .for file and Microsoft Visual Studio. How Can I Run It?. https://stackoverflow.com/questions/66154829/fortran-for-file-and-microsoft-visual-studio-how-can-i-run-it.
(11) Releases · fortran-lang/vscode-fortran-support · GitHub. https://github.com/fortran-lang/vscode-fortran-support/releases.
(12) Building gcc 8.3 [Makefile:955: all] Error 2 - Stack Overflow. https://stackoverflow.com/questions/62435946/building-gcc-8-3-makefile955-all-error-2.
(13) VS Code for Fortran Ep.1: Introduction & Installation of Visual Studio Code. https://www.youtube.com/watch?v=-X_71WrV63c.
(14) How to Start Coding in Fortran on Windows. https://www.youtube.com/watch?v=RrsoM6wVEWE.
(15) HOW TO: Compile & Run Fortran In VS Code using WSL?. https://www.youtube.com/watch?v=WoIWEnsVbpM.
(16) Visual Studio Code support with Modern Fortran - Intel Community. https://community.intel.com/t5/Intel-Fortran-Compiler/Visual-Studio-Code-support-with-Modern-Fortran/m-p/1406360.
(17) Having trouble running Fortran on Visual Studio Code. https://stackoverflow.com/questions/62376439/having-trouble-running-fortran-on-visual-studio-code.
(18) undefined. https://gcc.gnu.org/wiki/InstallingGCC.
(19) undefined. https://icl.utk.edu/lapack-for-windows/lapack/.
