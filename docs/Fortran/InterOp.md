### Can Fortran 2003 be used by C#, Java or Python via an interface?

Yes, Fortran 2003 can be interfaced with C#, Java, and Python:

1. **C#**: Fortran can be called from C# using P/Invoke¹¹. There are also resources available that provide a detailed guide on Fortran interoperability with .NET[^10^].

2. **Java**: One way to use Fortran code from Java is to provide a C interface in Fortran and then use Java's native interface². There are also tools like gtk-fortran, which is a partial GTK / Fortran binding 100% written in Fortran, thanks to the ISO_C_BINDING module for interoperability between C and Fortran, which is a part of the Fortran 2003 standard¹.

3. **Python**: The F2PY (Fortran to Python) utility is part of NumPy and provides a connection between Python and Fortran⁵. It creates Python C/API extension modules that make it possible to call Fortran 77/90/95 external subroutines and Fortran 90/95 module subroutines as well as C functions⁵. Another approach is to use the `iso_c_binding` feature introduced in the Fortran 2003 standard to improve interoperability with C, and then use Python libraries like `ctypes` or `Cython`⁸.

Please note that while these methods can facilitate the use of Fortran 2003 with these languages, the exact implementation may depend on the specific requirements of your project. Let me know if you need more detailed information on any of these methods!

Source: Conversation with Copilot, 7/25/2024
(1) CSharp Interoperability in Fortran Wiki. https://fortranwiki.org/fortran/show/CSharp+Interoperability.
(2) Introduction to FORTRAN Interoperability with .NET. https://www.codeproject.com/articles/1065197/introduction-to-fortran-interoperability-with-net.
(3) Putting a Java Interface on your C, C++, or Fortran Code. https://www.math.ucla.edu/~anderson/JAVAclass/JavaInterface/JavaInterface.html.
(4) user interface - How do you integrate fortran code with a java gui .... https://stackoverflow.com/questions/5562463/how-do-you-integrate-fortran-code-with-a-java-gui.
(5) F2PY user guide and reference manual — NumPy v2.0 Manual. https://numpy.org/doc/stable/f2py/.
(6) Interfacing Fortran code - FutureLearn. https://www.futurelearn.com/info/courses/python-in-hpc/0/steps/65132.
(7) Interoperability in Fortran Wiki. https://fortranwiki.org/fortran/show/Interoperability.
(8) Abstract interface (Fortran 2003) - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/16.1.0?topic=procedures-abstract-interface-fortran-2003.
(9) F2PY - Calling Fortran routines from Python - NumFys. https://www.numfys.net/howto/F2PY/.
(10) How to Call Fortran from Python - MatecDev. https://www.matecdev.com/posts/fortran-in-python.html.
(11) Can C# style interfaces be used in Fortran, or any other ideas for .... https://stackoverflow.com/questions/25518362/can-c-sharp-style-interfaces-be-used-in-fortran-or-any-other-ideas-for-common-f.
(12) A Bridge From Fortran to C# - CodeProject. https://www.codeproject.com/Articles/1172025/A-Bridge-From-Fortran-to-Csharp.
(13) undefined. https://github.com/vmagnin/gtk-fortran/wiki.
