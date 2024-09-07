### What is the best way to add debugging features to fortran 2003 such displaying data as it executes and send the output to stdout or some other file?

There are several ways to add debugging features to Fortran 2003:

1. **Compiler Flags**: When compiling your Fortran code, you can use certain flags that aid in debugging. For example, the `-g` flag includes debugging information in the executable⁴. Other flags like `-Wall`, `-Wextra`, `-fcheck=bounds`, and `-pedantic-errors` can help catch potential issues³.

2. **Floating Point Exceptions**: The `-ffpe-trap` option can be used to specify a list of floating point exception traps to enable. If a floating point exception occurs and the trap for that exception is enabled, a `SIGFPE` signal will be sent and the program being aborted, producing a core file useful for debugging¹².

3. **Internal Variables**: The `-fdebug-aux-vars` option renames internal variables created by the gfortran front end and makes them accessible to a debugger¹².

4. **Parse Tree Output**: Options like `-fdump-fortran-original` and `-fdump-fortran-optimized` output the internal parse tree after translating the source program into internal representation, which can be useful for debugging¹.

To display data as it executes and send the output to `stdout` or some other file, you can use `print` or `write` statements at appropriate places in your code. For example:

```fortran
print *, 'This is a debug message'
write(unit=*, fmt=*) 'This is another debug message'
```

In the above code, `*` denotes `stdout`. If you want to write to a file, you can open a file with a certain unit number and use that unit number in the `write` statement.

Remember, the best way to add debugging features depends on your specific needs and the nature of the bugs you are trying to find. It's often helpful to use a combination of these techniques for effective debugging.

Source: Conversation with Copilot, 7/25/2024
(1) Debugging a Fortran program - IBM. https://www.ibm.com/docs/en/xl-fortran-linux/15.1.3?topic=debugging-fortran-program.
(2) Fortran debugging — AM129 documentation. https://people.ucsc.edu/~imay1/_static/AM129_F23/chapters/chapt02/ch02_fortran_debugging.html.
(3) Options for debugging your program or GNU Fortran. https://gcc.gnu.org/onlinedocs/gfortran/gnu-fortran-command-options/options-for-debugging-your-program-or-gnu-fortran.html.
(4) Debugging Options (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/Debugging-Options.html.
