### Do you have any good examples of reading command line arguments using Fortran 2003 and its gcc 8.3.0 using the following and looking similar to unix like switches with or without values:

1. GET_COMMAND — Get the entire command line,
2. GET_COMMAND_ARGUMENT — Get command line arguments,
3. COMMAND_ARGUMENT_COUNT — Get number of command line arguments

Sure, here's an example of a Fortran 2003 program that uses `GET_COMMAND_ARGUMENT` and `COMMAND_ARGUMENT_COUNT` to read command line arguments:

```fortran
PROGRAM CommandLineArgs
    INTEGER :: i, num_args
    CHARACTER(len=32) :: arg

    num_args = COMMAND_ARGUMENT_COUNT()

    DO i = 1, num_args
        CALL GET_COMMAND_ARGUMENT(i, arg)
        WRITE (*,*) 'Argument ', i, ': ', TRIM(arg)
    END DO
END PROGRAM CommandLineArgs
```

In this program, `COMMAND_ARGUMENT_COUNT` is used to get the number of command line arguments⁴. Then, a loop is used to iterate over each argument. `GET_COMMAND_ARGUMENT` is called to retrieve each argument, which is then printed to the console¹².

Please note that this program does not handle Unix-like switches. If you need to parse Unix-like switches (e.g., `-a value` or `--arg=value`), you would need to implement additional logic to split the argument string around the `=` character and identify the switch name and value. This might involve using intrinsic string handling functions like `INDEX`, `TRIM`, etc. Also, please remember to compile this program using a Fortran 2003 compliant compiler like gfortran 8.3.0.

Let me know if you need further assistance!

Source: Conversation with Copilot, 7/25/2024
(1) COMMAND_ARGUMENT_COUNT — Get number of command line arguments. https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-procedures/commandargumentcount.html.
(2) GET_COMMAND_ARGUMENT (The GNU Fortran Compiler). https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND_005fARGUMENT.html.
(3) How to use command line arguments in Fortran? - Stack Overflow. https://stackoverflow.com/questions/13843772/how-to-use-command-line-arguments-in-fortran.
(4) GET_COMMAND_ARGUMENT — Get command line arguments - Using GNU Fortran .... https://gcc.gnu.org/onlinedocs/gfortran/intrinsic-procedures/getcommandargument.html.
