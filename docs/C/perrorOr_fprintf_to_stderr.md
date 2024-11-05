### question
What is better to use fprintf(stderr,....) or perror?

### answer
Both fprintf(stderr, ...) and perror(...) are used for error reporting in C, but they serve slightly different purposes and are used in different contexts. Here's a comparison to help you decide which one to use:
fprintf(stderr, ...)
•	Flexibility: Allows you to format the error message in any way you like.
•	Custom Messages: You can provide detailed and specific error messages.
•	Usage: Typically used when you want to provide more context or a custom error message.


### Example:
```c
if (ptr == NULL) {
    fprintf(stderr, "Error: ptr is NULL in function %s at line %d\n", __func__, __LINE__);
}
```

perror(...)
•	Simplicity: Automatically appends a description of the last error encountered (based on errno).
•	Standardized Messages: Provides a standard error message based on the current value of errno.
•	Usage: Best used when you want to report system call errors or standard library function errors.
### Example:
```c
if (file == NULL) {
    perror("Failed to open file");
}
```

### When to Use Which:
•	Use fprintf(stderr, ...) when you need to provide a custom error message or additional context that perror does not provide.
•	Use perror(...) when you want to report an error related to a system call or standard library function and you want to include the standard error message associated with errno.
### Combined Usage:
In some cases, you might want to use both to provide a custom message along with the standard error description.
### Example:
```c
if (file == NULL) {
    fprintf(stderr, "Error: Failed to open file '%s'\n", filename);
    perror("Reason");
}
```

This way, you get the best of both worlds: a custom message and the standard error description.












