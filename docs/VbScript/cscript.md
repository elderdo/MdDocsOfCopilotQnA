### question

For VbScript what is the difference between WScript.StdErr and FileSystemObject's GetStandardStream(2) method? So, which one is used just for cscript.exe?

### answer

`WScript.StdErr` is specifically used with `cscript.exe` (command-line script host). It allows you to write error messages directly to the standard error stream.

On the other hand, `FileSystemObject.GetStandardStream(2)` can also be used with `cscript.exe`, but it provides more flexibility by returning a `TextStream` object for the standard error stream. However, it is not supported in `wscript.exe` (Windows-based script host).

In summary:

- **WScript.StdErr**: Used just for `cscript.exe`.
- **FileSystemObject.GetStandardStream(2)**: Primarily for `cscript.exe`, but not supported in `wscript.exe`.
