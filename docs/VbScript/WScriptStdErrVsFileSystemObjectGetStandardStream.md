### question

For VbScript what is the difference between WScript.StdErr and FileSystemObject's GetStandardStream(2) method?

### answer

In VBScript, both `WScript.StdErr` and `FileSystemObject.GetStandardStream(2)` are used to write to the standard error stream, but they have some differences:

1. **WScript.StdErr**:

   - This is a property of the `WScript` object.
   - It is used to write error messages when running scripts with `cscript.exe` (command-line script host).
   - Example usage:
     ```vbscript
     WScript.StdErr.WriteLine "This is an error message."
     ```

2. **FileSystemObject.GetStandardStream(2)**:
   - This method is part of the `FileSystemObject`.
   - It returns a `TextStream` object corresponding to the standard error stream.
   - It can be used in both `cscript.exe` and `wscript.exe` (Windows-based script host), but `wscript.exe` does not support standard streams, so it will result in an error.
   - Example usage:
     ```vbscript
     Set fso = CreateObject("Scripting.FileSystemObject")
     Set stderr = fso.GetStandardStream(2)
     stderr.WriteLine "This is an error message."
     ```

In summary, `WScript.StdErr` is more straightforward for writing to the error stream when using `cscript.exe`, while `FileSystemObject.GetStandardStream(2)` provides more flexibility but is limited to `cscript.exe` for standard stream operations¹²³.

Source: Conversation with Copilot, 9/7/2024
(1) windows - vbscript output to console - Stack Overflow. https://stackoverflow.com/questions/4388879/vbscript-output-to-console%29.
(2) FileSystemObject.GetStandardStream Method - VBScript in a Nutshell, 2nd .... https://www.oreilly.com/library/view/vbscript-in-a/0596004885/re88.html.
(3) Writing to STDERR and STDOUT in VBScript - Dragon-IT. http://scripts.dragon-it.co.uk/scripts.nsf/docs/vbscript-writing-to-stdout-stderr!OpenDocument&ExpandSection=3&AutoFramed.
(4) VBScript - GetStandardStream Method - VbsEdit. https://www.vbsedit.com/html/6ae9a1dc-35ae-4e06-94b2-1578ba153fce.asp.
