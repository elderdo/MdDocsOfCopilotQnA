### From a Oracle.ManagedDataAccess.dll how do I tell if it contains 32 bit or 64 bit code?

There are several ways to check if a DLL is 32-bit or 64-bit:

1. **Using Visual Studio's dumpbin tool**: Open a command prompt, navigate to the directory containing the DLL, and type the following command¹:

```cmd
dumpbin /headers YourDLLFileName.dll
```

Replace `YourDLLFileName.dll` with the name of your DLL file. Look for the `machine` keyword in the output. If it says `x86`, the DLL is 32-bit; if it says `x64`, the DLL is 64-bit¹.

2. **Using 7-Zip**: If you have 7-Zip installed, you can use it to check the DLL file¹:

```cmd
"C:\Program Files\7-Zip\7z.exe" l "YourDLLFileName.dll" | findstr CPU
```

Replace `YourDLLFileName.dll` with the name of your DLL file¹.

3. **Using Notepad**: Open the DLL file in Notepad and find the first occurrence of the string `PE`. The following character defines if the DLL is 32 or 64 bits. If it's `L`, the DLL is 32-bit; if it's `d†`, the DLL is 64-bit⁴.

Remember to replace `YourDLLFileName.dll` with the actual name of your DLL file in the above commands.

Source: Conversation with Copilot, 7/29/2024
(1) Windows command to tell whether a .dll file is 32 bit or 64 bit?. https://stackoverflow.com/questions/14560866/windows-command-to-tell-whether-a-dll-file-is-32-bit-or-64-bit.
(2) How to find if a native DLL file is compiled as x64 or x86?. https://stackoverflow.com/questions/480696/how-to-find-if-a-native-dll-file-is-compiled-as-x64-or-x86.
(3) How can I test a Windows DLL file to determine if it is 32 bit or 64 .... https://stackoverflow.com/questions/495244/how-can-i-test-a-windows-dll-file-to-determine-if-it-is-32-bit-or-64-bit.
(4) How can I test a Windows DLL file to determine if it is 32 bit or 64 b. https://www.matheusmello.io/posts/windows-how-can-i-test-a-windows-dll-file-to-determine-if-it-is-32-bit-or-64-bit.
