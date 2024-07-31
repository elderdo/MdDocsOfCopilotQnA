### For VS 2022 where is the "Application Pool Settings for IIS Express used by VS 2022" to indicate if it should be 32-bit or 64-bit ?

In Visual Studio 2022, you can set the IIS Express to run in 32-bit or 64-bit mode through the following steps¹:

1. Go to **Tools** -> **Options**.
2. Navigate to **Projects and Solutions** -> **Web Projects**.
3. Here, you will find an option called **"Use the 64 bit version of IIS Express for web sites and projects"**¹.

If this option is checked, IIS Express will run in 64-bit mode. If it's unchecked, IIS Express will run in 32-bit mode¹. Please note that this setting applies globally to all web projects in Visual Studio¹. If you want to set this on a per-project basis, you can add `<Use64BitIISExpress>false</Use64BitIISExpress>` under the `<PropertyGroup>` tag of your `.csproj` project file¹.

Source: Conversation with Copilot, 7/29/2024
(1) How can I force IIS Express to run in 32 bit mode?. https://stackoverflow.com/questions/16226582/how-can-i-force-iis-express-to-run-in-32-bit-mode.
(2) How do you configure iis express for VS 2022? - Microsoft Q&A. https://learn.microsoft.com/en-us/answers/questions/1287457/how-do-you-configure-iis-express-for-vs-2022.
(3) How to check if IIS is in 32bit or 64bit mode - Stack Overflow. https://stackoverflow.com/questions/2804040/how-to-check-if-iis-is-in-32bit-or-64bit-mode.
(4) Running a 32-bit application under IIS on a 64-bit machine. https://www.codeproject.com/tips/325824/running-a-32-bit-application-under-iis-on-a-64-bit.
(5) 32-bit Mode Worker Processes | Microsoft Learn. https://learn.microsoft.com/en-us/iis/web-hosting/web-server-for-shared-hosting/32-bit-mode-worker-processes.
