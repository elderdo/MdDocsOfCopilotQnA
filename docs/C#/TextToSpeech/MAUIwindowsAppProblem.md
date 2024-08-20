### question
When trying to create a MAUI project just for a Windows UI, I get the following error when I try to compile the VS 2022 template project.  
Error (active)	NU1105	Unable to find project information for 'C:\Users\Douglas\Documents\C#\MauiFlipClockApp\MauiFlipClock\MauiFlipClock\MauiFlipClock.csproj'. If you are using Visual Studio, this may be because the project is unloaded or not part of the current solution so run a restore from the command-line. Otherwise, the project file may be invalid or missing targets required for restore.
What is wrong with my VS 2022 and what steps do I need to take to fix it?

### answer
The **NU1105** error you're encountering in Visual Studio 2022 indicates that the project information for your **MauiFlipClock.csproj** file cannot be found. Here are some steps to troubleshoot and resolve this issue:

1. **Check Project File**: First, ensure that the **MauiFlipClock.csproj** file exists and is valid. Make sure it's not corrupted or missing any essential targets.

2. **Restore from Command Line**: Run a restore from the command line using the .NET CLI. Open a terminal or command prompt, navigate to your solution directory, and execute the following command:
   ```
   dotnet restore MySolution.sln
   ```
   Replace **MySolution.sln** with the actual name of your solution file.

3. **Import NuGet Targets**: If you're still encountering the issue, check if the **NuGet.targets** are properly imported. Usually, it's recommended to import the **Microsoft.Common.targets**. Ensure that your project file references these targets correctly.

4. **Reload the Project**: In Visual Studio, try reloading the project. If you're using Visual Studio 2019 or later, NuGet can reuse artifacts from the previous restore for unloaded projects. To make this work, ensure that all projects in your solution have been restored from the command line before loading them in Visual Studio.

By following these steps, you should be able to resolve the NU1105 error and successfully compile your MAUI project. If the issue persists, consider checking for any specific project system settings or configurations related to MAUI and Windows UI development.¹²³

Source: Conversation with Copilot, 8/19/2024
(1) NuGet Error NU1105 | Microsoft Learn. https://learn.microsoft.com/en-us/nuget/reference/errors-and-warnings/nu1105.
(2) Visual Studio shows NU1105 error even though build and tests pass. https://github.com/dotnet/aspnetcore/issues/4183.
(3) Visual Studio 2017 error: Cannot find project info for "" This can .... https://stackoverflow.com/questions/42657618/visual-studio-2017-error-cannot-find-project-info-for-this-can-indicate-a-mi.
(4) Error message around "Unable to find project information ... - GitHub. https://github.com/NuGet/Home/issues/5350.