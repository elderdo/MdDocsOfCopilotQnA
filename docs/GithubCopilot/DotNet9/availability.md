### question

How do I get .NET 9 for VS 2022 Enterprise edition?

### answer

To get .NET 9 for Visual Studio 2022 Enterprise Edition, follow these steps:

1. **Update Visual Studio**: Ensure your Visual Studio 2022 is updated to version 17.12 or later, which supports .NET 9. You can update Visual Studio by navigating to the Visual Studio 2022 release history page, downloading the bootstrapper for the desired version, and running it³.

2. **Install .NET 9 SDK**:

   - Visit the [.NET 9 download page](https://dotnet.microsoft.com/download/dotnet/9.0).
   - Download the appropriate installer for your operating system.
   - Run the installer and follow the on-screen instructions to complete the installation.

3. **Configure Visual Studio**:

   - Open Visual Studio 2022.
   - Go to **Tools > Options > Projects and Solutions > .NET Core**.
   - Ensure that the .NET 9 SDK is listed and selected.

4. **Create or Upgrade Projects**:
   - To create a new project, select **Create a new project** and choose a template that supports .NET 9.
   - To upgrade an existing project, open the project file (.csproj) and update the `TargetFramework` to `net9.0`.

By following these steps, you'll be able to develop and run .NET 9 applications in Visual Studio 2022 Enterprise Edition².

Source: Conversation with Copilot, 9/5/2024
(1) Update Visual Studio installation to recent release. https://learn.microsoft.com/en-us/visualstudio/install/update-visual-studio?view=vs-2022.
(2) First preview of Visual Studio 2022 v17.12 with .NET 9. https://devblogs.microsoft.com/visualstudio/first-preview-of-visual-studio-2022-v17-12-with-net-9/.
(3) Visual Studio 2022 IDE - Programming Tool for Software Developers. https://visualstudio.microsoft.com/vs/.
(4) Microsoft Spotlights .NET 9, Cloud in First Visual Studio 2022 v17.12 .... https://visualstudiomagazine.com/Articles/2024/08/16/Microsoft-Spotlights-NET-9-Cloud-in-First-Visual-Studio-2022-v17-12-Preview.aspx.
