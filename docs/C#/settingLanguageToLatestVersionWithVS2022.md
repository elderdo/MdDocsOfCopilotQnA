### question
Can you show me a UWP app how do you set the C# language level to use the latest version using Visual Studio 2022?  How do you make Visual Studio 2022 open the project file?

### answer
Certainly! Let's address both parts of your request:

1. **Setting C# Language Level in Visual Studio 2022**:
   To set the C# language level to use the latest version in Visual Studio 2022, follow these steps:

   - Open your project in Visual Studio.
   - Right-click on your project in the Solution Explorer.
   - Select "Properties."
   - In the properties window, navigate to the "Build" tab.
   - Under the "Advanced" section, you'll find the "Language version" dropdown.
   - Choose the desired language version (e.g., "latest" or a specific version like "10.0").
   - Click "Apply" or "OK" to save the changes.

   You can also configure the language version globally for all projects in your solution by creating a `Directory.Build.props` file in your solution directory. Add the following setting to that file:

   ```xml
   <Project>
       <PropertyGroup>
           <LangVersion>latest</LangVersion>
       </PropertyGroup>
   </Project>
   ```

   This will ensure that all projects in your solution use the specified language version².

2. **Opening a Project File in Visual Studio 2022**:
   To open a project file in Visual Studio 2022, follow these steps:

   - Launch Visual Studio 2022.
   - Click on the "File" menu.
   - Select "Open" > "Project/Solution."
   - Navigate to the location of your project file (usually with a `.csproj` extension).
   - Select the project file and click "Open."

   Alternatively, if you've already cloned a repository from a source control system (such as GitHub), you can open a project directly from there. Use the "Clone a repository" option in the start window, and Visual Studio will guide you through the process⁵.

Now you're all set! Feel free to explore and develop your UWP app using Visual Studio 2022. 😊

Source: Conversation with Copilot, 8/19/2024
(1) Configure language version - C# reference | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/configure-language-version.
(2) Tutorial: Open project from repo in Visual Studio - Visual Studio .... https://learn.microsoft.com/en-us/visualstudio/get-started/tutorial-open-project-from-repo?view=vs-2022.
(3) Tutorial: Create UWP Apps with Visual Studio & C#. https://learn.microsoft.com/en-us/visualstudio/get-started/csharp/tutorial-uwp?view=vs-2022.
(4) Windows11, developer mode for UWP development in visual studio 2022 .... https://answers.microsoft.com/en-us/insider/forum/all/windows11-developer-mode-for-uwp-development-in/21e21c9a-8193-43fe-86f6-5195ed0dd793.
(5) Set C# Language Version for All the Projects in a Solution. https://code-maze.com/set-csharp-language-version-all-projects-in-solution/.
(6) Opening Multiple Projects in Visual Studio 2022: A Simple Guide. https://devcodef1.com/news/1119213/open-multiple-projects-in-vs2022.
(7) What are solutions and projects in Visual Studio?. https://learn.microsoft.com/en-us/visualstudio/ide/solutions-and-projects-in-visual-studio?view=vs-2022.

### question
VS 2022 creates a UWP app with the C# language level at 7.3.  I want to use global usings.  How do I change the language level?  I tried using the latest version Directory.Build.props file specifying the following:
```xml
<Project>
       <PropertyGroup>
           <LangVersion>latest</LangVersion>
       </PropertyGroup>
   </Project>
```   
but it is not working.  How do I fix this?

### answer
To enable global usings in your UWP app created with Visual Studio 2022, follow these steps:

1. **Configure Language Version**:
   - Create a `Directory.Build.props` file in your solution directory (if it doesn't already exist).
   - Add the following setting to the `Directory.Build.props` file:
     ```xml
     <Project>
         <PropertyGroup>
             <LangVersion>preview</LangVersion>
         </PropertyGroup>
     </Project>
     ```
   - This sets the language version to the latest preview version, which includes support for global usings⁵.

2. **Enable Implicit Global Using Directives**:
   - In your `.csproj` file, add the following setting inside a `<PropertyGroup>`:
     ```xml
     <ImplicitUsings>enable</ImplicitUsings>
     ```
   - This enables the generation of implicit global using directives⁶.

3. **Convert Existing Usings to Global Usings**:
   - If you're using ReSharper, you can use the "Convert to Global Using" refactoring:
     - Place the caret at a `using` directive or select multiple directives.
     - Press `Ctrl + Shift + R` and choose "Convert to Global Using" or right-click and choose "Refactor | Convert to Global Using" from the context menu⁴.
   - Alternatively, manually replace individual `using` directives with global usings in your code files:
     ```csharp
     // GlobalUsings.cs
     global using System;

     // Program.cs (no usings)
     Console.WriteLine("Hello World");
     ```

By following these steps, you'll be able to use global usings in your UWP app, improving code readability and simplifying your development process⁷. Let me know if you need further assistance! 😊

Source: Conversation with Copilot, 8/19/2024
(1) Configure language version - C# reference | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/configure-language-version.
(2) C# 10.0 implicit global using directives | endjin. https://endjin.com/blog/2021/09/dotnet-csharp-10-implicit-global-using-directives.
(3) Convert to Global Using refactoring | ReSharper Documentation - JetBrains. https://www.jetbrains.com/help/resharper/Refactorings__Extract_Global_Using.html.
(4) Global Usings – A Look at New Language Features in C# 10. https://blog.jetbrains.com/dotnet/2021/11/18/global-usings-in-csharp-10/.
(5) Visual Studio 2022 Global Usings in C# 10 in 3 min @jssuthahar @MicrosoftDeveloper @visualstudio. https://www.youtube.com/watch?v=n4s6mWq4dVE.
(6) Practical Example of Global Using In C# 10 Code And Visual Studio 2022. https://www.youtube.com/watch?v=NTXNP4vY_9Y.
(7) Global Usings in C# 10 and .NET 6 In 10 Minutes or Less. https://www.youtube.com/watch?v=RWYrafpP53A.
(8) Exploring C# 10: Global Using Declarations - Dave Brock. https://www.daveabrock.com/2021/10/21/csharp-10-global-usings/.

