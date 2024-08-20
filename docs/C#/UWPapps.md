### question
For my UWP app VS 2022 won't let me change the language level to at least 10 to recognize global using statements.  Why won't it allow me to change the language level?  Should I have selected a spec

### answer
**Changing Language Level for UWP Apps in Visual Studio 2022**

It seems you're encountering issues changing the language level for your UWP (Universal Windows Platform) app in Visual Studio 2022. Let's address this step by step:

1. **Language Level Limitations**:
   - Visual Studio 2022 has certain limitations regarding language levels, especially for UWP projects.
   - Unfortunately, UWP apps do not currently support the latest C# language features (such as C# 10) out of the box. This limitation is due to the specific platform constraints and compatibility requirements for UWP.
   - As a result, you might not be able to set the language level directly to 10 in your UWP project settings.

2. **Project Type Selection**:
   - When creating a new UWP project, you should choose the appropriate project type that aligns with your requirements.
   - The recommended project type for UWP apps is the **Blank App (Universal Windows)** template. This template provides a basic starting point for UWP development.
   - If you've already created your project using a different template, consider creating a new project with the Blank App template and migrating your existing code if necessary.

3. **Workaround for Global Usings**:
   - While you can't directly set the language level to 10 for UWP, you can still use global usings by following these steps:
     - Create a `Directory.Build.props` file in your solution directory (if it doesn't exist).
     - Add the following setting to the `Directory.Build.props` file:
       ```xml
       <Project>
           <PropertyGroup>
               <LangVersion>preview</LangVersion>
           </PropertyGroup>
       </Project>
       ```
     - This sets the language version to the latest preview version, which includes support for global usings.
   - Additionally, enable implicit global using directives in your `.csproj` file:
     ```xml
     <PropertyGroup>
         <ImplicitUsings>enable</ImplicitUsings>
     </PropertyGroup>
     ```
     - This allows you to use global usings in your code files.

4. **Existing Project Modification**:
   - If you've already created your UWP project and global usings are not working, consider creating a new Blank App project and migrating your existing code.
   - Alternatively, manually refactor your existing code to use global usings where possible.

Remember that UWP development has its own specific requirements and limitations, but with the right adjustments, you can still achieve your desired functionality. If you encounter any further issues or need additional assistance, feel free to ask! 😊