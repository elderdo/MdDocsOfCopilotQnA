### question

For VS 2022 how do I find out what path this points to:
`$(MSBuildExtensionsPath)`

### answer

In Visual Studio 2022, the `$(MSBuildExtensionsPath)` property typically points to the path of the MSBuild subfolder under the `Program Files` or `Program Files (x86)` folder, depending on the bitness of the current process¹.

To find out the exact path, you can use the following steps:

1. **Open a Developer Command Prompt for Visual Studio**:

   - You can find this in the Start menu under Visual Studio 2022.

2. **Run the following command**:

   ```shell
   msbuild -property:Configuration=Debug -verbosity:diagnostic
   ```

   This command will output detailed information about the build process, including the paths of various MSBuild properties.

3. **Look for the `MSBuildExtensionsPath` property** in the output to see its resolved value.

Alternatively, you can create a simple MSBuild project file to print the value of `$(MSBuildExtensionsPath)`:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <Target Name="ShowMSBuildExtensionsPath">
    <Message Text="MSBuildExtensionsPath: $(MSBuildExtensionsPath)" Importance="high" />
  </Target>
</Project>
```

Run this project using MSBuild, and it will print the value of `$(MSBuildExtensionsPath)`.

¹: [MSBuild Reserved and Well-known Properties](https://learn.microsoft.com/en-us/visualstudio/msbuild/msbuild-reserved-and-well-known-properties?view=vs-2022)

Source: Conversation with Copilot, 9/10/2024
(1) MSBuild Reserved and Well-known Properties - MSBuild. https://learn.microsoft.com/en-us/visualstudio/msbuild/msbuild-reserved-and-well-known-properties?view=vs-2022.
(2) Customize your local build - MSBuild | Microsoft Learn. https://learn.microsoft.com/en-us/visualstudio/msbuild/customize-your-local-build?view=vs-2022.
(3) MSBuild has a grudge against $(VSToolsPath) #6607 - GitHub. https://github.com/dotnet/msbuild/issues/6607.
(4) Customize the solution build - MSBuild | Microsoft Learn. https://learn.microsoft.com/en-us/visualstudio/msbuild/customize-solution-build?view=vs-2022.
