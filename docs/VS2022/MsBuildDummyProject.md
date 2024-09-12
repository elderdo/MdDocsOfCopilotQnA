### question

When creating a dummy MSBuild project with this xml:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <Target Name="ShowMSBuildExtensionsPath">
    <Message Text="MSBuildExtensionsPath: $(MSBuildExtensionsPath)" Importance="high" />
  </Target>
</Project>
```

what do you name the file and what msbuild command do you use to see all the output including the expansion of all the variables?

### answer

You can name the file with a `.proj` extension, for example, `ShowMSBuildExtensionsPath.proj`.

To see all the output, including the expansion of all the variables, you can use the following MSBuild command:

```shell
msbuild ShowMSBuildExtensionsPath.proj -verbosity:diagnostic
```

This command will provide detailed information about the build process, including the values of all the MSBuild properties.
