Does C# have a Nuget package that has a templeting tool similar to Perl's template where a template can be used to generate one or more files using the template and filling in the names, expressions, or code similar to how Razor and/or Blazor C# code works to generate html pages?

Yes, C# does have NuGet packages that provide templating tools similar to Perl's template. Here are a few:

1. **.NET Template Engine**: This is used by `dotnet new` and provides libraries for template instantiation and template package management¹. It allows you to create your own .NET code templates and use them from anywhere².

2. **Toolbelt.AspNetCore.Blazor.Minimum.Templates**: This package contains a .NET Template Package you can call from the shell/command line³.

3. **Ardalis.CleanArchitecture.Template**: This template can be installed using `dotnet new`⁴.

4. **Razor.Templating.Core**: This package allows you to render a Razor view to a string from any project, i.e., class libraries or console apps⁵.

These packages should help you generate one or more files using a template and filling in the names, expressions, or code, similar to how Razor and/or Blazor C# code works to generate HTML pages. Remember to check the documentation for each package to understand how to use them effectively.

Source: Conversation with Copilot, 7/2/2024
(1) GitHub - dotnet/templating: This repo contains the Template Engine .... https://github.com/dotnet/templating.
(2) Quickly Create Your Own .NET Code Templates and Use Them ... - cfrenzel. https://cfrenzel.com/dotnet-new-templating-nuget/.
(3) Toolbelt.AspNetCore.Blazor.Minimum.Templates 8.0.301 - NuGet Gallery. https://www.nuget.org/packages/Toolbelt.AspNetCore.Blazor.Minimum.Templates/.
(4) NuGet Gallery | Ardalis.CleanArchitecture.Template 9.1.2. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/.
(5) NuGet Gallery | Razor.Templating.Core 2.0.0. https://www.nuget.org/packages/Razor.Templating.Core.
(6) undefined. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/%29.
