Can you give me a C# examples using one or more of those template NuGet package?

Sure, here are some examples of how you can use these NuGet packages in C#:

1. **.NET Template Engine**
   You can create a template package using the Microsoft.TemplateEngine.Authoring.Templates NuGet package¹. Here's a basic example of how to use it:

   ```csharp
   // Install the template package
   dotnet new install Microsoft.TemplateEngine.Authoring.Templates

   // Create a template package project
   dotnet new templatepack
   ```

   This will create a new template package project. You can then customize this project to suit your needs¹.

2. **Toolbelt.AspNetCore.Blazor.Minimum.Templates**
   This package allows you to create a new Blazor Server app project, a new Blazor WebAssembly app project, or a new Blazor Web app project⁸. Here's how you can use it:

   ```csharp
   // Install the template package
   dotnet new install Toolbelt.AspNetCore.Blazor.Minimum.Templates

   // Create a new Blazor Server app project
   dotnet new blazorservermin

   // Create a new Blazor WebAssembly app project
   dotnet new blazorwasmmin

   // Create a new Blazor Web app project
   dotnet new blazormin
   ```

   These commands create a new project in the current directory, and the project name will be the same as the current directory name⁸.

3. **Ardalis.CleanArchitecture.Template**
   This template can be installed using `dotnet new`¹². Here's an example of how to use it:

   ```csharp
   // Install the template
   dotnet new install Ardalis.CleanArchitecture.Template
   ```

   This will install the template, and you can then use it to create new projects¹².

4. **Razor.Templating.Core**
   This package allows you to render a Razor view to a string⁵. Here's a basic example of how to use it:

   ```csharp
   using Razor.Templating.Core;

   var model = new ExampleModel()
   {
       PlainText = "This text is rendered from Razor Views using Razor.Templating.Core",
       HtmlContent = "<em>You can use it to generate email content, report generation and so on</em>"
   };

   var html = await RazorTemplateEngine.RenderAsync("/Views/ExampleView.cshtml", model);
   ```

   This will render the specified Razor view to a string, using the provided model⁵.

Remember to add the necessary `using` statements and replace the placeholders with your actual data or file paths. Also, ensure that the necessary packages are installed in your project before using them. You can do this by adding them to your project file or using the `dotnet add package` command.

Source: Conversation with Copilot, 7/2/2024
(1) Create a template package for dotnet new - .NET | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/core/tutorials/cli-templates-create-template-package.
(2) Toolbelt.AspNetCore.Blazor.Minimum.Templates 8.0.301 - NuGet Gallery. https://www.nuget.org/packages/Toolbelt.AspNetCore.Blazor.Minimum.Templates/.
(3) NuGet Gallery | Ardalis.CleanArchitecture.Template 9.1.2. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/.
(4) NuGet Gallery | Razor.Templating.Core 2.0.0. https://www.nuget.org/packages/Razor.Templating.Core.
(5) Custom templates for dotnet new - .NET CLI | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/core/tools/custom-templates.
(6) Using the .Net Core Template Engine to Create Custom Templates ... - InfoQ. https://www.infoq.com/articles/dotnet-core-template-engine/.
(7) c# - Using Razor outside of MVC in .NET Core - Stack Overflow. https://stackoverflow.com/questions/38247080/using-razor-outside-of-mvc-in-net-core.
(8) Render Emails Using Razor Templating | Twilio. https://www.twilio.com/en-us/blog/render-emails-using-razor-templating.
(9) soundaranbu/Razor.Templating.Core - GitHub. https://github.com/soundaranbu/Razor.Templating.Core.
(10) Blazor Minimum Project Templates - GitHub. https://github.com/jsakamoto/BlazorMinimumTemplates.
(11) Toolbelt.AspNetCore.Blazor.Minimum.Templates 6.0.8.1 - NuGet Gallery. https://www.nuget.org/packages/Toolbelt.AspNetCore.Blazor.Minimum.Templates/6.0.8.1.
(12) Toolbelt.AspNetCore.Blazor.Minimum.Templates 6.0.8 - NuGet Gallery. https://www.nuget.org/packages/Toolbelt.AspNetCore.Blazor.Minimum.Templates/6.0.8.
(13) Clean Architecture with ASP.NET Core | Blog - Ardalis. https://ardalis.com/clean-architecture-asp-net-core/.
(14) NuGet Gallery | Ardalis.CleanArchitecture.Template 6.2.8. https://www.nuget.org/packages/ardalis.cleanarchitecture.template/6.2.8.
(15) NuGet Gallery | Ardalis.CleanArchitecture.Template 6.0.10. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/6.0.10.
(16) NuGet Gallery | Ardalis.CleanArchitecture.Template 9.1.1. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/9.1.1.
(17) undefined. https://twitter.com/intent/follow?screen_name=ardalis.
(18) undefined. https://img.shields.io/twitter/follow/ardalis.svg?label=Follow%20@ardalis.
(19) undefined. https://twitter.com/intent/follow?screen_name=nimblepros.
(20) undefined. https://img.shields.io/twitter/follow/nimblepros.svg?label=Follow%20@nimblepros.
(21) undefined. https://www.nuget.org/packages/Ardalis.CleanArchitecture.Template/%29.
(22) undefined. https://medium.com/@soundaranbu/render-razor-view-cshtml-to-string-in-net-core-7d125f32c79.
(23) github.com. https://github.com/soundaranbu/RazorTemplating/tree/73531ba992b6cdc8969b55f82d6c64be8c55d0af/README.md.
