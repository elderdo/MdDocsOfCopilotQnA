Does C# have a class or nuget package similar to Perl's Test::Template class as documented here https://metacpan.org/pod/Text::Template  

In C#, there isn't a direct equivalent to Perl's `Text::Template` class. However, there are several templating engines available that can provide similar functionality. One such engine is `StringTemplate`⁴. Here's an example of how you might use it:

```csharp
using Antlr.StringTemplate;
using Antlr.StringTemplate.Language;

StringTemplate hello = new StringTemplate("Hello, $name$", typeof(DefaultTemplateLexer));
```

This allows you to create templates with placeholders (like `$name$` in the example), which you can then replace with actual values.

Another approach is to use the `Regex` class for Perl-style replacement expressions⁵:

```csharp
var regex = new Regex(@"^(?<first>.*?):(?<second>.*)$", RegexOptions.Compiled);
Console.WriteLine(regex.Replace("moon:glow", "${second}:${first}"));
```

This example demonstrates how you can use named groups in a regular expression and then use those group names in the replacement string.

Remember to install the necessary packages via NuGet to use these classes in your project.

Source: Conversation with Copilot, 7/2/2024
(1) What's a good way of doing string templating in .NET?. https://stackoverflow.com/questions/733378/whats-a-good-way-of-doing-string-templating-in-net.
(2) How to use perl-style replacement expressions in .NET. https://stackoverflow.com/questions/5587397/how-to-use-perl-style-replacement-expressions-in-net.
(3) How to do template specialization in C# - Stack Overflow. https://stackoverflow.com/questions/600978/how-to-do-template-specialization-in-c-sharp.
(4) c++ - What is the equivalent of a template class in C# that inherits .... https://stackoverflow.com/questions/12161983/what-is-the-equivalent-of-a-template-class-in-c-sharp-that-inherits-from-the-spe.
(5) C# generics equivalent of C++ template specialization/implicit constror. https://stackoverflow.com/questions/67232691/c-sharp-generics-equivalent-of-c-template-specialization-implicit-constror.