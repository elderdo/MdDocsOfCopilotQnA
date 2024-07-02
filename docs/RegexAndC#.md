Can C#'s Regex do string manipulation similar to how scripting tools like perl or python can do? If it can, could you show me a simple exampe?

Yes, C#'s `Regex` class in the `System.Text.RegularExpressions` namespace can be used for string manipulation similar to scripting languages like Perl or Python. It provides methods for pattern matching and text replacement.

Here's a simple example of using `Regex` to replace all occurrences of a pattern in a string:

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Hello, my name is John Doe. I live in Doe City.";
        string pattern = "Doe";
        string replacement = "Smith";

        string result = Regex.Replace(input, pattern, replacement);

        Console.WriteLine(result);
        // Output: Hello, my name is John Smith. I live in Smith City.
    }
}
```

In this example, the `Regex.Replace` method is used to replace all occurrences of "Doe" with "Smith" in the input string. The first argument is the input string, the second is the pattern to match, and the third is the replacement string. The method returns a new string with the replacements made. This is similar to how you might use the `sub` function in Python's `re` module or the `s///` operator in Perl for string substitution.
