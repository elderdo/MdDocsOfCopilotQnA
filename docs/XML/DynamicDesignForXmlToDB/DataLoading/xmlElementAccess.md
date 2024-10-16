### question

How do you access the elemant value of an xml tag when using the XDocument? Can you show another xml file example and the code reading the element value?

### answer

Certainly! Let's consider another XML file example:

```xml
<library>
  <book>
    <title>The Great Gatsby</title>
    <author>F. Scott Fitzgerald</author>
    <year>1925</year>
  </book>
  <book>
    <title>1984</title>
    <author>George Orwell</author>
    <year>1949</year>
  </book>
</library>
```

Here's the C# code to read the element values using `XDocument`:

```csharp
using System;
using System.Xml.Linq;

class Program
{
    static void Main()
    {
        string xmlFilePath = "path_to_your_xml_file.xml";
        XDocument xmlDoc = XDocument.Load(xmlFilePath);

        foreach (var book in xmlDoc.Descendants("book"))
        {
            string title = book.Element("title")?.Value;
            string author = book.Element("author")?.Value;
            string year = book.Element("year")?.Value;

            Console.WriteLine($"Title: {title}");
            Console.WriteLine($"Author: {author}");
            Console.WriteLine($"Year: {year}");
            Console.WriteLine();
        }
    }
}
```

In this example:

- The `XDocument.Load` method loads the XML file.
- The `foreach` loop navigates through the `book` elements.
- The `Element` method is used to access the value of the `title`, `author`, and `year` elements within each `book` tag.

This code will output the title, author, and year for each book in the XML file.
