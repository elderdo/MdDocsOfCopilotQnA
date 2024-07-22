Using C#, xUnit, and FluentAssertions how would you test a collection to make sure it does NOT contain a certain value?

Sure, here's an example of how you could write a unit test in C# using xUnit and FluentAssertions to verify that a collection does not contain a certain value:

```csharp
using Xunit;
using FluentAssertions;
using System.Collections.Generic;

public class CollectionTest
{
    [Fact]
    public void CollectionDoesNotContainValue()
    {
        // Arrange
        var collection = new List<int> { 1, 2, 3, 4, 5 };

        // Act
        var valueToCheck = 6;

        // Assert
        collection.Should().NotContain(valueToCheck);
    }
}
```

In this example, the `NotContain` method from FluentAssertions is used to assert that the `valueToCheck` is not in the `collection`. If the `valueToCheck` is found in the `collection`, the test will fail. Otherwise, it will pass. This is a simple and effective way to ensure a collection does not contain a certain value.
