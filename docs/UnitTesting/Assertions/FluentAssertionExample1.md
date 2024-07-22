### For C# replacing nUnit tests with xUnit tests and using FluentAssertions what do you use to replace Assert.That

In C#, when replacing NUnit tests with xUnit tests and using FluentAssertions, you can replace `Assert.That` with the `Should()` method provided by FluentAssertions. Here's an example:

```csharp
// NUnit
Assert.That(actualValue, Is.EqualTo(expectedValue));

// xUnit with FluentAssertions
actualValue.Should().Be(expectedValue);
```

If you're dealing with exceptions, you can replace `Assert.Fail()` with the `ShouldThrow<>` assertion extension¹. Here's an example:

```csharp
// NUnit
try
{
    MethodToTest(variable1, variable2);
    Assert.Fail();
}
catch (Exception ex)
{
    Assert.That(ex, Is.TypeOf<DataException>());
}

// xUnit with FluentAssertions
Action act = () => MethodToTest(variable1, variable2);
act.ShouldThrow<DataException>();
```

In the above example, if the expected exception is not thrown, the assertion would fail, stopping the test case¹. For more complex assertions, FluentAssertions provides a variety of methods to assert object properties, collection contents, and more²³.

Source: Conversation with Copilot, 7/16/2024
(1) How to replace Assert.Fail () with FluentAssertions. https://stackoverflow.com/questions/45035649/how-to-replace-assert-fail-with-fluentassertions.
(2) c# - FluentAssertions equivalent to xUnit Assert.Collection - Stack .... https://stackoverflow.com/questions/50471950/fluentassertions-equivalent-to-xunit-assert-collection.
(3) c# - xUnit Assert.Equivalent throws an System.ArgumentException when .... https://stackoverflow.com/questions/77693485/xunit-assert-equivalent-throws-an-system-argumentexception-when-trying-to-test-a.
(4) C# – Use FluentAssertions to improve unit tests - MAKOLYTE. https://makolyte.com/csharp-use-fluentassertions-to-improve-unit-tests/.
(5) NUnit or Fluent Assertions test for reference equality?. https://stackoverflow.com/questions/17741761/nunit-or-fluent-assertions-test-for-reference-equality.
(6) undefined. https://github.com/fluentassertions/fluentassertions/issues/118.
