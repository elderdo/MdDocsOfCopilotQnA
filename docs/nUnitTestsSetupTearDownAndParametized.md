Could you show examples of annotations: [Setup] and [teardown ] and parameterized tests?

Certainly! Here are examples of NUnit annotations `[SetUp]`, `[TearDown]`, and parameterized tests:

### `[SetUp]` and `[TearDown]` Annotations

```csharp
using NUnit.Framework;

[TestFixture]
public class ExampleTests
{
    [SetUp]
    public void Init()
    {
        // Code to set up test environment
        Console.WriteLine("Setting up before each test.");
    }

    [TearDown]
    public void Cleanup()
    {
        // Code to clean up after each test
        Console.WriteLine("Cleaning up after each test.");
    }

    [Test]
    public void Test1()
    {
        Assert.Pass("Test1 passed.");
    }

    [Test]
    public void Test2()
    {
        Assert.Pass("Test2 passed.");
    }
}
```

### Parameterized Tests

```csharp
using NUnit.Framework;

[TestFixture]
public class ParameterizedTests
{
    [Test]
    [TestCase(1, 2, 3)]
    [TestCase(2, 3, 5)]
    [TestCase(3, 4, 7)]
    public void AddTest(int a, int b, int expectedResult)
    {
        Assert.AreEqual(expectedResult, a + b);
    }

    [Test]
    [TestCaseSource(nameof(TestData))]
    public void MultiplyTest(int a, int b, int expectedResult)
    {
        Assert.AreEqual(expectedResult, a * b);
    }

    static object[] TestData =
    {
        new object[] { 1, 2, 2 },
        new object[] { 2, 3, 6 },
        new object[] { 3, 4, 12 }
    };
}
```

These examples demonstrate how to use `[SetUp]` and `[TearDown]` for initializing and cleaning up test environments, as well as how to create parameterized tests using `[TestCase]` and `[TestCaseSource]`¹²³.

Source: Conversation with Copilot, 7/1/2024
(1) NUnit Tutorial: Parameterized Tests With Examples | LambdaTest. https://www.lambdatest.com/blog/nunit-parameterized-test-examples/.
(2) SetUp and TearDown | NUnit Docs. https://docs.nunit.org/articles/nunit/writing-tests/setup-teardown/index.html.
(3) Parameterized Tests · nunit/docs Wiki · GitHub. https://github.com/nunit/docs/wiki/Parameterized-Tests.
(4) How to write Nunit Parameterized Test | BrowserStack. https://www.browserstack.com/guide/nunit-parameterized-test.
