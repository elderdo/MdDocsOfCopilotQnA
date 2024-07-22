### For FluentAssertions using how do you appliy the assertions to collections like having an expected result of not empty and where you compare lists of values to make sure they are equivalent.

In FluentAssertions, you can apply assertions to collections in various ways. Here are some examples:

1. **Asserting a collection is not empty**:

```csharp
collection.Should().NotBeEmpty();
```

2. **Comparing two collections for equivalence** (order does not matter):

```csharp
collection.Should().BeEquivalentTo(expectedCollection);
```

3. **Asserting a collection contains items in a specific order**:

```csharp
collection.Should().ContainInOrder(new[]{1,2,3});
```

4. **Asserting all items in a collection satisfy a condition**:

```csharp
collection.Should().OnlyContain(x => x.IsActive);
```

This will ensure that all items in the collection satisfy the condition provided¹².

Remember, FluentAssertions provides a wide range of methods to assert various properties of collections¹. You can choose the one that best suits your testing needs.

Source: Conversation with Copilot, 7/16/2024
(1) Collections - Fluent Assertions. https://fluentassertions.com/collections/.
(2) How to assert all items in a collection using fluent-assertions?. https://stackoverflow.com/questions/18899755/how-to-assert-all-items-in-a-collection-using-fluent-assertions.
(3) c# - Satisfy() in Fluent Assertions does not work with collections of .... https://stackoverflow.com/questions/70282547/satisfy-in-fluent-assertions-does-not-work-with-collections-of-class-objects.
(4) Fluent Assertions: Fluently Assert the Result of .NET Tests. https://dev.to/fabriziobagala/fluent-assertions-fluently-assert-the-result-of-net-tests-2nie.
