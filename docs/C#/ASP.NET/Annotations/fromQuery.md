### question
So this annotation: 
```csharp
[FromQuery] string amount 
```

would result in a url having a query string of amount="$125.50" where amount is determined by the argument name. Is that correct?

### answer
Yes, that is correct! The `[FromQuery]` annotation in ASP.NET MVC and ASP.NET Core is used to bind a query string parameter from the URL to the parameter in the action method. The argument name in the annotation determines the name of the query string parameter.

### Example

Here's a controller method using the `[FromQuery]` annotation:

```csharp
[HttpGet]
[Route("calculate")]
public ActionResult CalculateTotal([FromQuery] string amount)
{
    // Use the amount parameter in your logic
    decimal parsedAmount;
    if (decimal.TryParse(amount, out parsedAmount))
    {
        var total = parsedAmount * 1.10m; // Example calculation
        return Ok(new { Total = total });
    }
    else
    {
        return BadRequest("Invalid amount format.");
    }
}
```

### URL Example

With the above method, you can pass the `amount` parameter via the URL like this:
```
https://localhost:<port>/calculate?amount=$125.50
```

### How It Works

1. **Annotation**: `[FromQuery] string amount`
   - The `[FromQuery]` attribute tells the framework to bind the value of the `amount` query string parameter to the `amount` parameter in the method.

2. **URL Query String**: `amount=$125.50`
   - The key `amount` in the query string matches the parameter name `amount` in the method.
   - The value `$125.50` is assigned to the `amount` parameter in the method when the request is made.

### Why Use `[FromQuery]`?

- **Clarity**: It's clear to both developers and users of the API how parameters should be passed in the URL.
- **Flexibility**: Allows easy modification and retrieval of query string parameters without changing the route definitions.

By using the `[FromQuery]` annotation, you ensure that the method can process the `amount` parameter directly from the URL's query string. This approach keeps your URLs clean and intuitive, enhancing both usability and SEO.

Would you like more examples or a deeper dive into how to handle various types of query parameters? 😊🌐💡
