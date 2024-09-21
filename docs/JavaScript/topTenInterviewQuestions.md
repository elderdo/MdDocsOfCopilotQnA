### question
What are the top 10 interview questions for javascript and their answers with examples?

### answer
### Top 10 JavaScript Interview Questions and Answers

1. **What is JavaScript?**
   - **Answer**: JavaScript is a high-level, interpreted programming language that enables developers to create interactive and dynamic web content. It is commonly used for front-end development and can be embedded directly into HTML pages or included from external files.
   - **Example**:
     ```javascript
     console.log("Hello, World!");
     ```

2. **What are the different data types in JavaScript?**
   - **Answer**: JavaScript has six primitive data types: Number, String, Boolean, Null, Undefined, and Symbol. It also has two compound data types: Object and Array.
   - **Example**:
     ```javascript
     let num = 42; // Number
     let str = "Hello"; // String
     let bool = true; // Boolean
     let obj = { name: "Alice" }; // Object
     let arr = [1, 2, 3]; // Array
     ```

3. **Explain the difference between `let`, `var`, and `const`.**
   - **Answer**: `var` is function-scoped and can be reassigned. `let` is block-scoped and can be reassigned. `const` is block-scoped but cannot be reassigned once declared.
   - **Example**:
     ```javascript
     function exampleScope() {
       if (true) {
         var varVariable = "I am var";
         let letVariable = "I am let";
         const constVariable = "I am const";
       }
       console.log(varVariable); // Output: "I am var"
       console.log(letVariable); // Error: "letVariable is not defined"
       console.log(constVariable); // Error: "constVariable is not defined"
     }
     ```

4. **What is hoisting in JavaScript?**
   - **Answer**: Hoisting is a JavaScript mechanism where variable and function declarations are moved to the top of their scope before code execution. This means you can use functions and variables before they are declared.
   - **Example**:
     ```javascript
     console.log(greet()); // Output: "Hello, World!"
     function greet() {
       return "Hello, World!";
     }
     ```

5. **What is the difference between `==` and `===` in JavaScript?**
   - **Answer**: `==` (loose equality) compares values after type conversion, while `===` (strict equality) compares values without type conversion.
   - **Example**:
     ```javascript
     console.log(5 == "5"); // Output: true
     console.log(5 === "5"); // Output: false
     ```

6. **Explain closures in JavaScript.**
   - **Answer**: A closure is a function that retains access to its lexical scope, even when the function is executed outside that scope.
   - **Example**:
     ```javascript
     function outerFunction() {
       let outerVariable = "I'm outside!";
       function innerFunction() {
         console.log(outerVariable);
       }
       return innerFunction;
     }
     const inner = outerFunction();
     inner(); // Output: "I'm outside!"
     ```

7. **What is a callback function in JavaScript?**
   - **Answer**: A callback function is a function passed as an argument to another function and is executed after the completion of the parent function.
   - **Example**:
     ```javascript
     function fetchData(callback) {
       setTimeout(function() {
         const data = "Fetched data";
         callback(data);
       }, 1000);
     }
     function processData(data) {
       console.log("Processing data: " + data);
     }
     fetchData(processData); // Output after 1 second: "Processing data: Fetched data"
     ```

8. **What are promises in JavaScript?**
   - **Answer**: Promises are objects that represent the eventual completion (or failure) of an asynchronous operation and its resulting value.
   - **Example**:
     ```javascript
     let promise = new Promise(function(resolve, reject) {
       let success = true;
       if (success) {
         resolve("Operation was successful");
       } else {
         reject("Operation failed");
       }
     });
     promise.then(function(message) {
       console.log(message); // Output: "Operation was successful"
     }).catch(function(message) {
       console.log(message);
     });
     ```

9. **What is the purpose of the `setTimeout` function in JavaScript?**
   - **Answer**: The `setTimeout` function is used to execute a function after a specified delay (in milliseconds).
   - **Example**:
     ```javascript
     setTimeout(function() {
       console.log("This message is delayed by 2 seconds");
     }, 2000);
     ```

10. **How can you remove duplicates from an array in JavaScript?**
    - **Answer**: You can remove duplicates from an array using the `Set` object or the `filter` method.
    - **Example**:
      ```javascript
      let array = [1, 2, 2, 3, 4, 4, 5];
      let uniqueArray = [...new Set(array)];
      console.log(uniqueArray); // Output: [1, 2, 3, 4, 5]

      // Using filter method
      let uniqueArray2 = array.filter((item, index) => array.indexOf(item) === index);
      console.log(uniqueArray2); // Output: [1, 2, 3, 4, 5]
      ```

These questions and answers cover fundamental concepts and are commonly asked in JavaScript interviews¹²³.

Source: Conversation with Copilot, 9/21/2024
(1) Top 50 JavaScript Interview Questions With Example Answers. https://builtin.com/software-engineering-perspectives/javascript-interview-questions.
(2) Top 50 JavaScript Interview Questions and Answers (2024). https://interviewzilla.com/javascript/javascript-interview-questions/.
(3) Top 100 JavaScript Interview Questions and Answers (2024) - Guru99. https://www.guru99.com/javascript-interview-questions-answers.html.
(4) 100 Must-Know JavaScript Interview Questions and Answers - Turing. https://www.turing.com/interview-questions/javascript.