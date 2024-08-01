

Yes, JavaScript can connect to a database and run queries, but it's important to note that this is typically done on the server-side using Node.js. Here are a couple of examples:

**Example 1: Using MySQL with Node.js**
```javascript
var mysql = require('mysql');

var con = mysql.createConnection({
  host: "localhost",
  user: "yourusername",
  password: "yourpassword",
  database: "mydb"
});

con.connect(function(err) {
  if (err) throw err;
  console.log("Connected!");
  var sql = "SELECT * FROM customers";
  con.query(sql, function (err, result) {
    if (err) throw err;
    console.log(result);
  });
});
```
In this example, we first include the `mysql` module³. Then, we create a connection to the database³. If the connection is successful, we run a SQL query to select all records from a table named `customers`³.

**Example 2: Using SQL Server with Node.js**
```javascript
var sql = require('mssql');

var config = {
    user: 'username',
    password: 'password',
    server: 'localhost', 
    database: 'database' 
};

sql.connect(config, function (err) {
    if (err) console.log(err);

    var request = new sql.Request();
   
    request.query('select * from table', function (err, recordset) {
        if (err) console.log(err)

        console.log(recordset);
    });
});
```
In this example, we first include the `mssql` module. Then, we create a connection to the database. If the connection is successful, we run a SQL query to select all records from a table¹.

Please replace `'username'`, `'password'`, `'localhost'`, `'database'`, and `'table'` with your actual database details.

Remember, it's not recommended to use client-side JavaScript to connect to a database due to security reasons¹. Instead, you should create a server-side API using Node.js and then call that API from your client-side JavaScript code⁴.

Source: Conversation with Copilot, 7/11/2024
(1) Using MySQL with Node.js and the mysql JavaScript Client - SitePoint. https://www.sitepoint.com/using-node-mysql-javascript-client/.
(2) How to connect to SQL Server database from JavaScript in the browser .... https://stackoverflow.com/questions/857670/how-to-connect-to-sql-server-database-from-javascript-in-the-browser.
(3) how to run a MySQL query using JavaScript - Stack Overflow. https://stackoverflow.com/questions/11217832/how-to-run-a-mysql-query-using-javascript.
(4) From zero to hero: using SQL databases in Node.js made easy. https://www.honeybadger.io/blog/using-sql-databases-in-javascript/.
(5) AJAX Database Example - W3Schools. https://www.w3schools.com/js/js_ajax_database.asp.
(6) undefined. https://github.com/giridhar7632/relational-node.
(7) github.com. https://github.com/2999-Capstone-Purple-B01/NutsAndBolts-TESTING/tree/aea74ec727753fed0f3b5e71744355cd15cf1d2c/login.php.
(8) github.com. https://github.com/JeevanNaikOP/Learning_NodeJS/tree/9203264c31f105b1fdc0a9e17e7efef7dfa3647d/Tutorials%2FCreating_Table%2Fcreate_Table_primarykey.js.
(9) github.com. https://github.com/matejpolj/nodejs-tutorial/tree/8d957e542257dd543bb042437b2164b45ab3011a/mysql_demo_select.js.
(10) github.com. https://github.com/Shubhrmcf07/weather-app/tree/a4ed9743c251c69f614ad33c2a597eacb1bf3581/demo_module.js.

### Can JavaScript connect to DB when executing in a browser?

While JavaScript running in a browser can send requests to a server that interacts with a database, it **cannot directly connect to a database** for security and architecture reasons. 

This is because allowing client-side JavaScript to directly access a database would expose sensitive information like database credentials to the client, which could be a serious security risk. 

Instead, the common practice is to have JavaScript in the browser send HTTP requests (using fetch, axios, or other libraries) to a server-side application (like Node.js, Django, etc.). The server-side application then interacts with the database, retrieves the data, and sends it back to the client-side JavaScript. This way, the database credentials and other sensitive information are kept secure on the server. 

Here's a simple example of how you might do this using fetch to send a GET request to a server-side application:

```javascript
fetch('https://yourserver.com/api/data')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

In this example, `'https://yourserver.com/api/data'` would be the URL of your server-side application that interacts with the database. The server-side application would handle the database connection, query the database, and then return the data as a response to the fetch request. The data is then logged to the console in the browser. 

Remember, the actual implementation will depend on the specifics of your application and the database you're using.