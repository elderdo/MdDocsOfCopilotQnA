### question

What is Oracle's stored procedure UTL_HTTP.set_header used for when it is invoked from another Oracle stored procedure? Can you give me an example and indicate what permissions are required to invoke the UTL_HTTP package's procedures?

### answer

The `UTL_HTTP.set_header` procedure in Oracle is used to set HTTP request headers when making HTTP calls from PL/SQL. This is particularly useful when you need to specify custom headers, such as authentication tokens, content types, or other metadata required by the web service you are interacting with.

### Example Usage

Here's an example of how to use `UTL_HTTP.set_header` within a stored procedure:

```sql
CREATE OR REPLACE PROCEDURE call_web_service AS
  l_http_request  UTL_HTTP.req;
  l_http_response UTL_HTTP.resp;
  l_url           VARCHAR2(32767) := 'http://example.com/api/resource';
  l_response_text VARCHAR2(32767);
BEGIN
  -- Initialize the HTTP request
  l_http_request := UTL_HTTP.begin_request(l_url, 'GET', 'HTTP/1.1');

  -- Set custom headers
  UTL_HTTP.set_header(l_http_request, 'User-Agent', 'Mozilla/4.0');
  UTL_HTTP.set_header(l_http_request, 'Authorization', 'Bearer your_token_here');

  -- Get the response
  l_http_response := UTL_HTTP.get_response(l_http_request);

  -- Read the response
  BEGIN
    LOOP
      UTL_HTTP.read_text(l_http_response, l_response_text, 32766);
      DBMS_OUTPUT.put_line(l_response_text);
    END LOOP;
  EXCEPTION
    WHEN UTL_HTTP.end_of_body THEN
      UTL_HTTP.end_response(l_http_response);
  END;
END;
/
```

### Required Permissions

To use the `UTL_HTTP` package, you need to ensure that the appropriate network access control list (ACL) permissions are granted. Here are the steps to grant the necessary permissions:

1. **Create an ACL**:

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.create_acl(
       acl         => 'http_acl.xml',
       description => 'HTTP Access',
       principal   => 'YOUR_USERNAME',
       is_grant    => TRUE,
       privilege   => 'connect'
     );
   END;
   /
   ```

2. **Assign the ACL to a host**:

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.assign_acl(
       acl  => 'http_acl.xml',
       host => 'example.com'
     );
   END;
   /
   ```

3. **Verify the ACL**:
   ```sql
   SELECT * FROM DBA_NETWORK_ACLS;
   ```

These steps ensure that your Oracle user has the necessary permissions to make HTTP requests using the `UTL_HTTP` package¹².

If you have any more questions or need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/17/2024
(1) UTL_HTTP and SSL (HTTPS) using Oracle Wallets. https://oracle-base.com/articles/misc/utl_http-and-ssl.
(2) UTL_HTTP package usage - Ask TOM. https://asktom.oracle.com/ords/f?p=100:11:0::::P11_QUESTION_ID:285215954607.
(3) How to Pass a Username/Password to a Secure Web Site Using UTL_HTTP?. https://support.oracle.com/knowledge/Oracle%20Database%20Products/804293_1.html.
(4) 266 UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/UTL_HTTP.html.
(5) undefined. https://oracle-base.com/.
(6) UTL_HTTP to Call a Web Service from PL/SQL - Oratable. https://www.oratable.com/utl_http/.
(7) Using oracle utl_http for http requests - Oradev.com. https://www.oradev.com/utlhttp.html.
(8) Understanding UTL_HTTP With Help of Samples: PART I - How to Perform .... https://support.oracle.com/knowledge/Oracle%20Database%20Products/1052796_1.html.
(9) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://bing.com/search?q=UTL_HTTP+package+usage+permissions.
(10) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://knowledge.informatica.com/s/article/502561.
(11) B.1 Appendix: Troubleshooting UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/23/adfns/troubleshooting-utl_http.html.

### quesiotn

How can you tell if you have permission to use Oracle's UTL_HTTP package? Is there any query or with the Oracle Toad utility can it show the permissions for the current user?

### answer

To check if you have permission to use Oracle's `UTL_HTTP` package, you can query the database to see if the necessary network access control list (ACL) permissions are granted. Here are a couple of ways to do this:

### Using SQL Query

You can run the following SQL query to check the ACL assignments for your user:

```sql
SELECT acl, principal, privilege, is_grant
FROM dba_network_acls
WHERE principal = 'YOUR_USERNAME';
```

This query will show you the ACLs assigned to your user and the privileges granted.

### Using Oracle Toad

In Oracle Toad, you can view the permissions for the current user by following these steps:

1. **Connect to your database** using Toad.
2. **Navigate to the "Schema Browser"**.
3. **Select "Users"** from the left-hand side.
4. **Find and select your user** from the list.
5. **View the "Grants" tab** to see the privileges and roles assigned to your user.

### Example of Checking Permissions

Here's an example of how you might check if your user has the necessary permissions to use `UTL_HTTP`:

1. **Create an ACL** (if not already created):

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.create_acl(
       acl         => 'utl_http_acl.xml',
       description => 'HTTP Access',
       principal   => 'YOUR_USERNAME',
       is_grant    => TRUE,
       privilege   => 'connect'
     );
   END;
   /
   ```

2. **Assign the ACL to a host**:

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.assign_acl(
       acl  => 'utl_http_acl.xml',
       host => 'example.com'
     );
   END;
   /
   ```

3. **Verify the ACL**:
   ```sql
   SELECT * FROM dba_network_acls;
   ```

These steps ensure that your Oracle user has the necessary permissions to make HTTP requests using the `UTL_HTTP` package¹².

If you have any more questions or need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/17/2024
(1) 266 UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/UTL_HTTP.html.
(2) Configuring UTL_HTTP access using certificates and an Oracle wallet. https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Oracle.Concepts.ONA.html.
(3) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://bing.com/search?q=Oracle+UTL_HTTP+package+permissions+query.
(4) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://knowledge.informatica.com/s/article/502561.
(5) Permissions for Packages DBMS_RANDOM, UTL_FILE, UTL_HTTP, UTL_SMTP and .... https://support.oracle.com/knowledge/Middleware/470366_1.html.
(6) Managing Fine-Grained Access in PL/SQL Packages and Types. https://docs.oracle.com/en//database/oracle/oracle-database/21/dbseg/managing-fine-grained-access-in-pl-sql-packages-and-types.html.

### question

What should be the output of this query SELECT \* FROM dba_network_acls; If the user has permission to execute procedures for Oracle package UTL_HTTP?

### answer

When you run the query `SELECT * FROM dba_network_acls;`, the output will show the details of the Access Control Lists (ACLs) that are configured in your Oracle database. If the user has permission to execute procedures for the `UTL_HTTP` package, you should see entries that include the relevant hostnames, ports, and ACL files.

### Example Output

Here is an example of what the output might look like:

| HOST               | LOWER_PORT | UPPER_PORT | ACL                        | ACLID | ACL_OWNER |
| ------------------ | ---------- | ---------- | -------------------------- | ----- | --------- |
| example.com        | 0          | 65535      | /sys/acls/utl_http_acl.xml | 12345 | SYS       |
| anotherexample.com | 0          | 65535      | /sys/acls/utl_http_acl.xml | 67890 | SYS       |

### Explanation

- **HOST**: The hostname or IP address for which the ACL is configured.
- **LOWER_PORT**: The lower bound of the port range.
- **UPPER_PORT**: The upper bound of the port range.
- **ACL**: The path to the ACL file.
- **ACLID**: The unique identifier for the ACL.
- **ACL_OWNER**: The owner of the ACL.

### Checking Specific Permissions

To check if a specific user has the necessary permissions, you can also query the `DBA_NETWORK_ACL_PRIVILEGES` view:

```sql
SELECT acl, principal, privilege, is_grant
FROM dba_network_acl_privileges
WHERE principal = 'YOUR_USERNAME';
```

This will show you the specific privileges granted to the user, such as `connect` and `resolve`, which are required for using the `UTL_HTTP` package¹².

If you have any more questions or need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/17/2024
(1) What is the Access Control List ( ACL ) and How to Create and Grant an .... https://ittutorial.org/oracle-acl-the-access-control-list-acl-and-how-to-create-and-grant-an-acl-in-oracle-database/.
(2) 116 DBMS_NETWORK_ACL_ADMIN - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/DBMS_NETWORK_ACL_ADMIN.html.
(3) Oracle 11g Acces Control List ORA-24247. - ORACLE-SOLUTIONS. http://oracle-solutions.com/oracle-11g-acces-control-list-ora-24247-2/.
(4) ORA-24247: network access denied by access control list (ACL). https://ittutorial.org/ora-24247-network-access-denied-by-access-control-list-acl/.
(5) 266 UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/UTL_HTTP.html.
(6) Configuring UTL_HTTP access using certificates and an Oracle wallet. https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Oracle.Concepts.ONA.html.
(7) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://bing.com/search?q=Oracle+UTL_HTTP+package+permissions+query.
(8) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://knowledge.informatica.com/s/article/502561.
(9) Permissions for Packages DBMS_RANDOM, UTL_FILE, UTL_HTTP, UTL_SMTP and .... https://support.oracle.com/knowledge/Middleware/470366_1.html.
(10) Managing Fine-Grained Access in PL/SQL Packages and Types. https://docs.oracle.com/en//database/oracle/oracle-database/21/dbseg/managing-fine-grained-access-in-pl-sql-packages-and-types.html.

### question

Once you have permissions for Oracle's UTL_HTTP package how is it used and what is the purpose of its set_header procedure? Can you give me an example of how UTL_HTTP is used in conjunction with its set_header procedure?

### answer

The `UTL_HTTP` package in Oracle allows PL/SQL programs to make HTTP requests and interact with web services. The `set_header` procedure within this package is used to set HTTP request headers, which are essential for specifying metadata such as content type, authentication tokens, and other necessary information for the web service.

### Purpose of `UTL_HTTP.set_header`

The `set_header` procedure is used to add or modify HTTP headers in the request. This is crucial when you need to:

- Specify the content type (e.g., `application/json`, `text/xml`).
- Include authentication tokens (e.g., `Bearer token`).
- Set custom headers required by the web service.

### Example Usage

Here's an example of how to use the `UTL_HTTP` package along with the `set_header` procedure to make an HTTP GET request to a web service:

```sql
CREATE OR REPLACE PROCEDURE call_web_service AS
  l_http_request  UTL_HTTP.req;
  l_http_response UTL_HTTP.resp;
  l_url           VARCHAR2(32767) := 'http://example.com/api/resource';
  l_response_text VARCHAR2(32767);
BEGIN
  -- Initialize the HTTP request
  l_http_request := UTL_HTTP.begin_request(l_url, 'GET', 'HTTP/1.1');

  -- Set custom headers
  UTL_HTTP.set_header(l_http_request, 'User-Agent', 'Mozilla/4.0');
  UTL_HTTP.set_header(l_http_request, 'Authorization', 'Bearer your_token_here');

  -- Get the response
  l_http_response := UTL_HTTP.get_response(l_http_request);

  -- Read the response
  BEGIN
    LOOP
      UTL_HTTP.read_text(l_http_response, l_response_text, 32766);
      DBMS_OUTPUT.put_line(l_response_text);
    END LOOP;
  EXCEPTION
    WHEN UTL_HTTP.end_of_body THEN
      UTL_HTTP.end_response(l_http_response);
  END;
END;
/
```

### Required Permissions

To use the `UTL_HTTP` package, you need to ensure that the appropriate network access control list (ACL) permissions are granted. Here are the steps to grant the necessary permissions:

1. **Create an ACL**:

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.create_acl(
       acl         => 'http_acl.xml',
       description => 'HTTP Access',
       principal   => 'YOUR_USERNAME',
       is_grant    => TRUE,
       privilege   => 'connect'
     );
   END;
   /
   ```

2. **Assign the ACL to a host**:

   ```sql
   BEGIN
     DBMS_NETWORK_ACL_ADMIN.assign_acl(
       acl  => 'http_acl.xml',
       host => 'example.com'
     );
   END;
   /
   ```

3. **Verify the ACL**:
   ```sql
   SELECT * FROM DBA_NETWORK_ACLS;
   ```

These steps ensure that your Oracle user has the necessary permissions to make HTTP requests using the `UTL_HTTP` package²⁶.

If you have any more questions or need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/17/2024
(1) 266 UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/UTL_HTTP.html.
(2) UTL_HTTP and SSL (HTTPS) using Oracle Wallets. https://oracle-base.com/articles/misc/utl_http-and-ssl.
(3) UTL_HTTP package usage - Ask TOM. https://asktom.oracle.com/ords/f?p=100:11:0::::P11_QUESTION_ID:285215954607.
(4) UTL_HTTP to Call a Web Service from PL/SQL - Oratable. https://www.oratable.com/utl_http/.
(5) Using oracle utl_http for http requests - Oradev.com. https://www.oradev.com/utlhttp.html.
(6) Understanding UTL_HTTP With Help of Samples: PART I - How to Perform .... https://support.oracle.com/knowledge/Oracle%20Database%20Products/1052796_1.html.
(7) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://bing.com/search?q=UTL_HTTP+package+usage+permissions.
(8) FAQ: What is the permission needed to use the "UTL_HTTP" package from .... https://knowledge.informatica.com/s/article/502561.
(9) B.1 Appendix: Troubleshooting UTL_HTTP - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/23/adfns/troubleshooting-utl_http.html.
(10) undefined. https://oracle-base.com/.
