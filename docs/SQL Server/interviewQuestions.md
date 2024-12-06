### Question
What are typical interview questions for T-SQL & SQL Server ?

### Answer
Here are some typical interview questions for T-SQL and SQL Server, along with brief explanations:

### **T-SQL Interview Questions**
1. **What is the difference between a clustered and a non-clustered index?**
   - A clustered index determines the physical order of data in a table, while a non-clustered index creates a separate object within the database holding a sorted list of references pointing to the physical data rows.

2. **Can you explain the use of `CROSS JOIN`?**
   - A `CROSS JOIN` combines all rows from two or more tables, returning the Cartesian product of the sets. It's useful when you need to generate all possible combinations of rows from the tables.

3. **What are the differences between `SQL` and `T-SQL`?**
   - SQL is a standard query language for managing and manipulating relational databases, while T-SQL is Microsoft's proprietary extension to SQL, adding procedural programming features and support for enterprise-level applications.

4. **How do you handle transactions in T-SQL?**
   - Transactions are managed using `BEGIN TRANSACTION`, `COMMIT TRANSACTION`, and `ROLLBACK TRANSACTION` to ensure data integrity and consistency.

5. **What is a `VIEW` in SQL Server?**
   - A `VIEW` is a virtual table representing the result of a SELECT query. It can be used to simplify complex queries and provide a level of abstraction.

### **SQL Server Interview Questions**
1. **What is SQL Server Agent and how is it used?**
   - SQL Server Agent is a Microsoft Windows service that executes scheduled administrative tasks, such as backups, database maintenance, and running SQL Server Integration Services (SSIS) packages.

2. **How do you monitor SQL Server performance?**
   - Performance can be monitored using tools like SQL Server Profiler, Activity Monitor, Performance Monitor, and Dynamic Management Views (DMVs).

3. **What are the steps to create a new database in SQL Server?**
   - Steps include using SQL Server Management Studio (SSMS) to create a new database, setting initial size, growth settings, and configuring file locations.

4. **How do you implement security in SQL Server?**
   - Security is implemented using roles, permissions, and encryption. This includes creating logins, assigning roles, and configuring permissions for database objects.

5. **What is the purpose of a `Stored Procedure`?**
   - A stored procedure is a precompiled set of SQL statements that can be executed as a single unit, improving performance and reusability.

### **Common Utilities and Tools**
1. **SQL Server Management Studio (SSMS)**: For database management and development.
2. **SQL Server Profiler**: For monitoring and analyzing SQL Server events.
3. **SQL Server Integration Services (SSIS)**: For data integration and workflow applications.
4. **Database Engine Tuning Advisor**: For optimizing SQL Server queries and indexes.
5. **SQL Server Data Tools (SSDT)**: For developing and testing SQL Server databases.

These questions and tools should help you prepare for an interview focused on T-SQL and SQL Server. Do you have any specific areas you'd like to dive deeper into?
