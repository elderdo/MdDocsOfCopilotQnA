### question

What are the ways that Oracle's SQLPlus finds an script file when you enter @filename? What is the difference between @ and @@ for SQLPlus?

### answer

When you enter `@filename` in Oracle's SQL\*Plus, it searches for the script file in the following locations:

1. **Current Directory**: The directory from which SQL\*Plus was started.
2. **SQLPATH**: Directories specified in the SQLPATH environment variable.
3. **ORACLE_PATH**: Directories specified in the ORACLE_PATH environment variable.

The difference between `@` and `@@` in SQL\*Plus is:

- **@**: Runs a script in the current directory or a specified path.
- **@@**: Runs a script that is in the same directory as the script that called it. This is useful for nested command files⁹.

Source: Conversation with Copilot, 9/27/2024
(1) undefined. https://www.orafaq.com/wiki/SQL.
(2) SQL*Plus FAQ - Oracle FAQ - orafaq.com. https://www.orafaq.com/wiki/SQL*Plus_FAQ.
(3) SQLPlus vs SQL – Coder Tutorial. https://www.codertutor.com/sqlplus-vs-sql.html.
(4) SQL*Plus Command Reference - GeeksforGeeks. https://www.geeksforgeeks.org/sqlplus-command-reference/.
(5) 6 Using Substitution Variables - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/21/sqpug/using-substitution-variables-sqlplus.html.
(6) SQL*Plus Substitution Variables - DEFINE variables and parameters in .... https://blogs.oracle.com/opal/post/sql*plus-substitution-variables-define-variables-and-parameters-in-sql-queries.
(7) Possible to retrieve current script name in SQL (from SQL Plus). https://forums.oracle.com/ords/apexds/map/tech/developers/discussion/503128/possible-to-retrieve-current-script-name-in-sql-from-sql-plus.
(8) 5 Using Scripts in SQL\*Plus - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/sqpug/using-scripts-in-SQL-Plus.html.
(9) On Running Oracle Scripts with Relative Paths. https://www.thatjeffsmith.com/archive/2018/02/on-running-oracle-scripts-with-relative-paths/.
(10) undefined. https://blogs.oracle.com/opal/post/sql.
