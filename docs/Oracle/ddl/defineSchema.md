### question
For Oracle 19c write the ddl to create a schema with owner ipdm_owner.

### answer
```sql
CREATE USER ipdm_owner IDENTIFIED BY your_password;

GRANT CONNECT, RESOURCE TO ipdm_owner;

ALTER USER ipdm_owner DEFAULT TABLESPACE users TEMPORARY TABLESPACE temp;

GRANT UNLIMITED TABLESPACE TO ipdm_owner;
```

Replace `your_password` with a secure password of your choice. This script creates a new user `ipdm_owner`, grants necessary privileges, and sets the default and temporary tablespaces.