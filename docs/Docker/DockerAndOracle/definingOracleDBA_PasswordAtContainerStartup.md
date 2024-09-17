### question

When starting a docker container for Oracle can you specify the Oracle password for SYS, SYSTEM and PDBADMIN?

### answer

Yes, you can specify the passwords for the `SYS`, `SYSTEM`, and `PDBADMIN` users when starting an Oracle Docker container by setting environment variables. Here's how you can do it:

1. **Using Environment Variables**:
   When you run the Docker container, you can pass the passwords as environment variables. For example:

   ```bash
   docker run -d --name oracle-db \
     -p 1521:1521 -p 5500:5500 \
     -e ORACLE_PWD=YourSysPassword \
     -e ORACLE_SYS_PASSWORD=YourSysPassword \
     -e ORACLE_SYSTEM_PASSWORD=YourSystemPassword \
     -e ORACLE_PDB_ADMIN_PASSWORD=YourPdbAdminPassword \
     container-registry.oracle.com/database/enterprise:latest
   ```

2. **Environment Variables Explanation**:

   - `ORACLE_PWD`: Sets the password for the `SYS`, `SYSTEM`, and `PDBADMIN` users.
   - `ORACLE_SYS_PASSWORD`: Specifically sets the password for the `SYS` user.
   - `ORACLE_SYSTEM_PASSWORD`: Specifically sets the password for the `SYSTEM` user.
   - `ORACLE_PDB_ADMIN_PASSWORD`: Specifically sets the password for the `PDBADMIN` user.

3. **Example Command**:
   Here’s a complete example command:
   ```bash
   docker run -d --name oracle-db \
     -p 1521:1521 -p 5500:5500 \
     -e ORACLE_PWD=MySysPassword123 \
     -e ORACLE_SYS_PASSWORD=MySysPassword123 \
     -e ORACLE_SYSTEM_PASSWORD=MySystemPassword123 \
     -e ORACLE_PDB_ADMIN_PASSWORD=MyPdbAdminPassword123 \
     container-registry.oracle.com/database/enterprise:latest
   ```

This way, you can ensure that the passwords for these users are set when the container starts.

Is there anything else you need help with regarding Docker or Oracle?
