### question

I executed the following docker command to run sqlplus against oracle and it appears to be looking for a TNS name. How do I determine what tnsnames my Docker container is using when I ran it as follows:

```sh
docker exec -it distracted_hellman
```

and a

```sh
docker ps -a
```

shows:
ba5e6cf2fdbf doctorkirk oracle-19c:latest "/bin/sh -c 'exec $O…" 43 minutes ago Up 43 minutes (healthy) 1521/tcp distracted_hellman

### answer

To determine the TNS names your Docker container is using, you can check the `tnsnames.ora` file inside the container. Here are the steps:

1. **Access the Docker Container**:

   ```sh
   docker exec -it distracted_hellman /bin/bash
   ```

2. **Locate the `tnsnames.ora` File**:
   The `tnsnames.ora` file is typically located in the `$ORACLE_HOME/network/admin` directory. You can find it using the following command:

   ```sh
   find $ORACLE_HOME -name tnsnames.ora
   ```

3. **View the `tnsnames.ora` File**:
   Once you locate the file, you can view its contents using `cat`:
   ```sh
   cat $ORACLE_HOME/network/admin/tnsnames.ora
   ```

The `tnsnames.ora` file contains network service names mapped to connect descriptors. Look for entries that define the service names and connection details.

If you need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/13/2024
(1) SID, PDB, tnsnames, and db_name confusion #480 - GitHub. https://github.com/oracle/docker-images/issues/480.
(2) Net Service Names and the tnsnames.ora File - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/rilin/net-service-names-and-the-tnsnames-ora-file.html.
(3) How to add custom tnsnames.ora to Oracle 12.1.0.2 se #1181 - GitHub. https://github.com/oracle/docker-images/issues/1181.
(4) docker-oracle-client/tnsnames.ora at master - GitHub. https://github.com/grenadejumper/docker-oracle-client/blob/master/tnsnames.ora.

### question

I found this tnsnames.ora file:
ORCLCDB=localhost:1521/ORCLCDB
ORCLCDB=
(DESCRIPTION =
(ADDRESS = (PROTOCOL = TCP)(HOST = 0.0.0.0)(PORT = 1521))
(CONNECT_DATA =
(SERVER = DEDICATED)
(SERVICE_NAME = ORCLCDB)
)
)

How do I execute docker to run sqlplus to connect as a DBA so I can create a schema?

### answer

Great! Now that you have the `tnsnames.ora` file, you can use the following steps to connect to the Oracle database as a DBA using `sqlplus`:

1. **Access the Docker Container**:

   ```sh
   docker exec -it distracted_hellman /bin/bash
   ```

2. **Run `sqlplus` to Connect as a DBA**:
   Use the `sqlplus` command to connect to the database. Replace `your_password` with the actual password for the `SYS` user:

   ```sh
   sqlplus sys/your_password@ORCLCDB as sysdba
   ```

3. **Create a Schema**:
   Once connected, you can create a new schema (user) with the following SQL commands:
   ```sql
   CREATE USER new_schema IDENTIFIED BY new_password;
   GRANT CONNECT, RESOURCE, DBA TO new_schema;
   ```

This will create a new schema named `new_schema` with the specified password and grant it the necessary privileges.

If you have any more questions or need further assistance, feel free to ask!

### question

I didn't create a password for SYS is there a default password for the your_password. I just ran a docker container for Oracle 19c. Would that have a default password associated with it?

```sh
sqlplus sys/your_password@ORCLCDB as sysdba
```

So, I can run the previous sqlplus command and connect to the DB.

### answer

When you run an Oracle 19c Docker container, the `SYS` password is typically auto-generated unless you specify it during the container setup. You can set the password using the `ORACLE_PWD` environment variable when you start the container. Here's an example command to set the password:

```sh
docker run -d --name oracle-db -p 1521:1521 -e ORACLE_PWD=your_password container-registry.oracle.com/database/enterprise:latest
```

If you didn't set the password during the initial setup, you might need to check the container logs to find the auto-generated password. You can do this with the following command:

```sh
docker logs oracle-db | grep "PASSWORD"
```

This should display the auto-generated password for the `SYS` user.

Once you have the password, you can use the `sqlplus` command to connect to the database as a DBA:

```sh
sqlplus sys/your_password@ORCLCDB as sysdba
```

Let me know if you need any more help!

Source: Conversation with Copilot, 9/13/2024
(1) How to Deploy an Oracle Database 19c Docker Container. https://seanstacey.org/deploying-an-oracle-database-19c-as-a-docker-container/2020/09/.
(2) Oracle Database Enterprise Edition Installation Guide for Docker Containers. https://docs.oracle.com/en/database/oracle/oracle-database/21/deeck/index.html.
(3) GitHub - steveswinsburg/oracle19c-docker: A docker container for .... https://github.com/steveswinsburg/oracle19c-docker.
(4) OracleDB 19c Docker Container - GitHub. https://github.com/vittorioexp/oracledb-docker-container.
(5) How to run Oracle Database in a Docker Container using Docker Compose. https://dev.to/docker/how-to-run-oracle-database-in-a-docker-container-using-docker-compose-1c9b.
(6) undefined. https://docs.oracle.com/en/database/oracle/oracle-database/19/ladbi/running-rpm-packages-to-install-oracle-database.html.
