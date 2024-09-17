### question

Once I have docker running Oracle and I have found the tnsname, how do I run docker and connect with sqlplus as the dba?

### answer

To connect to your Oracle database running in a Docker container using `sqlplus` as the DBA, follow these steps:

1. **Start the Docker Container**:
   Ensure your Oracle Docker container is running. You can start it with:

   ```bash
   docker run -d --name oracle-db -p 1521:1521 container-registry.oracle.com/database/enterprise:latest
   ```

2. **Access the Docker Container**:
   Use the `docker exec` command to access the container's shell:

   ```bash
   docker exec -it oracle-db /bin/bash
   ```

3. **Connect with `sqlplus`**:
   Once inside the container, you can connect to the database using `sqlplus`. For example:
   ```bash
   sqlplus sys/SYS_PASSWORD@localhost:1521/YOUR_SID as sysdba
   ```
   Replace `SYS_PASSWORD` with the actual password for the `SYS` user, and `YOUR_SID` with your Oracle SID (System Identifier).

Here's a quick summary of the commands:

```bash
docker run -d --name oracle-db -p 1521:1521 container-registry.oracle.com/database/enterprise:latest
docker exec -it oracle-db /bin/bash
sqlplus sys/SYS_PASSWORD@localhost:1521/YOUR_SID as sysdba
```

This should get you connected to your Oracle database as the DBA using `sqlplus`¹²³.

Is there anything else you'd like to know or any specific issues you're encountering?

Source: Conversation with Copilot, 9/16/2024
(1) How to Install Oracle Database 21c on Docker. https://docs.oracle.com/en/database/oracle/oracle-database/21/deeck/oracle-database-enterprise-edition-installation-guide-docker-containers-oracle-linux.pdf.
(2) How to run Oracle Database in a Docker Container using Docker Compose. https://dev.to/docker/how-to-run-oracle-database-in-a-docker-container-using-docker-compose-1c9b.
(3) Oracle 19c with Docker - DEV Community. https://dev.to/pazyp/oracle-19c-with-docker-36m5.
(4) How to connect to SQL Plus in oracle xe running in a docker container. https://dev.to/codever/how-to-connect-to-sql-plus-in-oracle-xe-running-in-a-docker-container-3391.
(5) Oracle 如何在 Docker 容器中使用 sqlplus 连接 Oracle 数据库. https://geek-docs.com/oracle/oracle-questions/237_oracle_how_to_use_sqlplus_on_oracle_database_inside_a_docker_container.html.
(6) undefined. https://container-registry.oracle.com/.
(7) undefined. https://www.oracle.com/database/technologies/instant-client/downloads.
