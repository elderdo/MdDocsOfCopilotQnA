### login

login sres.web.boeing.com:6000

### use bemsid and password setup by sres - see $HOME/.m2/settings.xml for password

### docker pull

```docker
docker pull sres.web.boeing.com:6000/oraclelinux:8
```

### starts linux in docker

docker run -it sres.web.boeing.com:6000/oraclelinux:8

### instructions

GitHub Copilot
The file name OracleXE213_Win64.zip likely refers to Oracle Database Express Edition (XE) version 21.3 for 64-bit Windows. Here's a breakdown of the components in the file name:

OracleXE: Indicates that this is Oracle Database Express Edition (XE), which is a free, lightweight, and limited version of Oracle Database.
213: This likely refers to the version number, which is 21.3. Oracle Database versioning typically follows a pattern where the first two digits represent the major version (21), and the third digit represents the release update (3).
Win64: Indicates that this is a 64-bit version for Windows operating systems.
Steps to Set Up Oracle Database Express Edition (XE) in Docker
Since you have the OracleXE213_Win64.zip file, you can create a Docker image that sets up Oracle Database Express Edition (XE) using this zip file. Here’s how you can do it:

Prepare the Dockerfile: Create a new file named Dockerfile and add the following content to it. This example assumes you have the OracleXE213_Win64.zip file available.

```Dockerfile
# Use the oraclelinux:8 base image
FROM oraclelinux:8

# Set environment variables
ENV ORACLE_BASE=/opt/oracle
ENV ORACLE_HOME=$ORACLE_BASE/product/21c/dbhomeXE
ENV ORACLE_SID=XE
ENV PATH=$ORACLE_HOME/bin:$PATH

# Copy Oracle XE installation files to the container
COPY OracleXE213_Win64.zip /tmp/

# Install necessary packages
RUN yum install -y unzip && \
    yum install -y oracle-database-preinstall-21c

# Unzip and install Oracle XE
RUN unzip /tmp/OracleXE213_Win64.zip -d /tmp/ && \
    /tmp/Disk1/runInstaller -silent -responseFile /tmp/response/db_install.rsp && \
    rm -rf /tmp/OracleXE213_Win64.zip /tmp/Disk1

# Expose Oracle ports
EXPOSE 1521 5500

# Start Oracle Database
CMD ["/bin/bash", "-c", "source /etc/profile && /etc/init.d/oracle-xe-21c start && tail -f /dev/null"]
```

Build the Docker Image: Build the Docker image using the Dockerfile you created.

```cli
docker run -d --name oracle-xe \
  -p 1521:1521 -p 5500:5500 \
  my-oracle-xe
```

Check Container Logs: Check the logs to ensure the Oracle Database XE is starting correctly.

```cli
docker logs -f oracle-xe
```

Connect to the Oracle Database: Once the database is up and running, you can connect to it using SQL\*Plus or any other Oracle database client.

```cli
docker exec -it oracle-xe sqlplus sys/YourPassword@XE as sysdba
```

Notes:
Replace OracleXE213_Win64.zip with the actual path to your Oracle XE zip file.
Ensure you have the necessary permissions and licenses to use Oracle Database XE.
Adjust environment variables and paths as needed based on your Oracle Database XE version and installation files.
By following these steps, you can create a Docker image that sets up and runs Oracle Database Express Edition (XE) using the OracleXE213_Win64.zip file.
