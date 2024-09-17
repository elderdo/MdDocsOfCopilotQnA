### question

How can I export an Oracle table: its ddl and a subset of its data so that I can load that into a test table that will be managed in a Docker container with a C# .NET 8 app that loads it with an xml file. I will want to be able to use an xml file on my Windows PC as input and persist the Oracle table upon completion. I will also want a quick way to truncate and restore the table for easy testing and to allow for a repeatable integration test. Can you outline the steps required to do this?

### answer

Sure! Here’s a step-by-step outline to achieve your goal:

### 1. Export Oracle Table DDL and Data

**Export DDL:**

1. Use Oracle's `DBMS_METADATA` package to extract the DDL of the table.
   ```sql
   SET LONG 10000;
   SELECT DBMS_METADATA.GET_DDL('TABLE', 'YOUR_TABLE_NAME') FROM DUAL;
   ```

**Export Data:**

1. Use Oracle's `SQL*Plus` or `SQL Developer` to export a subset of data to a CSV or XML file.
   ```sql
   SPOOL data_export.csv;
   SELECT * FROM YOUR_TABLE_NAME WHERE ROWNUM <= 1000;
   SPOOL OFF;
   ```

### 2. Create Docker Container for Oracle Database

**Dockerfile:**

1. Create a `Dockerfile` to set up the Oracle Database.
   ```dockerfile
   FROM oracle/database:21c
   ENV ORACLE_SID=ORCLCDB
   ENV ORACLE_PDB=ORCLPDB1
   ENV ORACLE_PWD=YourPassword
   COPY setup.sql /docker-entrypoint-initdb.d/
   ```

**Setup Script:**

1. Create a `setup.sql` script to create the table and load the data.
   ```sql
   -- setup.sql
   CREATE TABLE YOUR_TABLE_NAME (...);
   -- Insert data from CSV or XML
   ```

### 3. Create .NET 8 Application

**Project Setup:**

1. Create a new .NET 8 project.
   ```bash
   dotnet new console -n OracleDataLoader
   cd OracleDataLoader
   ```

**Add NuGet Packages:**

1. Add necessary NuGet packages.
   ```bash
   dotnet add package Oracle.ManagedDataAccess.Core
   dotnet add package System.Xml
   ```

**Code to Load XML Data:**

1. Write C# code to load data from XML and insert it into the Oracle table.

   ```csharp
   using System;
   using System.Data;
   using Oracle.ManagedDataAccess.Client;
   using System.Xml;

   class Program
   {
       static void Main(string[] args)
       {
           string connString = "User Id=your_user;Password=your_password;Data Source=your_db";
           using (OracleConnection conn = new OracleConnection(connString))
           {
               conn.Open();
               XmlDocument doc = new XmlDocument();
               doc.Load("data.xml");

               foreach (XmlNode node in doc.DocumentElement)
               {
                   string sql = "INSERT INTO YOUR_TABLE_NAME (col1, col2) VALUES (:col1, :col2)";
                   using (OracleCommand cmd = new OracleCommand(sql, conn))
                   {
                       cmd.Parameters.Add(new OracleParameter("col1", node["col1"].InnerText));
                       cmd.Parameters.Add(new OracleParameter("col2", node["col2"].InnerText));
                       cmd.ExecuteNonQuery();
                   }
               }
           }
       }
   }
   ```

### 4. Dockerize the .NET Application

**Dockerfile:**

1. Create a `Dockerfile` for your .NET application.

   ```dockerfile
   FROM mcr.microsoft.com/dotnet/runtime:8.0 AS base
   WORKDIR /app

   FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
   WORKDIR /src
   COPY ["OracleDataLoader/OracleDataLoader.csproj", "OracleDataLoader/"]
   RUN dotnet restore "OracleDataLoader/OracleDataLoader.csproj"
   COPY . .
   WORKDIR "/src/OracleDataLoader"
   RUN dotnet build "OracleDataLoader.csproj" -c Release -o /app/build

   FROM build AS publish
   RUN dotnet publish "OracleDataLoader.csproj" -c Release -o /app/publish

   FROM base AS final
   WORKDIR /app
   COPY --from=publish /app/publish .
   ENTRYPOINT ["dotnet", "OracleDataLoader.dll"]
   ```

### 5. Truncate and Restore Table for Testing

**Truncate Table:**

1. Use a simple SQL command to truncate the table.
   ```sql
   TRUNCATE TABLE YOUR_TABLE_NAME;
   ```

**Restore Table:**

1. Re-run the .NET application to reload data from the XML file.

### 6. Running Everything

**Build and Run Docker Containers:**

1. Build and run the Oracle Database container.

   ```bash
   docker build -t oracle-db .
   docker run -d -p 1521:1521 --name oracle-db oracle-db
   ```

2. Build and run the .NET application container.
   ```bash
   docker build -t oracle-data-loader .
   docker run --network="host" oracle-data-loader
   ```

This setup allows you to export the Oracle table's DDL and data, load it into a Docker-managed Oracle database, and use a .NET 8 application to load data from an XML file. You can easily truncate and restore the table for repeatable integration tests. If you have any questions or need further details, feel free to ask!

Source: Conversation with Copilot, 9/17/2024
(1) Containerize an app with dotnet publish - .NET | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/core/docker/publish-as-container.
(2) Containerize an app with Docker tutorial - .NET | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/core/docker/build-container.
(3) Oracle Database Development with Docker. https://blogs.oracle.com/coretec/post/oracle-database-with-docker.
(4) How to Install Oracle Database 21c on Docker. https://docs.oracle.com/en/database/oracle/oracle-database/21/deeck/oracle-database-enterprise-edition-installation-guide-docker-containers-oracle-linux.pdf.
(5) How to Deploy an Oracle Database 19c Docker Container. https://seanstacey.org/deploying-an-oracle-database-19c-as-a-docker-container/2020/09/.
(6) Oracle Hands on Lab: Containerized Development with Docker on .... https://go.oracle.com/dockerhol.
(7) Deliver Oracle Database 18c Express Edition in Containers - Oracle Blogs. https://blogs.oracle.com/connect/post/deliver-oracle-database-18c-express-edition-in-containers.
(8) undefined. https://download.docker.com/linux/centos/docker-ce.repo.
