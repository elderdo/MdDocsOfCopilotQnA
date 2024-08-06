### question
When using C# .NET 8 and deploying an application to Azure, how do you connect to an Oracle DB that is on the network and is Ok when connecting with TNS.  How do you help Azure connect to Oracle?  Is there a way to give it the TNS information so it can access an Oracle DB that is not in the Azure environment?

### answer
To connect a C# .NET 8 application deployed on Azure to an Oracle DB using TNS, follow these steps:

1. **Install Oracle Managed Data Access**:
   - Add the `Oracle.ManagedDataAccess` NuGet package to your project.

2. **Set Up TNS Information**:
   - Create a `tnsnames.ora` file with the necessary TNS entries. This file contains the network service names and their corresponding connect descriptors.

3. **Configure Environment Variables**:
   - Set the `TNS_ADMIN` environment variable to the directory containing your `tnsnames.ora` file. This can be done in the Azure App Service configuration settings.

4. **Connection String**:
   - Use the TNS alias in your connection string. For example:
     ```csharp
     var connectionString = "User Id=myUser;Password=myPassword;Data Source=myTNSAlias";
     using (var connection = new OracleConnection(connectionString))
     {
         connection.Open();
         // Your code here
     }
     ```

5. **Deploy to Azure**:
   - Ensure the `tnsnames.ora` file is included in your deployment package and that the `TNS_ADMIN` environment variable is correctly set in the Azure App Service.

By following these steps, your Azure-deployed application should be able to connect to the Oracle DB using TNS information⁴⁵⁷.

⁴: https://stackoverflow.com/questions/41459631/how-to-connect-to-an-oracle-database-connection-from-net-core
⁵: https://stackoverflow.com/questions/55827110/how-do-i-setup-tnsnames-ora-in-asp-net-core-web-application
⁷: https://learn.microsoft.com/en-us/answers/questions/1344398/azure-data-factory-oracle-tns-connection

Is there anything specific you'd like to know more about?

Source: Conversation with Copilot, 8/6/2024
(1) How to connect to an Oracle database Connection from .Net Core. https://stackoverflow.com/questions/41459631/how-to-connect-to-an-oracle-database-connection-from-net-core.
(2) How do I setup tnsnames.ora in asp.net core web application?. https://stackoverflow.com/questions/55827110/how-do-i-setup-tnsnames-ora-in-asp-net-core-web-application.
(3) Azure Data Factory Oracle TNS Connection - Microsoft Q&A. https://learn.microsoft.com/en-us/answers/questions/1344398/azure-data-factory-oracle-tns-connection.
(4) Getting started with Oracle Database@Azure. https://www.youtube.com/watch?v=OxKRqc_9UoI.
(5) Understanding TNSNAMES.ORA file in oracle database (and how to generate it). https://www.youtube.com/watch?v=8ad8OhbN5-Y.
(6) How to connect to Oracle Database using Azure Data Studio. https://www.youtube.com/watch?v=8vlAlU9_LAg.
(7) Java JDBC - How to connect to Oracle using tnsnames.ora. https://stackoverflow.com/questions/14194750/java-jdbc-how-to-connect-to-oracle-using-tnsnames-ora.
(8) Azure Data Factory Oracle connection through self Hostes IR using .... https://learn.microsoft.com/en-us/answers/questions/607650/azure-data-factory-oracle-connection-through-self.
(9) undefined. http://www.oracle.com/technetwork/topics/dotnet/downloads/net-downloads-160392.html.
(10) undefined. https://www.nuget.org/packages/Oracle.ManagedDataAccess.Core.
(11) undefined. https://github.com/ericmend/oracleClientCore-2.0.
(12) undefined. https://github.com/LinqDan/oracleclientcore.
(13) undefined. https://docs.oracle.com/en/database/oracle/oracle-data-access-components/18.3/odpnt/InstallCoreConfiguration.html.

### question
Can you point to a Github repository that does this kind of connection from Azure to Oracle?

### answer

To connect a C# .NET 8 application deployed on Azure to an Oracle DB using TNS, follow these steps:

1. **Install Oracle Managed Data Access**:
   - Add the `Oracle.ManagedDataAccess` NuGet package to your project.

2. **Set Up TNS Information**:
   - Create a `tnsnames.ora` file with the necessary TNS entries. This file contains the network service names and their corresponding connect descriptors.

3. **Configure Environment Variables**:
   - Set the `TNS_ADMIN` environment variable to the directory containing your `tnsnames.ora` file. This can be done in the Azure App Service configuration settings.

4. **Connection String**:
   - Use the TNS alias in your connection string. For example:
     ```csharp
     var connectionString = "User Id=myUser;Password=myPassword;Data Source=myTNSAlias";
     using (var connection = new OracleConnection(connectionString))
     {
         connection.Open();
         // Your code here
     }
     ```

5. **Deploy to Azure**:
   - Ensure the `tnsnames.ora` file is included in your deployment package and that the `TNS_ADMIN` environment variable is correctly set in the Azure App Service.

For a practical example, you can refer to the [microsoft/azure-oracle](https://github.com/microsoft/azure-oracle) GitHub repository¹. This repository contains automation scripts and terraform modules to simplify Oracle workload deployments on Azure, including setting up connectivity between Azure and Oracle.

¹: https://github.com/microsoft/azure-oracle

Is there anything specific you'd like to know more about?

Source: Conversation with Copilot, 8/6/2024
(1) GitHub - microsoft/azure-oracle: Automation scripts to simplify Oracle .... https://github.com/microsoft/azure-oracle.
(2) How to connect to an Oracle database Connection from .Net Core. https://stackoverflow.com/questions/41459631/how-to-connect-to-an-oracle-database-connection-from-net-core.
(3) Oracle TNS connection for jdbc · GitHub. https://gist.github.com/andysteel/1ea56e06b4942f4185e2a5e5f2d13d65.
(4) establish Oracle connection via .NET Core from Powershell using TNS .... https://gist.github.com/stelf/46cdcae9bf5c807c3c7b7805c8d2958a.
(5) undefined. http://www.oracle.com/technetwork/topics/dotnet/downloads/net-downloads-160392.html.
(6) undefined. https://www.nuget.org/packages/Oracle.ManagedDataAccess.Core.
(7) undefined. https://github.com/ericmend/oracleClientCore-2.0.
(8) undefined. https://github.com/LinqDan/oracleclientcore.