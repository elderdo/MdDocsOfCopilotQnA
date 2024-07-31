### Question: For older .Net 4.8 C# ASPX app, I had to add a missing package for Oracle. I added the Oracle.ManagedDataAccess package. Upon running I got this error:

The requested page cannot be accessed because the related configuration data for the page is invalid.
It pointed to this config:
7: <configSections>
8: <section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
9: </configSections>
and highlighted this <section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" /> in red and provided this:
More Information:
This error occurs when there is a problem reading the configuration file for the Web server or Web application. In some cases, the event logs may contain more information about what caused this error.

If you see the text "There is a duplicate 'system.web.extensions/scripting/scriptResourceHandler' section defined", this error is because you are running a .NET Framework 3.5-based application in .NET Framework 4. If you are running WebMatrix, to resolve this problem, go to the Settings node to set the .NET Framework version to ".NET 2". You can also remove the extra sections from the web.config file.

Here is the web.config:

<?xml version="1.0" encoding="utf-8"?>
<!--
  For more information on how to configure your ASP.NET application, please visit
  http://go.microsoft.com/fwlink/?LinkId=169433
  -->
<configuration>
	<configSections>
		<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
	</configSections>
	<connectionStrings>
		<add name="ApplicationServices" connectionString="data source=.\SQLEXPRESS;Integrated Security=SSPI;AttachDBFilename=|DataDirectory|\aspnetdb.mdf;User Instance=true" providerName="System.Data.SqlClient" />
		<add name="ConnectionString" connectionString="DATA SOURCE=amdc17dv_ovpd1292;PASSWORD=cvQJaoEE35kq44cdaa_avsSFF00igp;PERSIST SECURITY INFO=True;USER ID=IRVT_USER" providerName="Oracle.DataAccess.Client" />
		<add name="ConnectionString2" connectionString="Provider=MSDAORA;Data Source=amdc17dv_ovpd1292;Persist Security Info=True;Password=cvQJaoEE35kq44cdaa_avsSFF00igp;User ID=IRVT_USER" providerName="System.Data.OleDb" />
		<add name="ConnectionString3" connectionString="Data Source=irvtp;Persist Security Info=True;User ID=IRVT_USER;Password=cvQJaoEE35kq44cdaa_avsSFF00igp;Unicode=True" providerName="System.Data.OracleClient" />
	</connectionStrings>
	<!--
    For a description of web.config changes see http://go.microsoft.com/fwlink/?LinkId=235367.

    The following attributes can be set on the <httpRuntime> tag.
      <system.Web>
        <httpRuntime targetFramework="4.8" />
      </system.Web>

-->
<system.web>
<customErrors mode="Off" />
<compilation debug="true" targetFramework="4.8" />
<authentication mode="Forms">
<forms loginUrl="~/Account/Login.aspx" timeout="2880" />
</authentication>
<membership>
<providers>
<clear />
<add name="AspNetSqlMembershipProvider" type="System.Web.Security.SqlMembershipProvider" connectionStringName="ApplicationServices" enablePasswordRetrieval="false" enablePasswordReset="true" requiresQuestionAndAnswer="false" requiresUniqueEmail="false" maxInvalidPasswordAttempts="5" minRequiredPasswordLength="6" minRequiredNonalphanumericCharacters="0" passwordAttemptWindow="10" applicationName="/" />
</providers>
</membership>
<profile>
<providers>
<clear />
<add name="AspNetSqlProfileProvider" type="System.Web.Profile.SqlProfileProvider" connectionStringName="ApplicationServices" applicationName="/" />
</providers>
</profile>
<roleManager enabled="false">
<providers>
<clear />
<add name="AspNetSqlRoleProvider" type="System.Web.Security.SqlRoleProvider" connectionStringName="ApplicationServices" applicationName="/" />
<add name="AspNetWindowsTokenRoleProvider" type="System.Web.Security.WindowsTokenRoleProvider" applicationName="/" />
</providers>
</roleManager>
<pages controlRenderingCompatibilityVersion="4.0" />
</system.web>
<system.webServer>
<modules runAllManagedModulesForAllRequests="true" />
<defaultDocument>
<files>
<clear />
<add value="Default.aspx" />
</files>
</defaultDocument>
</system.webServer>
<system.data>
<DbProviderFactories>
<remove invariant="Oracle.ManagedDataAccess.Client" />
<add name="ODP.NET, Managed Driver" invariant="Oracle.ManagedDataAccess.Client" description="Oracle Data Provider for .NET, Managed Driver" type="Oracle.ManagedDataAccess.Client.OracleClientFactory, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
</DbProviderFactories>
</system.data>
</configuration>

What corrections are required to fix this error?

### Answer:

The error you're encountering is due to a duplicate reference to the `oracle.manageddataaccess.client` assembly in your configuration file. Here are the steps to resolve this issue:

1. **Remove Duplicate References**: Open your `web.config` file and ensure that there is only one reference to `oracle.manageddataaccess.client`. If there are duplicate references, remove them³.

2. **Check machine.config**: Sometimes, the `oracle.manageddataaccess.client` section can also be defined in the `machine.config` file. If it's defined there, you might need to remove it from `web.config`¹.

3. **Set Correct Version**: Make sure the version of `Oracle.ManagedDataAccess` in your `web.config` matches the version you installed via NuGet⁴.

4. **Reinstall Oracle Developer Tools for Visual Studio**: If the issue persists, consider reinstalling the Oracle Developer Tools for Visual Studio⁵.

Remember to save all changes and restart the server for the changes to take effect. Let me know if you need further assistance!

Source: Conversation with Copilot, 7/29/2024
(1) There is a duplicate 'oracle.manageddataaccess.client' section defined .... https://learn.microsoft.com/en-us/answers/questions/1167328/there-is-a-duplicate-oracle-manageddataaccess-clie.
(2) c# - How to replace the "oracle.manageddataaccess.client" section of .... https://stackoverflow.com/questions/58324931/how-to-replace-the-oracle-manageddataaccess-client-section-of-machine-config-w.
(3) NuGet Gallery | Oracle.ManagedDataAccess 23.5.0. https://www.nuget.org/packages/Oracle.ManagedDataAccess.
(4) c# - The ADO.NET provider 'Oracle.ManagedDataAccess.Client' is either .... https://stackoverflow.com/questions/37271009/the-ado-net-provider-oracle-manageddataaccess-client-is-either-not-registered.
(5) c# - Why would Oracle.ManagedDataAccess not work when Oracle.DataAccess .... https://stackoverflow.com/questions/17456024/why-would-oracle-manageddataaccess-not-work-when-oracle-dataaccess-does.
(6) undefined. http://www.oracle.com/technetwork/topics/dotnet/downloads/odacmsidownloadvs2017-3806459.html.

### Quesion: For the .NET 4.8 C# ASPX app,

I see two references in the web.config:
<configSections>

<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
</configSections>

and

    <system.data>
    	<DbProviderFactories>
    		<remove invariant="Oracle.ManagedDataAccess.Client" />
    		<add name="ODP.NET, Managed Driver" invariant="Oracle.ManagedDataAccess.Client" description="Oracle Data Provider for .NET, Managed Driver" type="Oracle.ManagedDataAccess.Client.OracleClientFactory, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
    	</DbProviderFactories>
    </system.data>

which one should be removed and which one should stay?

### Answer:

Both sections you mentioned in the `web.config` file are important and serve different purposes:

1. `<configSections>`: This section is used to define custom configuration sections. The `oracle.manageddataaccess.client` section is used to configure Oracle Data Provider for .NET².

2. `<system.data>`: This section is used to define settings for .NET data providers. The `DbProviderFactories` section is used to configure the factories for .NET data providers. The `Oracle.ManagedDataAccess.Client` is the factory for Oracle's managed data access client².

Therefore, you should not remove any of these sections as they are both necessary for the Oracle Data Provider for .NET to function correctly in your .NET 4.8 C# ASPX app. If you remove any of these sections, it might cause your application to malfunction or not run at all².

Source: Conversation with Copilot, 7/29/2024
(1) Configuring Oracle Data Provider for .NET - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/odpnt/InstallConfig.html.
(2) c# - How to connect to an Oracle database Connection from .Net Core .... https://stackoverflow.com/questions/41459631/how-to-connect-to-an-oracle-database-connection-from-net-core.
(3) Getting Started with Oracle Data Provider for .NET (C# Version). https://www.oracle.com/webfolder/technetwork/tutorials/obe/db/dotnet/2015/getting_started_net_version/oracle_data_provider_for_dotnet.html.
(4) c# - Connecting to Oracle using Oracle.ManagedDataAccess - Stack Overflow. https://stackoverflow.com/questions/31564288/connecting-to-oracle-using-oracle-manageddataaccess.
(5) Getting Started with Oracle Data Provider for .NET (C# Version). https://www.oracle.com/webfolder/technetwork/tutorials/obe/db/12c/r1/appdev/dotnet/GettingStartedNETVersion/index.html.
(6) undefined. http://www.oracle.com/technetwork/topics/dotnet/downloads/net-downloads-160392.html.
(7) undefined. https://www.nuget.org/packages/Oracle.ManagedDataAccess.Core.
(8) undefined. https://github.com/ericmend/oracleClientCore-2.0.
(9) undefined. https://github.com/LinqDan/oracleclientcore.

### Question: For the .NET 4.8 C# ASPX app,

I see two references in the web.config:
<configSections>

<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
</configSections>

and

    <system.data>
    	<DbProviderFactories>
    		<remove invariant="Oracle.ManagedDataAccess.Client" />
    		<add name="ODP.NET, Managed Driver" invariant="Oracle.ManagedDataAccess.Client" description="Oracle Data Provider for .NET, Managed Driver" type="Oracle.ManagedDataAccess.Client.OracleClientFactory, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
    	</DbProviderFactories>
    </system.data>

which one should be removed and which one should stay?

I am getting this web page when I try to run the app:

HTTP Error 500.19 - Internal Server Error
The requested page cannot be accessed because the related configuration data for the page is invalid.
Detailed Error Information:
Module IIS Web Core
Notification Unknown
Handler Not yet determined
Error Code 0x800700b7
Config Error There is a duplicate 'oracle.manageddataaccess.client' section defined
Config File \\?\C:\Users\zf297a\Documents\Visual Studio 2013\Projects\IRVT\IRVT\IRVT\web.config
Requested URL http://localhost:53982/testDriver.aspx
Physical Path  
Logon Method Not yet determined
Logon User Not yet determined
Request Tracing Directory C:\Users\zf297a\Documents\IISExpress\TraceLogFiles\
Config Source:

    7: 	<configSections>
    8: 		<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
    9: 	</configSections>

More Information:
This error occurs when there is a problem reading the configuration file for the Web server or Web application. In some cases, the event logs may contain more information about what caused this error.

If you see the text "There is a duplicate 'system.web.extensions/scripting/scriptResourceHandler' section defined", this error is because you are running a .NET Framework 3.5-based application in .NET Framework 4. If you are running WebMatrix, to resolve this problem, go to the Settings node to set the .NET Framework version to ".NET 2". You can also remove the extra sections from the web.config file.

So, how should this be fixed?

### Answer:

The error message indicates that there are multiple instances of the `oracle.manageddataaccess.client` section in your application's configuration file². This duplication is causing the HTTP Error 500.19.

To resolve this issue, you need to ensure that there is only one `oracle.manageddataaccess.client` section in your entire application's configuration hierarchy, which includes the `web.config` file at the application level and the `machine.config` file at the machine level³.

Here are the steps to fix this issue:

1. Open the `web.config` file and ensure there is only one `oracle.manageddataaccess.client` section under `<configSections>`.
2. Check the `machine.config` file, which is located in the .NET Framework config directory (e.g., `%windir%\Microsoft.NET\Framework64\v4.0.30319\Config\machine.config` for .NET 4.x on a 64-bit machine). If there is an `oracle.manageddataaccess.client` section in this file, you should remove it³.
3. Save the changes and restart your application.

Please remember to back up your configuration files before making any changes. If the issue persists, you might want to compare the server's configuration with a working development environment to identify any differences².

Source: Conversation with Copilot, 7/29/2024
(1) There is a duplicate 'oracle.manageddataaccess.client' section defined. https://stackoverflow.com/questions/75323950/there-is-a-duplicate-oracle-manageddataaccess-client-section-defined.
(2) Configuring Oracle Data Provider for .NET - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/odpnt/InstallConfig.html.
(3) Error 500.19 The configuration section 'oracle.manageddataaccess.client .... https://stackoverflow.com/questions/52859728/error-500-19-the-configuration-section-oracle-manageddataaccess-client-cannot.
(4) undefined. https://go.microsoft.com/fwlink/?LinkId=301880.
(5) undefined. http://go.microsoft.com/fwlink/?LinkID=237468.

### Question: For the .NET 4.8 C# ASPX app,

I see two references in the web.config:
<configSections>

<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
</configSections>

I found a duplicate in the machine.config file. I commented it out and reran the app and got this huge error message:
Could not load file or assembly 'Oracle.DataAccess' or one of its dependencies. An attempt was made to load a program with an incorrect format.
Description: An unhandled exception occurred during the execution of the current web request. Please review the stack trace for more information about the error and where it originated in the code.

Exception Details: System.BadImageFormatException: Could not load file or assembly 'Oracle.DataAccess' or one of its dependencies. An attempt was made to load a program with an incorrect format.

Source Error:

An unhandled exception was generated during the execution of the current web request. Information regarding the origin and location of the exception can be identified using the exception stack trace below.

Assembly Load Trace: The following information can be helpful to determine why the assembly 'Oracle.DataAccess' could not be loaded.

=== Pre-bind state information ===
LOG: DisplayName = Oracle.DataAccess
(Partial)
WRN: Partial binding information was supplied for an assembly:
WRN: Assembly Name: Oracle.DataAccess | Domain ID: 4
WRN: A partial bind occurs when only part of the assembly display name is provided.
WRN: This might result in the binder loading an incorrect assembly.
WRN: It is recommended to provide a fully specified textual identity for the assembly,
WRN: that consists of the simple name, version, culture, and public key token.
WRN: See whitepaper http://go.microsoft.com/fwlink/?LinkId=109270 for more information and common solutions to this issue.
LOG: Appbase = file:///C:/Users/zf297a/Documents/Visual Studio 2013/Projects/IRVT/IRVT/IRVT/
LOG: Initial PrivatePath = C:\Users\zf297a\Documents\Visual Studio 2013\Projects\IRVT\IRVT\IRVT\bin
Calling assembly : (Unknown).
===
LOG: This bind starts in default load context.
LOG: Using application configuration file: C:\Users\zf297a\Documents\Visual Studio 2013\Projects\IRVT\IRVT\IRVT\web.config
LOG: Using host configuration file: C:\Users\zf297a\Documents\IISExpress\config\aspnet.config
LOG: Using machine configuration file from C:\Windows\Microsoft.NET\Framework64\v4.0.30319\config\machine.config.
LOG: Policy not being applied to reference at this time (private, custom, partial, or location-based assembly bind).
LOG: Attempting download of new URL file:///C:/Users/zf297a/AppData/Local/Temp/Temporary ASP.NET Files/vs/338ef7c1/18c5af3b/Oracle.DataAccess.DLL.
LOG: Attempting download of new URL file:///C:/Users/zf297a/AppData/Local/Temp/Temporary ASP.NET Files/vs/338ef7c1/18c5af3b/Oracle.DataAccess/Oracle.DataAccess.DLL.
LOG: Attempting download of new URL file:///C:/Users/zf297a/Documents/Visual Studio 2013/Projects/IRVT/IRVT/IRVT/bin/Oracle.DataAccess.DLL.
ERR: Failed to complete setup of assembly (hr = 0x8007000b). Probing terminated.

Stack Trace:

[BadImageFormatException: Could not load file or assembly 'Oracle.DataAccess' or one of its dependencies. An attempt was made to load a program with an incorrect format.]
System.Reflection.RuntimeAssembly.\_nLoad(AssemblyName fileName, String codeBase, Evidence assemblySecurity, RuntimeAssembly locationHint, StackCrawlMark& stackMark, IntPtr pPrivHostBinder, Boolean throwOnFileNotFound, Boolean forIntrospection, Boolean suppressSecurityChecks) +0
System.Reflection.RuntimeAssembly.InternalLoadAssemblyName(AssemblyName assemblyRef, Evidence assemblySecurity, RuntimeAssembly reqAssembly, StackCrawlMark& stackMark, IntPtr pPrivHostBinder, Boolean throwOnFileNotFound, Boolean forIntrospection, Boolean suppressSecurityChecks) +232
System.Reflection.RuntimeAssembly.InternalLoad(String assemblyString, Evidence assemblySecurity, StackCrawlMark& stackMark, IntPtr pPrivHostBinder, Boolean forIntrospection) +113
System.Reflection.RuntimeAssembly.InternalLoad(String assemblyString, Evidence assemblySecurity, StackCrawlMark& stackMark, Boolean forIntrospection) +23
System.Reflection.Assembly.Load(String assemblyString) +35
System.Web.Configuration.CompilationSection.LoadAssemblyHelper(String assemblyName, Boolean starDirective) +49

[ConfigurationErrorsException: Could not load file or assembly 'Oracle.DataAccess' or one of its dependencies. An attempt was made to load a program with an incorrect format.]
System.Web.Configuration.CompilationSection.LoadAssemblyHelper(String assemblyName, Boolean starDirective) +762
System.Web.Configuration.CompilationSection.LoadAllAssembliesFromAppDomainBinDirectory() +259
System.Web.Configuration.CompilationSection.LoadAssembly(AssemblyInfo ai) +167
System.Web.Compilation.BuildManager.GetReferencedAssemblies(CompilationSection compConfig) +238
System.Web.Compilation.BuildManager.GetPreStartInitMethodsFromReferencedAssemblies() +78
System.Web.Compilation.BuildManager.CallPreStartInitMethods(String preStartInitListPath, Boolean& isRefAssemblyLoaded) +334
System.Web.Compilation.BuildManager.ExecutePreAppStart() +178
System.Web.Hosting.HostingEnvironment.Initialize(ApplicationManager appManager, IApplicationHost appHost, IConfigMapPathFactory configMapPathFactory, HostingEnvironmentParameters hostingParameters, PolicyLevel policyLevel, Exception appDomainCreationException) +746

[HttpException (0x80004005): Could not load file or assembly 'Oracle.DataAccess' or one of its dependencies. An attempt was made to load a program with an incorrect format.]
System.Web.HttpRuntime.FirstRequestInit(HttpContext context) +552
System.Web.HttpRuntime.EnsureFirstRequestInit(HttpContext context) +122
System.Web.HttpRuntime.ProcessRequestNotificationPrivate(IIS7WorkerRequest wr, HttpContext context) +732

Do I uncomment the managed.config file? If I do that how do I fix the original error of

and

    <system.data>
    	<DbProviderFactories>
    		<remove invariant="Oracle.ManagedDataAccess.Client" />
    		<add name="ODP.NET, Managed Driver" invariant="Oracle.ManagedDataAccess.Client" description="Oracle Data Provider for .NET, Managed Driver" type="Oracle.ManagedDataAccess.Client.OracleClientFactory, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
    	</DbProviderFactories>
    </system.data>

which one should be removed and which one should stay?

I am getting this web page when I try to run the app:

HTTP Error 500.19 - Internal Server Error
The requested page cannot be accessed because the related configuration data for the page is invalid.
Detailed Error Information:
Module IIS Web Core
Notification Unknown
Handler Not yet determined
Error Code 0x800700b7
Config Error There is a duplicate 'oracle.manageddataaccess.client' section defined
Config File \\?\C:\Users\zf297a\Documents\Visual Studio 2013\Projects\IRVT\IRVT\IRVT\web.config
Requested URL http://localhost:53982/testDriver.aspx
Physical Path  
Logon Method Not yet determined
Logon User Not yet determined
Request Tracing Directory C:\Users\zf297a\Documents\IISExpress\TraceLogFiles\
Config Source:

    7: 	<configSections>
    8: 		<section name="oracle.manageddataaccess.client" type="OracleInternal.Common.ODPMSectionHandler, Oracle.ManagedDataAccess, Version=4.122.23.1, Culture=neutral, PublicKeyToken=89b483f429c47342" />
    9: 	</configSections>

More Information:
This error occurs when there is a problem reading the configuration file for the Web server or Web application. In some cases, the event logs may contain more information about what caused this error.

If you see the text "There is a duplicate 'system.web.extensions/scripting/scriptResourceHandler' section defined", this error is because you are running a .NET Framework 3.5-based application in .NET Framework 4. If you are running WebMatrix, to resolve this problem, go to the Settings node to set the .NET Framework version to ".NET 2". You can also remove the extra sections from the web.config file.

So, how should this be fixed?

### Answer

The new error message indicates that there's an issue with the Oracle.DataAccess assembly. The `System.BadImageFormatException` typically occurs when a 32-bit assembly is loaded in a 64-bit environment or vice versa¹.

Here are some steps to troubleshoot this issue:

1. **Check the Oracle Client Version**: Ensure that the Oracle Client installed on your machine matches the platform target of your application (i.e., both are either 32-bit or 64-bit).

2. **Check the Application Pool Settings in IIS**: If you're using IIS, you can try enabling or disabling the "Enable 32-bit Applications" setting in the application pool¹. If your application and Oracle Client are 64-bit, this setting should be `False`; if they are 32-bit, it should be `True`.

3. **Check the Platform Target of Your Application**: In your project properties in Visual Studio, ensure that the "Platform target" under the "Build" tab matches the bitness of your Oracle Client¹. It should be "x86" for 32-bit and "x64" for 64-bit.

4. **Reinstall Oracle Client**: If the above steps don't resolve the issue, consider reinstalling the Oracle Client. Ensure that you're installing the correct version (32-bit or 64-bit) that matches your application.

Regarding the `web.config` and `machine.config` files, the `<section name="oracle.manageddataaccess.client".../>` should only be defined once across these files. If it's defined in `web.config`, it shouldn't be in `machine.config`, and vice versa². The `<system.data>` section can remain in your `web.config` file².

Remember to back up your files before making changes and restart your application after making modifications. If the issue persists, you might want to compare the server's configuration with a working development environment to identify any differences².

Source: Conversation with Copilot, 7/29/2024
(1) Could not load file or assembly "Oracle.DataAccess" or one of its .... https://stackoverflow.com/questions/18779518/could-not-load-file-or-assembly-oracle-dataaccess-or-one-of-its-dependencies.
(2) Could not load file or assembly 'Oracle.DataAccess' or one of its .... https://stackoverflow.com/questions/15596834/could-not-load-file-or-assembly-oracle-dataaccess-or-one-of-its-dependencies.
(3) Could not load file or assembly for Oracle.DataAccess in .NET. https://stackoverflow.com/questions/1953676/could-not-load-file-or-assembly-for-oracle-dataaccess-in-net.
(4) undefined. http://www.alexjamesbrown.com/development/could-not-load-file-or-assembly-chilkatdotnet2-or-one-of-its-dependencies-an-attempt-was-made-to-load-a-program-with-an-incorrect-format/.
