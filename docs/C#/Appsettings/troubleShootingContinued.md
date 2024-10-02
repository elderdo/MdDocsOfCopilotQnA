### question

Will these secrets be read into Appsettings:

```json
{
  "AppSettings": {
    "ConnectionStrings": {
      "JitOperDb": "Data Source=SDCDSDV; User Id=jitoper; Password=******************;"
    }
  }
}
where Appsettings is:
```

```csharp
namespace XmlToDb.Configurations;
public class AppSettings
{
    public const string AppSettingsKey = "AppSettings";
    public string XmlFilePath { get; set; } = string.Empty;
    public ConnectionStrings? ConnectionStrings { get; set; }
}
where the Program.cs has this method:
static IHostBuilder CreateHostBuilder(string[] args) =>
    Host.CreateDefaultBuilder(args)
        .UseSerilog()
        .ConfigureServices((context, services) =>
        {
            services.ConfigureAppSettings(context.Configuration);
            services.AddSingleton<IConnectionService,ConnectionService>();
            services.AddSingleton<IDatabaseService, DatabaseService>();
            services.AddSingleton<IXmlToDbService, XmlToDbService>();
            services.AddSingleton<ILogger>(new LoggerConfiguration()
                .WriteTo.Console()
                .CreateLogger());
        });
```

and uses this extension class:

```csharp
public static class AppSettingsOptionsExtension
{
    public static IServiceCollection ConfigureAppSettings(this IServiceCollection services,
                                                            IConfiguration configuration)
    {
        var appSettings = new AppSettings();
        configuration.GetSection(AppSettings.AppSettingsKey).Bind(appSettings);

        if (string.IsNullOrEmpty(appSettings.XmlFilePath) || !File.Exists(appSettings.XmlFilePath))
        {
            throw new FileNotFoundException($"The file '{appSettings.XmlFilePath}' does not exist.");
        }

        services.Configure<AppSettings>(configuration.GetSection(AppSettings.AppSettingsKey));
        services.Configure<ConnectionStrings>(configuration.GetSection("ConnectionStrings"));

        // Register DatabaseService
        services.AddSingleton<DatabaseService>();

        return services;
    }
}
```

and a Properties\launchSettings.json:

```json
{
  "profiles": {
    "XmlToDb": {
      "commandName": "Project",
      "environmentVariables": {
        "ASPNETCORE_ENVIRONMENT": "Development"
      }
    }
  }
}
```

Why is the ConnectionStrings not being picked up by the Configuration class when using this class the \_connectionString gets set to null.

```csharp
namespace XmlToDb.Services;
public class XmlToDbService(IConfiguration configuration, IOptions<AppSettings> options, ILogger logger) : IXmlToDbService
{
    private readonly string? _connectionString = options.Value.ConnectionStrings?.JitOperDb;
    private readonly string _xmlFilePath = options.Value.XmlFilePath;
    private readonly ILogger _logger = logger;

    public void Run()
    {
        LoadXmlToDb(_xmlFilePath);
    }
    public void LoadXmlToDb(string xmlFilePath)
    {
        try
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(xmlFilePath);

            if (xmlDoc.DocumentElement == null)
            {
                _logger.Error("The XML document is empty or invalid.");
                return;
            }

            using var conn = new OracleConnection(_connectionString);
            conn.Open();
            int rowsInserted = 0;

            foreach (XmlNode node in xmlDoc.DocumentElement.ChildNodes)
            {
                string? attribute1 = node.Attributes?["attribute1"]?.InnerText;
                string? element1 = node["element1"]?.InnerText;

                if (attribute1 != null && element1 != null)
                {
                    string insertQuery = "INSERT INTO your_table (column1, column2) VALUES (:attribute1, :element1)";
                    using var cmd = new OracleCommand(insertQuery, conn);
                    cmd.Parameters.Add(new OracleParameter("attribute1", attribute1));
                    cmd.Parameters.Add(new OracleParameter("element1", element1));

                    rowsInserted += cmd.ExecuteNonQuery();
                    _logger.Information("Inserted row with attribute1: {Attribute1}, element1: {Element1}", attribute1, element1);
                }
                else
                {
                    _logger.Warning("Skipped row due to null attribute1 or element1");
                }
            }

            _logger.Information("Total rows inserted: {RowsInserted}", rowsInserted);
        }
        catch (Exception ex)
        {
            _logger.Error(ex, "An error occurred while processing the XML file.");
        }
    }
}
```

### answer

The issue seems to be with how the configuration sections are being bound. Here are a few things to check and adjust:

1. **Ensure Correct JSON Structure**:
   Make sure your `appsettings.json` file has the correct structure:

   ```json
   {
     "AppSettings": {
       "XmlFilePath": "path/to/xmlfile.xml",
       "ConnectionStrings": {
         "JitOperDb": "Data Source=SDCDSDV; User Id=jitoper; Password=******************;"
       }
     }
   }
   ```

2. **Bind Configuration Correctly**:
   Ensure that the `AppSettings` class and `ConnectionStrings` class are correctly defined and bound:

   ```csharp
   public class ConnectionStrings
   {
       public string JitOperDb { get; set; } = string.Empty;
   }

   public class AppSettings
   {
       public const string AppSettingsKey = "AppSettings";
       public string XmlFilePath { get; set; } = string.Empty;
       public ConnectionStrings? ConnectionStrings { get; set; }
   }
   ```

3. **Check Configuration Binding in Extension Method**:
   Ensure that the configuration is being bound correctly in the `ConfigureAppSettings` method:

   ```csharp
   public static class AppSettingsOptionsExtension
   {
       public static IServiceCollection ConfigureAppSettings(this IServiceCollection services,
                                                               IConfiguration configuration)
       {
           var appSettings = new AppSettings();
           configuration.GetSection(AppSettings.AppSettingsKey).Bind(appSettings);

           if (string.IsNullOrEmpty(appSettings.XmlFilePath) || !File.Exists(appSettings.XmlFilePath))
           {
               throw new FileNotFoundException($"The file '{appSettings.XmlFilePath}' does not exist.");
           }

           services.Configure<AppSettings>(configuration.GetSection(AppSettings.AppSettingsKey));
           // No need to configure ConnectionStrings separately if it's part of AppSettings

           // Register DatabaseService
           services.AddSingleton<DatabaseService>();

           return services;
       }
   }
   ```

4. **Check Program.cs**:
   Ensure that the `CreateHostBuilder` method is correctly configured:

   ```csharp
   static IHostBuilder CreateHostBuilder(string[] args) =>
       Host.CreateDefaultBuilder(args)
           .UseSerilog()
           .ConfigureServices((context, services) =>
           {
               services.ConfigureAppSettings(context.Configuration);
               services.AddSingleton<IConnectionService, ConnectionService>();
               services.AddSingleton<IDatabaseService, DatabaseService>();
               services.AddSingleton<IXmlToDbService, XmlToDbService>();
               services.AddSingleton<ILogger>(new LoggerConfiguration()
                   .WriteTo.Console()
                   .CreateLogger());
           });
   ```

5. **Check Dependency Injection in Service**:
   Ensure that the `XmlToDbService` class is correctly using the injected configuration:

   ```csharp
   namespace XmlToDb.Services;
   public class XmlToDbService(IConfiguration configuration, IOptions<AppSettings> options, ILogger logger) : IXmlToDbService
   {
       private readonly string? _connectionString = options.Value.ConnectionStrings?.JitOperDb;
       private readonly string _xmlFilePath = options.Value.XmlFilePath;
       private readonly ILogger _logger = logger;

       public void Run()
       {
           LoadXmlToDb(_xmlFilePath);
       }

       public void LoadXmlToDb(string xmlFilePath)
       {
           try
           {
               var xmlDoc = new XmlDocument();
               xmlDoc.Load(xmlFilePath);

               if (xmlDoc.DocumentElement == null)
               {
                   _logger.Error("The XML document is empty or invalid.");
                   return;
               }

               using var conn = new OracleConnection(_connectionString);
               conn.Open();
               int rowsInserted = 0;

               foreach (XmlNode node in xmlDoc.DocumentElement.ChildNodes)
               {
                   string? attribute1 = node.Attributes?["attribute1"]?.InnerText;
                   string? element1 = node["element1"]?.InnerText;

                   if (attribute1 != null && element1 != null)
                   {
                       string insertQuery = "INSERT INTO your_table (column1, column2) VALUES (:attribute1, :element1)";
                       using var cmd = new OracleCommand(insertQuery, conn);
                       cmd.Parameters.Add(new OracleParameter("attribute1", attribute1));
                       cmd.Parameters.Add(new OracleParameter("element1", element1));

                       rowsInserted += cmd.ExecuteNonQuery();
                       _logger.Information("Inserted row with attribute1: {Attribute1}, element1: {Element1}", attribute1, element1);
                   }
                   else
                   {
                       _logger.Warning("Skipped row due to null attribute1 or element1");
                   }
               }

               _logger.Information("Total rows inserted: {RowsInserted}", rowsInserted);
           }
           catch (Exception ex)
           {
               _logger.Error(ex, "An error occurred while processing the XML file.");
           }
       }
   }
   ```

By ensuring the correct structure and binding of your configuration, the `ConnectionStrings` should be properly picked up by the `Configuration` class. If the issue persists, consider adding logging to verify that the configuration sections are being read correctly.
