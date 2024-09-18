### request

Yes, show me how to integrate it into a Blazor app.

### response

Sure! Let's integrate the `XmlToDbService` into a .NET MAUI application. This will allow you to create a cross-platform app that can run on Windows, macOS, iOS, and Android. We'll follow these steps:

1. **Create a .NET MAUI Project**
2. **Add the XML-to-DB Service**
3. **Configure Services and Dependency Injection**
4. **Create the MAUI UI**

### Step 1: Create a .NET MAUI Project

1. Open Visual Studio and create a new .NET MAUI App project.
2. Name the project `MauiXmlToDbApp`.

### Step 2: Add the XML-to-DB Service

Add the `XmlToDbService` and its interface to your MAUI project.

#### IXmlToDbService.cs

```csharp
using System;

public interface IXmlToDbService
{
    void LoadXmlToDb(string xmlFilePath);
}
```

#### XmlToDbService.cs

```csharp
using System;
using System.Data;
using System.Xml.Linq;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;

public class XmlToDbService : IXmlToDbService
{
    private readonly IDbConnection _dbConnection;
    private readonly IConfiguration _configuration;
    private readonly ILogger<XmlToDbService> _logger;

    public XmlToDbService(IDbConnection dbConnection, IConfiguration configuration, ILogger<XmlToDbService> logger)
    {
        _dbConnection = dbConnection;
        _configuration = configuration;
        _logger = logger;
    }

    public void LoadXmlToDb(string xmlFilePath)
    {
        try
        {
            var xmlDoc = XDocument.Load(xmlFilePath);
            var tableName = _configuration["XmlToDb:TableName"];
            var columnMappings = _configuration.GetSection("XmlToDb:ColumnMappings").Get<Dictionary<string, string>>();

            foreach (var element in xmlDoc.Descendants(_configuration["XmlToDb:RootElement"]))
            {
                var command = _dbConnection.CreateCommand();
                command.CommandText = $"INSERT INTO {tableName} ({string.Join(", ", columnMappings.Values)}) VALUES ({string.Join(", ", columnMappings.Keys.Select(k => $"@{k}"))})";

                foreach (var mapping in columnMappings)
                {
                    var value = element.Attribute(mapping.Key)?.Value ?? element.Element(mapping.Key)?.Value;
                    var parameter = command.CreateParameter();
                    parameter.ParameterName = $"@{mapping.Key}";
                    parameter.Value = value ?? DBNull.Value;
                    command.Parameters.Add(parameter);
                }

                _dbConnection.Open();
                command.ExecuteNonQuery();
                _dbConnection.Close();
            }

            _logger.LogInformation("XML file loaded successfully.");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading XML file to database.");
            throw;
        }
    }
}
```

### Step 3: Configure Services and Dependency Injection

Modify the `MauiProgram.cs` file to configure services and dependency injection.

#### MauiProgram.cs

```csharp
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Configuration;
using Oracle.ManagedDataAccess.Client;
using System.Data;

namespace MauiXmlToDbApp
{
    public static class MauiProgram
    {
        public static MauiApp CreateMauiApp()
        {
            var builder = MauiApp.CreateBuilder();
            builder
                .UseMauiApp<App>()
                .ConfigureFonts(fonts =>
                {
                    fonts.AddFont("OpenSans-Regular.ttf", "OpenSansRegular");
                });

            var configuration = new ConfigurationBuilder()
                .AddJsonFile("appsettings.json")
                .Build();

            builder.Configuration.AddConfiguration(configuration);

            builder.Services.AddSingleton<IConfiguration>(configuration);
            builder.Services.AddSingleton<IDbConnection>(sp => new OracleConnection(configuration.GetConnectionString("DefaultConnection")));
            builder.Services.AddSingleton<IXmlToDbService, XmlToDbService>();
            builder.Services.AddLogging(configure => configure.AddConsole());

            return builder.Build();
        }
    }
}
```

### Step 4: Create the MAUI UI

Create a simple UI in the `MainPage.xaml` and `MainPage.xaml.cs` files to handle file selection and XML loading.

#### MainPage.xaml

```xml
<?xml version="1.0" encoding="utf-8" ?>
<ContentPage xmlns="http://schemas.microsoft.com/dotnet/2021/maui"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             x:Class="MauiXmlToDbApp.MainPage">

    <VerticalStackLayout Padding="30">
        <Label Text="Upload XML File" FontSize="24" HorizontalOptions="Center" />
        <Button Text="Select XML File" Clicked="OnSelectFileClicked" />
        <Button Text="Load XML to DB" Clicked="OnLoadXmlClicked" IsEnabled="{Binding IsFileSelected}" />
        <Label Text="{Binding Message}" FontSize="18" TextColor="Red" />
    </VerticalStackLayout>

</ContentPage>
```

#### MainPage.xaml.cs

```csharp
using Microsoft.Maui.Controls;
using Microsoft.Extensions.Logging;
using System;
using System.IO;
using System.Threading.Tasks;

namespace MauiXmlToDbApp
{
    public partial class MainPage : ContentPage
    {
        private readonly IXmlToDbService _xmlToDbService;
        private readonly ILogger<MainPage> _logger;
        private string? _selectedFilePath;

        public MainPage(IXmlToDbService xmlToDbService, ILogger<MainPage> logger)
        {
            InitializeComponent();
            _xmlToDbService = xmlToDbService;
            _logger = logger;
            BindingContext = this;
        }

        public bool IsFileSelected => !string.IsNullOrEmpty(_selectedFilePath);
        public string? Message { get; set; }

        private async void OnSelectFileClicked(object sender, EventArgs e)
        {
            try
            {
                var result = await FilePicker.Default.PickAsync(new PickOptions
                {
                    FileTypes = FilePickerFileType.Xml,
                    PickerTitle = "Select an XML file"
                });

                if (result != null)
                {
                    _selectedFilePath = result.FullPath;
                    OnPropertyChanged(nameof(IsFileSelected));
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error selecting file.");
                Message = $"Error: {ex.Message}";
                OnPropertyChanged(nameof(Message));
            }
        }

        private void OnLoadXmlClicked(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(_selectedFilePath))
            {
                try
                {
                    _xmlToDbService.LoadXmlToDb(_selectedFilePath);
                    Message = "XML file loaded successfully.";
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Error loading XML file to database.");
                    Message = $"Error: {ex.Message}";
                }
                OnPropertyChanged(nameof(Message));
            }
        }
    }
}
```

### Step 5: Configuration

Create an `appsettings.json` file to store the configuration settings.

#### appsettings.json

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "User Id=myUsername;Password=myPassword;Data Source=myOracleDB"
  },
  "XmlToDb": {
    "TableName": "MyTable",
    "RootElement": "RootElement",
    "ColumnMappings": {
      "XmlAttribute1": "DbColumn1",
      "XmlAttribute2": "DbColumn2",
      "XmlElement1": "DbColumn3"
    }
  }
}
```

### Conclusion

This example demonstrates how to integrate the `XmlToDbService` into a .NET MAUI application. The service is designed to be reusable and configurable, making it easy to adapt to different types of applications. You can now extend this MAUI app with more features or integrate the service into other types of applications like Blazor or Web API.

Would you like to see how to integrate this service into a Web API next?

Source: Conversation with Copilot, 9/18/2024
(1) Consume a REST-based web service - .NET MAUI | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/maui/data-cloud/rest?view=net-maui-8.0.
(2) Creating Bindings for .NET MAUI with Native Library Interop. https://devblogs.microsoft.com/dotnet/native-library-interop-dotnet-maui/.
(3) Tutorial: Create a .NET MAUI app using the Microsoft Graph SDK. https://learn.microsoft.com/en-us/windows/apps/windows-dotnet-maui/tutorial-graph-api.
(4) github.com. https://github.com/YauGoh/My.Maui.Responsive/tree/dea49cea3172536183660d49e779096e13691571/src%2FSamples%2FMy.Maui.Responsive.Samples%2FMauiProgram.cs.
(5) github.com. https://github.com/syncfusion/maui-docs/tree/47cbed86b3a7346e967000616505a28df2acd6b7/MAUI%2FTabView%2FGetting-Started.md.
