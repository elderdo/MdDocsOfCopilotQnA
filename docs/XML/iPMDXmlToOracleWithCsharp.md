###

### answer

```csharp
using System;
using System.Data;
using System.Data.SqlClient;
using System.Xml;

class Program
{
    static void Main()
    {
        string connectionString = "your_connection_string_here";
        string xmlFilePath = "data.xml";

        DataSet dataSet = new DataSet();
        dataSet.ReadXml(xmlFilePath);

        using (SqlConnection connection = new SqlConnection(connectionString))
        {
            connection.Open();

            foreach (DataTable table in dataSet.Tables)
            {
                foreach (DataRow row in table.Rows)
                {
                    string insertCommandText = GenerateInsertCommand(table.TableName, row);
                    using (SqlCommand command = new SqlCommand(insertCommandText, connection))
                    {
                        command.ExecuteNonQuery();
                    }
                }
            }
        }
    }

    static string GenerateInsertCommand(string tableName, DataRow row)
    {
        string columns = string.Join(", ", row.Table.Columns.Cast<DataColumn>().Select(c => c.ColumnName));
        string values = string.Join(", ", row.ItemArray.Select(v => $"'{v}'"));
        return $"INSERT INTO {tableName} ({columns}) VALUES ({values})";
    }
}
```

This C# application reads the XML file, loads it into a `DataSet`, and then iterates through each table and row to insert the data into the corresponding SQL tables. Make sure to replace `"your_connection_string_here"` with your actual database connection string.
