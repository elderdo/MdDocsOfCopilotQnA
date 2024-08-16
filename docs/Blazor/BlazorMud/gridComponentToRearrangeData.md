### question
For Blazor Mud is there a way to create a component that will allow data in a grid to be rearranged by the user and have the new order persisted?  If there is can you show an example of that component and how to use it?

### answer
Yes, you can create a component in Blazor using MudBlazor that allows users to rearrange data in a grid and persist the new order. Below is an example of how to achieve this:

### Step 1: Install MudBlazor
First, install the MudBlazor package via NuGet:
```bash
Install-Package MudBlazor
```

### Step 2: Set Up MudBlazor in Your Project
In your `Program.cs` or `Startup.cs`, configure MudBlazor services:
```csharp
builder.Services.AddMudServices();
```

### Step 3: Create the Component
Create a new component, e.g., `ReorderableGrid.razor`:
```razor
@page "/reorderablegrid"
@using MudBlazor
@using System.Collections.Generic
@inject ILocalStorageService LocalStorage

<MudDataGrid Items="@items" @ref="dataGrid" RowDraggable="true" RowDropZone="true" OnRowDrop="OnRowDrop">
    <Columns>
        <PropertyColumn Property="x => x.Id" Title="ID" />
        <PropertyColumn Property="x => x.Name" Title="Name" />
        <PropertyColumn Property="x => x.Age" Title="Age" />
    </Columns>
</MudDataGrid>

@code {
    private MudDataGrid<Item> dataGrid;
    private List<Item> items;

    protected override async Task OnInitializedAsync()
    {
        items = await LocalStorage.GetItemAsync<List<Item>>("items") ?? GetDefaultItems();
    }

    private List<Item> GetDefaultItems()
    {
        return new List<Item>
        {
            new Item { Id = 1, Name = "John", Age = 30 },
            new Item { Id = 2, Name = "Jane", Age = 25 },
            new Item { Id = 3, Name = "Bob", Age = 35 }
        };
    }

    private async Task OnRowDrop(MudItemDropInfo<Item> dropInfo)
    {
        var item = dropInfo.Item;
        items.Remove(item);
        items.Insert(dropInfo.Index, item);
        await LocalStorage.SetItemAsync("items", items);
    }

    public class Item
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Age { get; set; }
    }
}
```

### Step 4: Add Local Storage Service
To persist the new order, you can use a local storage service. Install the `Blazored.LocalStorage` package:
```bash
Install-Package Blazored.LocalStorage
```

Configure it in your `Program.cs` or `Startup.cs`:
```csharp
builder.Services.AddBlazoredLocalStorage();
```

### Step 5: Use the Component
Finally, use the `ReorderableGrid` component in your application:
```razor
<ReorderableGrid />
```

This setup allows users to drag and drop rows in the grid, and the new order is saved in local storage¹.

Source: Conversation with Copilot, 8/15/2024
(1) MudBlazor - Blazor Component Library. https://www.mudblazor.com/components/datagrid.
(2) Implementing Blazor CRUD using Mudblazor Component Library in .NET 5 .... https://codewithmukesh.com/blog/blazor-crud-using-mudblazor/.
(3) MudBlazor - Blazor Component Library. https://www.mudblazor.com/api/datagrid.
(4) MudBlazor/src/MudBlazor/Components/DataGrid/MudDataGrid.razor ... - GitHub. https://github.com/MudBlazor/MudBlazor/blob/dev/src/MudBlazor/Components/DataGrid/MudDataGrid.razor.
(5) How do you set a default column sort OR programmatically set the sort .... https://github.com/MudBlazor/MudBlazor/discussions/7740.
(6) undefined. https://mudblazor.com/api/datagrid.
(7) en.wikipedia.org. https://en.wikipedia.org/wiki/Blazor.