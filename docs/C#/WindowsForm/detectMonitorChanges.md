### question
Regarding the Changed event, that is a property of the DisplayManager. Does the DisplayManager have to be explicitly defined in the Main or do you get it by default via another object?

### answer 
The DisplayManager in the code example I provided was from the Windows.Devices.Display.Core namespace, which is part of the Universal Windows Platform (UWP) API, and it needs to be explicitly defined and obtained for it to work. However, this may not be available or suitable for traditional desktop applications (WinForms).

For a more traditional WinForms application, you would typically use the SystemEvents.DisplaySettingsChanged event from Microsoft.Win32 to detect monitor changes, and you don't need to explicitly define a DisplayManager.

## Using SystemEvents.DisplaySettingsChanged in WinForms
Here's how you can set up a WinForms application to respond to monitor changes using SystemEvents.DisplaySettingsChanged:

1. Add References:

- Make sure to add a reference to Microsoft.Win32.SystemEvents.

2. Set Up the Event Handler:

- In your form constructor, subscribe to the SystemEvents.DisplaySettingsChanged event.

3. Handle the Event:

- Implement logic in the event handler to reposition your application window.

### Example Code:
```csharp
using System;
using System.Drawing;
using System.Windows.Forms;
using Microsoft.Win32;

public class MonitorSwapHandler : Form
{
    public MonitorSwapHandler()
    {
        this.Text = "Monitor Swap Example";
        this.Size = new Size(800, 600);

        // Subscribe to the DisplaySettingsChanged event
        SystemEvents.DisplaySettingsChanged += OnDisplaySettingsChanged;

        // Initial positioning
        RepositionApp();
    }

    protected override void OnFormClosed(FormClosedEventArgs e)
    {
        // Unsubscribe from the event when the form is closed
        SystemEvents.DisplaySettingsChanged -= OnDisplaySettingsChanged;
        base.OnFormClosed(e);
    }

    private void OnDisplaySettingsChanged(object sender, EventArgs e)
    {
        Console.WriteLine("Monitor configuration changed. Repositioning the app...");
        RepositionApp();
    }

    private void RepositionApp()
    {
        // Get all connected screens
        Screen[] screens = Screen.AllScreens;

        // Example: Move to the primary screen
        Screen targetScreen = screens.FirstOrDefault(screen => screen.Primary) ?? screens.First();

        // Set the form location to the top-left corner of the target screen
        this.StartPosition = FormStartPosition.Manual;
        this.Location = targetScreen.Bounds.Location;

        // Optionally resize the form to fit within the target screen
        this.Size = new Size(Math.Min(this.Size.Width, targetScreen.Bounds.Width),
                            Math.Min(this.Size.Height, targetScreen.Bounds.Height));
    }

    [STAThread]
    public static void Main()
    {
        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);
        Application.Run(new MonitorSwapHandler());
    }
}
```

## Explanation:
1. Subscribe to Event:

- SystemEvents.DisplaySettingsChanged += OnDisplaySettingsChanged; subscribes to the display settings change event.

- The OnDisplaySettingsChanged method will be called whenever a display settings change is detected.

2. Repositioning Logic:

- RepositionApp() gets all connected screens and moves the app to the primary screen.

- Adjust the form's location and size to ensure it fits within the bounds of the target screen.

3. Unsubscribe on Close:

- To avoid potential memory leaks, unsubscribe from the event when the form is closed using SystemEvents.DisplaySettingsChanged -= OnDisplaySettingsChanged;.

This approach ensures that your app will respond to monitor configuration changes and reposition itself appropriately.