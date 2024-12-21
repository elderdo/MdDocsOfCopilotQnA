To make a `TextBox` display multiple lines in a WinForms application, you need to set a few properties of the `TextBox` control. Here's how you can do it:

### Steps in the Designer

1. **Open the Form Designer**:
   - Open your WinForms project and navigate to the form containing the `TextBox`.
   - Open the form in Design view.

2. **Select the `TextBox` Control**:
   - Click on the `TextBox` control that you want to modify.

3. **Open the Properties Window**:
   - If the Properties window is not already visible, press `F4` or go to `View` > `Properties Window`.

4. **Set the `Multiline` Property**:
   - Find the `Multiline` property in the Properties window and set it to `True`.

5. **Adjust the `Size` Property**:
   - Increase the height of the `TextBox` to accommodate multiple lines. You can do this by dragging the control's resize handles or by setting the `Height` property manually in the Properties window.

6. **Optional: Enable Scroll Bars**:
   - If you want to add scroll bars, set the `ScrollBars` property to `Vertical`, `Horizontal`, or `Both` as needed.

### Example in Code

Alternatively, you can set these properties programmatically in your code:

```csharp
// Create a new TextBox control
TextBox multiLineTextBox = new TextBox();

// Set the Multiline property to true
multiLineTextBox.Multiline = true;

// Adjust the size to fit multiple lines
multiLineTextBox.Size = new Size(200, 100);

// Optional: Enable vertical scroll bars
multiLineTextBox.ScrollBars = ScrollBars.Vertical;

// Add the TextBox to the form's Controls collection
this.Controls.Add(multiLineTextBox);
```

By setting the `Multiline` property to `True`, the `TextBox` will be able to display multiple lines of text, making it useful for comments, notes, or any other multi-line input.
