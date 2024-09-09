### question

When using developer tools for MS Edge after clicking the network icon how do I observe the network requests to see the headers and credentials being used?

### answer

To observe network requests, including headers and credentials, in Microsoft Edge Developer Tools, follow these steps:

1. **Open Developer Tools**:

   - Right-click on the webpage and select "Inspect".
   - Or, press `Ctrl+Shift+I` (Windows/Linux) or `Command+Option+I` (macOS).

2. **Navigate to the Network Tab**:

   - Click on the "Network" tab in the Developer Tools.

3. **Record Network Activity**:

   - Refresh the webpage (`F5` or `Ctrl+R`).
   - The Network tab will start logging all network activity.

4. **Inspect Individual Requests**:

   - Click on any network request in the list to view its details.
   - In the right-hand pane, you can see various tabs:
     - **Headers**: View request and response headers.
     - **Cookies**: View cookies sent with the request.
     - **Timing**: View the timing breakdown of the request.
     - **Response**: View the response body.

5. **View Headers and Credentials**:
   - In the **Headers** tab, you can see:
     - **General**: General information about the request.
     - **Request Headers**: Headers sent with the request, including credentials if applicable.
     - **Response Headers**: Headers received in the response.

For more detailed information, you can refer to the [Microsoft Edge Developer documentation](4).

Is there anything specific you are looking for within the network requests?

Source: Conversation with Copilot, 9/9/2024
(1) Microsoft Edge | Learn to Use the Network Tool. https://www.youtube.com/watch?v=niG_Ck6E5L8.
(2) Customizing Microsoft Edge Developer Tools and quick feature access. https://www.youtube.com/watch?v=ypRzEBYNptQ.
(3) Microsoft Edge | What's New in DevTools 101. https://www.youtube.com/watch?v=kv6Q8a9bsbA.
(4) Inspect network activity - Microsoft Edge Developer documentation. https://learn.microsoft.com/en-us/microsoft-edge/devtools-guide-chromium/network/.
(5) Network features reference - Microsoft Edge Developer documentation. https://learn.microsoft.com/en-us/microsoft-edge/devtools-guide-chromium/network/reference.
(6) How to check HTTP Response and Security Headers using developer tools .... https://community.progress.com/s/article/how-to-check-http-security-headers-using-develop-tools-in-web-browser.
