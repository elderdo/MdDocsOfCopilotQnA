### question

There are multiple tools for OpenText Exceed 15 x64 in its start menu. When I look at the list of programs I am not certain which tool contains the Configure SSH option. When I run just Exceed it placces an icon on the Windows taskbar. When I right click that icon, I get a list of programs: Exceed XDMCP Broadcast, Exceed XDMCP Query, Xconfig, Xstart, Xsession, and Exceed for x64. Which program should I select to configure SSH as you indicate?

### answer

To configure SSH in OpenText Exceed 15 x64, you should use the **Xstart** utility. Here are the steps:

1. **Open Xstart**: Launch the Xstart utility from the list of programs.
2. **Configure SSH**:
   - In the Xstart window, set the `Connection Method` to `Secure Shell (SSH)`.
   - Enter the `Host Name` and `User Name` for the remote server.
   - Click on the `Settings` button next to the `Connection Method`.
   - In the `SSH Settings` window, ensure that `Tunnel X11 Connections` is checked.
   - Click `OK` to save the settings.

This will enable X11 forwarding and allow you to run graphical applications from the remote server on your local machine.

If you have any more questions or need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/26/2024
(1) OpenText Exceed User's Guide. http://mimage.opentext.com/evals/connv15/docs/Exceed.pdf.
(2) Opentext Exceed User's Guide - DocsLib. https://docslib.org/doc/10551191/opentext-exceed-users-guide.
(3) Windows 10 Support - OpenText. https://www.opentext.com/support/connectivity-windows-10.
(4) OpenText Secure Shell. https://www.opentext.com/file_source/OpenText/en_US/PDF/opentext-secure-shell-product-overview.pdf.
(5) undefined. https://knowledge.opentext.com.
(6) undefined. http://support.opentext.com.
(7) undefined. https://www.opentext.com.
(8) undefined. http://www.openssl.org/%29.
(9) undefined. http://www.apache.org/%29.
