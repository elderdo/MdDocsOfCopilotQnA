### question

With Exceed 15 where do I connect via ssh? If I have a .ssh directory with my private keys, how do I indciate what private key to use for ssh to connect without requiring a passwrd via Exceed 15? I have used ssh via my Windows command prompt and I'm able to connect to my Linux computers that way and I have PuTTY to do the same, but now I'm trying to use the Exceed 15 software so I can help users configure their Exceed 15. So, I need a list of explicit but simple steps to connect via ssh without a password and have Exceed 15 start a nice looking xterm for the user and Exceed 15 does have a Windows like HostExplorer allowing sftp via drag and drop of files from Windws to the HostExplorer or visa versa. I need to configure that utilizing ssh without a password and using the private keys defined in a .ssh directory that gets used by a command line tool. Can you help configure Exceed's HostExplorer?

### answer

To configure OpenText Exceed 15 for SSH connections without a password and to use HostExplorer for SFTP, follow these steps:

### Configuring SSH in Exceed 15

1. **Open Xstart**:

   - Launch the Xstart utility from the list of programs.

2. **Set Up SSH Connection**:

   - In the Xstart window, set the `Connection Method` to `Secure Shell (SSH)`.
   - Enter the `Host Name` and `User Name` for the remote server.
   - Click on the `Settings` button next to the `Connection Method`.
   - In the `SSH Settings` window, ensure that `Tunnel X11 Connections` is checked.
   - Click `OK` to save the settings.

3. **Specify Private Key**:

   - In the `SSH Settings` window, locate the option to specify the private key file.
   - Browse to your `.ssh` directory and select the appropriate private key file (e.g., `id_rsa`).

4. **Save and Connect**:
   - Save the configuration and use Xstart to initiate the SSH connection.
   - This should allow you to connect without a password, using your private key.

### Configuring HostExplorer for SFTP

1. **Open HostExplorer**:

   - Launch HostExplorer from the list of programs.

2. **Set Up SFTP Connection**:

   - In HostExplorer, create a new connection profile.
   - Set the `Connection Method` to `SFTP (SSH File Transfer Protocol)`.
   - Enter the `Host Name` and `User Name` for the remote server.
   - Specify the private key file in the connection settings, similar to the steps in Xstart.

3. **Drag and Drop Files**:
   - Once connected, you can drag and drop files between your Windows machine and the remote server using HostExplorer.

By following these steps, you should be able to configure Exceed 15 for SSH connections without a password and use HostExplorer for SFTP with your private keys.

If you need further assistance, feel free to ask!

Source: Conversation with Copilot, 9/26/2024
(1) How to SSH Without a Password (like a boss). https://www.youtube.com/watch?v=j2vBT3T79Pg.
(2) How to Set Up SSH Passwordless Login (Step-by-Step Tutorial) | StrongDM. https://www.strongdm.com/blog/ssh-passwordless-login.
(3) How to SSH Without a Password Using PuTTY | Secure and Convenient Authentication. https://www.youtube.com/watch?v=4wMCM-LSIEs.
(4) OpenText Exceed User's Guide. https://mimage.opentext.com/evals/connv15/docs/Exceed.pdf.
(5) Configure SSH for login without a password - PragmaticLinux. https://www.pragmaticlinux.com/2021/05/configure-ssh-for-login-without-a-password/.
(6) OpenText Exceed. https://www.opentext.com/file_source/OpenText/en_US/PDF/OpenText-portfolio-Exceed-15-PO-EN.pdf.
(7) Exceed Product Overview - OpenText. https://www.opentext.com/file_source/OpenText/en_US/PDF/opentext-po-exceed-15-en.pdf.
(8) OpenText Exceed User's Guide. http://mimage.opentext.com/evals/connv15/docs/Exceed.pdf.
(9) OpenText Connectivity 15.0 Installation Guide. https://mimage.opentext.com/evals/connv15/docs/Installation.pdf.
(10) Opentext Exceed User's Guide - DocsLib. https://docslib.org/doc/10551191/opentext-exceed-users-guide.
(11) HostExplorer Product overview - OpenText. https://www.opentext.com/file_source/OpenText/en_US/PDF/opentext-po-host-explorer-15-en.pdf.
(12) undefined. https://tinyurl.com/4dv3xsxw.
(13) undefined. https://zacs-tech.com/go/digitalocean/.
(14) undefined. https://zacs-tech.com/go/vultr.
(15) undefined. https://knowledge.opentext.com.
(16) undefined. http://support.opentext.com.
(17) undefined. https://www.opentext.com.
(18) undefined. http://www.openssl.org/%29.
(19) undefined. http://www.apache.org/%29.
(20) undefined. http://connectivity.opentext.com.
