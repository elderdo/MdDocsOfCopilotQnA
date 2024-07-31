### For older .Net 4.8 C# ASPX the web.config looks like this:

<?xml version="1.0"?>
<configuration>

  <location path="Register.aspx">
    <system.web>
      <authorization>
        <allow users="*"/>
      </authorization>
    </system.web>
  </location>

<system.web>
<authorization>
<deny users="?"/>
</authorization>
</system.web>

</configuration>

VS 2022 says that SQL Server Express is required, but it does not appear to be utilized by this old project. I believe whatever template was used to created added an Account folder with a Login.aspx, Register.aspx, ChangePassword.aspx, and ChangePasswordSuccess.asp which appear to have not be utilized.

Also I do not see an IIS Express for the Start option. What corrects should I make to the project to get the Default.aspx running with an IIS Express Server?

To run your Default.aspx with an IIS Express Server, you can follow these steps:

1. **Set IIS Express as the default web server**: Right-click on the Default Startup project file in your Solution Explorer. Select "Properties". Click on the Web Tab on the left-hand side, scroll down and select the "Use Local IIS Web server" radio button. Then, tick the checkbox "Use IIS Express"³.

2. **Run IIS Express from the command line**: Navigate to the IIS Express installation folder using the command `cd \\Program Files\\IIS Express` or `cd \\Program Files (x86)\\IIS Express` for a 64-bit OS. Then, run your site using one of the following commands¹:

   - `iisexpress /site:WebSite1` to run the WebSite1 site from the user profile configuration file.
   - `iisexpress /config:c:\\myconfig\\applicationhost.config` to run the first site in the specified configuration file.
   - `iisexpress /path:c:\\myapp\\ /port:80` to run the site from the c:\\myapp folder over port 80.

3. **Set the Default Page in ASP.NET**: You can set the default page in your web.config file. Add the following lines in the `<system.webServer>` tag²:

```xml
<defaultDocument>
  <files>
    <clear />
    <add value="Default.aspx" />
  </files>
</defaultDocument>
```

Please replace "Default.aspx" with the name of your default page if it's different. This will ensure that when people first visit your website, they will see your default page.

Remember to save all changes and restart the server for the changes to take effect. Let me know if you need further assistance!

Source: Conversation with Copilot, 7/29/2024
(1) How to make IIS Express the default web server in Visual Studio?. https://stackoverflow.com/questions/8171378/how-to-make-iis-express-the-default-web-server-in-visual-studio.
(2) Running IIS Express from the Command Line | Microsoft Learn. https://learn.microsoft.com/en-us/iis/extensions/using-iis-express/running-iis-express-from-the-command-line.
(3) iis 7 - How to set the Default Page in ASP.NET? - Stack Overflow. https://stackoverflow.com/questions/1913058/how-to-set-the-default-page-in-asp-net.
(4) How to enable Directory browsing by default on IIS Express. https://stackoverflow.com/questions/8543761/how-to-enable-directory-browsing-by-default-on-iis-express.
(5) undefined. https://learn.microsoft.com/en-us/iis/configuration/system.webServer/defaultDocument/.
