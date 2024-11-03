### question
How do I configure project XmlSearch so that the xml test files gets copied from the project folder to the same folder as the exe - for Debug or Release?

### answer
To configure your project so that the XML test files are copied from the project folder to the same folder as the executable (for both Debug and Release configurations), you can use the post-build event feature in Visual Studio. This feature allows you to specify commands that run after the build process completes.
Here are the steps to set this up:
1.	Open Project Properties:
•	Right-click on your project in the Solution Explorer and select "Properties".
2.	Navigate to Build Events:
•	In the Project Properties window, navigate to "Build Events" on the left-hand side.
3.	Edit Post-Build Event:
•	In the "Post-Build Event" section, click on the "Edit Post-Build" button.
4.	Add Copy Command:
•	Add the following command to copy the XML test files from the project folder to the output directory:
```batch
xcopy "$(ProjectDir)path\to\xml\files\*.xml" "$(TargetDir)" /Y /I
```    

 - Replace `path\to\xml\files\` with the relative path to your XML files within the project directory.
 - The `/Y` option suppresses prompting to confirm you want to overwrite an existing destination file.
 - The `/I` option specifies that the destination is a directory if the source is multiple files and the destination does not exist.

5.	Save and Close:
•	Click "OK" to save the post-build event command.
•	Click "Apply" and then "OK" to close the Project Properties window.
Example
If your XML files are located in a folder named TestFiles within your project directory, the command would look like this:
```batch
xcopy "$(ProjectDir)TestFiles\*.xml" "$(TargetDir)" /Y /I
```

## Summary
By following these steps, you ensure that every time you build your project, the XML test files are automatically copied to the output directory where the executable is located. This setup works for both Debug and Release configurations.



