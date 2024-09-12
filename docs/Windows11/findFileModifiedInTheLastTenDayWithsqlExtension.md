### question
What is a quick way to search for files with an sql extension that were created in the last 10 days?

### answer
You can use the `find` command in a Unix-based system to search for `.sql` files created in the last 10 days. Here's the command:

```sh
find /path/to/search -name "*.sql" -type f -mtime -10
```

- `/path/to/search`: Replace this with the directory path where you want to search.
- `-name "*.sql"`: This specifies that you are looking for files with an `.sql` extension.
- `-type f`: This ensures that only files are searched.
- `-mtime -10`: This finds files modified in the last 10 days.

If you're using Windows, you can use PowerShell:

```powershell
Get-ChildItem -Path "C:\path\to\search" -Filter "*.sql" -Recurse | Where-Object { $_.CreationTime -gt (Get-Date).AddDays(-10) }
```

- `-Path "C:\path\to\search"`: Replace this with the directory path where you want to search.
- `-Filter "*.sql"`: This specifies that you are looking for files with an `.sql` extension.
- `-Recurse`: This searches all subdirectories.
- `Where-Object { $_.CreationTime -gt (Get-Date).AddDays(-10) }`: This filters files created in the last 10 days.

Let me know if you need any more help!