### question

Could you write a Perl script that would read an XML file and load it to an Oracle Table by injecting the XML into a SQL\*Plus script or a Perl API that could do the equivalent of this:

```sql
DECLARE
  xml_data   CLOB := '<employees>
                      <employee>
                        <id>1</id>
                        <name>John Doe</name>
                        <birthdate>1980-01-01</birthdate>
                        <salary>50000</salary>
                      </employee>
                      <employee>
                        <id>2</id>
                        <name>Jane Smith</name>
                        <birthdate>1985-05-15</birthdate>
                        <salary>60000</salary>
                      </employee>
                    </employees>';
BEGIN
  INSERT INTO your_table (id,
                          name,
                          birthdate,
                          salary)
          SELECT x.id,
                 x.name,
                 TO_DATE (x.birthdate, 'YYYY-MM-DD'),
                 x.salary
            FROM XMLTABLE ('/employees/employee'
                           PASSING XMLTYPE (xml_data)
                           COLUMNS id           NUMBER PATH 'id',
                                   name         VARCHAR2 (100) PATH 'name',
                                   birthdate    VARCHAR2 (10) PATH 'birthdate',
                                   salary       NUMBER PATH 'salary') x;

  DBMS_OUTPUT.put_line ('No Of rows' || SQL%ROWCOUNT);
END;
/
```

### answer

Here's a Perl script that reads an XML file and loads it into an Oracle table using the `DBI` and `XML::LibXML` modules:

```perl
use strict;
use warnings;
use DBI;
use XML::LibXML;

# Oracle connection details
my $dsn = 'dbi:Oracle:host=hostname;port=port;sid=sid';
my $username = 'username';
my $password = 'password';

# Connect to Oracle database
my $dbh = DBI->connect($dsn, $username, $password, { RaiseError => 1, AutoCommit => 0 })
    or die $DBI::errstr;

# Read XML file
my $xml_file = 'employees.xml';
my $parser = XML::LibXML->new();
my $doc = $parser->parse_file($xml_file);

# Convert XML to CLOB
my $xml_data = $doc->toString();

# SQL*Plus script
my $sql_script = qq{
DECLARE
  xml_data   CLOB := q'{$xml_data}';
BEGIN
  INSERT INTO your_table (id, name, birthdate, salary)
          SELECT x.id,
                 x.name,
                 TO_DATE(x.birthdate, 'YYYY-MM-DD'),
                 x.salary
            FROM XMLTABLE ('/employees/employee'
                           PASSING XMLTYPE(xml_data)
                           COLUMNS id           NUMBER PATH 'id',
                                   name         VARCHAR2(100) PATH 'name',
                                   birthdate    VARCHAR2(10) PATH 'birthdate',
                                   salary       NUMBER PATH 'salary') x;

  DBMS_OUTPUT.put_line('No Of rows' || SQL%ROWCOUNT);
END;
};

# Execute SQL script
my $sth = $dbh->prepare($sql_script);
$sth->execute();
$dbh->commit();

# Close connection
$sth->finish();
$dbh->disconnect();
```

### Steps:

1. **Install Required Modules**: Ensure you have the `DBI` and `XML::LibXML` modules installed. You can install them using CPAN:
   ```sh
   cpan DBI
   cpan XML::LibXML
   ```
2. **Oracle Connection**: Update the connection details (`hostname`, `port`, `sid`, `username`, `password`) with your Oracle database credentials.
3. **XML File**: Place your XML file (`employees.xml`) in the same directory as the script or provide the correct path.
4. **Table Name**: Replace `your_table` with the actual name of your Oracle table.

This script reads the XML file, converts it to a CLOB, and executes the SQL script to insert the data into the Oracle table.

Source: Conversation with Copilot, 8/15/2024
(1) Read XML file in perl - Stack Overflow. https://stackoverflow.com/questions/30171183/read-xml-file-in-perl.
(2) Extracting xml values using perl - Stack Overflow. https://stackoverflow.com/questions/29255614/extracting-xml-values-using-perl.
(3) Read XML file using perl - Stack Overflow. https://stackoverflow.com/questions/25522910/read-xml-file-using-perl.
(4) oracle - how to run a sql(.sql file) via perl script - Stack Overflow. https://stackoverflow.com/questions/25562617/how-to-run-a-sql-sql-file-via-perl-script.
(5) perl script to read xml file and create a property file. https://stackoverflow.com/questions/18224039/perl-script-to-read-xml-file-and-create-a-property-file.
