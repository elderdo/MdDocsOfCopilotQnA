### question

Can you write a Python script that would read an excel spreadsheet with column headings that may not be in row 1 and contain the following: 5 regex strings: 1. SOS 2. TRAN. _DT 3. DOC._ 4. FRT.\*ADJ 5. NSN. It would extract every data row and append the file name with a prepended timestamp in the format YYYY_MM_DD_theFilename.xlsx and loads each row into an Oracle table wbTable with columns sos, tranDate, DocNum, Qty, filename.

### answer

```python
import pandas as pd
import re
import cx_Oracle
import argparse
import os
from datetime import datetime

def find_header_row(sheet, regex_list):
    for i, row in sheet.iterrows():
        if all(any(re.match(regex, str(cell)) for cell in row) for regex in regex_list):
            return i
    return None

def extract_data(sheet, header_row, regex_dict):
    data = []
    for i, row in sheet.iterrows():
        if i <= header_row:
            continue
        row_data = {}
        for col_name, regex in regex_dict.items():
            for cell in row:
                if re.match(regex, str(cell)):
                    row_data[col_name] = cell
                    break
        if row_data:
            data.append(row_data)
    return data

def load_to_oracle(data, filename, user, password):
    dsn = cx_Oracle.makedsn('hostname', 'port', service_name='service_name')
    connection = cx_Oracle.connect(user=user, password=password, dsn=dsn)
    cursor = connection.cursor()

    for row in data:
        sql = """
        INSERT INTO wbTable (sos, tranDate, DocNum, Qty, filename)
        VALUES (:1, :2, :3, :4, :5)
        """
        cursor.execute(sql, (row.get('SOS'), row.get('TRAN_DT'), row.get('DOC'), row.get('FRT_ADJ'), filename))

    connection.commit()
    cursor.close()
    connection.close()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Extract data from Excel and load to Oracle.')
    parser.add_argument('-f', '--file', required=True, help='Path to Excel file')
    parser.add_argument('-u', '--user', required=True, help='Oracle username')
    parser.add_argument('-p', '--password', required=True, help='Oracle password')
    args = parser.parse_args()

    regex_dict = {
        'SOS': r'SOS',
        'TRAN_DT': r'TRAN.*DT',
        'DOC': r'DOC.*',
        'FRT_ADJ': r'FRT.*ADJ',
        'NSN': r'NSN'
    }

    sheet = pd.read_excel(args.file, sheet_name=0, header=None)
    header_row = find_header_row(sheet, regex_dict.values())
    if header_row is None:
        raise ValueError("Header row not found")

    data = extract_data(sheet, header_row, regex_dict)
    timestamp = datetime.now().strftime('%Y_%m_%d')
    filename = f"{timestamp}_{os.path.basename(args.file)}"
    load_to_oracle(data, filename, args.user, args.password)
```

### Steps:

1. **Install Required Modules**: Ensure you have the `pandas`, `cx_Oracle`, and `openpyxl` modules installed. You can install them using pip:
   ```sh
   pip install pandas cx_Oracle openpyxl
   ```
2. **Update Connection Details**: Replace `hostname`, `port`, `service_name`, and `wbTable` with your actual Oracle database details.
3. **Run the Script**: Use the following command to run the script:
   ```sh
   python script_name.py -f /path/to/excel_file.xlsx -u oracle_user -p oracle_password
   ```

Source: Conversation with Copilot, 8/15/2024
(1) Python: how to read the data from the oracle, and then write to Excel .... https://stackoverflow.com/questions/43962703/python-how-to-read-the-data-from-the-oracle-and-then-write-to-excel.
(2) Excel data extraction using regular expressions through Python. https://stackoverflow.com/questions/74216690/excel-data-extraction-using-regular-expressions-through-python.
(3) Regular expression to extract data from a table in python. https://stackoverflow.com/questions/16036337/regular-expression-to-extract-data-from-a-table-in-python.
(4) PythonEasy | Python how to read the data from the oracle database and .... https://www.pythoneasy.com/learn/python-how-to-read-the-data-from-the-oracle-database-and-then-write-to-excel.
(5) undefined. https://i.sstatic.net/Htdze.png%29.
