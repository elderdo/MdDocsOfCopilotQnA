### question

Does Oracle 19c have type XMLTYPE?

### answer

Yes, Oracle 19c supports the `XMLTYPE` data type. It is a system-defined opaque type for handling XML data. You can create columns of `XMLTYPE` and insert XML documents into them. Additionally, `XMLTYPE` provides various member functions to extract XML nodes and fragments¹²³.

### Example

```sql
CREATE TABLE my_table (
    id NUMBER,
    xml_data XMLTYPE
);
```

This allows you to store and manipulate XML data efficiently within an Oracle 19c database.

Source: Conversation with Copilot, 9/5/2024
(1) 295 XMLTYPE - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/arpls/XMLTYPE.html.
(2) Oracle XML DB Developer's Guide, 19c - Oracle Help Center. https://docs.oracle.com/en/database/oracle/oracle-database/19/adxdb/index.html.
(3) Introduction to Oracle XMLTYPE Data Type - SQLiz. https://www.sqliz.com/oracle-ref/xmltype-datatype/.
(4) Class XMLType - Oracle. https://docs.oracle.com/en/database/oracle/oracle-database/19/jaxml/oracle/xdb/XMLType.html.
