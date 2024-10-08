### question

Here's an Oracle query:

```sql
select crib, count(*)
from trans
where typedescription in ('ISSRT', 'ISSUE','RETN','RETNW')
and crib in ('808','817','8118')
and to_date(transdate,'mm/dd/yyyy') = '09/17/2024'
group by rollup (crib);
```

Can you optimize it using Oracle functions so that it is efficient yet does basically the same functionality? Also, can you add the text Total for the count(\*) aggregation function?

#### answer

Certainly! Here's an optimized version of your Oracle query using Oracle functions and adding the text "Total" for the count(\*) aggregation:

```sql
SELECT NVL(crib, 'Total') AS crib, COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

Changes made:

1. Used `NVL` function to replace `NULL` values with 'Total' in the `crib` column.
2. Simplified the `TO_DATE` function usage for the `transdate` column.

### question

Here's an Oracle query:

```sql
select crib, count(*)
from trans
where typedescription in ('ISSRT', 'ISSUE','RETN','RETNW')
and crib in ('808','817','8118')
and to_date(transdate,'mm/dd/yyyy') = '09/17/2024'
group by rollup (crib);
```

Can you optimize it using Oracle functions so that it is efficient yet does basically the same functionality? Also, can the Text 'Total' for the rollup amount?

### answer

Certainly! Here's an optimized version of your Oracle query using Oracle functions and adding the text "Total" for the rollup amount:

```sql
SELECT NVL(crib, 'Total') AS crib, COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

Changes made:

1. Used `NVL` function to replace `NULL` values with 'Total' in the `crib` column.
2. Simplified the `TO_DATE` function usage for the `transdate` column.

### question

Your query

```sql
SELECT NVL(crib, 'Total') AS crib, COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

assign Text to crib if it is null, but what I would like is to have 'Text' appear to the final ROLLUP amount which is the sum of eac count and appears as the last number that is output by itself. Can you do that?

### answer

Certainly! Here's the updated query to have 'Total' appear for the final ROLLUP amount:

```sql
SELECT
  CASE
    WHEN GROUPING(crib) = 1 THEN 'Total'
    ELSE crib
  END AS crib,
  COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

Changes made:

1. Used `CASE` statement with `GROUPING` function to assign 'Total' to the final rollup amount.
2. Ensured the `crib` column retains its original values unless it's the final rollup amount.

### question

Your query

```sql
SELECT
  CASE
    WHEN GROUPING(crib) = 1 THEN 'Total'
    ELSE crib
  END AS crib,
  COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

produces the following error:
[Error] Execution (4: 10): ORA-00932: inconsistent datatypes: expected CHAR got NUMBER

What I want is something like this:
808 31
817 61
8118 23
Total 115
Where the first three lines are the crib and their count and the final line has the text Total followed by the sum of the 3 counts 31 + 61 + 23 = 115.
Can you fix the query so the error does not occur and the rollup number has the text Total followed by the rollup number?

### answer

The error occurs because the `crib` column is being treated as a number in some cases and as a string in others. To fix this, you can cast the `crib` column to a string. Here's the corrected query:

```sql
SELECT
  CASE
    WHEN GROUPING(crib) = 1 THEN 'Total'
    ELSE TO_CHAR(crib)
  END AS crib,
  COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

This should produce the desired output without the error.

### question

Your query

```sql
SELECT
  CASE
    WHEN GROUPING(crib) = 1 THEN 'Total'
    ELSE crib
  END AS crib,
  COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

produces the following error:
[Error] Execution (4: 10): ORA-00932: inconsistent datatypes: expected CHAR got NUMBER

What I want is something like this:
808 31
817 61
8118 23
Total 115
Where the first three lines are the crib and their count and the final line has the text Total followed by the sum of the 3 counts 31 + 61 + 23 = 115.
Can you fix the query so the error does not occur and the rollup number has the text Total followed by the rollup number?

Trying your answer

```sql
SELECT
  CASE
    WHEN GROUPING(crib) = 1 THEN 'Total'
    ELSE TO_CHAR(crib)
  END AS crib,
  COUNT(*) AS count
FROM trans
WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
  AND crib IN ('808', '817', '8118')
  AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
GROUP BY ROLLUP(crib);
```

The error went away but no data was returned whereas I was getting the 3 lines of data an the rollup amount before using the CASE statement. So, whatever the CASE statement does it eliminates all the data. Any idea how to fix it so data is returned along with the final rollup preceded by the word Text? Does a subquery have to be used?

### answer

The issue might be related to the data types or the way the `GROUP BY ROLLUP` is being handled. Let's try using a subquery to ensure the data types are consistent and then apply the `CASE` statement:

```sql
SELECT
  CASE
    WHEN crib = 'Total' THEN 'Total'
    ELSE crib
  END AS crib,
  count
FROM (
  SELECT
    CASE
      WHEN GROUPING(crib) = 1 THEN 'Total'
      ELSE TO_CHAR(crib)
    END AS crib,
    COUNT(*) AS count
  FROM trans
  WHERE typedescription IN ('ISSRT', 'ISSUE', 'RETN', 'RETNW')
    AND crib IN ('808', '817', '8118')
    AND transdate = TO_DATE('09/17/2024', 'MM/DD/YYYY')
  GROUP BY ROLLUP(crib)
) subquery;
```

This should ensure that the data is returned correctly along with the final rollup preceded by the word "Total".
