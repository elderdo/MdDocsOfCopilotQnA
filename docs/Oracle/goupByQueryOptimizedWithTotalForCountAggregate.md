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
