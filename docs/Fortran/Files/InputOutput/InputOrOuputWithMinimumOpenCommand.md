### question

For Fortran applications what determines if a file is being used as input or output if an OPEN command only uses UNIT and the Filename?

### answer

If the **OPEN** command in Fortran only specifies **UNIT** and the **Filename**, the file's usage as input or output is determined by the subsequent **READ** or **WRITE** statements.

### Example:

#### Input File:

```fortran
PROGRAM ReadData
  INTEGER :: io
  REAL :: a, b
  OPEN(UNIT=10, FILE='input.dat')
  READ(10, *) a, b
  CLOSE(10)
END PROGRAM ReadData
```

In this example, `input.dat` is used as an input file because the **READ** statement is used.

#### Output File:

```fortran
PROGRAM WriteData
  INTEGER :: io
  REAL :: x(100), y(100)
  INTEGER :: i
  DO i = 1, 100
    x(i) = i * 0.1
    y(i) = SIN(x(i))
  END DO
  OPEN(UNIT=20, FILE='output.dat')
  DO i = 1, 100
    WRITE(20, *) x(i), y(i)
  END DO
  CLOSE(20)
END PROGRAM WriteData
```

In this example, `output.dat` is used as an output file because the **WRITE** statement is used.

The **READ** and **WRITE** statements determine the file's role when the **OPEN** command does not explicitly specify **ACTION** or **STATUS**.
