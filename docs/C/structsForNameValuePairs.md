AI Overview
Learn more
…
C
```c
typedef struct NameValuePair {
    char *name;  // String representing the key (name)
    void *value; // Pointer to the value (can be any data type)
} NameValuePair; 
```
## Explanation: 
- typedef:
This keyword creates a new type named NameValuePair which can be used like any other data type in C. 
- struct:
This keyword defines a structure, which is a collection of related data items grouped together under a single name. 
- name:
A character pointer (char *) to store the string representing the "key" or "name" in a key-value pair. 
- value:
A void pointer (void *) to store the actual value associated with the key. This allows the value field to hold data of any type, as you need to explicitly cast it to the correct type when accessing it. 
How to use:
Code
```c
NameValuePair pair1;

pair1.name = "username"; 

pair1.value = (void *)"johndoe"; // Cast to void* as the value is a string



NameValuePair pair2;

pair2.name = "age"; 

pair2.value = (void *)30; // Cast to void* as the value is an integer
```

## Important Considerations: 
- Memory Management:
Since the name field is a character pointer, you need to allocate memory for the string using malloc and deallocate it using free when you no longer need it.
- Type Safety:
Because the value field is a void *, you need to cast it to the correct data type when accessing it. This means you need to keep track of the data type stored in each NameValuePair instance.
- Dynamic Arrays:
To create a collection of name-value pairs, you can use an array of NameValuePair structs or a linked list, depending on your needs. 
Alternative Design (with a type indicator):
If you want to add more type safety, you could modify the structure to include a type indicator:
Code
```c
typedef struct NameValuePair {

    char *name;

    enum DataType { STRING, INTEGER, FLOAT } type;

    void *value;

} NameValuePair;
```

This allows you to store the data type of the value and check it before accessing it to ensure correct usage.