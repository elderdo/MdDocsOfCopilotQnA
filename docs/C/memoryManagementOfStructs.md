In C, memory management for structs involves allocating memory for a struct, accessing the struct's members, and using dynamic memory allocation: 
- Allocating memory
To allocate memory for a struct, declare a variable using the new data type. For example, struct flightType plane;. 
- Accessing members
To access a member of a struct, use the . operator. For example, plane.airSpeed = 800.0;. To access the struct itself, reference it by the variable name. 
- Dynamic memory allocation
C requires manual memory management, which means the programmer is responsible for explicitly deallocating memory. Dynamically allocated memory persists until the programmer deallocates it, or a garbage collector collects it. 
- Struct layout
A struct occupies a contiguous block of memory, with each field located at a fixed offset from the start. The members of a struct are laid out in the order specified by the definition. 
Structs that are small enough and don't contain reference types are allocated on the stack. If a struct is too large to fit on the stack, the runtime may allocate it on the heap. 

- [How C structs Use Memory](https://www.youtube.com/watch?v=eioSR8_YKEc)
- [C Programming Language Tutoriall - Advanced - Memory...](https://www.youtube.com/watch?v=RjAU1JKWA1w)
- [C Programming: Lesson 3, malloc, free and struct](https://www.youtube.com/watch?v=1WA7RSta0Ds)
- [How are struct members allocated in memory?](https://stackoverflow.com/questions/20737176/how-are-struct-members-allocated-in-memory)
- [C - Structs and Dynamic Memory Allocation](https://cse.buffalo.edu/~eblanton/course/cse220-2019-2f/materials/08-structs-malloc.pdf)
- [Best practice for struct management](https://www.reddit.com/r/C_Programming/comments/1b2ur2x/best_practice_for_struct_management/)
- [Memory Allocation for Structure](https://www.avabodh.com/cin/structure.html)