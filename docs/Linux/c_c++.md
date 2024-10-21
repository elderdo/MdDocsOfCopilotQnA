### question
what linux implementations have a c compiler?

### answer
Most Linux distributions come with a C compiler, or provide one that can easily be installed. The most common C compiler on Linux is **GCC (GNU Compiler Collection)**, but there are several others available depending on your needs. Here are a few implementations of C compilers that you can find or install on various Linux distributions:

### 1. **GCC (GNU Compiler Collection)**
   - **Default on most distributions**: Most Linux distros include or provide access to GCC through their package managers.
   - **Installation**:
     - Ubuntu/Debian: `sudo apt install gcc`
     - Fedora: `sudo dnf install gcc`
     - Arch: `sudo pacman -S gcc`
   - **Features**: Support for C, C++, and other programming languages like Fortran, Objective-C.

### 2. **Clang/LLVM**
   - **Alternative to GCC**: Clang is part of the LLVM project and offers better diagnostics and is often faster than GCC.
   - **Installation**:
     - Ubuntu/Debian: `sudo apt install clang`
     - Fedora: `sudo dnf install clang`
     - Arch: `sudo pacman -S clang`
   - **Features**: More modular, and great for tools and custom compilers.

### 3. **TinyCC (TCC)**
   - **Lightweight compiler**: A very fast and small compiler that can be used for smaller projects or embedded systems.
   - **Installation**:
     - Ubuntu/Debian: `sudo apt install tcc`
   - **Features**: Very small footprint, but doesn’t support all C standards.

### 4. **Intel C Compiler (ICC)**
   - **High performance**: Proprietary C compiler developed by Intel, aimed at high-performance computing.
   - **Installation**: Available for download from Intel’s website.
   - **Features**: Optimized for Intel processors, supports advanced vectorization and performance tuning.

### 5. **PGI/HP C Compiler (NVIDIA HPC SDK)**
   - **Specialized for HPC**: Useful for high-performance computing and supports GPUs.
   - **Installation**: Available via the NVIDIA HPC SDK.
   - **Features**: Optimized for multi-core and GPU computing.

### 6. **PCC (Portable C Compiler)**
   - **Old-school**: One of the oldest compilers, still maintained and useful for certain legacy systems.
   - **Installation**: Can be built from source or installed on some systems via package managers.
   - **Features**: Supports older C standards.

For most users, **GCC** and **Clang** are the most common and widely used. They are both free, open-source, and available on nearly all Linux distributions.