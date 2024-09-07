### I found a Fortran 2003 github project for fpm: https://github.com/fortran-lang/fpm. I was looking at the code and saw several interesting uses for whatever version of Fortran is being used for this project.

```fortran
type, extends(dependency_config_t) :: dependency_node_t
    !> Actual version of this dependency
    type(version_t), allocatable :: version
    !> Installation prefix of this dependencies
    character(len=:), allocatable :: proj_dir
    !> Checked out revision of the version control system
    character(len=:), allocatable :: revision
    !> Dependency is handled
    logical :: done = .false.
    !> Dependency should be updated
    logical :: update = .false.
    !> Dependency was loaded from a cache
    logical :: cached = .false.
  contains
    !> Update dependency from project manifest.
    procedure :: register
    !> Get dependency from the registry.
    procedure :: get_from_registry
    procedure, private :: get_from_local_registry
    !> Print information on this instance
    procedure :: info

    !> Serialization interface
    procedure :: serializable_is_same => dependency_node_is_same
    procedure :: dump_to_toml => node_dump_to_toml
    procedure :: load_from_toml => node_load_from_toml

  end type dependency_node_t
```

Can you explain what this type is? It appears to use inheritance and is extending an existing type. Also, it uses some syntax for defining procedures that I am not familiar with. For example what does this define?

```fortran
 procedure :: load_from_toml => node_load_from_toml
```

Is it similar to a C# interface? What does the => mean?

The `type, extends(dependency_config_t) :: dependency_node_t` is a derived type in Fortran that extends from the `dependency_config_t` type. This is similar to inheritance in object-oriented programming languages like C++. The `dependency_node_t` type inherits all the members and methods of `dependency_config_t` and can add additional members and methods.

The `procedure :: load_from_toml => node_load_from_toml` is a way to define a type-bound procedure in Fortran. It's similar to defining a method in a class in other object-oriented languages. The `=>` symbol is used to associate the procedure `load_from_toml` with the actual procedure `node_load_from_toml`. This means that when `load_from_toml` is called on an object of type `dependency_node_t`, the procedure `node_load_from_toml` will be executed.

This is somewhat similar to an interface in C#, in the sense that it defines a contract for a method (in this case `load_from_toml`) that the type (`dependency_node_t`) must fulfill. However, unlike C# interfaces, which only declare method signatures, this Fortran construct also associates the method with an actual procedure (`node_load_from_toml`).

Here's a breakdown of the `dependency_node_t` type:

- **Data Members**:

  - `version`: An allocatable object of type `version_t` representing the actual version of this dependency.
  - `proj_dir`: A character string representing the installation prefix of this dependency.
  - `revision`: A character string representing the checked-out revision of the version control system.
  - `done`: A logical flag indicating whether the dependency is handled.
  - `update`: A logical flag indicating whether the dependency should be updated.
  - `cached`: A logical flag indicating whether the dependency was loaded from a cache.

- **Procedures**:
  - `register`: Updates the dependency from the project manifest.
  - `get_from_registry`: Gets the dependency from the registry.
  - `get_from_local_registry`: A private procedure.
  - `info`: Prints information on this instance.
  - `serializable_is_same`: A serialization interface procedure associated with `dependency_node_is_same`.
  - `dump_to_toml`: A serialization interface procedure associated with `node_dump_to_toml`.
  - `load_from_toml`: A serialization interface procedure associated with `node_load_from_toml`.

I hope this helps! Let me know if you have any other questions.
