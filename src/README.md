
# Notes

## No recursive data structures

No recursive data structures are allowed. In order to create recursive data, use an array and indices to other elements of the array. This simplifies memory management, though more complicated data structures will need their own allocators for array elements.

## Dynamic Ownership

Instead of ownership of arrays being expressed in the type system, it is expressed dynamically using an "owned" bit, which could be packed into a pointer. So an array (or slice) is represented as a pointer to the beginning, the length, and a bit indicating ownership. Slices are never owned. If an array is owned, it is deallocated when it goes out of scope. Ownership may be transferred if the array is passed to a function before it goes out of scope. This may elide more copies than C++ or Rust, and is very cheap to determine at runtime.

## Interfaces

Interfaces are used to provide functions that must exist when another function is called. Typically this is used to constrain generics. It can also be used for a static form of dependency injection.
