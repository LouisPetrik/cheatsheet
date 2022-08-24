# C++ Cheatsheet 

## Overview

- ### [Variables and Data Types]
- ### [Stack, Heap and Static Memory]
- ### [Pointers]
- ### [Object Orienation]



## Variables and Data Types



## Stack, Heap and Static Memory

### Allocating variables in the memory

```cpp 
int main() {
  // in the stack
  int value = 5; 
  int array[5]; 

  // in the heap 
  int* hvalue = new int, 
  // dereferencing it 
  *hvalue = 5; 

  int* harray = new int[5];
}

```

Generally, allocate heap objects with the *-notation. 