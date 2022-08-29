# C++ Cheatsheet 

## Overview

- ### [Variables and Data Types]
- ### [Arrays]
- ### [Dynamic arrays (std::vector)]
- ### [Stack, Heap and Static Memory]
- ### [Pointers]
- ### [References]
- ### [new-keyword]
- ### [Classes]
- ### [Structs]





## Variables and Data Types


## Arrays

An array is a collecting of data, of the same type and with a fixed length. 

### Defining an array: 

```cpp
int example[5]; 
example[0] = 1; 
example[1] = 2; 
```

 
### Using a for loop shorthand
```cpp
int example[5]; 

for (int i = 0; i < 5; i++) 
  example[i] = 2; 
```

### Using pointers on arrays 
In theory, one could use pointer arithmetics to set single values in the array. Here is an example. 
First, the third value in the array will be set to 5, then, via the pointer, to 6. 
```cpp
int example[5];
int* ptr = example;
  
example[2] = 5;
// changing the value with the pointer:
*(ptr + 2) = 6;

// prints 6   
cout << example[2] << endl;

```

### Creating arrays on the heap 

```cpp
int* anotherArray = new int[5]; 
```

As this array is now created on the heap, the data will last until we destroy it manually: 
```cpp
delete[] anotherArray; 
```

### Getting the size of an array

Getting the number of elements of an array is different for array on the stack vs. arrays on the heap. 

```cpp
// this array is created on the stack 
int a[5]; 
// is the number of elements in this array: 
int count = sizeof(a) / sizeof(int); 

// this array is created on the heap: 
int* example = new int[5]; 
```

For arrays on the heap you can't retrieve the size via a function. You need to keep track of it yourself. 

### Returning an array from a function

```cpp
int *returnArr() {
  int coordinates[2] = {1, 2};
  return coordinates;
}

int main()
{
  int x = returnArr()[0];
}
```

### Passing an array as a parameter
Its as simple as that. 

```cpp
void doubleNumbers(int numbers[3]) {
  for (int i = 0; i < 3; i++) {
    cout << numbers[i] * 2 << endl;
  }
}

int main() {
  int numbers[3] = {1, 2, 3};
  doubleNumbers(numbers);
  return 0;
}
```

## Dynamic arrays (std::vector)
std::vector are usually not called vectors, often array links or dynamic arrays. 
Usually, you do not to provide a length to the vector - you can add elements and it will resize under the hood, 
as inside of the container a new array is created on each resize. 

First of all, you need to include the container: 
```cpp 
#include <vector>
```

### Creating a vector 
```cpp 
std::vector<int> numbers; 
```
Primitive types can be passed to the vector as the type. 

### Adding data to the vector: 
```cpp 
numbers.push_back(1); 
numbers.push_back(2); 
```

### Iterating over the vector: 
one can iterate over it with an for loop, as we can get the number of the elements in the vector: 

```cpp
for (int i = 0; i < numbers.size(); i++)
  cout << numbers[i] << endl; 

// or: 

for (int number : numbers)
  cout << number << endl; 
```

### Cleaning the whole vector: 
```cpp
numbers.clear(); 
```

### Returning a vector

```cpp
vector<int> returnVector() {
  vector<int> values;

  return values;
}
```

With assigning the value to another vector: 

```cpp
vector<int> returnNumbers() {
  vector<int> numbers;
  numbers.push_back(1);
  numbers.push_back(2);

  return numbers;
};

int main() {  
  vector<int> newNumbers = returnNumbers();

  cout << newNumbers[0] << endl;
  // prints "1"
  return 0;
}
```


### Passing a vector to a function 

There are two ways to do so: 
1. Passing by value
2. Passing by reference

When passing by value, inside of the recievings function scope, a copy is created: 
```cpp
void printFirstNumber(vector<int> numbers) {
  cout << numbers[0] << endl;
};

int main() {

  vector<int> numbers;

  numbers.push_back(1);
  numbers.push_back(2);
  numbers.push_back(3);

  printFirstNumber(numbers);

  return 0;
}
```

When passing by reference, we can mutate the original state: 

```cpp
void changeFirstNumber(vector<int> &numbers) {
  numbers[0] = 2;
};

int main() {
  // vector<int> numbers;
  vector<int> numbers;
  numbers.push_back(1);

  changeFirstNumber(numbers);

  cout << numbers[0] << endl;
  // prints "2" 

  return 0;
}
```


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


Stack-located stuff gets freed automatically. This happens once the end of the scope is reached. 
Everything in the heap is up to us to be cleaned up. 
Deleting stuff from the heap: 

```cpp
int main() {
  int* hvalue = new int; 
  *hvalue = 5; 

  int* harray = new int[5]; 

  delete hvalue; 
  delete[] harray;
}
```

Generally, allocate heap objects with the *-notation. 


### malloc and free 

First of all, both are pretty outdated and there are better alternatives. Use new and delete instead. 
malloc and free are the c-way of new and delete in c++. 
malloc generelly allocates memory in the heap. The parameter for the malloc-function 
is the number of bytes. malloc returns a void-pointer, so casting is necessary, therefore there is no type-safety. 
malloc will never call the constructor of the class behind the object. Also, malloc and free and delete and new must never be mixed. 

```cpp
// For an 10-length array of integers: 10 * size of int, so 40 bytes which are allocated. 
int* p = (int*)malloc(sizeof(int) * 10); 
p[0] = 100; 

// same as delete-function. 
free(p); 

```


## Pointers 

A pointer is an integer, which stores a memory address. Types can be used with pointers but more on a syntactical-level to make clear what the value retrieved will be. 
Under the hood, using types for a pointer doesn't change anything at all. 

```cpp
// surely the simplest pointer one could write. 
void* ptr = nullptr; 
```

```cpp
int var = 8; 
// assining the memory address of this value to the pointer: 
void* ptr = &var; 

// would lead to the same result: 
int* ptr = &var; 
```

Only when assining for example an double to this pointer as type, this would lead to an error. 

### Writing to the data behind the memory address: 
```cpp 
int var = 8; 
int* ptr = &var; 
*ptr = 10; 

```

### Creating a variable on the heap

```cpp
// asks for 8 bytes of memory, returns a pointer 
char* buffer = new char[8]; 
// 8 bytes, all set to 0. This is heap-allocated 
memset(buffer, 0, 8); 
// therefore needs to be deleted: 
delete[] buffer; 
```

## References 

References are related to pointers - one needs to understand the first one. They work similary like pointers. 
"References are pointers in disguise." References need to reference an already existing value, therefore they reference a variable. 

```cpp
int a = 5; 
// that's basically it "ref" is also called an alias. 
int& ref = a; 
// we can use ref now as if it were a, for example logging it. 
```

References can be used to make working with variables across different scopes easier: 
```cpp 
void increment(int value) {
  value++; 
}

int main() {
  int a = 5; 
  increment(a); 
  cout << a << endl; 
  // prints 5
}
```
The shown code will simply copy the value of a into the scope of "increment". Therefore, within the functions scope a brand new 
variable with the value of 5 will be created, and then incremented. a in the original scope of the main function is not changed. 

Instead, we can pass it as a reference in order to increment the original variable: 
```cpp
void increment(int& value) {
  value++; 
}

int main() {
  int a = 5; 
  increment(a); 
  cout << a << endl; 
  // prints 6
}
```

This does the same as the following code, but in an easier manner
```cpp
void increment(int* value) {
  (*value)++; 
}

int main() {
  int a = 5; 
  increment(&a); 
}
```

## new Keyword 

Finds a block of memory big enough for the structur and then returns a pointer to it. 
The new-keyword will allocate the memory on the heap. 
```cpp
int* b = new int;
// an array: 
int* arr = new int[50];  
```

Under the hood the new-keyword will use malloc, which should not be used by hand in C++. 

Don't forget to delete the value again: 

```cpp
delete b; 
// deleting the array: 
delete[] arr; 
```

## Classes
By default, all properties of a class are kept private, therefore not visble outside of the scope. 

```cpp 
class Position {
  public:
    int x, y;
};

int main(int argc, const char * argv[]) {
  Position position;
  position.x = 10;
  position.y = 10;
}
```


### Creating objects 

