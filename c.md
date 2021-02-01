## Returning a String from a function 
In C, Strings are arrays of char. We need to return 
a pointer to the first element of the string. 
```c
const char* hello() {
  return "Hello World";
}

int main(int argc, const char * argv[]) {
  printf("%s", hello());
  return 0;
}
```