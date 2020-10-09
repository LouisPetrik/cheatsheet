# Go

## Getting started

### Hello world

#### code - app.go

```go
package main // a package must be defined

import "fmt"

func main() {
  fmt.Println("Hello World!")
}
```

#### executing it

```bash
go run app.go
go build app.go
./app.go
```

## Variables

### short declaring

This works inside of functions

```go
func main() {
   name := "Max"
   fmt.Println(name)
}
```

### var keyword for declaring

```go
var name string = "Max"
var age = 23
// declaring more than one variable:
var name, age = "Max", 23

func main() {
   fmt.Println(name)
   fmt.Println(age)
}
```

It is not needed to provide the datatype when declaring.

### const keyword for declaring

with const constant variables can be declared. It works the same way as var.

## For Loops

For-Loops are the only loops which can be used in Go.

```go
func main() {
  for i := 0; i <= 3; i++ {
    fmt.Println(i) // 0, 1, 2, 3
  }
}
```

Using an already existing variable: (I did not find a better way, scoping seems to be strange)

```go
i := 0
for i := i; i <= 3; i++ {
   fmt.Println(i)
}
```

## Conditionals

```go
if true == true {
   fmt.Print("Fucking true")
} else {
   fmt.Print("Not.")
}
```

You can declare variables even within the if-syntax:

```go
if age := 9; age < 18 {
   fmt.Print("Not grown up")
}
```

When using this way to quickly declare a variable, the variable is not available outside of the if-statement.

## fmt package

fmt is a default package, used for IO.

```go
fmt.Println()
fmt.Printf()
```

Both functions from this package can be used to print something on the console.
Println() does a linebreak after printing, Printf() does not.
Printf() is for formatting strings.

```go
a := 5
b := 10
fmt.Println(a, "Numbers", b) // '5 Numbers 10'
fmt.Printf(a, "Numbers", b)  // error - variables are not a string
```

## Arrays

```go
func main() {
   // creates an empty array with 5 elements of the type string
   var names [3]string
   fmt.Println(names) // [  ]
}
```

Whitespaces when printing the empty array will indicate the number of elements, the array is made for.
No joke.

## Concurrency

In this example, there will never be "fish" printed.
Code will print sheep forever, because Go is blocking.

```go

func main() {
	count("sheep")
	count("fish")
}

func count(thing string) {
	for i := 1; true; i++ {
		fmt.Println(i, thing)
	}
}
```

We can create a go-routine, making the function execute at the same time with the go-keyword.
Now both will be executed, sheep, fish, sheep and so on.

```go
func main() {
	go count("sheep")
	count("fish")
}
```

When we make both functions a go-routine, the programm will finish instantly. Why? Because the go-routine takes the function into the background, and then continues to execute the code below.
When the second count() function also is a go routine, the main function is done.

We can avoid this by setting a Sleep or wait for userinput. Such code will block the further execution. In this example, sheep and fish will be printed for 2 seconds, then the code stops.

```go
func main() {
	go count("sheep")
	go count("fish")
	time.Sleep(time.Second * 2)
}
```
