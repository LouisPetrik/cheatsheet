# PureScript Cheatsheet


## Overview 

- ### [Functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#functions)
- ### [Using the console](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#using-the-console-1)
- ### [Records](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#records)
- ### [Conditionals](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#conditionals-1)
- ### [Impure functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#impure-functions-1)
- ### [Map, Reduce and Filter](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#map-reduce-and-filter-1)


## The REPL 

You can start the REPL with "spago repl". 

You can import any module from the src-directory with: "import Module"
For example, "import Main". 

From now on, you can access data written in the file in the REPL. 
Whenever you change something in the file, make sure to reload the REPL: 
":reload". 

To quit: ":quit".

Getting the type of some data: ":type <data>"

## Great ressources for PureScript 

- [The official PureScript book](https://book.purescript.org/)
- [Jordan's notes](https://jordanmartinez.github.io/purescript-jordans-reference-site/Preface.html) 
- [Pursuit](https://pursuit.purescript.org/)

## Functions

### Writing a function with a return

```haskell
double a = a * 2
```
This functions receives a parameter "a" and returns it (everything behind the equals-sign). 

#### Providing types for the function: 

```haskell
double :: Int -> Int 
double a = a * 2
```
This is optional, yet, a best practise. 

### Writing a function with more than one parameter: 

```haskell 
add :: Int -> Int -> Int 
add a b = a + b 
```

Since functions are curried, just add another Int -> for each parameter. 
The code, under the hood looks like this: 

```javascript
function add(a) {
  return function(b) {
    return a + b
  }
}
```
Confusing, hm? 

## Function recursion 
Recursive is a function that calls itself. This is really useful to write clean code. 
The following example is a recursive function. It receives a number as starting value, 
and calls the function as long the number is < 10. Of course, this is absolutely useless, because it makes out of any passed number below 10 a 10 as return. 

```haskell 
sumToTen :: Int -> Int 
sumToTen n = 
  if n == 10 then 
    10 
  else 
    sumToTen (n + 1)

main = log (show (sumToTen 0))
```
Result will be "10". 

## Using the console 
To log something in the console, make sure to import the proper package. 
Then, in the main function, we can log like this: 

```haskell
import Effect (Effect)

main = log "Hello world"
```

### Logging more than one thing
To run more than one thing in the main function, we need to use the do-keyword an proper indentation: 

```haskell 
main = do
  log "Hello world"
  log "Hello World"
```

### Logging an expression
In case of a variable: 

```haskell 
name = "John Doe"

main = log (show name) 
```

In case of a function-call: 

```haskell 
double :: Int -> Int 
double x = x * 2

main = log (show (double 2))
```
This will get "4" logged. Surprise! 
Please notice the additional braces around the function-call. 
Optionally, store the function call in a variable before logging. 

### Concatinating stuff in the console 
In this example, we have a function for doubling an integer. The second function
calls the first one, but converts and returns the first ones value to a string. 
Plus, it concatinates it with another string: 

```haskell 
doubleNumber :: Int -> Int
doubleNumber x = x * 2

printDoubleNumber :: Int -> String 
printDoubleNumber x = "The Result: " <> (show (doubleNumber x))

main = log (printDoubleNumber 2)
```

In the console: 
"The Result: 4" 

## Records
Records are basically like Objects in JS. Yet, immutable, of course. 
As with functions, we can and should describe their structure and types before
initialising them: 

```haskell
type Person =
  { name :: String,
    age :: Int }

max :: Person
max = { name: "Max", age: 22 }

main = log (show max)
```

The code will log the following in the console: 
```
{ age: 22, name: "Max" }
```
As you acn see, just like an object in JavaScript. 

### Accessing a records properties
Accessing those works just like in JavaScript, using the "."-operator. 

```haskell
main = log (show max.name)
```

We can also create a function to access the name-property of a Person-type variable: 

```haskell
getName :: Person -> String 
getName person = person.name 

main = log (getName max)
```


## Conditionals 

```haskell
biggerThan10 :: Int -> String 
biggerThan10 num = 
  if num > 10 
  then "Number is > 10"
  else "Number is NOT > 10"


main = log (biggerThan10(2))
```
The output: "Number is NOT > 10". 

You can also pass a condition itself as a parameter, using the type Boolean: 

```haskell 
test :: Boolean -> String
test condition =
  if condition
  then "true"
  else "false"

main = log (test(1 > 2))
```
The output: "false" 

## Impure functions 
By definition, pure functions do not change anything outside of their scope. Logging something in the 
console therefore is an impure function. 
Of course we can log something from a function, instead of returning a value. 
This is the case of an impure function: 

```haskell
logSomething :: String -> Effect Unit 
logSomething message = log ("My message: " <> message)


main = logSomething "Hello"
```

## Map, Reduce and Filter 

### Map 
Map transforms a structure based on a pattern, which can be applied to each element. 
The following code doubles all the number in the passed array: 

```haskell
map (\n -> n + 1) [1, 2, 3]
```

Returns "[2, 4, 6]"

We can also use a function to be passed into the Map, applied to each element: 

```haskell
addOne :: Int -> Int 
addOne x = x + 1 

newArr = map addOne [1, 2, 3] 
```

The same works for predefined functions: 

```haskell
map show [1, 2, 3]
-- ["1","2","3"]
```

## Arrays 


## Importing modules 

When checking in the REPL (spago repl) whether a function is defined, you might find out, it isn't. 
For example, run ":type last" in the REPL - by default, an error occurs. 
The function needs to be imported first: 

```haskell
import Data.Array (last)
--- Or, to import all functions from the package: 
import Data.Array
```
