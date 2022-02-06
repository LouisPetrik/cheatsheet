# PureScript Cheatsheet


## Overview

- ### [Getting started, tools & the REPL](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#getting-started-tools--the-repl-1)
- ### [Functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#functions-1)
- ### [Using the console](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#using-the-console-1)
- ### [Records](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#records-1)
- ### [Conditionals](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#conditionals-1)
- ### [Impure functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#impure-functions-1)
- ### [Map, Reduce and Filter](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#map-reduce-and-filter-1)
- ### [Modules]




## Getting started, tools & the REPL

Installing PureScript on your machine: 

```bash
npm install -g purescript 
npm install -g spago 
```

Creating a new project: 
(Make sure to first create an empty directory, spago will set up in the current dir)
```bash
spago init
```

Running: 
```bash
spago run
```

Building for the web: 

```bash
spago bundle-app
```


### Getting started the recommended way: 
This way avoids a global installation of PureScript to ensure working with multiple projects in different versions. 

(Make sure to first create an empty directory, spago will set up in the current dir)

```bash 
npm init -y

npm install --save-dev spago purescript

# to check whether installation works: 
npx spago version
```
As you can see, using NPM like this only installs Spago and PureScript locally, for development - bound to your project. 

Then, using NPX you can initialise the PureScript project just like before: 

```bash
npx spago init

npx spago run

npx spago run
```



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
- [Functional Programming Made Easier - Book. There is a free sample which is awesome](https://leanpub.com/fp-made-easier)




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

### Function recursion
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

### The do-keyword in functions

The do keyword allows to make code in functions more readable and to work with different expressions. In the following example, "j" and "i" are both expressions, used in a final expression at the bottom which is finally returned.


```haskell
factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) do
  i <- 1 .. n
  j <- i .. n
  [ [ i, j ] ]
```

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
As you can see, just like an object in JavaScript.

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

### Filter
A filter can be applied to a structure, to only copy the values matching a certain pattern.

The following filter filters an array for all even numbers:

```haskell
filter (\n -> mod n 2 == 0) [1, 2, 3, 4, 5, 6]
-- [2, 4, 6]
```


## Arrays

### Generating Array in Range:

```haskell
range 0 5
-- [0,1,2,3,4,5]
```
Or:

```haskell
(0 .. 5)
```

### Concatenating Arrays
The concat function takes an Array of Arrays, and concatenates them:

```haskell
concat [[1, 2, 3], [4, 5]]
-- [1,2,3,4,5]
```
### Concat Map
Map receives a function from values to values. Concat Map takes a function from values to arrays of values.

Here is a quick example:

```haskell
map (\n -> [n, n * n]) (1 .. 5)
-- [[1,1],[2,4],[3,9],[4,16],[5,25]]
```
As you can see, the map in this case produces an array of arrays. Often, this is not what we wish for.

Through the concat map, it is turned into a single array:

```haskell
concatMap (\n -> [n, n * n]) (1 .. 5)
-- [1,1,2,4,3,9,4,16,5,25]
```

## Folds

Make sure to import:
```haskell
import Data.Foldable
```

There are two basic folds: ```foldr``` which stands for "fold from the right", and ```foldl``` which stands for "fold from the left".  

```haskell
foldl (+) 0 [1, 2, 3]
-- 6

foldr (+) 0 [1, 2, 3]
-- 6
```
No matter the direction, both fold-functions sum up the values of the array. We can provide other operations than just addition:

```haskell
foldl (*) 0 [1, 2, 3, 4]
-- 0
```
But why does multiplying all the values with each other lead to "0" as a result? Because of the "0" we provide after the operator. It can be thought of as an accumulator, accumulating a result after we traversed the array.

Let's improve that to multiply all the values with each other:

```haskell
foldl (*) 1 [1, 2, 3, 4]
-- 24
```

Or, to double the output:

```haskell
foldl (*) 2 [1, 2, 3, 4]
-- 48
```

## Importing modules

When checking in the REPL (spago repl) whether a function is defined, you might find out, it isn't.
For example, run ":type last" in the REPL - by default, an error occurs.
The function needs to be imported first:

```haskell
import Data.Array (last)
--- Or, to import all functions from the package:
import Data.Array
```

## Sum and Product Types 

Let's try to explain what these mean. 

The easiest sum type to grasp in PureScript is the boolean type. 
It can be "true" or "false", must be one, and can not be both. Written in a more formal 
way: true | false. Sum types are types to which we can add more possible representations, divided through "|", so a logical OR. 

Product types are data types, defined through more than just one constructor - tuples, for example.



## Tuples, Sets and other types 

Creating a tuple: 

```haskell
import Data.Tuple 

someTuple = Tuple 2 4 
```

Getting the first value with "fst" and the second with "scd": 

```haskell 

fst someTuple 
-- 2

snd someTuple 
-- 4
```

## Modules 

Modules are the way to split up our code into multiple files. Here is an example of writing a function in
a file, and importing it in our main-module (Main.purs)

Tests.purs: 
```haskell
module Tests where

import Prelude
import Effect (Effect)
import Effect.Console (log)

print :: Effect Unit 
print = do 
  log "from Tests.purs!"
```


Main.purs: 
```haskell
module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Tests as Tests 

main :: Effect Unit
main = Tests.print 
```