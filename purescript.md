# PureScript Cheatsheet

Almost all my knowledge about PureScript, a purely functional programming language, closely related to Haskell, which compiles to JavaScript. 

## Overview

-  ### [Getting started, tools & the REPL](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#getting-started-tools--the-repl-1)
-  ### [Functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#functions-1)
-  ### [Bindings](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#bindings-1)
-  ### [Custom types](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#custom-types-1)
-  ### [Using the console](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#using-the-console-1)
-  ### [Records](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#records-1)
-  ### [Conditionals](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#conditionals-1)
-  ### [Pattern matching](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#conditionals-1)
-  ### [Map, Reduce and Filter](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#map-reduce-and-filter-1)
-  ### [Arrays](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#arrays)
-  ### [Folds](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#folds)
-  ### [Sum and Product types](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#sum-and-product-types)
-  ### [Tuples, sets and others](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#tuples-sets-and-other-types)
-  ### [Modules](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#modules-1)
-  ### [Useful default functions](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#useful-default-functions-1)
-  ### [Lists](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#lists)
-  ### [Typeclasses](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#typeclasses-1)
-  ### [Interaction with JavaScript](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#interaction-with-javascript-1)

Coming soon: guards, creating operators 

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

-  [The official PureScript book](https://book.purescript.org/)
-  [Jordan's notes](https://jordanmartinez.github.io/purescript-jordans-reference-site/Preface.html)
-  [Pursuit](https://pursuit.purescript.org/)
-  [Functional Programming Made Easier - Book. There is a free sample which is awesome](https://leanpub.com/fp-made-easier) 

The FP made easier book also serves as the original source of knowledge for this cheatsheet. 

## Functions

As the name of functional programming suggests it, this style of writing code resolves all around writing functions - so, let's cover what you need to know. 

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
	return function (b) {
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


### Higher-order functions 

A higher-order function is a function, that takes another function as a parameter / and or returns another function. This might sound weird at first, but trust me, you used higher-order functions before, and the concept is really powerful. For example, map, filter and reduce as we also have them in JavaScript or Python, are higher-order functions. 

Let's write a function, that takes another function as a parameter: 

```haskell 
doubleNum :: Int -> Int 
doubleNum n = n * 2

divideByTwo :: (Int -> Int) -> Int -> Int 
divideByTwo fun n = (fun n) / 2 
```
Our divideByTwo function takes a function with the signature (Int -> Int) as the first parameter. In this functions signature, you can see the () - they always mean we expect a function as a parameter. Then, divideByTwo takes a single Integer, and finally returns an integer. 
In the definition you can see that we take the providied function and call it "fun", then pass n to this function. 

Now, let's use the functions: 

```haskell 
divideByTwo doubleNum 4 
--- 4
```

As we undo the multiplication through dividing, the output is 4 again. 

### Impure functions

By definition, pure functions do not change anything outside of their scope. Logging something in the
console therefore is an impure function.
Of course we can log something from a function, instead of returning a value.
This is the case of an impure function:

```haskell
logSomething :: String -> Effect Unit
logSomething message = log ("My message: " <> message)


main = logSomething "Hello"
```

### The do-keyword in functions

The do keyword allows to make code in functions more readable and to work with different expressions. In the following example, "j" and "i" are both expressions, used in a final expression at the bottom which is finally returned.

```haskell
factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) do
  i <- 1 .. n
  j <- i .. n
  [ [ i, j ] ]
```


## Bindings 

Many ask, whether it's in the style of functional programming to declare and mutate variables within the scope of a function. If you have a couple hours left, go ahead and Google the discussion yourself. Meanwhile, in PureScript we can assign constants within the scope of a function. This can be done with bindings. 

### where 

Using the where-keyword we declare a constant on top of our function which is later initialized. Finally, this constant will be returned from the function. Here is a small example: 

```haskell 
addNums :: Int -> Int -> Int 
addNums x y = sum 
  where 
    sum = x + y
```
As this is not a practical usecase for where, you may want to declare your constant as a complex type like a list or a tuple, and then return it: 

```haskell
sumAndProduct :: Int -> Int -> Tuple Int Int 
sumAndProduct x y = Tuple sum product 
  where 
    sum = x + y 
    product = x * y 
```
This function returns a tuple, filled with both the product and the sum of the given integers. 

### let 

The let-keyword enables us to do the same thing as through using where. As you can see, the function signature stays the same. 

```haskell 
sumAndProduct :: Int -> Int -> Tuple Int Int 
sumAndProduct x y = 
  let sum = x + y
      product = x * y in 
  Tuple sum product 
```

## Custom Types 

Creating data types and re-naming types is powerful. To achieve both things, PureScript offers two keywords: 
Using "Type" we can alias existing types. Using "Data" we can define custom data types. 

### type keyword 

This keyword helps to reference existing data types.

```haskell 
type MultipleChars = String 

someString :: MultipleChars 
someString = "Hello world" 
```

It is also used to create the type definition for records and other complex structures. 

### data keyword 

The data keyword is a little bit more complex. On the left-hand side, we have the actual data type. On the right-hand side of 
the equals-sign is the so-called data constructor. Custom data types must always start with an uppercase letter. 

```haskell 
data MyDataType = MyDataType 
```

Default types like Boolean can be writen with the data keyword, even though this will collapse with the existing type. 
```haskell 
data Boolean = True | False
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

There are a couple ways to execute code conditionally in PureScript: If-then-else, case expressions, pattern matching (there is a whole section below on this topic) and guards. 
Let's start with the classic one, the if-else-then syntax in PureScript

### if-then-else

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

### Case expressions 

Using the case-of keyword, we can have something similar like a switch-case syntax: 

```haskell 
printNumber :: Int -> String 
printNumber n = case n of 
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  _ -> "another number"

printNumber 2 
--- "two"
```

The underscore catches all other cases for n, which is required. 

### Pattern matching 
Depending on your knowledge of other languages, you might view this concept as PureScript's style of function-overloading. 
Through repeating our function definition without mentioning our parameter-variable, we do pattern matching. Instead of writing the 
variable, we instead provide the exact function definition for the exact given parameter. Here is our printNumber function again: 

```haskell 
printNumber :: Int -> String 
printNumber 0 = "zero"
printNumber 1 = "one"
printNumber 2 = "two"
printNumber n = "another number"
```

Make sure to cover every other case again. 

### Guards 
Last but not least, Guards can help us to realize the same function as with the ways I showed you before. 
Regarding the syntax, there are two important things: 
1. There is no equal-sign in the beginning. 
2. After the | (OR-sign) a condition must follow. That's why we always write 1 == n, etc. in the following code example. 

```haskell
printNumber :: Int -> String 
printNumber n 
  | 0 == n = "zero"
  | 1 == n = "one"
  | 2 == n = "two"
  | otherwise = "another number"
```

Guards can be combined with case-expressions. 


## Pattern matching 

Before, we just covered a little bit of pattern matching in functions - let's go into more detail, and learn about pattern matching for different data types. 

### Array pattern matching 
One of the benefits lists have compared to arrays is their pattern matching. With arrays, we have the problem that we can only pattern matchem them against arrays with fixed length. Let's look at an example. 

```haskell 
arraySum :: Array Int -> Int 
arraySum [x, y] = x + y
arraySum [x] = x 
arraySum _ = 0 
```
As you can see, the defined size of the arrays is fixed as we provide the exact structure. We can pass [], [x], or [x, y] 
to the function, but not an array with 3 or more fields. 
Thankfully, lists offer more flexibility when it comes to cases like this

### List pattern matching 

Disclaimer: x is used for the head of a list, and xs for the tail of it. 
Therefore, the default "head" function could look like this: 

```haskell 
listHead :: forall a. List a -> Maybe a 
listHead Nil = Nothing
listHead (x : _) = Just x 
```
As x and xs represent head and tail, we can pattern-match like this no matter the size of the list. 

### Record pattern matching 

Records are like objects in JavaScript or dictionaries in Python - I covered them in more detail [here](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md#records-1). 

Let's create a record type called Bankaccount, that holds some basic information: 

```haskell 
type Bankaccount = 
  { 
    owner :: String, 
    balance :: Int
  }
```

Now, let's create a function that doubles the balance for whatever reason: 

```haskell
doubleBalance :: Bankaccount -> Int 
doubleBalance { balance } = 2 * balance 
```
As you can see, we can pattern-match with an exact type of field in the record type. 


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

There are two basic folds: `foldr` which stands for "fold from the right", and `foldl` which stands for "fold from the left".

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

## Useful default functions

This section is going to cover some useful functions, available by default, like const, apply, flip and more.

### const

While in JavaScript, const is a keyword used to specifiy variables, in PureScript "const" is something different. 
The const-function returns a constant - it does this by receiving two parameters, and only returning the first one. 
You could implement it yourself like this: 

```haskell
const :: forall a b. a -> b -> a 
const x _ = x 
```
Using it: 

```haskell
filter (const true) [1, 2, 3]
--- [1, 2, 3]
```
In this example, const is used to solve a small problem. Filter takes a function as the first parameter. This function is used to determine, which values from the second parameter (an array) should be filtered out. To not filter anything at all, we pass const as it is a function and only returns the first parameter - "true". Therefore, our filter-criteria is "true", and thus, all values in the array are returned. 

In case you need const, just the other way around, there is snoc. 

### flip 

flip helps us to flip around the order of parameters, applied to a function. 

As we just learned about const, it helps us to visualize what flip actually does: 

```haskell 
log (show (const 1 2))
--- "1"
```
Since the second parameter is ignored, and only the first one is returned, the above shown code prints "1". 

Now, when applying flip to it, "2" is printed: 

```haskell
log (show (flip const 1 2))
--- "2"
```


## Lists 

In PureScript it is important to differentiate Lists and Arrays. 
While Arrays are compiled into JavaScript Arrays when building, Lists are more like our 
typical linked-lists, and not translated to Arrays under the hood. 

```haskell
myList = 1 : 2 : 3 : Nil
myArray = [1, 2, 3]
```

```haskell 
:type myList 
--- List Int 
:type myArray 
--- Array Int 
```
When building the bundle, our Array is translated to the following JS code: 
```javascript 
var myArray = [ 1, 2, 3 ];
```

Let's go over the most important functions to work with Lists:
head, tail, cons, snoc, null, singleton, length, last & index. 


### head & tail 
Our linked list is build up like this: 
(1 (2 (3 (Nil))))
As you can see, next to the 1 is the complete rest of the list. Next to the 2 is the complete rest of the list, and so on. 
This rest is called the tail - the element "on top" is the head. 

So, the head of the tail of myList is 2. 

```haskell
head myList
```
Gives us "Just 1", and 

```haskell 
tail myList 
```
gives us "(Just (2 : 3 : Nil))". 

### cons & snoc 

Another pair of function - cons puts takes a list as the first parameter, and an element as the second parameter. 
This element is added as the head to the list. 

```haskell 
Cons 1 (2 : 3 : Nil) 
--- (1 : 2 : 3 : Nil)
```

snoc adds the provided element to the end of the list. 
```haskell 
snoc (1 : 2 : 3 : Nil) 4 
--- (1 : 2 : 3 : 4 : Nil)
```

Important: Cons is written with an uppercase "C", and the to-be-added element is the first parameter. 
For snoc, the to-be-added element is the second parameter. 

### null 

Returns true, if the provided list is empty, and false, if this is not the case. 

```haskell 
null (1 : Nil)
--- false 
```

```haskell 
null (Nil)
--- true
```

### singleton 
Returns a list with a single element in it (Nil doesn't count)

```haskell
singleton (1 : 2 : Nil) 
--- ((1 : 2 : Nil) : Nil)
```

### length 

Returns the number of elements in the list provided

```haskell 
length (1 : 2 : 3 : Nil)
--- 3
```

### last 

Returns the last element of the list

```haskell 
last (1 : 2 : 3 : Nil)  
--- (Just 3)
```

### index 
For accessing a value behind the provided index. Counting starts at 0, so index 1 is the second element: 

```haskell
index (1 : 2 : 3 : Nil) 1 
--- (Just 2)
```

### concat 

Based on a list of lists, this function concatenates the given lists: 

```haskell 
concat ((1 : 2 : Nil) : (3 : Nil) : Nil) 
--- (1 : 2 : 3 : Nil)
```

## Typeclasses 

No, typeclasses are not related to classes in object oriented programming. Rather, typeclasses can be imagined as interfaces in functional programming. Typeclasses serve to overload functions for different data types, for example. First, we define the class itself and the signature of the functions it holds. Then, we can create as many instances as we like to - these instances are usually created for different data types. Nevertheless, all instances must define the functions which are declared in the related typeclass. Let's look at an example. 

As you might know, we can represent boolean values as integers. true is 1, and false is 0. Let's create a typeclass that gives us a method (functions in typeclasses are called methods) that returns the related integer of the boolean, or returns the integer we passed. So, our method should be able to work with integers but also with booleans. 

```haskell 
class ToInt n where 
  toNum :: n -> Int 

instance intToInt :: ToInt Int where 
  toNum n = n 

instance booleanToInt :: ToInt Boolean where
  toNum true = 1 
  toNum false = 0 
```

Calling the method of our typeclass: 

```haskell 
toNum 10 
--- 10

toNum true 
--- 1
```

## Interaction with JavaScript 

This part resolves around using PureScript functions in JavaScript, and the other way around - using JavaScript functions in 
PureScript. 

### Importing foregin JS functions in PureScript 

For this task, there is the so-called foreign function interface. For making a single or multiple JS functions 
available in PureScript, we always need a JavaScript file, holding the functions, and a PureScript file, referencing those, making them available as PureScript imports. 

Let's create a JS function for doubling a given integer. To do so, create in the src-folder of your PS app a Calculations.js: 

```javascript
'use strict'
exports.double = function (n) {
  return 2 * n
}
```

Then, the Calculations.purs, to make this function available: 

```haskell 
module Calculations where

foreign import double :: Int -> Int
```

Finally, you can import and use the double function: 

```haskell 
import Calculations 

double 2 
-- 4
```