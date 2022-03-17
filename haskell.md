# Haskell Cheatsheet

Disclaimer: As I covered many of the basics of PureScript, and as PureScript and Haskell are closely related, this Cheatsheet will try to cover Haskell-unique stuff primarly. If you want to know how recursion and high-order functions in Haskell work, have a look at the [PureScript Cheatsheet](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md) - it works the same ;) 

## Overview

- ### [GHCI & Hello World!]()
- ### [Datatypes]
- ### [Custom Data]
- ### [Functions]
- ### [Lists]
- ### [Typeclasses]
- ### [In and output]
- ### [Monads]


## GHCI

The Glasgow Haskell compiler 

Starting it: 

```bash 
ghci 
```

Executing a hello-world file (main.hs): 

```haskell 
main :: IO ()
main = putStrLn "Hello World"
```

When you saved your file, we can open it in GHCI like this: 
```bash
ghci main.hs
```

Alternatively, you can start GHCI first, and then import your module main.hs:

```bash
ghci 
:load main
```

No, your file won't execute right now - opening like this kind of loads your file into GHCI, so that you can now play around with it. To execute it, type "main" and press enter, and you should see the output. 

If you change something in the file, and want to run it again with GHCI opened, run: 

```bash
:reload 
main
```

Compiling your Haskell file to an executeable: 

```bash
ghc main.hs
```

Getting the function signature of something: 

```haskell
:type head 
---head :: [a] -> a
```

To quit GHCI:
```bash
:quit
```


## Data types 

### Int & Integer 

While "Int" is the classic integer type bound to a size, so limited in the number of numbers it can represent, "Integer" is not bound - yet, "Int" is more efficient. 

### Char & String

A char is a single character. A string is technically an array of chars, which is why strings are often noted as [Char] in Haskell. Nevertheless, we can also use "String". 

Types always start with an uppercase letter. 

### Type variables 

Often you will see function signatures like this one: 

```haskell 
head :: [a] -> a
```
Notice the a? While the [a] surely stands for a list, a itself can be anything when it comes to types. Therefore the head function can take a list of chars, strings, integers and return a single element of the same type. a is a type variable. 


```haskell
data Person = Person String Int

john = Person "John" 20
```

You might wonder why there is a second "Person" on the right side of the equals sign - yet, john would looks like this in JavaScript: 

```javascript
const john = new Person("John", 22);
```

"Person" in the right side of the equals sign is called the data-constructor. 

For better understanding: 

```haskell 
data Name = Lastname String | Firstname String
```

In fact, Lastname and Firstname are both available now as types - we can assign them to functions, constants etc. Once we did so, the values are also of the type "Name". 
Now, you might understand better what we mean with data constructors - Lastname and Firstname in this case. That's why in the above example of Person, Person is also used the data constructor - there is no subtype belonging to it. 

Using custom data in functions: 

```haskell
data Name = Lastname String | Firstname String

greetWithFirstname :: Name -> String
greetWithFirstname (Firstname name) = name
```
And calling the function: 

```haskell
greetWithFirstname (Firstname "Max")
"Max"
```

In the type declaration, you can see that we use Name as the first parameter - not Firstname, which the function actually wants to use. The reason is, that Firstname is a constructor - not a type. Name is the type, and in the function declaration, types are needed. 


### Using type parameters 



## Functions 


## Lists 

### Accessing element in list through index: 
```haskell 
[1, 2, 3] !! 2 
--- 3
```

### Head, tail, last 

```haskell 
head [1, 2, 3]
--- 1

tail [1, 2, 3]
--- 2, 3

last [1, 2, 3]
--- 3
```

### null, length 

```haskell 
null [1, 2, 3]
--- False 

null []
--- True 

length [1, 2, 3]
--- 3
```

### Range 

```haskell
[1, 2..10]
---[1,2,3,4,5,6,7,8,9,10]

[0.1, 0.2..1]
-- [0.1,0.2,0.30000000000000004,0.4,0.5,0.6,0.7000000000000001,0.8,0.9,1.0]
```

This works even with chars: 

```haskell 
['A' .. 'Z']
--- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

### elem

This function receives a value and a list and returns if the value is an element 
of the list. 

```haskell 
1 `elem`[1, 2, 3]
--- True
```

### cycle 

With cycle, we can create a list that repeats its content for ever - we receive an infitite list. For example, [1, 2] would be turned into [1, 2, 1, 2, 1, 2 ... and so on. 

To cut the list which turns the infinite list cycle creates into an ending one: 

```haskell 
take 10 (cycle[1, 2]) 
---[1,2,1,2,1,2,1,2,1,2]
```

The parameter "take" receives is the length of the list that should be generated. 

### repeat 

It does what it suggests.

While generate an infitite list consisting only of 5's: 

```haskell 
repeat 5
```

To make it a fixed size: (10 times 5): 

```haskell 
take 10 (repeat 5)
```

## Typeclasses 

```haskell 
:t (+) 
---(+) :: Num a => a -> a -> a
```

When asking GHCI for the type of an operator like the plus-operator, you will notice that it is basically a function. Everything before the => sign is called the class constraint.  

### Creating a typeclass on our own: 


## In and output 

When initialising records for example, you might have noticed that you are unable to print
them easily in GHCI. There is a simple solution to that. Derive Show so Haskell will know how to actually print your values: 

```haskell
data Dog = Dog
  { name :: String,
    race :: String,
    age :: Int
  }
  deriving (Show)

woody = Dog {name = "Woody", race = "Labrador", age = 12}
```

Then, in GHCI just type "woody" and the record will be printed. 


## Tuples 


Unlike lists, tuples can hold different types of data. 

We speak of pairs if a tuple has exactly two elements (no-brainer.)
Function we can use on tuple pairs are fst "first" and snd "second": 

```haskell 
fst (1, 2)
--- 1

snd (1, 2)
--- 2
```


## Modules 

Modules are a collection of functions, types and typeclasses, bundled. 

