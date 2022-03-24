# Haskell Cheatsheet

Disclaimer: As I covered many of the basics of PureScript, and as PureScript and Haskell are closely related, this Cheatsheet will try to cover Haskell-unique stuff primarly. If you want to know how recursion and high-order functions in Haskell work, have a look at the [PureScript Cheatsheet](https://github.com/LouisPetrik/cheatsheet/blob/master/purescript.md) - it works the same ;) 

## Overview

- ### [GHCI & Hello World!]()
- ### [Data types](https://github.com/LouisPetrik/cheatsheet/blob/master/haskell.md#data-types-1)
- ### [Custom Data]
- ### [Functions]
- ### [Lists]
- ### [Typeclasses]
- ### [In and output]
- ### [Monads]
- ### [Tips & Tricks]


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

### Enums 

Enum stands for enumeration, and might be known from many other programming languages. 

Creating a custom enumeration: 

```haskell
data Numbers = One | Two | Three | Four | Five | Six
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

Ignore the deriving-stuff for a moment. It just enables us to use a lot of functions on this type of data. When opening up the console, you can now see, that we have an ordered enumeration: 

```haskell 
One > Two 
False 

Two > One 
True

minBound :: Numbers 
One 

succ One 
Two 
```
More on the deriving-syntax [here](https://github.com/LouisPetrik/cheatsheet/blob/master/haskell.md#deriving-instances)

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

As we covered data constructors before, we can now move on to types parameters. 

```haskell 
data Maybe a = Nothing | Just a 
```

In this example, a is a type parameter. Maybe on the other hand, is the type constructor. 
Why Maybe is a type constructor, might be more clearly when interacting with the parameter. We can pass almost anything into a, resulting in a "Maybe String", "Maybe Char", "Maybe Person" etc. The values themselves can never just have a type of only Maybe, since Maybe is a constructor, not an actual type. 

To make it even more clear, when we pass String to Maybe, the resulting type will be Maybe String. On the other hand, Just 'Hello World' has a type of Maybe [Char]. 

Important to understand is also, that the type of "Nothing" is "Maybe a". Therefore, we can pass Nothing to each function requiring a "Maybe x" (String, Integer etc.). 

### Deriving instances 

In Haskell, we can force our data type to be derived of a certain typeclass. We will see in a second what this means. 
Here is an example. 

```haskell
data Person = Person {
  firstname: String, 
  lastname: String
} deriving (Eq)
```

Now, let's create two people of the data type "Person": 

```haskell 
maxi :: Person
maxi = Person {firstname = "Max", lastname = "Meyer"}

carl :: Person
carl = Person {firstname = "Carl", lastname = "Johnson"}
```

Thanks to deriving the Eq-class which is used for equality-checking and related stuff, we can check whether two people of type "Person" are equal: 

```haskell 
maxi == carl 
False 
```

When trying to show a Person in the console, you will initially fail - yet, it is another problem to be solved with deriving a typeclass. All we need to do is deriving the show-typeclass, too: 

```haskell
data Person = Person {
  firstname: String, 
  lastname: String
} deriving (Eq)
```

## Functions 


### Working with typeclasses as function parameters. 

Usually, we strictly define the types of values our function should take and return in the function declaration: 

```haskell 
add :: Int -> Int -> Int 
add x y = x + y 
```

Yet, sometimes we want to use a broader spectrum of types that might be applied to our function. Coming back to the add-function, we must keep in mind that not just integer-values can be added. To make our function more general, we can instead of mentioning single types, use the whole class: 

```haskell 
add2 :: Num x => x -> x -> x
add2 x y = x + y
```

Now we can even execute "add 2 2.5" succesfully. 



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


### Creating instances manually 

We've seen before how to derive from some classes for our custom data, so we end up being able to use their functionality. Therefore, while data can automatically become instance of a class, we can also do it manually: 

```haskell
data Options = Yes | No | Unsure

instance Eq Options where
  Yes == Yes = True
  No == No = True
  Unsure == Unsure = True
  _ == _ = False
```

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


## Tips & Tricks 

### Unable to show certain stuff in the console? 

```haskell 
instance Show (a -> b) where
  show a = "function"
```