# PureScript

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

## Records, aka. Objects 
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

main = log (show (getName max))
```



