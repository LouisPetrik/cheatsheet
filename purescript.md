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

```

## Variables 

