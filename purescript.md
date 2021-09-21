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

#### code - app.go

## Using the console 
To log something in the console, make sure to import the proper package. 
Then, in the main function, we can log like this: 

```haskell
import Effect (Effect)

main = log "Hello world"
```

## Variables 

