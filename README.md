# exception-via

Haskell supports hierarchical exceptions, but there's a bit of boilerplate involved.
The [documentation for `Control.Exception`](https://www.stackage.org/haddock/lts-16.20/base-4.13.0.0/Control-Exception.html#t:Exception) has a write-up:

```haskell
---------------------------------------------------------------------
-- Make the root exception type for all the exceptions in a compiler

data SomeCompilerException = forall e . Exception e => SomeCompilerException e

instance Show SomeCompilerException where
    show (SomeCompilerException e) = show e

instance Exception SomeCompilerException

compilerExceptionToException :: Exception e => e -> SomeException
compilerExceptionToException = toException . SomeCompilerException

compilerExceptionFromException :: Exception e => SomeException -> Maybe e
compilerExceptionFromException x = do
    SomeCompilerException a <- fromException x
    cast a

---------------------------------------------------------------------
-- Make a subhierarchy for exceptions in the frontend of the compiler

data SomeFrontendException = forall e . Exception e => SomeFrontendException e

instance Show SomeFrontendException where
    show (SomeFrontendException e) = show e

instance Exception SomeFrontendException where
    toException = compilerExceptionToException
    fromException = compilerExceptionFromException

frontendExceptionToException :: Exception e => e -> SomeException
frontendExceptionToException = toException . SomeFrontendException

frontendExceptionFromException :: Exception e => SomeException -> Maybe e
frontendExceptionFromException x = do
    SomeFrontendException a <- fromException x
    cast a

---------------------------------------------------------------------
-- Make an exception type for a particular frontend compiler exception

data MismatchedParentheses = MismatchedParentheses
    deriving Show

instance Exception MismatchedParentheses where
    toException   = frontendExceptionToException
    fromException = frontendExceptionFromException
```

Woof! That's a lot of code just to have nested exceptions.
Especially since Java devs can just write

```java
public class CompilerException extends Exception { ... }
public class FrontendException extends CompilerException {....}
```

This library attempts to help by providing a `newtype` wrapper you can use with `DerivingVia`.
With basic exceptions, you don't need this - the default methods on `Exception` default to a top-level exception.

```haskell
data  EasyException = EasyException
  deriving stock Show
  deriving anyclass Exception
```

Let's make those nested exceptions.

```haskell
data SomeCompilerException = forall e . Exception e => SomeCompilerException e

deriving stock instance Show SomeCompilerException
deriving anyclass instance Exception SomeCompilerException

instance Hierarchy SomeCompilerException where
  toParent = SomeCompilerException
  fromParent (SomeCompilerException e) = cast e
```

The `Hierarchy` class is required to tell us how to unpack and pack things in the exception type.
Now let's get to the frontend exception.
It's a subtype of `SomeCompilerException`, so we'll derive the `Exception` instance using our `via`-type.

```haskell
data SomeFrontendException = forall e . Exception e => SomeFrontendException e

deriving stock instance Show SomeFrontendException
deriving 
  via (SomeFrontendException <!!! SomeCompilerException) 
  instance Exception SomeFrontendException
```

That's it.
We need to define the `Hierarchy` instance, which is extremely boilerplate:

```haskell
instance Hierarchy SomeFrontendException where
  toParent = SomeFrontendException
  fromParent (SomeFrontendException e) = cast e
```

A `TemplateHaskell` helper would be nice...

```haskell
mkHierarchy ''SomeFrontendException
```

Much better.

And now we have our actual exception types:

```haskell
data MismatchedParentheses = MismatchedParentheses
  deriving stock Show
  deriving 
    via (MismatchedParentheses <!!! SomeFrontendException) 
    Exception MismatchedParentheses
```

Easy peasy.
