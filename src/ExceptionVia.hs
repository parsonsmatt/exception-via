{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Hierarchical exceptions are a powerful and useful tool in the Haskell
-- toolbox, but they're not used anywhere near often enough. I suspect it's
-- because they're a) not very commonly understood and b) a lot of
-- boilerplate to write. This library is intended to help the latter
-- problem.
--
-- Let's look at an example. We'll define a type for all of our
-- application's exceptions:
--
-- @
-- data AppException where
--   AppException :: 'Exception' e => AppException
--
-- deriving stock instance 'Show' AppException
--
-- instance 'Exception' AppException
--
-- 'mkHierarchy' ''AppException
-- @
--
-- Now, we can 'try' to catch all of the 'Exception's that we define
-- ourselves:
--
-- @
-- tryApp :: 'IO' a -> 'IO' ('Either' AppException a)
-- tryApp = 'try'
-- @
--
-- Now let's define a problem that might happen in our domain. We're going
-- to derive 'Exception' through our subtype wrapper.
--
-- @
-- data HttpException = HttpException
--   deriving stock Show
--   deriving
--     via (HttpException <!!! AppException)
--       Exception HttpException
-- @
--
-- Now, we can throw an @HttpException@, and catch it as part of
-- @AppException@:
--
-- @
-- throwHttp :: IO x
-- throwHttp = 'throwIO' HttpException
--
-- main = do
--   eresult <- tryApp throwHttp
--   case result of
--     Left (AppException err) ->
--       putStrLn "I caught it!"
--     Right _ ->
--       putStrLn "Wait what??
-- @
--
-- For each "step" in the hierarchy, you define a GADT like @AppException@
-- above. Define an instance of 'Hierarchy' for it, either via the Template
-- Haskell helper 'mkHierarchy', or manually.
module ExceptionVia
  ( -- * Deriving Via Helpers
    type (<!!!)
  , ExceptionVia(..)
  -- * Establishing Hierarchy
  , mkHierarchy
  , Hierarchy(..)
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Typeable
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- | This is the explicit word version of '(<!!!)'. You can use this if you
-- don't like @TypeOperators@.
--
-- Given a wrapper exception type like @SomeCompilerException@, you can
-- derive an instance of 'Exception' like so:
--
-- @
-- data MismatchedParentheses = MismatchedParentheses
--   deriving stock Show
--   deriving
--     via (ExceptionVia SomeCompilerException MismatchedParentheses)
--       Exception MismatchedParentheses
-- @
--
-- @since 0.1.0.0
newtype ExceptionVia big lil = ExceptionVia { unExceptionVia :: lil }
  deriving Show

-- | A concise operator alias for 'ExceptionVia'.
--
-- Given a wrapper exception type like @SomeCompilerException@, you can
-- derive an instance of 'Exception' like so:
--
-- @
-- data MismatchedParentheses = MismatchedParentheses
--   deriving stock Show
--   deriving
--     via (MismatchedParentheses <!!! SomeCompilerException)
--       Exception MismatchedParentheses
-- @
--
-- @since 0.1.0.0
type lil <!!! big = ExceptionVia big lil

-- | This class tells us how to wrap and unwrap values from our
-- hierarchical wrapper types. It is very similar to 'Exception', but
-- instead of specifying how to put some value into a 'SomeException' or
-- cast a value from a 'SomeException', we say how to put any value into
-- this @big@ type or cast any value out of the @big@ type.
--
-- Instances are very straightforward. For any type:
--
-- @
-- data ExceptionWrapper where
--   ExceptionWrapper :: Exception e => e -> ExceptionWrapper
-- @
--
-- The instance will look like this:
--
-- @
-- instance 'Hierarchy' ExceptionWrapper where
--   'toParent' = ExceptionWrapper
--   'fromParent' (ExceptionWrapper e) = 'cast' e
-- @
--
-- You can skip the boilerplate with the 'mkHierarchy' Template Haskell
-- function.
--
-- @since 0.1.0.0
class (Typeable big) => Hierarchy big where
  -- | Given any 'Exception'al value, wrap it up in the @big@ type.
  --
  -- @since 0.1.0.0
  toParent :: Exception lil => lil -> big

  -- | Given a @big@ type, 'cast' out the 'Exception' buried within. Will
  -- return 'Nothing' if the requested type is different from the actual
  -- contained value.
  --
  -- @since 0.1.0.0
  fromParent :: (Exception lil) => big -> Maybe lil

-- | Create a boilerplate 'Hierarchy' instance for a type given a name.
--
-- This code block defines an exception wrapper type and an accompanying
-- 'Hierarchy' instance.
--
-- @
-- data ExceptionWrapper where
--   ExceptionWrapper :: Exception e => e -> ExceptionWrapper
--
-- mkHierarchy ''ExceptionWrapper
-- @
--
-- @since 0.1.0.0
mkHierarchy :: Name -> DecsQ
mkHierarchy nm = do
  info <- reify nm
  con <-
    case info of
      TyConI d ->
        case d of
          DataD _ _ _ _ [con] _ ->
            pure con
          NewtypeD _ _ _ _ con _ ->
            pure con
          DataInstD _ _ _ _ [con] _ ->
            pure con
          NewtypeInstD _ _ _ _ con _ ->
            pure con
          _ ->
            fail "Unsupported type constructor. Must have a single constructor."

  let
    getConName c =
      case c of
        NormalC n _ ->
          pure n
        RecC n _ ->
          pure n
        ForallC _ _ c ->
          getConName c
        GadtC [n] _ _ ->
          pure n
        RecGadtC [n] _ _ ->
          pure n
        _ ->
          fail "Can't use with an infix constructor. Must have a single argument."

  constrName <- getConName con

  [d|
    instance Hierarchy $(conT nm) where
      toParent = $(conE constrName)
#if MIN_VERSION_template_haskell(2,18,0)
      fromParent $(pure $ ConP constrName [] [VarP (mkName "e")]) = cast e
#else
      fromParent $(pure $ ConP constrName [VarP (mkName "e")]) = cast e
#endif
    |]
