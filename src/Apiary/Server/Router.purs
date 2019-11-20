module Apiary.Server.Router where

import Prelude
import Apiary.Route (Route)
import Apiary.Server.Url (PathParams)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn3, runEffectFn4)
import Node.HTTP as HTTP

newtype Router a
  = Router (RouterInstance -> Effect a)

instance functorRouter :: Functor Router where
  map f (Router a) = Router \r -> map f (a r)

instance applyRouter :: Apply Router where
  apply (Router f) (Router a) = Router \r -> apply (f r) (a r)

instance applicativeRouter :: Applicative Router where
  pure a = Router \_ -> pure a

instance bindRouter :: Bind Router where
  bind (Router ma) f =
    Router \r -> do
      a <- ma r
      case f a of
        Router mb -> mb r

instance monadRouter :: Monad Router

instance monadEffectRouter :: MonadEffect Router where
  liftEffect eff = Router \_ -> eff

foreign import data RouterInstance :: Type

foreign import _create :: EffectFn1 (EffectFn2 HTTP.Request HTTP.Response Unit) RouterInstance

create :: (HTTP.Request -> HTTP.Response -> Effect Unit) -> Effect RouterInstance
create fallback = runEffectFn1 _create (mkEffectFn2 fallback)

foreign import _lookup :: EffectFn3 RouterInstance HTTP.Request HTTP.Response Unit

lookup :: HTTP.Request -> HTTP.Response -> RouterInstance -> Effect Unit
lookup req res router = runEffectFn3 _lookup router req res

createServer ::
  forall m.
  MonadEffect m =>
  (HTTP.Request -> HTTP.Response -> Effect Unit) ->
  Router Unit ->
  m HTTP.Server
createServer fallback (Router runRouter) =
  liftEffect do
    router <- create fallback
    runRouter router
    HTTP.createServer \req res -> do
      lookup req res router

foreign import _on ::
  EffectFn4 RouterInstance String String
    (EffectFn3 HTTP.Request HTTP.Response PathParams Unit)
    Unit

type Method
  = String

type Path
  = String

on ::
  Method ->
  Path ->
  (HTTP.Request -> HTTP.Response -> PathParams -> Effect Unit) ->
  Router Unit
on method path handler = Router \router -> runEffectFn4 _on router method path (mkEffectFn3 handler)

class AttachToRouter route where
  attachToRouter :: route -> (HTTP.Request -> HTTP.Response -> PathParams -> Effect Unit) -> Router Unit

instance attachToRouterRoute ::
  ( IsSymbol method
  , IsSymbol path
  ) =>
  AttachToRouter (Route method path spec) where
  attachToRouter _ = on method path
    where
    method = reflectSymbol (SProxy :: _ method)

    path = reflectSymbol (SProxy :: _ path)
