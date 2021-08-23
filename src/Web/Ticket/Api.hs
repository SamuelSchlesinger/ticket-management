{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Web.Ticket.Api where

import Data.Ticket
import Servant.API
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Media.MediaType ((//))
import Data.Aeson.TypeScript.Recursive (getTypeScriptDeclarationsRecursively)
import Data.Aeson.TypeScript.TH (TSDeclaration)

data AllTypes

instance Accept AllTypes where
  contentType _ = "*" // "*"

instance MimeRender AllTypes () where
  mimeRender _ _ = ""

type family Contains (x :: k) (xs :: [k]) :: Bool where
  Contains x '[] = 'False
  Contains x (x ': xs) = 'True
  Contains x (y ': xs) = Contains x xs

type family TypeScriptTypes xs where
  TypeScriptTypes (ReqBody '[JSON] x :> xs) = (x, TypeScriptTypes xs)
  TypeScriptTypes (xs :<|> ys) = (TypeScriptTypes xs, TypeScriptTypes ys)
  TypeScriptTypes (_ :> xs) = TypeScriptTypes xs
  TypeScriptTypes GetNoContent = ()
  TypeScriptTypes (Verb method statusCode contentTypes a) = If (Contains JSON contentTypes) a () 
  TypeScriptTypes (NoContentVerb method) = ()
  TypeScriptTypes (UVerb method contentTypes returnTypes) = If (Contains JSON contentTypes) (UVerbTypeScript returnTypes) ()
  TypeScriptTypes Raw = ()

type family UVerbTypeScript xs where
  UVerbTypeScript (WithStatus n x ': xs) = (x, UVerbTypeScript xs)
  UVerbTypeScript (x ': xs) = (x, UVerbTypeScript xs)

typeScriptTypes :: [TSDeclaration]
typeScriptTypes = getTypeScriptDeclarationsRecursively (Proxy @(TypeScriptTypes TicketApi))

type TicketApi =
       "command" :> ReqBody '[JSON] [Command] :> PostNoContent
  :<|> "query" :> ReqBody '[JSON] Query :> Post '[JSON] [TicketDetails]
  :<|> "init" :> PostNoContent
  :<|> "validate" :> GetNoContent
  :<|> "graphviz" :> Capture "relationship-type" RelationshipType :> ReqBody '[JSON] Query :> Get '[JSON] String
  :<|> UVerb 'GET '[AllTypes] '[WithStatus 302 (Headers '[Header "Location" String] ())]
  :<|> Raw
