module Apiary.Status where

import Prelude
import Data.Symbol (SProxy(..))
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Beside, Text)

newtype Status
  = Status { code :: Int, reason :: String }

instance eqStatus :: Eq Status where
  eq a b = statusCode a == statusCode b

instance showStatus :: Show Status where
  show (Status { code, reason }) = "(status " <> show code <> " " <> show reason <> ")"

statusCode :: Status -> Int
statusCode (Status { code }) = code

statusReason :: Status -> String
statusReason (Status { reason }) = reason

status :: Int -> String -> Status
status code reason = Status { code, reason }

ok :: Status
ok = status 200 "OK"

_ok = SProxy :: SProxy "ok"

created :: Status
created = status 201 "Created"

_created = SProxy :: SProxy "created"

noContent :: Status
noContent = status 204 "No Content"

_noContent = SProxy :: SProxy "noContent"

notModified :: Status
notModified = status 304 "Not Modified"

_notModified = SProxy :: SProxy "notModified"

badRequest :: Status
badRequest = status 400 "Bad Request"

_badRequest = SProxy :: SProxy "badRequest"

unauthorized :: Status
unauthorized = status 401 "Unauthorized"

_unauthorized = SProxy :: SProxy "unauthorized"

forbidden :: Status
forbidden = status 403 "Forbidden"

_forbidden = SProxy :: SProxy "forbidden"

notFound :: Status
notFound = status 404 "Not Found"

_notFound = SProxy :: SProxy "notFound"

conflict :: Status
conflict = status 409 "Conflict"

_conflict = SProxy :: SProxy "conflict"

maintenanceInProgress :: Status
maintenanceInProgress = status 520 "Maintenance In Progress"

_maintenanceInProgress = SProxy :: SProxy "maintenanceInProgress"

class ResponseStatus (status :: Symbol) where
  toStatus :: forall proxy. proxy status -> Status

instance responseStatusOK :: ResponseStatus "ok" where
  toStatus _ = ok
else instance responseStatusCreated :: ResponseStatus "created" where
  toStatus _ = created
else instance responseStatusNoContent :: ResponseStatus "noContent" where
  toStatus _ = noContent
else instance responseStatusNotModified :: ResponseStatus "notModified" where
  toStatus _ = notModified
else instance responseStatusBadRequest :: ResponseStatus "badRequest" where
  toStatus _ = badRequest
else instance responseStatusUnauthorized :: ResponseStatus "unauthorized" where
  toStatus _ = unauthorized
else instance responseStatusForbidden :: ResponseStatus "forbidden" where
  toStatus _ = forbidden
else instance responseStatusNotFound :: ResponseStatus "notFound" where
  toStatus _ = notFound
else instance responseStatusConflict :: ResponseStatus "conflict" where
  toStatus _ = conflict
else instance responseStatusFail ::
  Fail (Beside (Text "Unsupported response status: ") (Text status)) =>
  ResponseStatus status where
  toStatus _ = unsafeCrashWith "impossible"
