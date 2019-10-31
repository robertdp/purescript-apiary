module Apiary.Status where

import Data.Symbol (SProxy)
import Partial.Unsafe (unsafeCrashWith)
import Prim.TypeError (class Fail, Beside, Text)

newtype Status = Status { code :: Int, reason :: String }

statusCode :: Status -> Int
statusCode (Status { code }) = code

statusReason :: Status -> String
statusReason (Status { reason }) = reason

status :: Int -> String -> Status
status code reason = Status { code, reason }

ok :: Status
ok = status 200 "OK"

created :: Status
created = status 201 "Created"

noContent :: Status
noContent = status 204 "No Content"

notModified :: Status
notModified = status 304 "Not Modified"

badRequest :: Status
badRequest = status 400 "Bad Request"

unauthorized :: Status
unauthorized = status 401 "Unauthorized"

forbidden :: Status
forbidden = status 403 "Forbidden"

notFound :: Status
notFound = status 404 "Not Found"

conflict :: Status
conflict = status 409 "Conflict"

internalServerError :: Status
internalServerError = status 500 "Internal Server Error"

maintenanceInProgress :: Status
maintenanceInProgress = status 520 "Maintenance In Progress"

class ResponseStatus (reason :: Symbol) where
  toStatus :: SProxy reason -> Status

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
else instance responseStatusInternalServerError :: ResponseStatus "internalServerError" where
  toStatus _ = internalServerError
else instance responseStatusMaintenanceInProgress :: ResponseStatus "maintenanceInProgress" where
  toStatus _ = maintenanceInProgress
else instance responseStatusFail ::
  Fail (Beside (Text "Unknown response status: ") (Text reason)) =>
  ResponseStatus reason where
  toStatus _ = unsafeCrashWith "impossible"
