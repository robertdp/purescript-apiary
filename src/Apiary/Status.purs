module Apiary.Status where

import Data.Symbol (SProxy)

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

instance responseStatusCreated :: ResponseStatus "created" where
  toStatus _ = created

instance responseStatusNoContent :: ResponseStatus "noContent" where
  toStatus _ = noContent

instance responseStatusNotModified :: ResponseStatus "notModified" where
  toStatus _ = notModified

instance responseStatusBadRequest :: ResponseStatus "badRequest" where
  toStatus _ = badRequest

instance responseStatusUnauthorized :: ResponseStatus "unauthorized" where
  toStatus _ = unauthorized

instance responseStatusForbidden :: ResponseStatus "forbidden" where
  toStatus _ = forbidden

instance responseStatusNotFound :: ResponseStatus "notFound" where
  toStatus _ = notFound

instance responseStatusConflict :: ResponseStatus "conflict" where
  toStatus _ = conflict

instance responseStatusInternalServerError :: ResponseStatus "internalServerError" where
  toStatus _ = internalServerError

instance responseStatusMaintenanceInProgress :: ResponseStatus "maintenanceInProgress" where
  toStatus _ = maintenanceInProgress
