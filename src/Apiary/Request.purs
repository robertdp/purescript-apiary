module Apiary.Request where

import Apiary.Types (Request)

class BuildRequest route params body rep | route -> params body rep where
  buildRequest :: route -> params -> body -> Request
