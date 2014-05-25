module Kan.Api.Actions.TH where
import Data.Aeson.TH
import Data.Char

deriveApiData = deriveFromJSON $ defaultOptions{ fieldLabelModifier = modifier } where
  modifier = ("api_" ++) . concatMap lower . strip
  lower c
    | isUpper c = ['_',  toLower c]
    | otherwise = [c]
  strip s =
    case break (== '_') s of
      (_, _:label) -> label
      _ -> s

