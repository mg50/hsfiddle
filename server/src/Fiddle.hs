module Fiddle where
import Data.Default
import Types

instance Default Fiddle where
  def = Fiddle "" "" ""
