module PlatformRunner.Prelude
  ( module Utility.Math
  , module RIO
  , module Control.Lens
  ) where

import           Control.Lens                   ( (%~)
                                                , (.~)
                                                , ASetter
                                                , ASetter'
                                                , Getting
                                                , Lens
                                                , Lens'
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , lens
                                                , over
                                                , preview
                                                , set
                                                , sets
                                                , to
                                                , view
                                                )
import           RIO                     hiding ( (%~)
                                                , (.~)
                                                , ASetter
                                                , ASetter'
                                                , Down
                                                , Getting
                                                , Lens
                                                , Lens'
                                                , (^.)
                                                , (^..)
                                                , (^?)
                                                , lens
                                                , over
                                                , preview
                                                , set
                                                , sets
                                                , to
                                                , view
                                                )
import           Utility.Math
