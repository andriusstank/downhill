{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language DerivingVia #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Downhill.Linear.Graph.Sharing
where
import GHC.StableName
import GHC.Exts (Any)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Trans.Class
import Control.Exception (evaluate)
import Downhill.Linear.Graph.OpenMap (OpenKey, OpenMap)
import qualified Downhill.Linear.Graph.OpenMap as OpenMap
import Downhill.Linear.Expr(Expr)
