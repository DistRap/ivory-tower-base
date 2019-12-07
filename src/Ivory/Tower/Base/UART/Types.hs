{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.Tower.Base.UART.Types where

import Ivory.Language
import Ivory.Tower

-- sample UARTBuffer
-- you might want to define your own
-- UARTBuffer with custom size
[ivory| string struct UARTBuffer 256 |]

uartTypes :: Module
uartTypes = package "uartTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

uartTowerDeps :: Tower e ()
uartTowerDeps = do
  towerDepends uartTypes
  towerModule uartTypes
