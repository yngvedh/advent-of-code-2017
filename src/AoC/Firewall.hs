module AoC.Firewall (parseFirewall, scoreFirewall, undetectableDelay) where

import qualified AoC.Firewall.Core as C
import qualified AoC.Firewall.Parse as P

parseFirewall = P.parseFirewall
scoreFirewall = C.scoreFirewall
undetectableDelay = C.undetectableDelay