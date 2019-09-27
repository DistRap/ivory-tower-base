
module Ivory.Base.Math where

import Ivory.Language

linearlyInterpolate :: (IDouble, IDouble)
                    -> (IDouble, IDouble)
                    -> IDouble
                    -> IDouble
linearlyInterpolate (x0, y0) (x1, y1) x = y0 + (x - x0) * (y1 - y0) / (x1 - x0)
