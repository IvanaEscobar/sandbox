from .utils import degMinSec2decimal, wgs84Distance

tm4 = [-degMinSec2decimal(70,54.0876), degMinSec2decimal(39,50.8542) ]
at  = [-degMinSec2decimal(70,53.1670), degMinSec2decimal(39,57.1500) ]

nesbaDist = wgs84Distance(at[0],  at[1],
                         tm4[0], tm4[1]) / 1000.
