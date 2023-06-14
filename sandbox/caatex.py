import os
from .utils import degMinSec2decimal

#             'name': [XC:lon,    YC:lat,   color]

sio1 =      [-degMinSec2decimal(150), degMinSec2decimal(81) ]
sio2 =      [-degMinSec2decimal(150), degMinSec2decimal(78) ]
sio3 =      [-degMinSec2decimal(150), degMinSec2decimal(73) ]
nersc1  =   [ degMinSec2decimal(30),  degMinSec2decimal(84) ]
nersc2  =   [ degMinSec2decimal(27),  degMinSec2decimal(83) ]
nersc3  =   [ degMinSec2decimal(27),  degMinSec2decimal(82) ]

# all positions are ESTIMATED until further notice from  CAATEX
caatex_coords = {   'sio1' : [sio1[0],  sio1[1],    'm'], \
                    'sio2' : [sio2[0],  sio2[1],    'r'], \
                    'sio3' : [sio3[0],  sio3[1],    'r'], \
                    'nersc1' : [nersc1[0],  nersc1[1],    'm'], \
                    'nersc2' : [nersc2[0],  nersc2[1],    'r'], \
                    'nersc3' : [nersc3[0],  nersc3[1],    'r']  }
