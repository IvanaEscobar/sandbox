from .utils import degMinSec2decimal, wgs84Distance

#             'name': [XC:lon,    YC:lat,   color]
CTD_coords = {'OSPM': [-70.88033, 39.93556, "#1772b1"], \
              'PMUO': [-70.78037, 39.94120, "#ff8214"], \
              'PMCO': [-70.88768, 40.10123, "#249c24"], \
              'PMCI': [-70.88893, 40.22695, "#d62728"]    }

tm4 = [-degMinSec2decimal(70,54.0876), degMinSec2decimal(39,50.8542) ]
at  = [-degMinSec2decimal(70,53.1670), degMinSec2decimal(39,57.1500) ]

# tm3, tm2, sm1 are ESTIMATED until further notice from WHOI
nesba_coords = {'tm4': [tm4[0],  tm4[1], 'm'], \
                'at' : [at[0],   at[1],  'r'], \
                'tm2': [-70.674, 39.949, 'yellow'],\
                'tm3': [-71.018, 40.037, 'yellow'],\
                'sm1': [-70.851, 40.087, 'yellow']}

# South track range: at to tm4
nesbaDist = wgs84Distance(at[0],  at[1],
                         tm4[0], tm4[1]) / 1000.

#             'name': [XC:lon,    YC:lat,   color]
gcm_coords = {'nw': [-75.531,   41.753, "k"], \
              'ne': [-69.906,   41.753, "k"], \
              'se': [-69.906,   37.750, "k"], \
              'sw': [-75.531,   37.750, "k"]    }
