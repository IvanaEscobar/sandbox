from numpy import abs, cos, sqrt

def ssp ( s, t, p ):
    # the unesco equation see http://resource.npl.co.uk/acoustics/techguides/soundseawater/underlying-phys.html
    # salinity is in psu (parts per thousand) 
    # temperature is in degrees celsius
    # pressure is in bar
    # equation is valid in the ranges:
    #   s = [0 40]
    #   t = [0 40]
    #   p = [0 1000]
    #
    # Returns: 
    # sound speed in m/s

    cw =    (1402.388           + 5.03830*t         - 5.81090E-2*t**2)      +\
            (3.3432E-4*t**3     - 1.47797E-6*t**4   + 3.1419E-9*t**5)       +\
            (0.153563           + 6.8999E-4*t       - 8.1829E-6*t**2)*p     +\
            (1.3632E-7*t**3     - 6.1260E-10*t**4)*p                        +\
            (3.1260E-5          - 1.7111E-6*t       + 2.5986E-8*t**2)*p**2  +\
            (-2.5353E-10*t**3   + 1.0415E-12*t**4)*p**2                     +\
            (-9.7729E-9         + 3.8513E-10*t      - 2.3654E-12*t**2)*p**3

    a  =    (1.389              - 1.262E-2*t        + 7.166E-5*t**2)        +\
            (2.008E-6*t**3      - 3.21E-8*t**4)                             +\
            (9.4742E-5          - 1.2583E-5*t       - 6.4928E-8*t**2)*p     +\
            (1.0515E-8*t**3     - 2.0142E-10*t**4)*p                        +\
            (-3.9064E-7         + 9.1061E-9*t       - 1.6009E-10*t**2)*p**2 +\
            (7.994E-12*t**3)*p**2                                           +\
            (1.100E-10          + 6.651E-12*t       - 3.391E-13*t**2)*p**3

    b  =    (-1.922E-2          - 4.42E-5*t)                                +\
            (7.3637E-5          + 1.7950E-7*t)*p
    d  =    1.727E-3            - 7.9836E-6*p 
    return cw + a*s + b*s**(3/2) + d*s**2
