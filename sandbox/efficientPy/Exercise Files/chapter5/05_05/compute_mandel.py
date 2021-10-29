import numpy as np

def compute_mandel_numpy(c,maxit=256):
    escaped = np.full_like(c,np.inf,'d')
    z = np.zeros_like(c,'c16')
    
    for it in range(1,maxit):
        z = np.where(escaped == np.inf,z**2 + c,0)        
        escaped[np.abs(z) > 2.0] = it

    return escaped
