from numpy import *
from pylab import *

def read_arrivals_asc(filename=None,Narrmx=None):    
    #*******************************************************************************
    # Faro, Dom Dez 18 21:03:46 WET 2016
    # Written by Tordar
    # Based on read_arrivals_asc.m by Michael Porter
    #*******************************************************************************
    fid = open(filename,'r')
    theline = str( fid.readline() )
    datai = theline.split()
    freq = float( datai[0] )
    Nsd  = int(   datai[1] )
    Nrd  = int(   datai[2] )
    Nrr  = int(   datai[3] )
    
    s_depth = zeros(Nsd)
    r_depth = zeros(Nrd)
    r_range = zeros(Nrr)
    
    theline = str( fid.readline() )
    datai = theline.split()
    for i in range(Nsd):
        s_depth[i] = float( datai[i] )
	
    theline = str( fid.readline() )
    datai = theline.split()
    for i in range(Nrd):
        r_depth[i] = float( datai[i] )
	
    theline = str( fid.readline() )
    datai = theline.split()	
    for i in range(Nrr):
        r_range[i] = float( datai[i] )
	
    Narr      = zeros( (Nrr,         Nrd, Nsd) )	
    A         = zeros( (Nrr, Narrmx, Nrd, Nsd) ) + 1j*zeros( (Nrr, Narrmx, Nrd, Nsd) )
    delay     = zeros( (Nrr, Narrmx, Nrd, Nsd) ) + 1j*zeros( (Nrr, Narrmx, Nrd, Nsd) )
    SrcAngle  = zeros( (Nrr, Narrmx, Nrd, Nsd) )
    RcvrAngle = zeros( (Nrr, Narrmx, Nrd, Nsd) )
    NumTopBnc = zeros( (Nrr, Narrmx, Nrd, Nsd) )
    NumBotBnc = zeros( (Nrr, Narrmx, Nrd, Nsd) )

    for isd in range(Nsd):
        Narrmx2 = int( fid.readline() )
        for ird in range(Nrd):
            for ir in range(Nrr):
	        narr = int( fid.readline() )
                Narr[ ir, ird, isd ] = narr
		if narr > 0:
		   narr = min( narr, Narrmx )
		   for k in range(narr):
                       theline = str( fid.readline() )
                       datai = theline.split()
		       amp   = float( datai[0] )
		       phase = float( datai[1] )
                       A[ ir, k, ird, isd ] = amp*exp( 1j*phase*pi/180.0 )
		       rtau = float( datai[2] )
		       itau = float( datai[3] )
 		       delay[ ir, k, ird, isd ] = rtau + 1j*itau
		       source_angle = float( datai[4] ) 
                       SrcAngle[ ir, k, ird, isd ] = source_angle
		       receiver_angle = float( datai[5] ) 
		       RcvrAngle[ ir, k, ird, isd ] = receiver_angle
		       bounces = int( datai[6] )
                       NumTopBnc[ ir, k, ird, isd ] = bounces
		       bounces = int( datai[7] )		       
                       NumBotBnc[ ir, k, ird, isd ] = bounces

    fid.close()
    Arr = {'Narr':Narr,'A':A,'delay':delay,'SrcAngle':SrcAngle,'RcvrAngle':RcvrAngle,'NumTopBnc':NumTopBnc,'NumBotBnc':NumBotBnc}
    Pos = {'s_depth':s_depth,'r_depth':r_depth,'r_range':r_range}
    return Arr,Pos
