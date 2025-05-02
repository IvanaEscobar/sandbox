#!/bin/py

from re import findall

'''
Class for extracting monitor information from standard output files 
e.g. STDOUT.####

Import based on the filename:
    - d = getData('STDOUT.0000')

The following objects are in the class:
    - data : dictionary including the variable name and value

The following methods are in the class:
    - getNames : list of variable names. Dictionary keys 
    - getVals  : list of floats for a given variable name. Dictionary values 
'''

class getMonData:
    def __init__( self, filename ):
        rx = r'%MON\s+(\S+)\s+=\s+(\S+)'
        with open( filename, 'r' ) as fileIn:
            self.data = {}
            for line in fileIn:
                match = findall( rx, line )
                if match: 
                    varName, varVal = match[0]
                    if varName not in self.data:
                        self.data[varName] = []
                    self.data[varName].append(float(varVal))

        # add hours and days 
        self.addHours()
        self.addDays()

    def getNames( self ):
        return list(self.data.keys())

    def getVals( self, varName ):
        return self.data.get( varName, [] )

    def addHours( self ):
        if 'hours' in self.data:
            # don't repeat addHours if it's already been called before
            return self
        else:
            assert 'time_secondsf' in self.data, "No `time_secondsf` in monitor output"
            self.data['hours'] = [t/3600 for t in self.data['time_secondsf']]
            return self

    def addDays( self ):
        if 'days' in self.data:
            # don't repeat addDays if it's already been called before
            return self
        else:
            assert 'time_secondsf' in self.data, "No `time_secondsf` in monitor output"
            self.data['days'] = [t/3600/24 for t in self.data['time_secondsf']]
            return self
