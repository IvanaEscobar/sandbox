#! python
## Example command
# python search_readparms.py vars.txt "$mitgcm/MITgcm_current/pkg/ecco/*.h"
## Another example command
# ./search_readparms.py vars.txt "$mitgcm/MITgcm_current/pkg/ecco/*.h"
import re
import sys
from glob import glob
import os

ff = open(sys.argv[1], 'r')
varList = ff.read()
varList = varList.replace('\n', '').replace(' ', '').split(',')
ff.close()

files = glob(os.path.join('.', sys.argv[2]))

for var in varList:
    for file in files:
        with open(file, 'r') as f:
            for line in f:
                if re.search(var, line):
                    print( var + ': ' + file )
