import sys
from sys import argv
import numpy as np
import ROOT as r
from matplotlib import pyplot as plt
from root_numpy import root2array, tree2array
import pandas
from collections import Counter

def cm2inch(*tupl):
    inch = 2.54
    if isinstance(tupl[0], tuple):
        return tuple(i/inch for i in tupl[0])
    else:
        return tuple(i/inch for i in tupl)


inputfile = argv[1]
print ' '
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print '  Input phase space file: ', inputfile
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print ' '
# read TFile, root format -> numpy array
f = r.TFile(inputfile,'read')
phasespace = f.Get('PhaseSpace')
particlename = tree2array(phasespace,
                          branches='ParticleName')
print 'Produced particles: ',np.unique(particlename)
num = len(np.unique(particlename))
print num
creatorprocess =tree2array(phasespace,
                          branches='CreatorProcess')
atomicnumber = tree2array(phasespace,
                          branches='AtomicNumber')
ekine=tree2array(phasespace,
                          branches='Ekine')
productionvolume=tree2array(phasespace,
                          branches='ProductionVolume')
#print particlename
#print creatorprocess
#print atomicnumber
#print ekine
#print productionvolume


particlecount=Counter(particlename)
hist1 = pandas.DataFrame.from_dict(particlecount, orient='index')
hist1.plot(kind='bar',figsize=(cm2inch(24, 18.15)), legend=False, rot=-75)
plt.savefig('PhaseSpace/ParticleName.png',dpi=300, bbox_inches="tight")
#plt.show()


creatorcount=Counter(creatorprocess)
hist2 = pandas.DataFrame.from_dict(creatorcount, orient='index')
hist2.plot(kind='bar',figsize=(cm2inch(24, 18.15)), legend=False, rot=-75)
plt.savefig('PhaseSpace/CreatorProcess.png',dpi=300, bbox_inches="tight")
#plt.show()

atomiccount=Counter(atomicnumber)
hist3 = pandas.DataFrame.from_dict(atomiccount, orient='index')
hist3.plot(kind='bar',figsize=(cm2inch(24, 18.15)), legend=False, rot=0)
plt.savefig('PhaseSpace/AtomicNumber.png',dpi=300, bbox_inches="tight")
plt.show()

hist4 = plt.figure(4, figsize=(cm2inch(24, 18.15)))
sub4 = hist4.add_subplot(111)
sub4.hist(ekine, 160, range=(0,160), align='mid', rwidth = 0.8)
hist4.savefig('PhaseSpace/Ekine.png',dpi=300, bbox_inches="tight")
#plt.show()

volumecount=Counter(productionvolume)
hist5 = pandas.DataFrame.from_dict(volumecount, orient='index')
hist5.plot(kind='bar',figsize=(cm2inch(24, 18.15)), legend=False, rot=-75)
plt.savefig('PhaseSpace/ProductionVolume.png',dpi=300, bbox_inches="tight")
#plt.show()
