# Process

"""
Tune the optimization parameters for XPRESS

based on https://www.fico.com/fico-xpress-optimization/docs/latest/evalguide2/dhtml/eg2sec1.html
"""
import xpress as xp
import glob as glob

# get a list of all .mps files available, and write to problems.set
mps_files = glob.glob('*.mps')
with open('problems.set', 'w') as the_file:
    for mps_file in mps_files:
        the_file.write(mps_file + '\n')

# tune based on problems.set
p = xp.problem()
p.tuneprobsetfile("problems.set")
p.chgobjsense(sense=xp.minimize)
p.controls.timelimit=60
p.tune(flags='l')        # specify flag 'l' (optional)
