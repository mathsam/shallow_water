#!/bin/tcsh -f
# Checkout Script for Experiment 't85shallow'
# ------------------------------------------------------------------------------
# The script created at 2015-08-26T20:00:09 via:
# /ncrc/home2/fms/local/opt/fre-commands/bronx-7/bin/fremake --link --ncores=8 --platform=ncrc2.default --target=openmp --walltime=120 --xmlfile=/autofs/mnt/ncrc-svm1_home2/Junyi.Chai/xml/idealized.xml t85shallow
# ------------------------------------------------------------------------------

source $MODULESHOME/init/csh
echo Using source directory = /lustre/f1/unswept/Junyi.Chai/siena_201303/t85shallow/src...
cd /lustre/f1/unswept/Junyi.Chai/siena_201303/t85shallow/src

module avail git >& .git_avail
if (! -z .git_avail) then
    module load git
endif

unalias *

# ---------------- component 'fms_spectral_shallow'
setenv CVSROOT :ext:cvs.princeton.rdhpcs.noaa.gov:/home/fms/cvs
cvs co -r siena_201303 fms_spectral_shallow
 
       

exit 0
