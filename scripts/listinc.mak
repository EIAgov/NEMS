# echo list of includes.  Invoke with a command similar to this:
# make -f -s listinc.mak all "file_inc=\${RESD_INC}"
include ${NEMS}/scripts/includedep
all	:	
	echo ${file_inc}
