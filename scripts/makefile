# don't edit with editor that converts tabs to spaces
# Note:
# FLAGB is set to either " /nocheck" or " /check:bounds" in nfort
# FLAGO is set to either " /debug:full /optimize:0" or " /nodebug /optimize:2" in nfort
# FLAGIO is set to either "" or "/iface:cvf /fpscomp:ldio_spacing" , the latter for CVF compatibility when Intel is being used
FLAGS1=/compile-only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0
FLAGI=/include:.
  
.SUFFIXES :	.f .obj .f90

.f.obj:
	echo 
	echo "The compiler being used is ${COMPILER}"
	echo 
	echo "Compiler command line options are:"
	echo ${FLAGS1} ${FLAGIO}
	echo ${FLAGB} ${FLAGO} 
	echo 
	echo 'Include Directories:  (To use a different directory, specify as 2nd nfort argument)'
	echo ${FLAGI}
	echo ${NEMSINCL}
	echo 
	"${COMPILER}" ${FLAGB} ${FLAGO} ${FLAGS1} ${FLAGIO} ${FLAGI} /include:"${NEMSINCL}" $<   2>&1  | tee ERROR

.f90.obj:
	echo "Compiling an .f90 file"
	echo 
	echo "The compiler being used is ${COMPILER}"
	echo 
	echo "Compiler command line options are:"
	echo ${FLAGS1} ${FLAGIO}
	echo ${FLAGB} ${FLAGO} 
	echo 
	echo 'Include Directories:  (To use a different directory, specify as 2nd nfort argument)'
	echo ${FLAGI}
	echo ${NEMSINCL}
	echo 
	"${COMPILER}" ${FLAGB} ${FLAGO} ${FLAGS1} ${FLAGIO} ${FLAGI} /include:"${NEMSINCL}" $<   2>&1  | tee ERROR

.SILENT: 
