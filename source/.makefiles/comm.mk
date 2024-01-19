# FILE IS CREATED AUTOMATICALLY BY MAKEFILE
comm.obj :  ../includes/cogen ../includes/AMPBLK ../includes/commrep ../includes/comvars ../includes/e111d ../includes/bldglrn ../includes/uecpout ../includes/uefpout ../includes/comparm ../includes/comout ../includes/emission ../includes/apq ../includes/ncntrl ../includes/macout ../includes/mxpblk ../includes/parametr ../includes/qsblk ../includes/QBLK ../includes/emmparm ../includes/eusprc ../includes/steoblock comm.f
	@echo "  > comm.obj"
	@ifort /check:bounds /debug:full /compile_only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0 comm.f /include:../includes -o comm.obj

