# FILE IS CREATED AUTOMATICALLY BY MAKEFILE
r_.mod :  resd.f
	@echo "  > r_.mod"
	@ifort /check:bounds /debug:full /compile_only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0 resd.f /include:../includes -o resd.obj

resd.obj :  ../includes/macout ../includes/emission ../includes/ncntrl ../includes/apq ../includes/eusprc ../includes/QBLK ../includes/e111d ../includes/emmparm ../includes/uefpout resd.f ../includes/cogen ../includes/steoblock ../includes/parametr ../includes/resdrep ../includes/bldglrn ../includes/rtek ../includes/rscon ../includes/rseff ../includes/qsblk ../includes/uecpout ../includes/emablk ../includes/AMPBLK
	@echo "  > resd.obj"
	@ifort /check:bounds /debug:full /compile_only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0 resd.f /include:../includes -o resd.obj

