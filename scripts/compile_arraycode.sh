# $Header: M:/default/scripts/RCS/compile_arraycode.sh,v 1.2 2017/04/17 14:35:39 dsa Exp $
# compiles/links arraycode.f to create arraycode.exe, program to generate aimms interface for efd and ecp
ifort  /free /include:'m:\default\includes' arraycode.f /link /force
