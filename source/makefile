# ----------------------------------------- COMPILER SETTINGS -----------------------------------------
SHELL=C:/cygwin64/bin/sh
COMPILER=ifort
FLAGB=/check:bounds
FLAGO=/debug:full
FLAGS1=/compile-only /nopdbfile /free /traceback /fpconstant /assume:byterecl /assume:source_include /nolist /static /Qsave /Qzero /heap-arrays0
FFLAGS= $(FLAGB) $(FLAGO) $(FLAGS1)

# ----------------------------------------- LINKER SETTINGS -----------------------------------------
LINKER = xilink

# ----------------------------------------- SOURCES AND HEADERS -----------------------------------------
INCLUDES=../includes
SOURCES = $(shell find *.f *.f90 )
objects_temp = $(SOURCES:.f90=.obj)
OBJECTS = $(patsubst %.f90,%.obj,$(patsubst %.f,%.obj,$(SOURCES)))
D_FILES = $(OBJECTS:^"$(BUILD_DIR)/":s/.obj/.d/)

TFILER_OBJS = tfiler.obj filer.obj fwk1io.obj cio4wk1.obj gdxf9def.obj gdxf9glu.obj

# ----------------------------------------- DEPENDENCY TRACKING -----------------------------------------
BUILD_DIR = build
# This loads .d and files for proper tracking of header dependecies
# It also loads .mk files in the .makefiles directory for user defined makefiles 
LOAD_INCLUDES := $(shell ../scripts/include_deps.py)
MAKEDEPEND = /gen-dep:$(BUILD_DIR)/$*.d /gen-depformat:make /QMMD
DEPENDENCY_INCLUDES = $(BUILD_DIR)/include_d.mk

# Fortran files that produce modules that other Fortran files use must be compiled first
# manually add the .obj file here that will produce a .mod file when compiled
MOD_OBJS = ecp_row_col.obj efd_row_col.obj fsqlite.obj


# ----------------------------------------- TARGETS -----------------------------------------
all : modules objects

objects : $(OBJECTS)
modules : $(MOD_OBJS)

include $(DEPENDENCY_INCLUDES)


tfiler.exe : $(TFILER_OBJS)
	echo $(SOURCE_DIR)
	$(LINKER) /subsystem:console /force /out:tfiler.exe /NODEFAULTLIB:libc $(TFILER_OBJS)
	
arraycode.exe : ../scripts/arraycode.exe ;

../scripts/arraycode.exe : arraycode.f
	echo $^
	$(COMPILER) -o ../scripts/arraycode.exe /free /include:$(INCLUDES) $^  /link /force

%.obj : $(BUILD_DIR)/%.d ;

$(BUILD_DIR)/%.d : %.f90 
	@echo "  > $*.obj"
	@rm -f $(BUILD_DIR)/$*.d
	@$(COMPILER) $(FFLAGS) $(MAKEDEPEND) $*.f90 /include:$(INCLUDES) -o $*.obj
	@$(SHELL) -ec "../scripts/gen_dep_to_makefile.py $(BUILD_DIR)/$*.d $(COMPILER) $(FFLAGS) $*.f90 /include:$(INCLUDES) -o $*.obj"

$(BUILD_DIR)/%.d : %.f
	@echo "  > $*.obj"
	@rm -f $(BUILD_DIR)/$*.d
	@$(COMPILER) $(FFLAGS) $(MAKEDEPEND) $*.f /include:$(INCLUDES) -o $*.obj
	@$(SHELL) -ec "../scripts/gen_dep_to_makefile.py $(BUILD_DIR)/$*.d $(COMPILER) $(FFLAGS) $*.f /include:$(INCLUDES) -o $*.obj"
	

clean: 
	rm -rf $(BUILD_DIR) $(OBJECTS) *.mod

# DO NOT DELETE
