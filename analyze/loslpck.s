# This links with osl, gupck*, and getmat routines to create oslpck (exe)
xlf -o oslpck -I/usr/lpp/osl/ekkincf -losl oslpck.o  \
       gupckgen.o gupcklib.o \
       gbagnda1.o gbagnda2.o gbrange.o getedit.o getiopck.o \
       getlib.o   getset.o   getrans.o
