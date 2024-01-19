# This links with osl, gupck*, and getmat routines to create oslanal (exe)
xlf -o oslanal -I/usr/lpp/osl/ekkincf -losl \
       oslanal.o gupckgen.o gupcklib.o gbagnda1.o gbagnda2.o gbrange.o \
       getedit.o getiopck.o getlib.o   getset.o   getrans.o
