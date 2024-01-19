# This compiles oslsolve.f and links with osl to create oslsolve (exe).
xlf -o oslsolve oslsolve.f -I/usr/lpp/osl/ekkincf -losl
