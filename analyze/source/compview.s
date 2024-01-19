# script to compile VIEWS Module under AIX

xlf -c -qsource \
       graphdo.f  graphio.f  graphsyn.f \
       schema.f   scheqn.f   screen.f  scrplots.f  table.f
