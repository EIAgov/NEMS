use dflib
character*125 lib
call getenv("LIB",lib)
write(6,'(a,a)') 'lib=',trim(lib)
end 
