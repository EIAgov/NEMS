# searches the default nems source code
# directory for which fortran files use a
# particular include file
# argument 1: include file name
# argument 2-5: optional additional include names 
gc="grep"
case $# in
 1) args="^[ ]*include[ ]*.$1";;
 2) args="^[ ]*include[ ]*.$1|^[ ]*include[ ]*.$2"
    gc="egrep"
    ;;
 3) args="^[ ]*include[ ]*.$1|^[ ]*include[ ]*.$2|^[ ]*include[ ]*.$3"
    gc="egrep"
    ;;
 4) args="^[ ]*include[ ]*.$1|^[ ]*include[ ]*.$2|^[ ]*include[ ]*.$3|^[ ]*include[ ]*.$4"
    gc="egrep"
    ;;
 *) args="^[ ]*include[ ]*.$1|^[ ]*include[ ]*.$2|^[ ]*include[ ]*.$3|^[ ]*include[ ]*.$4|^[ ]*include[ ]*.$5"
    gc="egrep"
    ;;
esac
searchstr="^[ ]*include[ ]*.\("$args"\)"
echo $gc " -il " $args
$gc -il "$args" $NEMS/source/*.f
