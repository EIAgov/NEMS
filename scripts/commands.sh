if [ -z "$NEMSDIR" ] ; then
  echo "warning: NEMSDIR not set to a drive in your profile"
  echo "will assume the drive for /default files is M:"
  NEMSDIR="m:"
  export NEMSDIR
fi

if [ -z "$NEMSJOBLOG" ] ; then
  NEMSJOBLOG="$NEMSDIR/NEMSJobLog"
  export NEMSJOBLOG
fi

#set bash befavior for echo command so the -n option can be used to provide prompt for read command
TK_ECHO_USE_BASH_BEHAVIOR=1; export TK_ECHO_USE_BASH_BEHAVIOR
#Establish Temp directory for RCS and other programs called from Korn shell.
TMPDIR=$TMP
#Convert backward slashes to forward slashes (obviously)
while true
do
     case $TMPDIR in
     *\\*)     TMPDIR=${TMPDIR%%\\*}/${TMPDIR#*\\};;
     *)   break;;
     esac
done
export TMPDIR
if [ "$OSTYPE" != "cygwin" ] ; then
#mks version 10 sets SHELL incorrectly and as a result, man command doesn't work. Correct it.
  SHELL='c:/PROGRA~2/MKSTOO~1/mksnt/sh.exe'
  export SHELL
fi 
#               Command Aliases for Using NEMS 
#          
#Adhere to the format here because the nhelp command lists
#lines in this file with the "#" followed by a blank
#
#        treesize:  Runs Treesize Professional
alias    treesize='"C:/Program Files (x86)/JAM Software/TreeSize Professional/treesize.exe"' ; export treesize
#        jognems:  prepare and submit NEMS run
pyver=$(awk -F',' '/pyver/ {print $2}' "$NEMS/scripts/setup/input/init_configs.csv")
submitpy="$pyver/Scripts/activate&python $NEMS/scripts/setup/src/nems_submit.py"
alias    jognems="cmd.exe /c '$submitpy jog'" ; export jognems
#        parnems:  prepare and submit parallel NEMS run
alias    parnems="cmd.exe /c '$submitpy par'" ; export parnems
#        scripts:  change directory to the scripts directory
alias    scripts="cd $NEMS/scripts" ; export scripts
#        fdef:     find the default version of a file and list the log
alias    fdef="sh $NEMS/scripts/fdef.sh"              ; export fdef
#        nfort:    list fortran files and give options to compile them
alias    nfort="sh $NEMS/utils/nfort.sh" ; export nfort
#        nadd:     add a file to the NEMS configuration management system
alias    nadd="sh $NEMS/scripts/nadd.sh" ; export nadd
#        naddalot: add several input files to NEMS with 1 varkeys/filemgr.shell update
alias    naddalot="sh $NEMS/scripts/naddalot.sh" ; export naddalot
#        tfiler:   run a standalone version of filer to read and write restart files, extract data
alias    tfiler="$NEMS/scripts/tfiler.py" ; export tfiler
#        analyze:  execute the analyze program to examine LPs
alias    analyze="sh $NEMS/analyze/analyze.s" ; export analyze
#        ffind:    list files, below the current directory, containing a given string or pattern 
alias    ffind="sh $NEMS/utils/ffind.sh" ; export ffind
#        includeuse: display NEMS fortran files that reference a given include file
alias    includeuse="sh $NEMS/utils/includeuse.sh" ; export includeuse
#        delrun:   display last 10 NEMS run and allow user to delete any of them
alias    delrun="sh $NEMS/scripts/delrun.sh" ; export delrun
#        trim:     removing trailing blanks from a file and create a new file
alias    trim="sh $NEMS/scripts/trim.sh" ; export trim
#        nhelp:    give help on NEMS commands
alias    nhelp="sh $NEMS/utils/nhelp.sh" ; export nhelp
#        prunetree: Create a script with the RCS commands to prune a single file's deadwood
alias    welcome="sh $NEMS/scripts/firstname.sh" ; export welcome
#        listruns: list runs in the runlog matching strings in the argument
alias    listruns="sh $NEMS/scripts/listruns.sh"    ; export listruns
#        addex:  add module execution switches to a scedes file
alias    addex="sh $NEMS/scripts/addex.sh" ; export addex
#        findrestart: report restart file name given scenario, datecode arguments
alias    findrestart="sh $NEMS/scripts/findrestart.sh" ; export findrestart
#        whois       Gives EIAHQ network information on a user named as the argument, such as: whois dsa"
alias    whois="sh $NEMS/utils/whois.sh" ; export whois
#        compress  alias for gzip compression routine
alias    compress=gzip ; export compress
#        nox_rpt:   Create a NOX Control Summary Report
alias    nox_rpt="sh M:/rec/rsc/scripts/nox_rpt.sh" ; export nox_rpt
#        sla:       Show active users on nemX
alias    sla="echo NEM1;query user /server:NEM1 | grep -i active;echo NEM2;query user /server:NEM2 | grep -i active;echo NEM3;query user /server:NEM3 | grep -i active;echo NEM4;query user /server:NEM4 | grep -i active;echo NEM5;query user /server:NEM5 | grep -i active;echo NEM6;query user /server:NEM6 | grep -i active"  ; export sla
#        get_retro:   Extract So2 Retrofits from EMMPRNT and Create simple report
alias    get_retro="sh $NEMS/scripts/get_retro.sh" ; export get_retro
#        runoml:    Compiles, links, and runs a fortran program that uses OML libraries (vers4lib) 
alias    runoml="sh $NEMS/scripts/runoml.ksh" ; export runoml
#        cleanqueue:   Clean out old queue-related and verscomp files from logs 
alias    cleanqueue="sh $NEMS/scripts/cleanqueue.sh" ; export cleanqueue
#        graf2000:   Run Graf2000 
alias    graf2000="$NEMS/scripts/launch_graf2000.exe" ; export graf2000
#        grafnem:   Run GrafNEM NEW!  which graf2000 does, too
alias    grafnem="$NEMS/scripts/launch_graf2000.exe" ; export grafnem
#        grafit:    set up graf2000 filelist.txt for a user using ran files in current directory 
alias    grafit=". $NEMS/scripts/grafit.sh" ; export grafit
#        eg:   summary of NEMS run success
alias    eg="sh $NEMS/scripts/eg.sh" ; export eg
#        showspace:   displays free disk space on drives L:, M:, N:, O: 
alias    showspace="sh $NEMS/scripts/showspace.sh" ; export showspace
#        dbpgrp:   compiles and links udbp.f to create udbp.exe
alias    dbpgrp="$NEMS/scripts/emm/udbp.compile.sh" ; export dbpgrp
#        readcog:   reads debug output of ind.f from nems nohup.out and creates report on cogen additions 
alias    readcog="$NEMS/scripts/readcog.exe" ; export readcog
#        readprodcur:   reads debug output of ind.f from nems nohup.out and creates report on productive capacity by vintage
alias    readprodcur="$NEMS/scripts/readprodcur.exe" ; export readprodcur
#        gamsmps creates an MPS file from GAMS output; uses gamsmps.awk
alias    gamsmps="sh $NEMS/scripts/gamsmps.sh" ; export gamsmps
#        tp:         launches textpad, a good programming editor
if [ -f "c:/program files/textpad 8/textpad.exe" ] ; then
 alias    tp='"c:/program files/textpad 8/textpad.exe"' ; export tp
elif [ -f "c:/program files/textpad 7/textpad.exe" ] ; then
 alias    tp='"c:/program files/textpad 7/textpad.exe"' ; export tp
elif [ -f "c:/program files (x86)/textpad 8/textpad.exe" ] ; then
 alias    tp='"c:/program files (x86)/textpad 8/textpad.exe"' ; export tp
elif [ -f "c:/program files (x86)/textpad 7/textpad.exe" ] ; then
 alias    tp='"c:/program files (x86)/textpad 7/textpad.exe"' ; export tp
else
 echo "Textpad not found on computer $COMPUTERNAME"
fi
#        checkscedes: Reads a scedes file (named as an argument) and checks that the input files designated in it exist. 
alias    checkscedes="$NEMS/scripts/checkscedes.exe" ; export checkscedes
#        pack_all:  Converts one, or all, .act file(s) to .pck files and creates text files for ROW, COL, MAT
alias    pack_all="$NEMS/scripts/pack_all.exe" ; export pack_all
#        lfopt:   Shortens/summarizes the lfmm optimization results
alias    lfopt="$NEMS/scripts/lfopt.sh" ; export lfopt
#        wp:         runs wordpad from within kornshell
alias    wp='"c:/program files/windows nt/accessories/wordpad.exe"' ; export wp
#        user:      gives user ID and PC ID for a given last name provided as the argument, like: user kondis
alias    user="$NEMS/scripts/userinfo.sh" ; export user
#        trunix:    Translates the CRLF end-of-line sequence to LF for cygwin/bash compatibility.  
alias    trunix="$NEMS/scripts/trunix.sh" ; export trunix
#        nemsver.sh:    set up NEMS platform or version folder in environment variable NEMS"
alias    nemsver=". $NEMS/scripts/nemsver.sh" ; export nemsver
#        $NEMS/scripts/ifortvars18.sh:    set up 64-bit fortran compiler and linker paths for 64-bit windows
IVERS=18.1
. $NEMS/scripts/ifortvars18.sh
#        cdd:       changes working directory from UNC style path to drive letter style path
alias cdd='cd `echo $PWD | sed "/nem2\/e/s@workdir@e\/workdir@;s@\/\/nem[0-9]\/@@;s@\/@:\/@"`';export cdd
#        caimms:    prepares the coal model input folder and launches the aimms coal model from the command line
alias caimms="$NEMS/scripts/caimms.sh"; export caimms
#        gpa:    reports GPA score from nohup.out in current directory
alias    gpa="egrep 'GPA \(run #|4 is bad' nohup.out" ; export gpa
#        makemps    creates .mps files from OML .act files using the pack_all and analyze programs
alias    makemps="sh $NEMS/scripts/makemps.sh" ; export makemps
#        def      searches for definition and include-file location of a NEMS fortran variable given as argument
alias    def=". $NEMS/utils/def.sh" ; export def
#        pdef      search for definition of a NEMS fortran parameter provided as an argument.
alias    pdef=". $NEMS/utils/pdef.sh" ; export pdef
#        prepro:   run the job submitter for the EMM Preprocessors  
alias    prepro="sh $NEMS/scripts/prepro.sh" ; export prepro
#        create emmdb.mdb file from edbpgrp.txt files
alias    cremmdb=". $NEMS/scripts/cremmdb.sh" ; export cremmdb
#        create geothermal debug files from WDUMP.txt
alias    geodbug="sh $NEMS/scripts/geodbug.sh" ; export geodbug
#        create hydro debug files from WDUMP.txt
alias    hyddbug="sh $NEMS/scripts/hyddbug.sh" ; export hyddbug
#        compile udbp code and create udbp executable
alias    compudbp="sh $NEMS/scripts/udbp.compile.sh" ; export compudbp