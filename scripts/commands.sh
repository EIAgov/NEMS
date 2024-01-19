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
#        qftab:  prepare nems tables using ftab program 
alias    qftab="$NEMS/scripts/qftab.exe" ; export qftab
#        qnems:  prepare and submit NEMS run
alias    qnems="$NEMS/scripts/qnems.exe" ; export qnems
#        runnems:  prepare and submit NEMS run
alias    runnems="$NEMS/scripts/qnems.exe" ; export runnems
#        jognems:  prepare and submit NEMS run
alias    jognems="PARNEMS=;export PARNEMS;sh $NEMS/scripts/submit.sh" ; export jognems
#        parnems:  prepare and submit parallel NEMS run
alias    parnems="PARNEMS=yes;export PARNEMS;sh $NEMS/scripts/submit.sh" ; export parnems

#        scripts:  change directory to the scripts directory
alias    scripts="cd $NEMS/scripts" ; export scripts
#        fdef:     find the default version of a file and list the log
alias    fdef="sh $NEMS/scripts/fdef.sh"              ; export fdef
#        nlook:    make a given version the default and copy it to a common area
alias    nlook="sh $NEMS/scripts/nlook.sh" ; export nlook
#        nemsci:   NEMS Check-in: put local file into RCS database and increment revision
alias    nemsci="sh $NEMS/scripts/nemsci.sh" ; export nemsci
#        nemsco:   NEMS Check-out: make local copy of a NEMS file, usually to edit
alias    nemsco="sh $NEMS/scripts/nemsco.sh" ; export nemsco
#        nfort:    list fortran files and give options to compile them
alias    nfort="sh $NEMS/scripts/nfort.sh" ; export nfort
#        nftab:    run the NEMS Ftab report writer on a standalone basis
alias    nftab="sh $NEMS/scripts/nftab.sh" ; export nftab
#        nadd:     add a file to the NEMS configuration management system
alias    nadd="sh $NEMS/scripts/nadd.sh" ; export nadd
#        naddalot: add several input files to NEMS with 1 varkeys/filemgr.shell update
alias    naddalot="sh $NEMS/scripts/naddalot.sh" ; export naddalot
#        listlock: list all files locked by a user and display the locations
alias    listlock="sh $NEMS/scripts/listlock.sh" ; export listlock
#        tfiler:   run a standalone version of filer to read and write restart files, extract data
alias    tfiler="$NEMS/scripts/tfiler.py" ; export tfiler
#        unlock:   remove the lock on a file under NEMS revsion control
alias    unlock="sh $NEMS/scripts/unlock.sh" ; export unlock
#        analyze:  execute the analyze program to examine LPs
alias    analyze="sh $NEMS/analyze/analyze.s" ; export analyze
#        ffind:    list files, below the current directory, containing a given string or pattern 
alias    ffind="sh $NEMS/scripts/ffind.sh" ; export ffind
#        getlog:   display last file from the Check-Out log matching file-name, user, version 
alias    getlog="sh $NEMS/scripts/getlog.sh" ; export getlog
#        includeuse: display NEMS fortran files that reference a given include file
alias    includeuse="sh $NEMS/scripts/includeuse.sh" ; export includeuse
#        delrun:   display last 10 NEMS run and allow user to delete any of them
alias    delrun="sh $NEMS/scripts/delrun.sh" ; export delrun
#        trim:     removing trailing blanks from a file and create a new file
alias    trim="sh $NEMS/scripts/trim.sh" ; export trim
#        nprint:   print a file to a LAN printer, the default printer, or the Mainframe printer
alias    nprint="sh $NEMS/scripts/nprint.sh" ; export nprint
#        nfreeze:     prevent NEMS runs while block lookups are being done
alias    nfreeze="sh $NEMS/scripts/nfreeze.sh" ; export nfreeze
#        nunfreeze:     unfreezing NEMS so runs can be made after nfreezing
alias    nunfreeze="sh $NEMS/scripts/nunfreeze.sh" ; export nunfreeze
#        nhelp:    give help on NEMS commands
alias    nhelp="sh $NEMS/scripts/nhelp.sh" ; export nhelp
#        cleanlog: clean up the runlog by removing deleted or lost runs
alias    cleanlog="sh $NEMS/scripts/cleanlog.sh" ; export cleanlog
#        fincinc:  Find INConsistences among INClude files in the complog files'
alias    fincinc="sh $NEMS/scripts/fincinc.sh" ; export fincinc
#        nhead:    Take a given revision and copy it to the "head" revision
alias    nhead="sh $NEMS/scripts/nhead.sh" ; export nhead
#        quiet:  sets environment variable QUIET for quiet nems checkin
alias    quiet="sh $NEMS/scripts/quiet.sh" ; export quiet
#        readda:  read data from direct access file and write out a range of value from @.MNPQIT  
alias    readda="sh $NEMS/scripts/readda.sh" ; export readda
#        diction:  Compares common block layout to data dictionary used by filer
alias    diction="sh $NEMS/scripts/diction.sh" ; export diction
#        archive: Create a tar file of the output of a run, and all files necessary to recreate it.
alias    archive=". $NEMS/scripts/archive.sh" ; export archive
#        prunetree: Create a script with the RCS commands to prune a single file's deadwood
alias    prunetree="sh $NEMS/scripts/prunetree.sh" ; export prunetree
alias    welcome="sh $NEMS/scripts/firstname.sh" ; export welcome
#        saverun:    Create scedes.all from keys.sed
alias    saverun="sh $NEMS/scripts/saverun.exe keys.sed" ; export saverun
#        listruns: list runs in the runlog matching strings in the argument
alias    listruns="sh $NEMS/scripts/listruns.sh"    ; export listruns
#        shaveftab: prepare fort.20 for on screen viewing--store in ftab.out
alias    shaveftab="$NEMS/scripts/shaveftabawk.exe <fort.20 >ftab.out" ; export shaveftab
#        addex:  add module execution switches to a scedes file
alias    addex="sh $NEMS/scripts/addex.sh" ; export addex
#        findrestart: report restart file name given scenario, datecode arguments
alias    findrestart="sh $NEMS/scripts/findrestart.sh" ; export findrestart
#        isteocomp: write 1-screen report comparing steo to NEMS for a run
alias    isteocomp='. $NEMS/scripts/isteocomp.sh'  ; export isteocomp
#        rpttrad:  generate the electricity trade reports with an argument
alias    nrpttrad="sh $NEMS/scripts/rpttrad.sh"  ; export nrpttrad
#        muchoftab:  sets up multiple runs of ftab 
alias    muchoftab="$NEMS/scripts/muchoftab.exe"  ; export muchoftab
#        pcfort:     converts aix fortran syntax to pc syntax
alias    pcfort="sh $NEMS/scripts/pcfort.sh" ; export pcfort
#        taxch:  shows iteration 1 and ending carbon tax from epmout for each year
alias    taxch="sh $NEMS/scripts/taxch.sh"  ; export taxch
#        listdefs:  print selected lines from the DEFAULTS log with an argument
alias    listdefs="$NEMS/scripts/listdefs.sh"  ; export listdefs
#        userdrives:  copy of UNIX df command to avoid command name conflict 
alias    userdrives="$NEMS/scripts/udrives.exe" ; export userdrives
#        freefrm:    convert file or set of files (like *.f) to free format source
alias    freefrm="sh $NEMS/scripts/freefrm.sh" ; export freefrm
#        sj:      displays and controls nems jobs: sj [menu|kill|suspend|change|monitor [jobnumber]]
alias    sj="$NEMS/scripts/jobcontrol.exe" ; export sj
#        jc:      displays and controls nems jobs: jc [menu|kill|suspend|change|monitor [jobnumber]]
alias    jc="$NEMS/scripts/jobcontrol.exe" ; export jc
#        sq:     creates list of the current nems job queue initiators 
alias    sq="sh $NEMS/scripts/shoqueue.sh" ; export sq
#        stopqueues  Closes downs the active job queues after their current jobs 
alias    stopqueues="sh $NEMS/scripts/stopqueues.sh" ; export stopqueues
#        whois       Gives EIAHQ network information on a user named as the argument, such as: whois dsa"
alias    whois="sh $NEMS/scripts/whois.sh" ; export whois
#        merge     syntax - merge file1 file2 file3; where file1 and file3 contain different updates of file2; changes placed in file1, with both versions included in cases of overlaps
alias    merge="sh $NEMS/scripts/merge.sh" ; export merge
#        compress  alias for gzip compression routine
alias    compress=gzip ; export compress
#        nox_rpt:   Create a NOX Control Summary Report
alias    nox_rpt="sh M:/rec/rsc/scripts/nox_rpt.sh" ; export nox_rpt
#        sl:        Show users on nemX
alias    sl="sh $NEMS/scripts/sl.sh"  ; export sl
#        sla:       Show active users on nemX
alias    sla="echo NEM1;query user /server:NEM1 | grep -i active;echo NEM2;query user /server:NEM2 | grep -i active;echo NEM3;query user /server:NEM3 | grep -i active;echo NEM4;query user /server:NEM4 | grep -i active;echo NEM5;query user /server:NEM5 | grep -i active;echo NEM6;query user /server:NEM6 | grep -i active"  ; export sla
#        get_retro:   Extract So2 Retrofits from EMMPRNT and Create simple report
alias    get_retro="sh $NEMS/scripts/get_retro.sh" ; export get_retro
#        coal_so2:   Get SO2 Compliance Info from CLDEBUG.txt and wrtite a simple report
alias    coal_so2="sh $NEMS/scripts/coal_so2.sh" ; export coal_so2
#        readmap:   Read nems.map and report on memory usag
alias    readmap="$NEMS/scripts/readmap.exe" ; export readmap
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
#        wk1head:   replace a string in a wk1 file 
alias    wk1head="$NEMS/scripts/wk1head.exe" ; export wk1head
#        eg:   summary of NEMS run success
alias    eg="sh $NEMS/scripts/eg.sh" ; export eg
#        mp:   searches for specific memory problem string
alias    mp="sh $NEMS/scripts/mp.sh" ; export eg
#        postftab:   run programs off of RAN files that create xls file and rtf files
alias    postftab="sh $NEMS/scripts/redo_post_ftab.sh" ; export postftab
#        comprtf:   create comparison rtf
alias    comprtf="sh $NEMS/scripts/redo_compare_rtf.sh" ; export comprtf
#        fgraph:    Program to create, edit graph layout files and create batch graphs from NEMS runs
alias    fgraph="$NEMS/scripts/fgraph.exe" ; export fgraph
#        fgraph_auto:    Non-interactive program to create batch graphs from graph layout files and NEMS runs
alias    fgraph_auto="$NEMS/scripts/fgraph-auto.exe" ; export fgraph_auto
#        nfgraph:    sets up fgraph-auto interface file and runs fgraph-auto to create batch graphs
alias    nfgraph="sh $NEMS/scripts/nfgraph.sh" ; export nfgraph
#        showspace:   displays free disk space on drives L:, M:, N:, O: 
alias    showspace="sh $NEMS/scripts/showspace.sh" ; export showspace
#        runspace:  creates runspace_by_user.txt, reporting disk space within current path used for runs. first cd /
alias    runspace="sh $NEMS/scripts/get_run_space_by_user.ksh" ; export runspace
#        dbpgrp:   compiles and links udbp.f to create udbp.exe
alias    dbpgrp="$NEMS/scripts/emm/udbp.compile.sh" ; export dbpgrp
#        readcog:   reads debug output of ind.f from nems nohup.out and creates report on cogen additions 
alias    readcog="$NEMS/scripts/readcog.exe" ; export readcog
#        readprodcur:   reads debug output of ind.f from nems nohup.out and creates report on productive capacity by vintage
alias    readprodcur="$NEMS/scripts/readprodcur.exe" ; export readprodcur
#        eftab:    run the NEMS Ftab report writer on a standalone basis
alias    eftab="sh $NEMS/scripts/eftab.sh" ; export eftab
#        gamsmps creates an MPS file from GAMS output; uses gamsmps.awk
alias    gamsmps="sh $NEMS/scripts/gamsmps.sh" ; export gamsmps
#        tp:         launches textpad, a good programming editor
if [ -f "c:/program files/textpad 7/textpad.exe" ] ; then
 alias    tp='"c:/program files/textpad 7/textpad.exe"' ; export tp
elif [ -f "c:/program files/textpad 5/textpad.exe" ] ; then
 alias    tp='"c:/program files/textpad 5/textpad.exe"' ; export tp
elif [ -f "c:/program files/textpad 4/textpad.exe" ] ; then
 alias    tp='"c:/program files/textpad 4/textpad.exe"' ; export tp
elif [ -f "c:/program files (x86)/textpad 7/textpad.exe" ] ; then
 alias    tp='"c:/program files (x86)/textpad 7/textpad.exe"' ; export tp
elif [ -f "c:/program files (x86)/textpad 5/textpad.exe" ] ; then
 alias    tp='"c:/program files (x86)/textpad 5/textpad.exe"' ; export tp
elif [ -f "c:/program files (x86)/textpad 4/textpad.exe" ] ; then
 alias    tp='"c:/program files (x86)/textpad 4/textpad.exe"' ; export tp
else
 echo "Textpad not found on computer $COMPUTERNAME"
fi
#          tps:   invoke textpad 7 in separate, background window so shell command processor keeps working
alias tps="sh $NEMS/scripts/tps.sh";export tps
#        makexls:    Creates xls file with ftab output using the RAN file. runs $NEMS/scripts/rantoexcel.ksh
alias    makexls="$NEMS/scripts/rantoexcel.ksh" ; export makexls
#        checkscedes: Reads a scedes file (named as an argument) and checks that the input files designated in it exist. 
alias    checkscedes="$NEMS/scripts/checkscedes.exe" ; export checkscedes
#        pack_all:  Converts one, or all, .act file(s) to .pck files and creates text files for ROW, COL, MAT
alias    pack_all="$NEMS/scripts/pack_all.exe" ; export pack_all
#        copyrun:   Copies a run to a shared archive folder on A45589 and deletes the run from the original location
alias    copyrun="$NEMS/scripts/copyrun.sh" ; export copyrun
#        lfopt:   Shortens/summarizes the lfmm optimization results
alias    lfopt="$NEMS/scripts/lfopt.sh" ; export lfopt
#        listcommons: list common blocks defined in up to 9 source code files
alias    listcommons="$NEMS/scripts/listcommons.sh" ; export listcommons
#        listincludes: creates list of include files referenced in a given fortran file supplied as an argument
alias    listincludes="$NEMS/scripts/listincludes.sh" ; export listincludes
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
alias 	 cdd='cd `echo $PWD | sed "/nem2\/e/s@workdir@e\/workdir@;s@\/\/nem[0-9]\/@@;s@\/@:\/@"`';export cdd
#        caimms:    prepares the coal model input folder and launches the aimms coal model from the command line
alias 	 caimms="$NEMS/scripts/caimms.sh"; export caimms
#        getgpa:    searches for GPA scores for runs in subfolders with a given date or datekey and prints them
alias 	 getgpa="$NEMS/scripts/getgpa.sh"
#        gpa:    reports GPA score from nohup.out in current directory
alias 	 gpa="egrep 'GPA \(run #|4 is bad' nohup.out" ; export gpa
#        makemps    creates .mps files from OML .act files using the pack_all and analyze programs
alias    makemps="sh $NEMS/scripts/makemps.sh" ; export makemps
#        excel      launches excel.exe with file given as the command argument
alias    excel=". $NEMS/scripts/excel.sh" ; export excel
#        def      searches for definition and include-file location of a NEMS fortran variable given as argument
alias    def=". $NEMS/scripts/def.sh" ; export def
#        pdef      search for definition of a NEMS fortran parameter provided as an argument.
alias    pdef=". $NEMS/scripts/pdef.sh" ; export pdef
#        groupmem:  lists members of group
alias    groupmem=". $NEMS/scripts/list_group_members.sh" ; export groupmem
#        countgroups: Reads a file (which can be named as an argument) and counts how many lines exist for each text label, provided the file is delimited (delimiter can be provided as argument)
alias    countgroups="$NEMS/scripts/countgroups.exe" ; export countgroups
#        run2jog:  makes a NEMS run via runnems equally acceptable to the archive script as a run via parnems or jognems
alias    run2jog="$NEMS/scripts/run_to_jog.sh" ; export run2jog
#EMM scripts
#        prepro:   run the job submitter for the EMM Preprocessors  
alias    prepro="sh $NEMS/scripts/prepro.sh" ; export prepro
#        create emmdb.mdb file from edbpgrp.txt files
alias    cremmdb="sh $NEMS/scripts/cremmdb.sh" ; export cremmdb
#        create geothermal debug files from WDUMP.txt
alias  	 geodbug="sh $NEMS/scripts/geodbug.sh" ; export geodbug
#        create hydro debug files from WDUMP.txt
alias 	 hyddbug="sh $NEMS/scripts/hyddbug.sh" ; export hyddbug
#        compile udbp code and create udbp executable
alias  	 compudbp="sh $NEMS/scripts/udbp.compile.sh" ; export compudbp