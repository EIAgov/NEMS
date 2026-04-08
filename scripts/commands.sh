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
#        analyze:  execute the analyze program to examine LPs
alias    ffind="sh $NEMS/utils/ffind.sh" ; export ffind
#        includeuse: display NEMS fortran files that reference a given include file
alias    includeuse="sh $NEMS/utils/includeuse.sh" ; export includeuse
#        nhelp:    give help on NEMS commands
alias    nhelp="sh $NEMS/utils/nhelp.sh" ; export nhelp
#        listruns: list runs in the runlog matching strings in the argument
alias    listruns="sh $NEMS/scripts/listruns.sh"    ; export listruns
#        findrestart: report restart file name given scenario, datecode arguments
alias    findrestart="sh $NEMS/scripts/findrestart.sh" ; export findrestart
#        whois       Gives EIAHQ network information on a user named as the argument, such as: whois dsa"
alias    whois="sh $NEMS/utils/whois.sh" ; export whois
#        compress  alias for gzip compression routine
alias    compress=gzip ; export compress
#        sla:       Show active users on nemX
alias    sla="echo NEM1;query user /server:NEM1 | grep -i active;echo NEM2;query user /server:NEM2 | grep -i active;echo NEM3;query user /server:NEM3 | grep -i active;echo NEM4;query user /server:NEM4 | grep -i active;echo NEM5;query user /server:NEM5 | grep -i active;echo NEM6;query user /server:NEM6 | grep -i active"  ; export sla
#        graf2000:   Run Graf2000, Run GrafNEM NEW!
alias    graf2000="$NEMS/scripts/launch_graf2000.exe" ; export graf2000
#        grafit:    set up graf2000 filelist.txt for a user using ran files in current directory 
alias    grafit=". $NEMS/scripts/grafit.sh" ; export grafit
#        readprodcur:   reads debug output of ind.f from nems nohup.out and creates report on productive capacity by vintage
alias    readprodcur="$NEMS/scripts/readprodcur.exe" ; export readprodcur
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
#        lfopt:   Shortens/summarizes the lfmm optimization results
alias    lfopt="$NEMS/scripts/lfopt.sh" ; export lfopt
#        wp:         runs wordpad from within kornshell
alias    wp='"c:/program files/windows nt/accessories/wordpad.exe"' ; export wp
#        user:      gives user ID and PC ID for a given last name provided as the argument, like: user kondis
alias    user="$NEMS/scripts/userinfo.sh" ; export user
#        $NEMS/scripts/ifortvars18.sh:    set up 64-bit fortran compiler and linker paths for 64-bit windows
IVERS=18.1
. $NEMS/scripts/ifortvars18.sh
#        cdd:       changes working directory from UNC style path to drive letter style path
alias cdd='cd `echo $PWD | sed "/nem2\/e/s@workdir@e\/workdir@;s@\/\/nem[0-9]\/@@;s@\/@:\/@"`';export cdd
#        def      searches for definition and include-file location of a NEMS fortran variable given as argument
alias    def=". $NEMS/utils/def.sh" ; export def
#        pdef      search for definition of a NEMS fortran parameter provided as an argument.
alias    pdef=". $NEMS/utils/pdef.sh" ; export pdef
#        prepro:   run the job submitter for the EMM Preprocessors  
alias    prepro="sh $NEMS/scripts/prepro.sh" ; export prepro
#        create geothermal debug files from WDUMP.txt
alias    geodbug="sh $NEMS/scripts/geodbug.sh" ; export geodbug
#        create hydro debug files from WDUMP.txt
alias    hyddbug="sh $NEMS/scripts/hyddbug.sh" ; export hyddbug
