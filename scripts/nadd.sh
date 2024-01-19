# 
TK_ECHO_USE_BASH_BEHAVIOR=1
# This is NEMS NADD. It adds the requested file into the appropriate
# NEMS system files. The addition to the git needs to be done manually
# This script only needs to be run for source and I/O files
#   -  input and output files are added to filemgr.shell
#   -  input files and source code are added to varkeys and the reference case scedes file
#**************************************************************

# add trap 
trap "rm -f *.$$ 2> /dev/null; exit" 1 2 15 3
    
 banner  "NEMS "
 banner  "NADD" 
 #if [[ "$USER" != "kem" && "$USER" != "avk" ]] ; then
 #  echo "You are not among the approved users of this command."
 #  echo "Please call a power user (edt, adr, kem, avk) to nadd files to the system."
 #  exit
 #fi
 if [[ -z "$1" ]] ; then
   echo  "Nadd.sh usage:  filename [type]" 
   echo  "File should exist before attempting a nadd."
   exit
 fi

 filename=$1

# Check if we have right filename. If the user gives a nonblank string, use it instead.
 echo  "You have entered $filename. Press <Enter> if this is ok, or type a new filename"

 echo -n "[$filename]:";read dummy
 if [[ -n "$dummy" ]]  ; then
   filename=$dummy
 fi

 type=$2
 not_valid=
 while [[ -z "$not_valid" ]] ; do      # loop until type is set
   case $type in
      input|output|source)  
        echo  "You have entered $type.  Press <Enter> if this is ok, or enter a new type."
        echo -n "[$type]";read dummy
        if [[ -n "$dummy" ]]  ; then
          type=$dummy
        fi 
# Allow users to enter comment
        echo 
        comment=
        if [[ -z "$comment" ]] ; then
          echo  "Please enter short (one line) description of $filename"
          read comment
        fi

# Find filename suffix
        echo $filename | awk -F\. '{print $2}' > suffix.$$;
        read suffix < suffix.$$; rm suffix.$$
# Trim off suffix; convert lowercase filename to an uppercase
        fileid=$(echo  ${filename%.$suffix} | tr '[a-z]' '[A-Z]')
        filed=$(echo  $fileid"D")
        filedq=$(echo  $fileid"D=?")


        if [[ "$type" != "output" ]] ; then

# For input and source files, add to varkeys file 
            case $type in input|source)

# Display Menu for all Modules 
              echo "********************************** "
              echo " Choose the letter that applies to your file:"
              echo " W  -> World               M  -> Macro "
              echo " R  -> Residential         K  -> Commercial "
              echo " I  -> Industrial          T  -> Transportation "
              echo " E  -> Electric Utility    C  -> Coal "
              echo " L  -> Oil & Gas Supply    G  -> Natural Gas T&D "
              echo " O  -> Refinery            N  -> Renewables "
              echo " H  -> Hydrogen            X  -> All modules"
              echo " Press <Enter> if none of the above modules apply to $filename "
              echo "********************************** "
              module=
              modstar=
              not_valid=""
              while [[ -z "$not_valid" ]] ; do
                echo  "Please enter which module does this file belongs to: "
                echo  "i.e., W -> World"
                echo -n "=> ";read module
                echo 
                module=$(echo  $module | tr '[a-z]' '[A-Z]')
                if [[ -z "$module" ]] ; then
                  not_valid=false
                  modstar="*"
                else
                  case $module in
                    W|M|R|K|I|T|E|C|L|G|O|N|H)
                       modstar="$module"
                       module="?$module@" 
                       not_valid=false ;;
                    X)
                       modstar="*"
                       module="" 
                       not_valid=false ;;
                    *) echo  "You have enter an invalid module letter!" ;;
                  esac
                fi
              done


# Check the first field of varkeys file for filename with "D" appended
              nline2=$(cat $NEMS/scripts/varkeys | cut -d= -f1 | grep -c -x $filed)
              if [ $nline2 -gt 0 ] ; then 
                echo  "FILENAME ALREADY EXISTS IN varkeys"
              else
                echo "$filedq,$modstar,$comment" |
                awk ' BEGIN { FS = "," } { printf("%-21s%-2s%-s\n",$1,$2,$3) }' >> $NEMS/scripts/varkeys
                echo  "varkeys was modified"     
              fi

      
            esac

#          fi
        fi
        not_valid=false ;;
      *)   
        echo  "You have not specified a valid type. "
        echo  "Please specify a type [input|output|source]"
        read type
        ;;
   esac
 done

# For Input and Output type, checkout Filemgr.shell to insert two lines  
   
 case $type in
   input|output)
  
# Check Filemgr.shell to see if it has filename (variable fileid from above)
#     sh -K $NEMS/scripts/nemsco.sh filemgr.shell default
     file_check=$(grep -c " $fileid " $NEMS/scripts/filemgr.shell)
     if [ $file_check -gt 0 ] ; then
       echo 
       echo  "FILENAME ALREADY EXISTS IN filemgr.shell"
     fi

     format=
     not_valid=
     while [[ -z "$not_valid" ]] ; do
       echo  "Please enter one of the followings for Filemgr open statement: "
       echo  "READ - (for Input files)"
       echo  "WRITE - (for most Output files)"
       echo  "READWRITE - (for most Direct Access files)"
       echo -n "=> ";read format
       echo 
       format=$(echo  $format | tr '[a-z]' '[A-Z]')
       case $format in
         READ|WRITE|READWRITE)   not_valid=false ;;
         *) echo  "You have enter an invalid format type!" ;;
       esac
     done

     file_access=
     echo  "Please specify SEQUENTIAL or DIRECT <default is SEQUENTIAL> "
     echo -n "=> ";read file_access
     file_access=$(echo  $file_access | tr '[a-z]' '[A-Z]')
     if [[ -z "$file_access" ]] ; then
       file_access=SEQUENTIAL
     fi
     if [[ $file_access = SEQUENTIAL ]] ; then
       case $format in 
         READ|READWRITE)
           lrecl=80
           file_fmt=FORMATTED
           ;;
         WRITE)        
           lrecl=133
           file_fmt=
           echo  "Please specify FORMATTED or UNFORMATTED <default is FORMATTED> "
           echo -n "=> ";read file_fmt
           file_fmt=$(echo  $file_fmt | tr '[a-z]' '[A-Z]')
           if [[ -z "$file_fmt" ]] ; then
             file_fmt=FORMATTED
           fi
           ;;
       esac
     fi
     if [[ "$file_access" = "DIRECT" ]] ; then
       file_fmt=UNFORMATTED

       lrecl=
       echo  "Please specify Record Length <default = 16384>: "
       echo -n "=> ";read lrecl
       if [[ -z "$lrecl" ]] ; then
         lrecl=16384
       fi
     fi 

#Build filename
     case $format in
       READ)
         u_namen=$(echo  $fileid"N")
         u_named=$(echo  $fileid"D")
         unix=?$u_namen@?$u_named@
         ;;
       WRITE|READWRITE)
         unix=?outdir@/?scen@/?date@/$fileid
         unix=$(echo  $unix | tr '[a-z]' '[A-Z]').$suffix
         ;;
     esac

# Put the results into Filemgr.shell
     echo "$module $fileid $unix $format $file_access $file_fmt $lrecl $comment" >> $NEMS/scripts/filemgr.shell
     echo  "filemgr.shell was modified"  
    
     ;;
 esac
# Add the new file to the git scedes files (input and source only)
 case $type in
   input)

     ls $NEMS/\scedes/\scedes.* >scedesfiles
     while read line 
     do
      echo "$fileid"N"=\$NEMS/$type/$filename"  >> $line
      echo  "$line was modified"  
     done < scedesfiles
     
     ;;
      source)
      echo "$fileid"N"=\$NEMS/$type/$fileid".obj""   >> $NEMS/scedes/scedes.ref2022
      echo  "scedes.ref2022 was modified"  
    ;;
 esac
echo "********************************************* "

 
