! -*- f90 -*-
module utils
    include 'parametr'
    include 'emmparm' ! $ to _, param
    include 'ecpcntl' ! $ to _, param, need to fix NOX_GRP and CO2_GRP
    include 'dsmdimen' ! param
    include 'cdsparms' ! param
    include 'coalemm' ! param
    !
    include 'qblk' ! param
    include 'mpblk'
    ! include 'ampblk' ! param
    include 'mxqblk'
    include 'mxpblk'
    include 'qsblk'
    include 'ncntrl'
    include 'lfmmout'
    include 'pmmout'
    include 'pmmrpt'
    include 'pmmftab'
    include 'ogsmout'
    include 'ngtdmout'
    ! include 'angtdm'
    include 'ngtdmrep'
    include 'ngrpt'
    include 'uefpout'
    include 'efpout'
    include 'uefdout'
    include 'udatout'
    include 'uecpout'
    include 'dsmtfefp'
    include 'uettout'
    include 'coalout'
    include 'coalrep'
    include 'indout'
    include 'indrep'
    ! include 'rscon'
    include 'resdrep'
    include 'comparm'
    include 'commrep'
    include 'tranrep' ! $ to _, param
    include 'macout'
    include 'mcdetail' ! REAL * 4 to REAL*4
    include 'intout'
    include 'emission'
    include 'emablk'
    include 'cogen'
    include 'wrenew'
    include 'convfact'
    include 'coalprc'
    ! include 'acoalprc'
    include 'eusprc'
    include 'emeblk'
    include 'uso2grp'
    include 'bldglrn'
    include 'rseff'
    include 'converge'
    include 'bifurc'
    include 'epmbank'
    include 'ghgrep'
    include 'qonroad'
    include 'ponroad'
    ! include 'aponroad'
    include 'hmmblk'
    ! include 'aeusprc'
    include 'continew' ! 1 + 13*1 + 16 + 16*13 to 238
    include 'ab32'
    include 'rggi'
    include 'csapr'
    include 'e111d'
    include 'tcs45q'

contains

subroutine init_filer

    implicit none
    INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
    CHARACTER*100 FNAMEI,FNAMEO
    CHARACTER*18 FNAMEI18,FNAMEO18, UNIQUE_NAMEIN
    LOGICAL new_bn
    INTEGER ISTATUS
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR
    CHARACTER*18 FM_NAME/' '/
    CHARACTER*11 FUNFMTC
    print*, "init_filer"

    ! get unit number
    print*, "   FILE_MGR - I"
    new_bn=.false.
    ISTATUS=FILE_MGR('I',FM_NAME,new_bn)
    print*, "       ISTATUS: ", ISTATUS

    ! optional, write file table
    ! print*, "FILE_MGR - T"
    ! new_bn=.false.
    ! ISTATUS=FILE_MGR('T',FM_NAME,new_bn)
    ! print*, "ISTATUS: ", ISTATUS

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'DICT'
    new_bn=.false.
    FUNITI=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #
    print*, "       ISTATUS: ", ISTATUS

    ! run filer on dict
    FRTYPE = 3
    FSOURC = 0
    ! FUNITI = 1
    FUNITO = 6
    FNAMEI = ' '
    FNAMEO = ' '
    FRETCD = 0
    FUNFMT = 1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "   Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "   Returning from FILER():" ! , CNADGPRD

    print*, "   FILE_MGR - C"
    UNIQUE_NAMEIN = 'DICT'
    new_bn=.false.
    FUNITI=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'RESTARTI'
    new_bn=.false.
    FUNITI=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    ! run filer to read RESTARTI
    FRTYPE = 2
    FSOURC = 1
    ! FUNITI = 1
    FUNITO = 6
    FNAMEI = ' '
    FNAMEO = ' '
    FRETCD = 0
    FUNFMT = 0

    INQUIRE(FUNITI,FORM=FUNFMTC)
    IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
    IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "   Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "   Returning from FILER():" ! , CNADGPRD

    print*, "   FILE_MGR - C"
    UNIQUE_NAMEIN = 'RESTARTI'
    new_bn=.false.
    FUNITI=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    print*, "   FILE_MGR - O"
    FNAMEI18='VARLIST'  ! input file with list of variable names to write
    new_bn=.FALSE.
    FUNITI=FILE_MGR('O',FNAMEI18,new_bn)

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.true.
    FUNITO=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    ! run filer to write new RESTARTO
    FRTYPE = 1
    FSOURC = 1
    ! FUNITI = 1
    ! FUNITO = 6
    FNAMEI = ' '
    FNAMEO = ' '
    FRETCD = 0
    FUNFMT = 0

    INQUIRE(FUNITO,FORM=FUNFMTC)
    IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
    IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "   Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "   Returning from FILER():" ! , CNADGPRD

    print*, "   FILE_MGR - C"
    FNAMEI18='VARLIST'  ! input file with list of variable names to write
    new_bn=.FALSE.
    FUNITI=FILE_MGR('C',FNAMEI18,new_bn)

    print*, "   FILE_MGR - C"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.FALSE.
    FUNITO=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

end subroutine init_filer

subroutine read_filer (RESTFILENAME)

    implicit none
    INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
    CHARACTER*100 FNAMEI,FNAMEO,RESTFILENAME
    CHARACTER*18 FNAMEI18,FNAMEO18, UNIQUE_NAMEIN
    LOGICAL new_bn
    INTEGER ISTATUS
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR
    CHARACTER*18 FM_NAME/' '/
    CHARACTER*11 FUNFMTC
    print*, "read_filer"

    ! get unit number
    print*, "   FILE_MGR - I"
    new_bn=.false.
    ISTATUS=FILE_MGR('I',FM_NAME,new_bn)
    print*, "       ISTATUS: ", ISTATUS

    ! optional, write file table
    ! print*, "FILE_MGR - T"
    ! new_bn=.false.
    ! ISTATUS=FILE_MGR('T',FM_NAME,new_bn)
    ! print*, "ISTATUS: ", ISTATUS

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'DICT'
    new_bn=.false.
    FUNITI=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #
    print*, "       ISTATUS: ", ISTATUS

    ! run filer on dict
    FRTYPE = 3
    FSOURC = 0
    ! FUNITI = 1
    FUNITO = 6
    FNAMEI = ' '
    FNAMEO = ' '
    FRETCD = 0
    FUNFMT = 1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "   Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "   Returning from FILER():" ! , CNADGPRD

    print*, "   FILE_MGR - C"
    UNIQUE_NAMEIN = 'DICT'
    new_bn=.false.
    FUNITI=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.false.
    FUNITI=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    ! run filer to read RESTARTI
    FRTYPE = 2
    FSOURC = 0
    !FUNITI = 1
    FUNITO = 6
    FNAMEI=RESTFILENAME
    FNAMEO = ' '
    FRETCD = 0
    FUNFMT = 0

    INQUIRE(FUNITI,FORM=FUNFMTC)
    IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
    IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "Returning from FILER():" ! , CNADGPRD

    print*, "FILE_MGR - C"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.false.
    FUNITI=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #
end subroutine read_filer

subroutine write_filer(RESTNAMEO)

    implicit none
    INTEGER*4 FRTYPE,FSOURC,FUNITI,FUNITO,FRETCD,FUNFMT
    CHARACTER*100 FNAMEI,FNAMEO,RESTNAMEO
    CHARACTER*18 FNAMEI18,FNAMEO18, UNIQUE_NAMEIN
    LOGICAL new_bn
    INTEGER ISTATUS
    INTEGER FILE_MGR
    EXTERNAL FILE_MGR
    CHARACTER*18 FM_NAME/' '/
    CHARACTER*11 FUNFMTC
    print*, "write_filer"

    print*, "   FILE_MGR - O"
    FNAMEI18='VARLIST'  ! input file with list of variable names to write
    new_bn=.FALSE.
    FUNITI=FILE_MGR('O',FNAMEI18,new_bn)

    print*, "   FILE_MGR - O"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.true.
    FUNITO=FILE_MGR('O',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

    ! run filer to write new RESTARTO
    FRTYPE = 1
    FSOURC = 1
    ! FUNITI = 1
    ! FUNITO = 6
    FNAMEI = ' '
    FNAMEO = RESTNAMEO
    FRETCD = 0
    FUNFMT = 0

    INQUIRE(FUNITO,FORM=FUNFMTC)
    IF(FUNFMTC.EQ.'FORMATTED') FUNFMT=0
    IF(FUNFMTC.EQ.'UNFORMATTED') FUNFMT=1

    ! run filer on dict
    print*, '   FRTYPE: ', FRTYPE
    print*, '   FSOURC: ', FSOURC
    print*, '   FUNITI: ', FUNITI
    print*, '   FUNITO: ', FUNITO
    print*, '   FNAMEI: ', FNAMEI
    print*, '   FNAMEO: ', FNAMEO
    print*, '   FRETCD: ', FRETCD
    print*, '   FUNFMT: ', FUNFMT

    PRINT*, "   Calling FILER(): " ! , CNADGPRD
    call FILER(FRTYPE,FSOURC,FUNITI,FUNITO,FNAMEI,FNAMEO,FRETCD,FUNFMT)
    PRINT*, "   Returning from FILER():" ! , CNADGPRD

    print*, "   FILE_MGR - C"
    FNAMEI18='VARLIST'  ! input file with list of variable names to write
    new_bn=.FALSE.
    FUNITI=FILE_MGR('C',FNAMEI18,new_bn)

    print*, "   FILE_MGR - C"
    UNIQUE_NAMEIN = 'RESTART'
    new_bn=.FALSE.
    FUNITO=FILE_MGR('C',UNIQUE_NAMEIN,new_bn)  ! OPEN FILE AND GET UNIT #

end subroutine write_filer

end module utils

module other
    include 'parametr'
    include 'ampblk'
    include 'angtdm'
    include 'rscon'
    include 'acoalprc'
    include 'aponroad'
    include 'aeusprc'
end module other