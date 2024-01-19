! $Header: m:/default/source/RCS/diction.f,v 1.9 2018/07/10 20:14:31 pkc Exp $
!***** *****************************************************
!*                                                                
!*  ===============================================================
!*  =                                                                        
!*  =    PROGRAM MAIN_COMPARISON_DRIVER                                     
!*  =                                                                      
!*  =    This program compares the Common Block Elements found in the dict-
!*  =    ionary file CN6005.PRJ.NEMS.DICT.AEO.<datecode> to the like-named
!*  =    Common File in CN6005.PRJ.NEMS.COMMON.PDS.<datecode>. It searches
!*  =    for the existence of the common block variable in the dictionary 
!*  =    and the dictionary variable in the common block. It also checks  
!*  =    that the variables in the dictionary are listed in the same order
!*  =    as in the common block file's COMMON statement. Any discrepancies
!*  =    are written out to a file reporting the common block they occurred
!*  =    in, the type of problem, i.e. Out of Order, Not Found, and the    
!*  =    location order.                                                  
!*  ====================================================================
!*                                                                     
!*********************************************************************

      Integer*4         I, J, Blocks, Cnt, DctVar, End_File, Length

      character*68      include_dir
      Character*17      Block_Name
      Character*60      Dictionary, FILEN
      Character*165     Line165

      Logical           LEXIST

      Parameter         (Blocks=100)
      Parameter         (End_File = 10000)
      Dimension         Block_Name(Blocks), DctVar(Blocks)

      Common /BlocksNames/ Block_Name, DctVar

      OPEN(UNIT=1, FILE='diction.files', &
       status='old')
      read(1,'(a)') dictionary
      write(6,*) ' Dictionary file: ',dictionary

      include_dir='/default/includes'
      read(1,'(a)',end=88) include_dir
   88 continue
      write(6,*) ' Include directory: ',include_dir
      close(1)

      open(unit=1,file=dictionary,STATUS='UNKNOWN', ACTION='READ')

      Cnt = 0
      Do I = 1,End_File
         Read(1,1150,End=175) Line165
            call low2up(line165)
         If (Line165(1:17).EQ.'*================') Then
            Read(1,1150,End=175) Line165
            call low2up(line165)
            If ((Line165(1:1).EQ.' ').AND.(Line165(2:2).NE.' ')) Then
               Cnt = Cnt + 1
               J = 2
               DctVar(Cnt) = 2
               Do While (Line165(J:J) .NE. ' ')
                  J = J + 1
               End DO
               Block_Name(Cnt)(1:1) = Char(J-2)
               Block_Name(Cnt)(2:17) = '                '
               Block_Name(Cnt)(2:J-1) = Line165(2:J-1)

               !------------------------------------------------------!
               ! Declaring Direct Access Common Block Dictionary File !
               !------------------------------------------------------!
               FILEN=' '
               FILEN=LINE165(2:J-1)//'.blk'
               call up2low(filen)
               INQUIRE(FILE=FILEN,EXIST=LEXIST)
               IF (LEXIST) THEN
                   OPEN(2,FILE=FILEN,STATUS='OLD',RECL=165,ACCESS='DIRECT',FORM='FORMATTED')
                   CLOSE(2,STATUS='DELETE')
               END IF
               OPEN(UNIT=2,FILE=FILEN,STATUS='NEW',ACCESS='DIRECT',RECL=165, FORM='FORMATTED')
               !------------------------------------------------------!

               Write(2,1150,Rec=1) Line165
               Read(1,1150,End=125) Line165
               call low2up(line165)
               Do While(Line165(1:1) .EQ. ' ')
                  Write(2,1150,Rec=DctVar(Cnt)) Line165
                  DctVar(Cnt) = DctVar(Cnt) + 1
                  Read(1,1150,End=125) Line165
                  call low2up(line165)
               End Do
               CLOSE(UNIT=2)

            End If
         End If
      End Do
125   Continue
      CLOSE(UNIT=2)
175   Continue
      CLOSE(UNIT=1)

      DctVar = DctVar - 1
      Do I = 1,Cnt
         Write(6,1200) I, Block_Name(I)(2:17), DctVar(I)-1
         Call Comparison(I,Dictionary,include_dir)

         !-------------------------------------------------------------!
         ! Erasing Direct Access Common Block Dictionary File          !
         !-------------------------------------------------------------!
         LENGTH=ICHAR(BLOCK_NAME(I)(1:1))
         FILEN=BLOCK_NAME(I)(2:LENGTH+1)//'.blk'
         call up2low(filen)
         OPEN(2,FILE=FILEN,STATUS='OLD',RECL=165,ACCESS='DIRECT',FORM='FORMATTED')
         CLOSE(2,STATUS='DELETE')
         !-------------------------------------------------------------!

      End Do

1150  Format(A165)
1200  Format(2X,'*********************************************************'/ &
             2X,I3,') Common Block Name: ',A16,'  # of Vars: ',I3/ &
             2X,'*********************************************************'/)
      END


! ******************************************************************************
! *                                                                            *
! *  SUBROUTINE COMPARISON                                                     *
! *                                                                            *
! *  This routine is designed to compare a Common Block File from CN6005.PRJ.  *
! *  NEMS.COMMON.PDS.<datecode> to the dictionary CN6005.PRJ.NEMS.DICT.AEO.    *
! *  <datecode>. In its PC format, these files must be downloaded from the     *
! *  mainframe.                                                                *
! *                                                                            *
! *  The routine accepts the name of a target Common Block File from the Main  *
! *  module which has created a set of direct access files of all the Common   *
! *  blocks defined in the Dictionary. The routine then seeks to to find any   *
! *  discrepancies existing between those variables declared in the common     *
! *  block file and those listed in the dictionary.                            *
! *                                                                            *
! ******************************************************************************
      Subroutine Comparison(Cnt, Dictionary, include_dir)
      character*68       include_dir
      Character*2       Com_Var, Off_Set
      Character*17      Type, Var_Name, Block_Name
      Character*25      Com_Array
      Character*36      Dmension
      Character*60      Com_File, Com_File_Name*17, Dct_File, Dictionary
      Character*160      Line80

      Integer*2         ComVar, OffSet
      Integer*4         I, ComNum, CurDctVar, DctVar, End_Of_File, Flag, Length, Cnt
      Integer*4         Cols, Rows

      Logical           Present, LEXIST, EOF

      Equivalence       (Com_Var, ComVar)
      Equivalence       (Off_Set, OffSet)

      Parameter         (Cols=900, Rows=6)
      Parameter         (End_Of_File = 10000)

      Dimension         Com_Array(Cols,Rows), Present(Rows)

      Parameter         (Blocks=100)
      Dimension         Block_Name(Blocks), DctVar(Blocks)
      Common /BlocksNames/ Block_Name, DctVar

      ! --------------------------------------------- !
      ! Variable Initialization                       !
      ! --------------------------------------------- !
      ComNum = 1
      Do I = 1,Rows
         ComVar = 1
         Com_Array(1,I) = ' '
         Com_Array(1,I)(4:5) = Com_Var
      End Do

      ! --------------------------------------------- !
      ! Extract the name of the Common Block PDS from !
      ! COM_FILE_NAME. Test to see if it exists. If   !
      ! it does, OPEN the file. If not report that    !
      ! the Common Block name found in the dictionary !
      ! has no named matching counterpart in the      !
      ! COMMON PDS.                                   !
      ! --------------------------------------------- !
      COM_FILE_NAME=Block_Name(Cnt)
      LENGTH = ICHAR(COM_FILE_NAME(1:1))
! use local copy of include file if it exists.  Otherwise use the one in the common array.
      com_file=com_file_name(2:)
      call up2low(com_file)
      INQUIRE(FILE=COM_FILE, EXIST=LEXIST)
      if(.not. lexist) then
        COM_FILE = include_dir(:len_trim(include_dir))// "/" //COM_FILE_NAME(2:2+LENGTH-1)
        write(6,*) " com_file=",com_file
        call up2low(com_file)
        INQUIRE(FILE=COM_FILE, EXIST=LEXIST)
        IF(.NOT. LEXIST) GOTO 10000
      else
        write(6,*) " com_file=",com_file
      endif
      
      OPEN(UNIT=11, FILE=COM_FILE, STATUS='OLD', ACTION='READ')

      ! --------------------------------------------- !
      ! The following DO Loop searches the Common     !
      ! Block file for the occurrence of any COMMON   !
      ! statements. Upon finding one, it parse the    !
      ! statements and stores all named variables in  !
      ! an array, COM_ARRAY. The variable names in    !
      ! this array are then compared to those named   !
      ! under the Common Block in the dictionary for  !
      ! any discrepancies                             !
      ! --------------------------------------------- !
      EOF=.FALSE.
      Do I = 1, End_Of_File
         IF(EOF)GOTO 175
         Read(11,1150,End=175) Line80
         call low2up(line80)
         call replacetab(line80)
         If (Line80(1:1) .NE. '!') Then
            If (index(line80,'  COMMON').gt.0.and.index(line80,'  COMMON').lt.8) Then
              Call Common_Block_Parse(Com_Array(1,ComNum),Line80,Cols,EOF)
              Call Compare_Dict_Block(Com_Array(1,ComNum),Cols,Present(ComNum))
              ComNum = ComNum + 1
            End If
         End If
      End Do
175   Continue

      ! --------------------------------------------- !
      ! The following DO Loop generates the report    !
      ! on any discrepancies found between the con-   !
      ! tents of the dictionary and the Common Block  !
      ! PDS. Three error types are reported:          !
      !      1) Out Of Order                          !
      !      2) Not Found in Dictionary               !
      !      3) Not Found in Common Block             !
      ! --------------------------------------------- !
      Do I = 1, ComNum-1
         Com_Var = Com_Array(1,I)(4:5)
         Length = IChar(Com_Array(1,I)(1:1))
         Do J = 1,Blocks+1
            if (J .EQ. Blocks+1) then
                WRITE(6,'(2X,2A//)') 'File not found for common block ',Com_Array(1,I)(6:6+Length)
                goto 9999
            endif
            If (Com_Array(1,I)(6:6+Length) .EQ. Block_Name(J)(2:2+Length)) exit
         End Do
         CurDctVar = DctVar(J)
         If (Present(I)) Then
            Write(6,1200) Com_File,Dictionary, &
                          Com_Array(1,I)(6:6+Length),ComVar-1,CurDctVar-1
         Else
            Write(6,2000) Com_File,Dictionary, &
                          Com_Array(1,I)(6:6+Length),ComVar-1
         End If
         Flag = 0
         ! ------------------------------------------ !
         ! Report Variables Not Found in Dictionary   !
         ! ------------------------------------------ !
         Do J = 2,ComVar
            If (Com_Array(J,I)(2:3) .EQ. 'NN') Then
                If (Flag .NE. 1) Then
                   WRITE(6,1600)
                   Flag = 1
                End If
                Call Var_Characteristics(Com_Array(J,I),Type,Dmension)
                Length = IChar(Com_Array(J,I)(1:1))
                Var_Name = Com_Array(J,I)(6:6+Length)
                WRITE(6,1700) Var_Name, J-1, Type, Dmension
            EndIf
         End Do
         If (Flag .EQ. 1) WRITE(6,1800)
         If (Present(I)) Then
            ! ------------------------------------------ !
            ! Report Variables Out of Order              !
            ! ------------------------------------------ !
            Do J = 2,ComVar
               If (Com_Array(J,I)(2:3) .EQ. 'YN') Then
                  If (Flag .NE. 2) Then
                     WRITE(6,1300)
                     Flag = 2
                   End If
                   Length = IChar(Com_Array(J,I)(1:1))
                   Var_Name = Com_Array(J,I)(6:6+Length)
                   Off_Set = Com_Array(J,I)(4:5)
                   WRITE(6,1400) Var_Name, J-1, OffSet
               EndIf
            End Do
            If (Flag .EQ. 2) WRITE(6,1500)
            ! ------------------------------------------ !
            ! Report Variables Not Found in Common Block !
            ! ------------------------------------------ !
            Length = IChar(Com_Array(1,I)(1:1))
            DCT_FILE=dictionary
            DCT_FILE=COM_ARRAY(1,I)(6:6+LENGTH) // '.blk'
            write(*,*)'common=',trim(com_file),'  dctvar=',CurDctVar
            Call Report_Dct(CurDctVar,Flag,Dct_File)
         End If
         If (Flag .EQ. 0) WRITE(6,1900)
9999     continue
      End Do
      Close(Unit=11)
      ! --------------------------------------------- !
      ! End Report Writer Loop                        !
      ! --------------------------------------------- !

1100  Format(1X// ' Comparing Common Block File: ',A/ '               to Dictionary: ',A/)
1150  Format(A)

! -----------------------------------------------------------------------------
! º   Report Writer's Format Statements                                       º
! -----------------------------------------------------------------------------

1200  Format(//2X,'RESULTS OF COMMON BLOCK TO DICTIONARY COMPARISON'/ &
       2X,'------------------------------------------------'// &
       2X,'Comparison Between Common Block File: ',A/ &
       2X,'                     Dictionary File: ',A// &
       2X,'Common Block Name: ',A/ &
       2X,'Number of Variables Common Block: ',I3/ &
       2X,'                      Dictionary: ',I3//)
1250  Format(A165)
1300  Format(T3,'VARIABLES OUT OF ORDER'// &
        T22,'Common Block',T37,'Dictionary'/ &
        T3,'Variable Name',T22,'Element No.',T37,'Element No.'/ &
        T3,'================',T22,'============',T37,'============')
1400  Format(T3,A16,T31,I3,T46,I3)
1500  Format(T3,'================',T22,'============',T37,'============' &
       ///)
1600  Format(T3,'COMMON BLOCK VARIABLES NOT FOUND IN DICTIONARY'// &
        T3,'Variable Name', T21,'Element',T30,'Var. Type',T46, &
        'Dimensions'/ &
        T3,'================',T21,'=======',T30,'==============',T46, &
        '====================================')
1700  Format(T3,A16,T24,I3,T30,A14,T46,A)
1800  Format(T3,'================',T21,'=======',T30,'==============', &
            T46,'===================================='///)
1900  Format(T10,'NO DISCREPANCIES FOUND'///)
2000  Format(//T3,'************************************************************************'/ &
       T3,'WARNING - The following Common Block Name Not Found in Dictionary File'/ &
       T3,'************************************************************************'// &
       T3,'Comparison Between Common Block File: ',A/ &
       T3,'                     Dictionary File: ',A// &
       T3,'Common Block Name: ',A/ &
       T3,'Number of Variables Common Block: ',I3//)

! -----------------------------------------------------------------------------
! º   End of Report Writer's Format Statements                                º
! -----------------------------------------------------------------------------

      Return

! -----------------------------------------------------------------------------
! º   Abnormal Termination Error Messages                                     º
! -----------------------------------------------------------------------------
10000 Continue
      WRITE(6,'(2X,2A//)') 'NOT FOUND - Common Block file: ',Com_File
      Return

      END

! ******************************************************************************
! *                                                                            *
! *   SUBROUTINE COMMON_BLOCK_PARSE                                            *
! *                                                                            *
! *   This subroutine searches the user-defined Common Block file for the      *
! *   occurrence of the name COM in Columns 7:9, indicating, hopefully, the    *
! *   presence of a COMMON block declaration. Once COM is found this routine   *
! *   passes control over to the Parse_Variables routine.                      *
! *                                                                            *
! ******************************************************************************
      Subroutine Common_Block_Parse(Row_Array,Line80,Cols,EOF)

      Character*25      Row_Array
      Character*(*)     Line80

      Integer*4         I, Cols, Head, Length, Tail

      Logical           NotFound,EOF

      Dimension         Row_Array(Cols)

      NotFound = .True.

      ! ------------------------------------------------ !
      ! Find the occurrence of COM in Common Block file. !
      ! ------------------------------------------------ !
      Do I=11,28
         If (Line80(I:I) .EQ. '/') Then
            If (NotFound) Then
               Head = I + 1
               NotFound = .False.
            Else
               Tail = I - 1
            End If
         End If
      End Do

      ! --------------------------------------------------- !
      ! Store the Common Block name and its length and mark !
      ! as not been found, though not yet looked for.       !
      ! --------------------------------------------------- !
      Length = Tail - Head
      Row_Array(1)(6:6+Length) = Line80(Head:Tail)
      Row_Array(1)(1:1) = Char(Length)
      Row_Array(1)(2:3) = 'NN'

      ! --------------------------------------------- !
      ! Parse the current line of the Common Block    !
      ! file for all variable names.                  !
      ! --------------------------------------------- !
      Tail = Tail + 2
      Call Parse_Variables(Tail,Line80,Row_Array,Cols)

      RETURN
175   Continue
      EOF=.TRUE.

      Return

1000  Format(1X,'Common Block Found!!   Name: ',A/)
1050  Format(A)

      END

! ******************************************************************************
! *                                                                            *
! *   SUBROUTINE PARSE_VARIABLES                                               *
! *                                                                            *
! *   This routine extracts the individual variable names declared in the      *
! *   COMMON statement.                                                        *
! *                                                                            *
! ******************************************************************************
      Subroutine Parse_Variables(Tail,Line80,Row_Array,Cols)

      Character*2       Com_Var
      Character*25      Row_Array
      Character*(*)     Line80
      character*16      vname
      Integer*2         ComVar
      Integer*4         I, Blank, Cols, Head, Length, Tail, EndOfLine

      Logical           NotFound, Continuation

      Equivalence       (Com_Var, ComVar)
      Dimension         Row_Array(Cols)

      Blank = 0
      NotFound = .True.
      Continuation = .True.
      Com_Var = Row_Array(1)(4:5)
      EndOfLine=160

      ! --------------------------------------------- !
      ! Start parsing the COMMON statement, using as  !
      ! delimiters commas and blank spaces.           !
      ! --------------------------------------------- !
      Do While (Continuation)
        Do I = Tail,len(line80)-6
           Continuation = .False.
           If (Line80(I:I) .NE. ' ') Then
              Blank = 0
              If (NotFound) Then
                 If (Line80(I:I) .NE. ',' .and. Line80(I:I) .NE. '&') Then
                    Head = I
                    NotFound = .False.
                 Else If (Line80(I:I) .EQ. '&') Then
    ! We have a continuation line, and are at the end of this one
                    Continuation = .True.
                    EndOfLine = I
                    Exit
                 End If
              Else If (Line80(I:I) .EQ. ',') Then
               ! --------------------------------------------- !
               ! Store the length of the variable name, the    !
               ! variable name itself, and indicate that it    !
               ! has not been found nor is it in order.        !
               ! --------------------------------------------- !
                 Length = I - Head
                 ComVar = ComVar + 1
                 vname=Line80(Head:I-1)
                 LENGTH=len_trim(vname)
                 Row_Array(ComVar)(6:6+Length) = trim(vname)//' '
                 Row_Array(ComVar)(1:1) = Char(Length)
                 Row_Array(ComVar)(2:3) = 'NN'
                 NotFound = .True.
              End If
           Else If (.NOT. NotFound) Then
              Blank = Blank + 1
           End If
        End Do

      ! --------------------------------------------- !
      ! LAST VARIABLE IN COMMON BLOCK Line            !
      ! --------------------------------------------- !
      ! Store the length of the variable name, the    !
      ! variable name itself, and indicate that it    !
      ! has not been found nor is it in order.        !
      ! --------------------------------------------- !
        If (.Not. NotFound .and. .Not. Continuation) Then
           Length = I - Blank - Head
           ComVar = ComVar + 1
           vname=Line80(Head:Head+Length)
           length=len_trim(vname)
           Row_Array(ComVar)(6:6+Length) = trim(vname)//' '
           Row_Array(ComVar)(1:1) = Char(Length)
           Row_Array(ComVar)(2:3) = 'NN'
           NotFound = .True.
        End If

      ! ----------------------------------------------------- !
      ! Save the number of the variables in the common block. !
      ! ----------------------------------------------------- !
        Row_Array(1)(4:5) = Com_Var

        Read(11,'(A)',End=175) Line80
        call low2up(line80)
        call replacetab(line80)
        Tail=1
      End Do

175   Continue
      Return

      END

! ******************************************************************************
! *                                                                            *
! *  SUBROUTINE COMP_DICT_BLOCK                                                *
! *                                                                            *
! *  This routine compares the variables found in the common block statement   *
! *  to those present in the Dictionary.                                       *
! *                                                                            *
! ******************************************************************************
      Subroutine Compare_Dict_Block(Row_Array,Cols,Present)

      Character*2       Com_Var, Off_Set
      Character*17      Block_Name
      Character*25      Row_Array
      Character*60      DCT_FILE
      Character*165     Line165

      Integer*2         ComVar, OffSet
      Integer*4         I, J, Cols, DctVar, Length
      Integer*4         End_of_File

      Logical           NotFound, Present, LEXIST

      Dimension         Row_Array(Cols)
      Equivalence       (Com_Var, ComVar)
      Equivalence       (Off_Set, OffSet)

      Parameter         (End_Of_File = 10000)
      Parameter         (Blocks=100)
      Dimension         Block_Name(Blocks), DctVar(Blocks)
      Common /BlocksNames/ Block_Name, DctVar

      Present = .True.
      Com_Var = Row_Array(1)(4:5)

      ! --------------------------------------------- !
      ! Extract the name of the Common Block      !
      ! Test to see if a direct-access dictionary     !
      ! file of the same name exists. If it does,     !
      ! OPEN the file. If not report that the Common  !
      ! Block name was found in the Common PDS file   !
      ! but was not declared in the dictionary.       !
      ! --------------------------------------------- !
      Length = IChar(Row_Array(1)(1:1))
      Do J = 1,Blocks+1
         If (J .EQ. Blocks+1) Goto 10000
         If (Row_Array(1)(6:6+Length) .EQ. Block_Name(J)(2:2+Length)) exit
      End Do
      CurDctVar = DctVar(J)
      LENGTH = ICHAR(ROW_ARRAY(1)(1:1))
      DCT_FILE=ROW_ARRAY(1)(6:6+LENGTH)//'.blk'
      call up2low(dct_file)
      INQUIRE(FILE=DCT_FILE,EXIST=LEXIST)
      IF (.NOT. LEXIST) GOTO 10000
      OPEN(UNIT=12,FILE=DCT_FILE,STATUS='OLD',ACCESS='DIRECT',RECL=165,FORM='FORMATTED')

      ! --------------------------------------------- !
      ! Start checking Common Block Variables to see  !
      ! if they are present in the Dictionary.        !
      ! --------------------------------------------- !
      Do I = 2, ComVar
         J = 2
         NotFound = .True.
         Length = IChar(Row_Array(I)(1:1))
         Read(12,1000,Rec=I,err=777) Line165
         call low2up(line165)
         ! --------------------------------------------- !
         ! Check for Presence & Order                    !
         ! --------------------------------------------- !
         If (Line165(19:19+Length).EQ.Row_Array(I)(6:6+Length)) Then
            Row_Array(I) (2:3) = 'YY'
            Line165(1:1) = '+'
            Write(12,1000,Rec=I) Line165
         Else
            ! --------------------------------------------- !
            ! Check for Presence                            !
            ! --------------------------------------------- !
            Do While (NotFound .And. (J .LE. CurDctVar))
               Read(12,1000,Rec=J,err=888) line165
               call low2up(line165)
               If (Line165(19:19+Length).EQ.Row_Array(I)(6:6+Length)) Then
                  Row_Array(I) (2:3) = 'YN'
                  OffSet = J - 1
                  Row_Array(I) (4:5) = Off_Set
                  Line165(1:1) = '+'
                  Write(12,1000,Rec=J) Line165
                  NotFound = .False.
               End If
888            continue
                J = J + 1

            End Do
         End If
777      continue
      End Do

      CLOSE(UNIT=12)

1000  Format(A165)

      Return

10000 Continue
      Present = .False.
      Return

      END

! ******************************************************************************
! *                                                                            *
! *  SUBROUTINE REPORT_DCT                                                     *
! *                                                                            *
! *  This subroutine reports on those variables present in the dictionary, but *
! *  not present in the common block. It lists the name of the variable, its   *
! *  position in the common block in the dictionary, its type, and its dimen-  *
! *  sions if any present.                                                     *
! *                                                                            *
! ******************************************************************************
      Subroutine Report_Dct(DctVar,Flag,DCT_FILE)

      Character         Chr
      Character*25      Var_Name, Type
      Character*36      Dmension
      Character*60      DCT_FILE
      Character*165     Line165

      Integer*4         I, J, DctVar, Flag
      call up2low(dct_file)
      OPEN(UNIT=12,FILE=DCT_FILE,STATUS='OLD',ACCESS='DIRECT',RECL=165,FORM='FORMATTED')

      ! --------------------------------------------------------------------- !
      ! Scanning for Variables in the Dictionary but not in the Common Block. !
      ! --------------------------------------------------------------------- !
      Do I = 2,DctVar
         Type = ' '
         Dmension = ' '
         Read(12,1050,Rec=I) Line165
         call low2up(line165)
         If (Line165(1:1) .EQ. ' ') Then
            If (Flag .NE. 3) Then
                WRITE(6,1000)
                Flag = 3
            End If
            ! --------------------------------------------- !
            ! Determine variable type.                      !
            ! --------------------------------------------- !
            Chr = Line165(36:36)
            IF    (CHR.EQ.'R') THEN
                  Type = 'Real*'//Line165(41:41)
            ELSEIF(CHR.EQ.'I') THEN
                  Type = 'Integer*'//Line165(41:41)
            ELSEIF(CHR.EQ.'C') THEN
                  J=38
                  Do While ((Line165(J:J).NE.' ').AND.(J.LE.1))
                     J = J + 1
                  End Do
                  Type = 'Character*'//Line165(J:41)
            ELSE
                  Type = '?? '//Chr//' ??'
            End If
            Var_Name = Line165(19:34)
            J = IChar(Line165(43:43)) - IChar('0')
            ! --------------------------------------------- !
            ! Check for variable dimensions.                !
            ! --------------------------------------------- !
            If (J .GT. 5) Then
               Dmension = 'Dimension Size > 5'
            ElseIf (J .EQ. 0) Then
               Dmension = 'No Dimensions'
            Else
               Dmension(1:1+J*7) = Line165(45:45+J*7)
            End If
            WRITE(6,1100) Var_Name, I-1, Type, Dmension
         EndIf
      End Do
      ! --------------------------------------------- !
      ! End of Scanning DO Loop.                      !
      ! --------------------------------------------- !

      CLOSE(UNIT=12)
      If (Flag .EQ. 3) WRITE(6,1200)

1000  Format(T3,'VARIABLE NAMES FOUND IN THE DICTIONARY BUT NOT IN THE' &
        ,' COMMON BLOCK'// &
        T3,'Variable Name', T21,'Element',T30,'Var. Type',T46, &
        'Dimensions'/ &
        T3,'================',T21,'=======',T30,'==============',T46, &
        '====================================')
1050  Format(A165)
1100  Format(T3,A16,T24,I3,T30,A14,T46,A)
1200  Format(T3,'================',T21,'=======',T30,'==============', &
            T46,'===================================='////)

      Return

      END

! ******************************************************************************
! *                                                                            *
! *  SUBROUTINE VAR_CHARACTERISTICS                                            *
! *                                                                            *
! *  This subroutine attempts to find the characteristics of a named variable  *
! *  in the Common Block File. It will report variable type, i.e, Integer,     *
! *  Real, or Character, and dimension size.                                   *
! *                                                                            *
! ******************************************************************************
      Subroutine Var_Characteristics(Var_Name, Type, Dmension)

      Character*16      Type
      Character*25      Var_Name
      Character*36      Dmension
      Character*160     Line80

      Integer*4         I, J, Length,l80

      Logical           NotFound

      ! --------------------------------------------- !
      ! Rewind Common Block File to start of file and !
      ! initialize variables.                         !
      ! --------------------------------------------- !
      Rewind(Unit=11)
      NotFound = .True.
      Type = '                '
      Dmension = '                                    '
      Length = IChar(Var_Name(1:1))

      ! --------------------------------------------- !
      ! Start scanning Common Block file for variable !
      ! declaration.                                  !
      ! --------------------------------------------- !
      Read(11,1000) Line80
      call low2up(line80)
      call replacetab(line80)
      l80=len(line80)
      Do While (NotFound)
         If ((Line80(1:1) .EQ. ' ')) Then
            I = 7
            Type = '                '
            Do While ((Line80(I:I) .EQ. ' ') .AND. (I .LT. l80-length))
               I = I + 1
            End DO
            Do While ((Line80(I:I) .NE. ' ') .AND. (I .LT. l80-length))
               I = I + 1
            End DO
            Type = Line80(7:I-1)
            Do While ((Line80(I:I) .EQ. ' ') .AND. (I .LT. l80-length))
               I = I + 1
            End DO
            ! --------------------------------------------- !
            ! Having isolated a variable name, test to see  !
            ! if it is the variable the routine is looking  !
            ! for. If so save its type and dimensions.      !
            ! --------------------------------------------- !
!           WRITE(6,'(1X,3(A,2X))') Var_Name(6:6+Length-1), Type,
!    *            Line80(I:I+Length-1)
            If ((Var_Name(6:6+Length-1).EQ.Line80(I:I+Length-1)).AND. &
               ((Line80(I+Length:I+Length).EQ.' ').OR. &
                (Line80(I+Length:I+Length).EQ.'('))) Then
               NotFound = .False.
               I = I + Length
               J = I
               Do While((Line80(J:J).NE.' ').AND.(J .LT. l80-5))
                  J = J + 1
               End DO
               If (J .NE. I) Then
                  Dmension = Line80(I:J-1)
               Else
                  Dmension = 'Not an Array Variable!!'
               End If
            End If
         End If
         If (NotFound) then
            Read(11,1000,End=10000) Line80
            call low2up(line80)
            call replacetab(line80)
         endif
      End Do
      ! --------------------------------------------- !
      ! End Scanning DO Loop                          !
      ! --------------------------------------------- !

1000  Format(A)

      Return

10000 Continue
!      Write(6,'(2X,2A/)') 'Error--Variable name not found: ',
!     *                      Var_Name(6:6+Length)
!
      Return

      END

      SUBROUTINE up2low(A)
!     CONVERTS STRING A TO lower CASE
      CHARACTER*(*) A
      integer i, j, n
      n=len_trim(a)
      DO 10 I=1,N
        J=ICHAR(A(I:I))
        IF(J.GE.65.AND.J.LE.90) A(I:I)=CHAR(J+32)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE LOW2UP(A)
!     CONVERTS STRING A TO UPPER CASE
      CHARACTER*(*) A
      integer i, j, n
      n=len_trim(a)
      DO 10 I=1,N
        J=ICHAR(A(I:I))
        IF(J.GT.96.AND.J.LT.123) A(I:I)=CHAR(J-32)
   10 CONTINUE
      RETURN
      END

      SUBROUTINE REPLACETAB(A)
!     converts a tab to a space in a string
      CHARACTER*(*) A
      integer i, j, n
      n=len_trim(a)
      DO 10 I=1,N
        J=ICHAR(A(I:I))
        IF(J.EQ. 9) A(I:I)=CHAR(32)
   10 CONTINUE
      RETURN
      END
