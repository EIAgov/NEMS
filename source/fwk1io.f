! $Header: m:/default/source/RCS/fwk1io.f,v 1.7 2010/08/18 19:58:16 DSA Exp $
!****************************************************************               
!      ROUTINES TO WRITE WK1 FILES FROM FORTRAN AND MAKE                        
!      THE NECESSARY MAINFRAME TO PC CONVERSIONS.                               
!      THESE ROUTINES ARE USED BY FTAB TO WRITE A WK1 VERSION OF THE            
!      TABLES.                                                                  
!****************************************************************               
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKOPEN(IUNIT,WKFILE)                                   !        
!  WKOPEN is used to open the file and to write the necessary "WK1"             
!  header.  There are two arguments:                                            
!     IUNIT = Unit number used for input-output file                            
!     WKFILE= Character variable of size 50 containing file name                
!  This subroutine must be called first and only once for each WK1 file         
!  being created.                                                               
      IMPLICIT NONE  

      EXTERNAL CWKOPEN    !FOR RS6000
      INTEGER  CWKOPEN    !FOR RS6000
      EXTERNAL CWKWRITE   !FOR RS6000
      INTEGER  CWKWRITE   !FOR RS6000
                                                                        !  
    ! CHARACTER   WKFILE*50
      CHARACTER*(*) WKFILE
      CHARACTER*6 CODE 
      INTEGER RESULT, IUNIT,II   !FOR RS6000
      integer*2 result2,result1
      equivalence(result,result2)
      RESULT = CWKOPEN(WKFILE)   !FOR RS6000                                                    
      CODE(1:6)=CHAR(0)//CHAR(0)//CHAR(2)//CHAR(0)//CHAR(6)//CHAR(4)    !        
      DO II = 1,6                                        
         RESULT = CWKWRITE(CODE(II:II))
      ENDDO
      RETURN                                                            !        
900   WRITE(6,*) ' *** ERROR OPENING WK1 FILE *** '                     !            
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKCLOSE(IUNIT)                                         !        
!  WKCLOSE is used to close the file.  There's one argument:                    
!     IUNIT = Unit number used for input-output file                            
!  This subroutine must be called last and only once for each WK1 file          
!  being created.                                                               
                              
      EXTERNAL CWKCLOSE   !FOR RS6000
      INTEGER  CWKCLOSE   !FOR RS6000
      EXTERNAL CWKWRITE   !FOR RS6000
      INTEGER  CWKWRITE   !FOR RS6000
     
      INTEGER RESULT,II   !FOR RS6000
      CHARACTER*4 CODE                                                  !        
      CODE(1:4)=CHAR(1)//CHAR(0)//CHAR(0)//CHAR(0)                      !        
!     WRITE(UNIT=IUNIT) CODE(1:4) !FOR MAINFRAME & PC
      DO II = 1,4
         RESULT = CWKWRITE(CODE(II:II))
      ENDDO
!     CLOSE (IUNIT)  !FOR MAINFRAME & PC
      RESULT = CWKCLOSE()
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKNUM(IUNIT,IROW,ICOL,REALNO,IFORM,ILEN)               !        
!  WKNUM IS USED TO WRITE A REAL NUMBER AT SOME LOCATION.                       
!     IUNIT= AN INTEGER WHICH DESIGNATES THE UNIT NUMER USED TO OPEN            
!            THE FILE.                                                          
!     IROW = AN INTEGER THAT DESIGNATES THE TARGET ROW                          
!     ICOL = AN INTEGER THAT DESIGNATES THE TARGET COLUMN                       
!    REALNO= THE NUMBER TO BE WRITTEN                                           
!     IFORM= NUMBER OF DECIMAL PLACES                                           
!            ADD 48 IF PERCENT FORMAT IS DESIRED                                
!     ILEN = AN INTEGER THAT DESIGNATES THE LENGTH OF THE INPUT                 
!  THIS SUBROUTINE IS CALLED ONCE FOR EACH NUMBER AND CAN BE CALLED ANY         
!  NUMBER OF TIMES.                                                             
!                                                                               
!  The following is the format of the input EBCDIC REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     63     Sign (1=-, 0=+)                                                    
!    62-56   Exponent in base 16 (Z40 = 0)                                      
!    55-0    Fractional part                                                    
!                                                                               
!                                                                               
!  The following is the format of the output ASCII REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     63     Sign (1=-, 0=+)                                                    
!    62-52   Exponent in base 2 (Z400 = 0)                                      
!    51 1/2  Assumed 1 bit                                                      
!    51-0    Fractional part                                                    
                                                                        !        
      IMPLICIT NONE

      EXTERNAL CWKWRITE       !FOR RS6000                                                             
      INTEGER  CWKWRITE       !FOR RS6000
      INTEGER  RESULT, II     !FOR RS6000

      CHARACTER*9  CODE    !FOR MF & RS6000
!     CHARACTER*17 CODE    !FOR PC                                              
!     INTEGER      J       !FOR PC                                              
      CHARACTER*8  CHROUT                                               !        
      CHARACTER*8  CR8OUT                                               !        
      CHARACTER*1  COUT(8),A1(4),A1IN(8)                                !        
      REAL*8       REAL8                                                !        
      REAL*8       REALVAL,REALNO,REALNUM                               !        
      INTEGER      IROWA,IROW,ICOLA,ICOLB,IUNIT,ILEN,I,                  & !        
                   IROWB,ICOL                                           !        
      INTEGER*4    INTOUT(2),IFORM,                                      & !        
                   FORMAT,I4,IRIGHT,Z40/Z00000040/,Z400/Z00000400/      !        
      EQUIVALENCE  (REALNUM,A1IN(1))                                    !        
      EQUIVALENCE  (REAL8,CR8OUT)                                       !        
      EQUIVALENCE  (INTOUT(1),CHROUT,COUT(1))                           !        
      EQUIVALENCE  (I4,A1(1))                                           !        
!                                                                               
!     ----- CREATE AND WRITE OUT HEADER FOR A REAL VALUE -----                  
!                                                                               
      CODE(1:4)=CHAR(14)//CHAR(0)//CHAR(13)//CHAR(0)                    !        
      FORMAT=IFORM+128                                                  !        
      CODE(5:5)=CHAR(FORMAT)                                            !        
      IROWA=MOD((IROW-1),256)                                           !        
      IROWB=(IROW-1)/256                                                !        
      ICOLA=MOD((ICOL-1),256)                                           !        
      ICOLB=(ICOL-1)/256                                                !        
      CODE(6:9)=CHAR(ICOLA)//CHAR(ICOLB)//CHAR(IROWA)//CHAR(IROWB)      !        
      DO II = 1,9                       !FOR RS6000
         RESULT=CWKWRITE(CODE(II:II))   !FOR RS6000
      ENDDO                             !FOR RS6000
!     WRITE(UNIT=IUNIT) CODE(1:9)    !FOR MAINFRAME & PC                                           
      INTOUT(1) = 0                                                     !        
      INTOUT(2) = 0                                                     !        
!                                                                               
                                                                        !        
      REALNUM=REALNO                                                    ! 
!     DO I=1,8                            !FOR PC                               
!        J=I+9                            !FOR PC                               
!        CODE(J:)=A1IN(I)                 !FOR PC                               
!     ENDDO                               !FOR PC                               
!     WRITE(UNIT=IUNIT) CODE(1:17)        !FOR PC                               
!                                                                               
!     REAL8 = REALNO                      !FOR MF                               
!     CALL IEEER8(REAL8)                  !FOR MF                               
!     WRITE (UNIT=IUNIT) CR8OUT(1:8)      !FOR MF                               

!      CALL RS2PCR8(REALNUM)               !FOR RS6000 
      REAL8=REALNUM                       !FOR RS6000 
      DO II = 1,8                         !FOR RS6000
         RESULT = CWKWRITE(CR8OUT(II:II)) !FOR RS6000 
      ENDDO                               !FOR RS6000

                                                                        !        
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKSTR(IUNIT,IROW,ICOL,INLAB,IFORM,IPROT,IVIS)          !        
!  WKSTR IS USED TO WRITE A TEXT LABEL AT SOME LOCATION.  THE TEXT LABEL        
!  TO BE WRITTEN MUST BE DECLARED AS CHARACTER*80.                              
!                                                                               
!     IUNIT= AN INTEGER WHICH DESIGNATES THE UNIT NUMER USED TO OPEN            
!            THE FILE.                                                          
!     IROW = AN INTEGER THAT DESIGNATES THE TARGET ROW                          
!     ICOL = AN INTEGER THAT DESIGNATES THE TARGET COLUMN                       
!     INLAB= VARIABLE CONTAINING THE TEXT                                       
!     IFORM= AN INTEGER DESIGNATING THE ALIGNMENT                               
!            (1=LEFT, 2=CENTER AND 3=RIGHT,4=Repeat )                                     
!     IPROT= PROTECTION STATUS FOR THE CELL                                     
!            (0=UNPROTECTED, 1=PROTECTED)                                       
!     IVIS = VISIBILITY STATUS                                                  
!            (0=HIDDEN, 1=VISIBLE)                                              
!                                                                               
!  THIS SUBROUTINE IS CALLED ONCE FOR EACH LABEL AND CAN BE CALLED              
!  ANY NUMBER OF TIMES.                                                         
!
      EXTERNAL CWKWRITE,CWKWRITE2  !FOR RS6000
      INTEGER  CWKWRITE,CWKWRITE2  !FOR RS6000
      INTEGER RESULT,II  !FOR RS6000
                                                                        !       
      CHARACTER*10 CODE                                                 !        
!     CHARACTER*80 LABEL,INLAB                                                  
      CHARACTER*166 LABEL,INLAB                                         !        
!     ----- GET LENGTH OF STRING MINUS TRAILING BLANKS -----                    
      LABEL = INLAB                 !  Protect input string                     
!     CALL LENTRIM(LABEL,LLEN)   !FOR MAINFRAME & PC                                               
      CALL ALENTRIM(LABEL,LLEN)     !FOR RS000 
!     ----- CREATE AND WRITE HEADER FOR CHARACTER STRING -----                  
      CODE(1:5) = CHAR(15) // CHAR(0) // CHAR(LLEN+7) // CHAR(0)         & !        
           // CHAR(118 + 128*IPROT + 9*IVIS)                            !        
      IROWA=MOD((IROW-1),256)                                           !        
      IROWB=(IROW-1)/256                                                !        
      ICOLA=MOD((ICOL-1),256)                                           !        
      ICOLB=(ICOL-1)/256                                                !        
      CODE(6:9)=CHAR(ICOLA)//CHAR(ICOLB)//CHAR(IROWA)//CHAR(IROWB)      !        
      CODE(10:10)=CHAR(39)                                              !        
      IF (IFORM.EQ.2) CODE(10:10)=CHAR(94)    ! CENTER STRING                   
      IF (IFORM.EQ.3) CODE(10:10)=CHAR(34)    ! RIGHT JUSTIFY STRING            
      IF (IFORM.EQ.4) CODE(10:10)=CHAR(92)    ! REPEAT CHAR 

      DO II = 1,10                        !FOR RS6000
         RESULT = CWKWRITE(CODE(II:II))   !FOR RS6000
      ENDDO                               !FOR RS6000
!     WRITE(UNIT=IUNIT) CODE(1:10)           !FOR MF & PC                                   
!     ----- CONVERT EBCDIC TO ASCII ----     !FOR MF                           
!     CALL EBC2ASC (LABEL,LLEN)              !FOR MF
!     ----- WRITE ASCII STRING -----                                            
!     WRITE(UNIT=IUNIT) LABEL(1:LLEN)//CHAR(0) !FOR MF & PC                                 
      RESULT = CWKWRITE2((LABEL(1:LLEN)//'\0'))    !FOR RS6000
      RESULT = CWKWRITE(CHAR(0))                   !FOR RS6000
!     ----- CLEAR LABEL FOR REUSE -----                                         
      LABEL(1:20)='                    '                                !        
      LABEL(21:80)=LABEL(1:20)//LABEL(1:20)//LABEL(1:20)                !        
      LABEL(81:160)=LABEL(1:80)
      LABEL(161:166)=LABEL(1:6)
      RETURN                                                            !        
      END                                                               !        
                                                                        !        
                                                                        !        
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
! *** SUBROUTINE LENTRIM ***                                                    
! *** THIS SUBROUTINE GIVES THE LENGTH OF LABEL MINUS ITS ***                   
! *** TRAILING BLANKS.                                    ***                   
      SUBROUTINE LENTRIM(LABEL,LLEN)                                    !        
      CHARACTER*(*) LABEL                                               !        
      INTEGER       LOOP                                                !        
      CHARACTER*1   BLANK                                               !        
                                                                        !        
      BLANK = CHAR(64)    ! BLANK IN EBCDIC                                     
      GOTO 800                                                          !        
      ENTRY ALENTRIM(LABEL,LLEN)                                        !        
      BLANK = CHAR(32)    ! BLANK IN ASCII                                      
 800  LOOP = LEN(LABEL)                                                 !        
      LLEN = LOOP                                                       !        
!     ----- SUBTRACT FROM ENTIRE STRING LENGTH UNTIL NO BLANKS -----            
!     ----- ARE ENCOUNTERED.  START ON THE RIGHT END AND MOVE  -----            
!     ----- ALONG THE STRING TO THE LEFT.                      -----            
      DO 810, IVAL = LOOP,1,-1                                          !        
         IF (LABEL(IVAL:IVAL).EQ.BLANK(1:1)) THEN                       !        
            LLEN = LLEN - 1                                             !        
         ELSE                                                           !        
            RETURN                                                      !        
         ENDIF                                                          !        
 810  CONTINUE                                                          !        
      RETURN                                                            !        
      END                                                               !        
                                                                        !        
! ********************************************************************          
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
! *** SUBROUTINE EBC2ASC *** !FOR MF                                            
! *** this subroutine converts the EBCDIC codes to ASCII codes     ***          
! *** so that the characters can be portable to PC.  It should     ***          
! *** be used for MF only.                                         ***          
      SUBROUTINE EBC2ASC(LABEL,LLEN)                                    !        
      IMPLICIT NONE                                                     !        
      CHARACTER*(*) LABEL                                               !        
      CHARACTER*1   TEMP                                                !        
      INTEGER       I,J,MAP(0:255),LLEN                                 !        
! --- assign the equivalent decimal value corresponding to a ---                
! --- particular character ---                                                  
      DATA (MAP(I),I=0,3)/0,1,2,3/,                                      & !        
           MAP(5)/9/,                                                    & !        
           MAP(7)/127/,                                                  & !        
          (MAP(I),I=11,19)/11,12,13,14,15,16,17,18,19/,                  & !        
           MAP(22)/8/,                                                   & !        
           MAP(24)/24/,                                                  & !        
           MAP(25)/25/,                                                  & !        
          (MAP(I),I=37,39)/10,23,27/,                                    & !        
          (MAP(I),I=45,47)/5,6,7/,                                       & !        
           MAP(50)/22/,                                                  & !        
           MAP(55)/4/,                                                   & !        
           MAP(60)/20/,                                                  & !        
           MAP(61)/21/,                                                  & !        
           MAP(63)/26/,                                                  & !        
           MAP(64)/32/,                                                  & !        
          (MAP(I),I=75,80)/46,60,40,43,124,38/,                          & !        
          (MAP(I),I=90,94)/33,36,42,41,59/,                              & !        
          (MAP(I),I=96,97)/45,47/,                                       & !        
          (MAP(I),I=107,111)/44,37,95,62,63/,                            & !        
           MAP(121)/96/,                                                 & !        
          (MAP(I),I=122,127)/58,35,64,39,61,34/,                         & !        
          (MAP(I),I=129,137)/97,98,99,100,101,102,103,104,105/,          & !        
          (MAP(I),I=145,153)/106,107,108,109,110,111,112,113,114/,       & !        
          (MAP(I),I=161,169)/126,115,116,117,118,119,120,121,122/,       & !        
          (MAP(I),I=192,201)/123,65,66,67,68,69,70,71,72,73/,            & !        
          (MAP(I),I=208,217)/125,74,75,76,77,78,79,80,81,82/,            & !        
           MAP(224)/92/,                                                 & !        
          (MAP(I),I=226,233)/83,84,85,86,87,88,89,90/,                   & !        
          (MAP(I),I=240,249)/48,49,50,51,52,53,54,55,56,57/             !        
! --- assign the equivalent decimal value of '?' to undefined ---               
! --- EBCDIC characters ---                                                     
      DATA  MAP(4)/63/,                                                  & !        
            MAP(6)/63/,                                                  & !        
           (MAP(I),I=8,10)/3*63/,                                        & !        
           (MAP(I),I=20,21)/2*63/,                                       & !        
            MAP(23)/63/,                                                 & !        
           (MAP(I),I=26,33)/8*63/,                                       & !        
           (MAP(I),I=34,36)/3*63/,                                       & !        
           (MAP(I),I=40,44)/5*63/,                                       & !        
           (MAP(I),I=48,49)/2*63/,                                       & !        
           (MAP(I),I=51,53)/3*63/,                                       & !        
            MAP(54)/63/,                                                 & !        
           (MAP(I),I=56,59)/4*63/,                                       & !        
            MAP(62)/63/,                                                 & !        
           (MAP(I),I=65,74)/10*63/,                                      & !        
           (MAP(I),I=81,89)/9*63/,                                       & !        
            MAP(95)/63/,                                                 & !        
           (MAP(I),I=98,106)/9*63/,                                      & !        
           (MAP(I),I=112,120)/9*63/,MAP(128)/63/,                        & !        
           (MAP(I),I=138,144)/7*63/,                                     & !        
           (MAP(I),I=154,160)/7*63/,                                     & !        
           (MAP(I),I=170,191)/22*63/,                                    & !        
           (MAP(I),I=202,207)/6*63/,                                     & !        
           (MAP(I),I=218,223)/6*63/,                                     & !        
            MAP(225)/63/,                                                & !        
           (MAP(I),I=234,239)/6*63/,                                     & !        
           (MAP(I),I=250,255)/6*63/                                     !        
      DO J = 1,LLEN                                                     !        
         TEMP=CHAR(MAP(ICHAR(LABEL(J:J))))                              !        
         LABEL(J:J)=TEMP                                                !        
      ENDDO                                                             !        
      RETURN                                                            !        
      END                                                               !        
! ********************************************************************          
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
! *** SUBROUTINE ASC2EBC *** !FOR MF                                            
! *** this subroutine converts the EBCDIC codes to ASCII codes     ***          
! *** so that the characters can be portable to PC.  It should     ***          
! *** be used for MF only.                                         ***          
      SUBROUTINE ASC2EBC(LABEL,LLEN)                                    !        
      IMPLICIT NONE                                                     !        
      CHARACTER*(*) LABEL                                               !        
      CHARACTER*1   TEMP                                                !        
      INTEGER       I,J,MAP(0:255),LLEN                                 !        
! --- assign the equivalent decimal value corresponding to a ---                
! --- particular character ---                                                  
      DATA (MAP(I),I=0,3)/0,1,2,3/,                                      & !        
        (MAP(I),I=4,6)/55,45,46/,                                        & !        
        (MAP(I),I=7,20)/47,22,5,37,11,12,13,14,15,16,17,18,19,60/,       & !        
        (MAP(I),I=21,27)/61,50,38,24,25,63,39/,                          & !        
        (MAP(I),I=32,43)/64,90,127,123,91,108,80,125,77,93,92,78/,       & !        
        (MAP(I),I=44,54)/107,96,75,97,240,241,242,243,244,245,246/,      & !        
        (MAP(I),I=55,65)/247,248,249,122,94,76,126,110,111,124,193/,     & !        
        (MAP(I),I=66,75)/194,195,196,197,198,199,200,201,209,210/,       & !        
        (MAP(I),I=76,85)/211,212,213,214,215,216,217,226,227,228/,       & !        
        (MAP(I),I=86,90)/229,230,231,232,233/,                           & !        
         MAP(92)/224/,                                                   & !        
        (MAP(I),I=95,106)/109,121,81,82,83,84,85,86,87,88,89,145/,       & !        
        (MAP(I),I=95,104)/109,121,129,130,131,132,133,134,135,136/,      & !        
        (MAP(I),I=105,106)/137,145/,                                     & !        
        (MAP(I),I=107,115)/146,147,148,149,150,151,152,153,162/,         & !        
        (MAP(I),I=116,125)/163,164,165,166,167,168,169,192,79,208/,      & !        
         MAP(126)/161/,                                                  & !        
         MAP(127)/7/                                                    !        
! --- assign the equivalent decimal value of '?' to undefined ---               
! --- EBCDIC characters ---                                                     
      DATA (MAP(I),I=28,30)/3*111/,                                      & !        
            MAP(91)/111/,                                                & !        
           (MAP(I),I=93,94)/2*111/,                                      & !        
           (MAP(I),I=128,255)/128*111/                                  !        
      DO J = 1,LLEN                                                     !        
         TEMP=CHAR(MAP(ICHAR(LABEL(J:J))))                              !        
         LABEL(J:J)=TEMP                                                !        
      ENDDO                                                             !        
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKWIND(IUNIT,LEFCOL,TOPROW,TITCOL,TITROW,LTITCOL,       & !        
                        TTITROW)                                        !        
!  WKWIND DESCRIBES THE WINDOW RECORD.  IT IS USED TO FREEZE THE ROWS           
!  AND COLUMNS ALONG THE TOP AND LEFT EDGES OF THE WORKSHEET.  IT MUST          
!  BE CALLED IMMEDIATELY AFTER WKOPEN.  IT CAN ALSO BE USED WITH                
!  WKCOLW TO CHANGE THE COLUMN'S WIDTH.                                         
!     IUNIT= AN INTEGER WHICH DESIGNATES THE UNIT NUMER USED TO OPEN            
!            THE FILE.                                                          
!    LEFCOL= AN INTEGER DESIGNATING THE LEFT COLUMN                             
!    TOPROW= AN INTEGER DESIGNATING THE TOP ROW                                 
!    TITCOL= AN INTEGER DESIGNATING THE NUMBER OF TITLE COLUMNS                 
!    TITROW= AN INTEGER DESIGNATING THE NUMBER OF TITLE ROWS                    
!   LTITCOL= AN INTEGER DESIGNATING THE LEFT TITLE COLUMN                       
!   TTITROW= AN INTEGER DESIGNATING THE TOP TITLE ROW                           

      EXTERNAL CWKWRITE  !FOR RS6000
      INTEGER  CWKWRITE  !FOR RS6000
      INTEGER RESULT,II  !FOR RS6000

      CHARACTER*36 CODE                                                 !        
      INTEGER      IUNIT,LEFCOL,TOPROW,TITCOL,TITROW,LTITCOL,            & !        
                   TTITROW                                              !        
! ----- CREATE AND WRITE OUT RECORD HEADER FOR WINDOW -----                     
      CODE(1:4)=CHAR(7)//CHAR(0)//CHAR(32)//CHAR(0)                     !        
! ----- WRITE OUT RECORD BODY -----                                             
      CODE(5:36)=CHAR(1)//CHAR(0)//CHAR(0)//CHAR(0)//CHAR(113)//         & !        
                 CHAR(0)//CHAR(9)//CHAR(0)//CHAR(7)//CHAR(0)//           & !        
                 CHAR(20)//CHAR(0)//CHAR(LEFCOL)//CHAR(0)//              & !        
                 CHAR(TOPROW)//CHAR(0)//CHAR(TITCOL)//CHAR(0)//          & !        
                 CHAR(TITROW)//CHAR(0)//CHAR(LTITCOL)//CHAR(0)//         & !        
                 CHAR(TTITROW)//CHAR(0)//CHAR(4)//CHAR(0)//              & !        
                 CHAR(4)//CHAR(0)//CHAR(48)//CHAR(0)//                   & !        
                 CHAR(0)//CHAR(0)                                       !        
!     WRITE(UNIT=IUNIT) CODE(1:36)  !FOR MF & PC
      DO II = 1,36                         !FOR RS6000                      
         RESULT = CWKWRITE(CODE(II:II))    !FOR RS6000
      ENDDO                                !FOR RS6000
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKCOLW(IUNIT,ICOL,ILEN)                                !        
!  WKCOLW IS USED TO CHANGE A COLUMN WIDTH.  THIS SUBROUTINE IS CALLED          
!  FOR EACH COLUMN WHOSE WIDTH DOES NOT EQUAL THE DEFAULT.  WKWIND MUST         
!  BE CALLED AT LEAST ONCE BEFORE THIS SUROUTINE IS CALLED.                     
!     IUNIT= AN INTEGER WHICH DESIGNATES THE UNIT NUMER USED TO OPEN            
!            THE FILE.                                                          
!     ICOL = AN INTEGER DESIGNATING THE TARGET COLUMN                           
!     ILEN = AN INTEGER DESIGNATING THE WIDTH OF THE COLUMN                     
!                            
      EXTERNAL CWKWRITE   !C##
      INTEGER  CWKWRITE   !C##
      INTEGER RESULT, II      !C##
                                                   
      CHARACTER*7 CODE                                                  !        
!     ----- CREATE AND WRITE HEADER FOR COLUMN WIDTH -----                      
      CODE(1:4)=CHAR(8)//CHAR(0)//CHAR(3)//CHAR(0)                      !        
      ICOLA=MOD((ICOL-1),256)                                           !        
      ICOLB=(ICOL-1)/256                                                !        
      CODE(5:7)=CHAR(ICOLA)//CHAR(ICOLB)//CHAR(ILEN)                    !        
!     WRITE(UNIT=IUNIT) CODE(1:7)    !FOR MF & PC
      DO II = 1,7                        !FOR RS6000
         RESULT = CWKWRITE(CODE(II:II))  !FOR RS6000
      ENDDO                              !FOR RS6000
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE WKNAME(IUNIT,IROW1,ICOL1,IROW2,ICOL2,NAME)             !        
!  WKNAME writes NRANGE record into the WK1 file.  It acts like the             
!  /Range Name Create command in 1-2-3.                                         
!                                                                               
!  IUNIT = The unit number used to open the file.                               
!  IROW1 = upper-left corner of the row.                                        
!  ICOL1 = upper-left corner of the column.                                     
!  IROW2 = lower-right corner of the row.                                       
!  ICOL2 = lower-right corner of the column.                                    
!  NAME  = range name (16 characters)                                           
                                                                        !        
      IMPLICIT NONE          

      EXTERNAL CWKWRITE   !FOR RS6000
      INTEGER  CWKWRITE   !FOR RS6000
      INTEGER RESULT,II   !FOR RS6000
                                                   
      CHARACTER*16 CODE,NAME                                            !        
      CHARACTER*4  CTEMP                                                !        
      CHARACTER*1  NUL/Z00/                                             !        
      INTEGER      COUNT,IUNIT,IROW1,IROW2,ICOL1,ICOL2,I                !        
      EQUIVALENCE  (CTEMP,COUNT)                                        !        
                                                                        !        
      CODE(1:4)=CHAR(11)//CHAR(0)//CHAR(24)//CHAR(0)                    !        
!     WRITE(UNIT=IUNIT) CODE(1:4)  !FOR MF & PC                                              
      DO II = 1,4                        !FOR RS6000
         RESULT = CWKWRITE(CODE(II:II))  !FOR RS6000
      ENDDO                              !FOR RS6000
      COUNT=1                                                           !        
      DO WHILE (COUNT .LE. 16 .AND. NAME(COUNT:COUNT) .NE. ' ')         !        
         CODE(COUNT:COUNT)=NAME(COUNT:COUNT)                            !        
         COUNT=COUNT+1                                                  !        
      ENDDO                                                             !        
!     CALL EBC2ASC(CODE,COUNT-1)      !FOR MF                                   
      DO I=COUNT,16                                                     !        
         CODE(I:I)=NUL                                                  !        
      ENDDO                                                             !        
!     WRITE(UNIT=IUNIT) CODE  !FOR MF & PC
      DO II = 1,16                         !FOR RS6000                                    
         RESULT = CWKWRITE(CODE(II:II))    !FOR RS6000
      ENDDO                                !FOR RS6000
      COUNT=ICOL1-1                                                     !        
!     WRITE(UNIT=IUNIT) CTEMP(4:4),CTEMP(3:3)   !FOR MF & PC                                
      RESULT = CWKWRITE(CTEMP(4:4))  !FOR RS6000
      RESULT = CWKWRITE(CTEMP(3:3))  !FOR RS6000
      COUNT=IROW1-1                                                     !        
!     WRITE(UNIT=IUNIT) CTEMP(4:4),CTEMP(3:3)  !FOR MF & PC                                   
      RESULT = CWKWRITE(CTEMP(4:4))   !FOR RS6000
      RESULT = CWKWRITE(CTEMP(3:3))   !FOR RS6000
      COUNT=ICOL2-1                                                     !        
!     WRITE(UNIT=IUNIT) CTEMP(4:4),CTEMP(3:3) !FOR MF & PC
      RESULT = CWKWRITE(CTEMP(4:4))   !FOR RS6000
      RESULT = CWKWRITE(CTEMP(3:3))   !FOR RS6000                            
      COUNT=IROW2-1                                                     !        
!     WRITE(UNIT=IUNIT) CTEMP(4:4),CTEMP(3:3) !FOR MF & PC
      RESULT = CWKWRITE(CTEMP(4:4))   !FOR RS6000
      RESULT = CWKWRITE(CTEMP(3:3))   !FOR RS6000
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
      SUBROUTINE WKINT(IUNIT,IROW,ICOL,IVALUE,IFORM)                    !   
!                                                                               
!  WKINT is used to write an integer number at some location.                   
!                                                                               
!     IUNIT= An integer which designates the unit numer used to open            
!            the file.                                                          
!     IROW = An integer that designates the target row                          
!     ICOL = An integer that designates the target column                       
!    IVALUE= The number to be written                                           
!     IFORM= An integer which designates the format.  It tells the              
!            number of decimal places.  Add 48 if percent format is             
!            desired.                                                           
!                                                                               
!  This subroutine is called once for each number and can be called any         
!  number of times.                                                             
!                                                                               
      IMPLICIT NONE                                                     !        

      EXTERNAL CWKWRITE   !FOR RS6000
      INTEGER  CWKWRITE   !FOR RS6000
      INTEGER  RESULT,II  !FOR RS6000
      CHARACTER*9 CODE                                                  !        
      INTEGER IUNIT,IROW,ICOL,ILEN,IROWA,IROWB,ICOLA,ICOLB              !        
!     INTEGER*4   IVALUE    !FOR MF & PC
      INTEGER*2   IVALUE    !FOR RS6000
      INTEGER*2   INTOUT                                                !        
      INTEGER*4   IFORM,FORMAT
      CHARACTER*2  CVALUE          !FOR RS6000
      EQUIVALENCE(INTOUT,CVALUE)   !FOR RS6000                                           
!     ----- CREATE AND WRITE HEADER FOR AN INTEGER -----                        
      CODE(1:4)=CHAR(13)//CHAR(0)//CHAR(7)//CHAR(0)                     !        
      FORMAT = 128 + IFORM                                              !        
      CODE(5:5)=CHAR(FORMAT)                                            !        
      IROWA=MOD((IROW-1),256)                                           !        
      IROWB=(IROW-1)/256                                                !        
      ICOLA=MOD((ICOL-1),256)                                           !        
      ICOLB=(ICOL-1)/256                                                !        
      CODE(6:9)=CHAR(ICOLA)//CHAR(ICOLB)//CHAR(IROWA)//CHAR(IROWB)      !        
!     WRITE(UNIT=IUNIT) CODE(1:9)   !FOR MF & PC
      DO II = 1,9                         !FOR RS6000
         RESULT = CWKWRITE(CODE(II:II))   !FOR RS6000                                             
      ENDDO                               !FOR RS6000
      INTOUT = IVALUE                                                   !        
!     ----- GET INTOUT INTO IEEE FORMAT -----                                   
!     CALL IEEEINT2(INTOUT)   !FOR MF & PC
!      CALL RS2PCI2(INTOUT)      !FOR RS6000                                                  
!     ----- WRITE INTEGER IN IEEE FORMAT -----   ?????????????                               
!     WRITE(UNIT=IUNIT) INTOUT   !FOR MF & PC
      RESULT = CWKWRITE(CVALUE(1:1))   !FOR RS6000   
      RESULT = CWKWRITE(CVALUE(2:2))   !FOR RS6000   
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
      SUBROUTINE IEEEINT2(INTIEEE)                                      !        
!                                                                               
!  IEEEINT is used to convert an integer from IBM format to IEEE format         
!                                                                               
!    INTIEE = An integer to be passes in as IBM format and out as IEEE.         
!                                                                               
!                                                                               
      IMPLICIT NONE                                                     !        
      INTEGER*2   INTOUT,INTIEEE                                        !        
      CHARACTER*2 CHROUT,CTEMP                                          !        
      EQUIVALENCE (CHROUT,INTOUT)                                       !        
!     ----- INITIALIZE DATA -----                                               
      INTOUT = INTIEEE                                                  !        
      CTEMP(1:2) = CHROUT(1:2)                                          !        
!     ----- REARRANGE BYTES -----                                               
      CHROUT(1:1) = CTEMP(2:2)                                          !        
      CHROUT(2:2) = CTEMP(1:1)                                          !        
!     ----- SET VALUE TO RETURN -----                                           
      INTIEEE = INTOUT                                                  !        
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
      SUBROUTINE IEEEINT4(INTIEEE)                                      !        
!                                                                               
!  IEEEINT is used to convert an integer from IBM format to IEEE format         
!                                                                               
!    INTIEE = An integer to be passes in as IBM format and out as IEEE.         
!                                                                               
!                                                                               
      IMPLICIT NONE                                                     !        
      INTEGER*4   INTOUT,INTIEEE                                        !        
      CHARACTER*4 CHROUT,CTEMP                                          !        
      EQUIVALENCE (CHROUT,INTOUT)                                       !        
!     ----- INITIALIZE DATA -----                                               
      INTOUT = INTIEEE                                                  !        
      CTEMP(1:4) = CHROUT(1:4)                                          !        
!     ----- REARRANGE BYTES -----                                               
      CHROUT(1:1) = CTEMP(4:4)                                          !        
      CHROUT(2:2) = CTEMP(3:3)                                          !        
      CHROUT(3:3) = CTEMP(2:2)                                          !        
      CHROUT(4:4) = CTEMP(1:1)                                          !        
!     ----- SET VALUE TO RETURN -----                                           
      INTIEEE = INTOUT                                                  !        
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!----  The following subroutine applicable on MAINFRAME only                    
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE IEEER8(REALNO)                                         !        
!  IEEER8 IS USED TO CONVERT A REAL NUMBER FROM IBM MAINFRAME FORMAT            
!     TO IEEE(PC) FORMAT.                                                       
!    REALNO= REAL*8 VALUE PASSED IN IN IBM FORMAT AND RETURNED IN IEEE          
!                                                                               
!  The following is the format of the input EBCDIC REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     63     Sign (1=-, 0=+)                                                    
!    62-56   Exponent in base 16 (Z40 = 0)                                      
!    55-0    Fractional part                                                    
!                                                                               
!                                                                               
!  The following is the format of the output ASCII REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     63     Sign (1=-, 0=+)                                                    
!    62-52   Exponent in base 2 (Z400 = 0)                                      
!    51 1/2  Assumed 1 bit                                                      
!    51-0    Fractional part                                                    
                                                                        !        
      IMPLICIT NONE                                                     !        
      CHARACTER*9  CODE    !FOR MF                                              
!     CHARACTER*17 CODE    !FOR PC                                              
!     INTEGER      J       !FOR PC                                              
      INTEGER      I,J                                                  !        
      CHARACTER*8  CHROUT,CHRVAL                                        !        
      CHARACTER*1  COUT(8),A1(4),A1IN(8)                                !        
      REAL*8       REALVAL,REALNO,REALNUM                               !        
      INTEGER*4    INTOUT(2),                                            & !        
                   I4,IRIGHT,Z40/Z00000040/,Z400/Z00000400/             !        
      EQUIVALENCE  (REALNUM,A1IN(1))                                    !        
      EQUIVALENCE  (REALVAL,CHRVAL)                                     !        
      EQUIVALENCE  (INTOUT(1),CHROUT,COUT(1))                           !        
      EQUIVALENCE  (I4,A1(1))                                           !        
! -----                                                                         
      INTOUT(1) = 0                                                     !        
      INTOUT(2) = 0                                                     !        
      REALVAL = REALNO                    !FOR MF                               
!     ----- NO BITS NEED TO BE SET FOR A VALUE OF ZERO -----                    
      IF (REALVAL.EQ.0) THEN              !FOR MF                               
         RETURN                           !FOR MF                               
      ENDIF                               !FOR MF                               
!     ----- INITIALIZE REALNUM ----                                             
      REALNUM = DABS(REALVAL)             !FOR MF                               
                                                                        !        
!                                                                               
!   REAL =  (2 ** (EXPONENT - 1023)) * 1.FRACTION                               
!                                                                               
!   Fractional part first:                                                      
      INTOUT(1) = 0              !  Zero out first half of output.              
      DO I=2,8                   !  Fill the rest with                          
         COUT(I) = A1IN(I)       !  the fractional part                         
      ENDDO                      !  of the input value.                         
!                                                                               
!   Search for the first 1 bit in the fraction:                                 
      DO I = 20,23               !  Places to search for bit.                   
         IF (BTEST(INTOUT(1),I)) IRIGHT = I                             !        
      ENDDO                                                             !        
      IRIGHT = IRIGHT - 20       !  Number of bits to shift right.              
!                                                                               
!  Shift right that number of bits:                         !FOR MF             
      IF(IRIGHT .LT. 0)                                      & !FOR MF             
        WRITE(6,'('' ERROR in REAL exponent in IEEER8'')')  !FOR MF             
      IF (IRIGHT .GT. 0)  THEN                              !FOR MF             
!  shift 2nd half                                           !FOR MF             
         INTOUT(2) = ISHFT(INTOUT(2),-IRIGHT)               !FOR MF             
         DO I=1,IRIGHT                                      !FOR MF             
!  move bits from 1st half to 2nd                           !FOR MF             
            IF (BTEST(INTOUT(1),I-1))                        & !FOR MF             
            INTOUT(2) = IBSET(INTOUT(2),31+I-IRIGHT)        !FOR MF             
         ENDDO                                              !FOR MF             
!  shift 1st half                                           !FOR MF             
         INTOUT(1) = ISHFT(INTOUT(1),-IRIGHT)               !FOR MF             
      ENDIF                                                 !FOR MF             
!  zap that 1st 1 bit                                       !FOR MF             
      INTOUT(1) = IBCLR(INTOUT(1),20)                       !FOR MF             
                                                                        !        
!   Now the Exponent part:                                  !FOR MF             
!   Integer in which to calculate                           !FOR MF             
      I4 = 0                                                !FOR MF             
!   Exponent in its low-order area                          !FOR MF             
      A1(4) = A1IN(1)                                       !FOR MF             
!   Calculate it                                            !FOR MF             
      I4 = ((I4 - Z40 - 1)*4 + IRIGHT - 1) + Z400           !FOR MF             
!   Push it to the exponent position                        !FOR MF             
      I4 = ISHFT(I4,20)                                     !FOR MF             
!   OR it onto the first half                               !FOR MF             
      INTOUT(1) = IOR(INTOUT(1),I4)                         !FOR MF             
!   Sign                                                    !FOR MF             
      IF (REALVAL .LT. 0)  INTOUT(1) = IBSET(INTOUT(1),31)  !FOR MF             
!     ---- SET VALUE TO RETURN ----                         !FOR MF             
      WRITE(CHRVAL,'(8A)') (COUT(I),I=8,1,-1)               !FOR MF             
      REALNO = REALVAL                                      !FOR MF             
                                                                        !        
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!----  The following subroutine applicable on MAINFRAME only                    
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE IEEER4(REALNO)                                         !        
!  IEEER4 IS USED TO CONVERT A REAL NUMBER FROM IBM MAINFRAME FORMAT            
!     TO IEEE(PC) FORMAT.                                                       
!    REALNO= REAL*4 VALUE PASSED IN IN IBM FORMAT AND RETURNED IN IEEE          
!                                                                               
!  The following is the format of the input EBCDIC REAL*4 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-24   Exponent in base 16 (Z40 = 0)                                      
!    23-0    Fractional part                                                    
!                                                                               
!                                                                               
!  The following is the format of the output ASCII REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-23   Exponent in base 2 (Z7F = 0)                                       
!    22-0    Fractional part                                                    
                                                                        !        
      IMPLICIT NONE                                                     !        
      INTEGER      I,J                                                  !        
      CHARACTER*4  CHROUT,CHRVAL                                        !        
      CHARACTER*1  COUT(4),A1(4),A1IN(4)                                !        
      REAL*4       REALVAL,REALNO,REALNUM                               !        
      INTEGER*4    INTOUT,                                               & !        
                   I4,IRIGHT,Z40/Z00000040/,Z7F/Z0000007F/              !        
      EQUIVALENCE  (REALNUM,A1IN(1))                                    !        
      EQUIVALENCE  (REALVAL,CHRVAL)                                     !        
      EQUIVALENCE  (INTOUT,CHROUT,COUT(1))                              !        
      EQUIVALENCE  (I4,A1(1))                                           !        
!     -----                                                                     
      REALVAL = REALNO                    !FOR MF                              
      INTOUT = 0                                                        !        
!     ----- NO BITS NEED TO BE SET FOR A VALUE OF ZERO -----                    
      IF (REALVAL.EQ.0) THEN              !FOR MF                                
         RETURN                           !FOR MF                                
      ENDIF                               !FOR MF                                
!     ----- INITIALIZE REALNUM ----                                             
      REALNUM = ABS(REALVAL)              !FOR MF                                
                                                                        !        
!                                                                               
!   REAL =  (2 ** (EXPONENT - 127)) * 1.FRACTION                                
!                                                                               
!   Fractional part first:                                                      
      DO I=2,4                   !  Fill output with                            
         COUT(I) = A1IN(I)       !  the fractional part                         
      ENDDO                      !  of the input value.                         
!                                                                               
!   Search for the first 1 bit in the fraction:                                 
      DO I = 20,23               !  Places to search for bit.                   
         IF (BTEST(INTOUT,I)) IRIGHT = I                                !        
      ENDDO                                                             !        
      IRIGHT = 23 - IRIGHT       !  Number of bits to shift right.              
!                                                                               
!  Shift right that number of bits:                         !FOR MF             
      IF(IRIGHT .LT. 0)                                      & !FOR MF             
        WRITE(6,'('' ERROR in REAL exponent in IEEER4'')')  !FOR MF             
      IF (IRIGHT .GT. 0)  THEN                              !FOR MF             
         INTOUT = ISHFT(INTOUT,IRIGHT)                      !FOR MF             
      ENDIF                                                 !FOR MF             
!  zap that 1st 1 bit                                       !FOR MF             
      INTOUT = IBCLR(INTOUT,23)                             !FOR MF             
                                                                        !        
!   Now the Exponent part:                                  !FOR MF             
!   Integer in which to calculate                           !FOR MF             
      I4 = 0                                                !FOR MF             
!   Exponent in its low-order area                          !FOR MF             
      A1(4) = A1IN(1)                                       !FOR MF             
!   Calculate it                                            !FOR MF             
      I4 = (I4 - Z40)*4 - (IRIGHT + 1) + Z7F                !FOR MF             
!   Push it to the exponent position                        !FOR MF             
      I4 = ISHFT(I4,23)                                     !FOR MF             
!   OR it onto the first half                               !FOR MF             
      INTOUT = IOR(INTOUT,I4)                               !FOR MF             
!   Sign                                                    !FOR MF             
      IF (REALVAL .LT. 0)  INTOUT = IBSET(INTOUT,31)        !FOR MF             
!     ---- SET VALUE TO RETURN ----                         !FOR MF             
      WRITE(CHRVAL,'(4A)') (COUT(I),I=4,1,-1)                           !        
      REALNO = REALVAL                                      !FOR MF             
                                                                        !        
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!----  The following subroutine applicable on MAINFRAME only                    
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE IBMR4(REALNO)                                          !        
!  IEEER4 IS USED TO CONVERT A REAL NUMBER FROM IBM MAINFRAME FORMAT            
!     TO IEEE(PC) FORMAT.                                                       
!    REALNO= REAL*4 VALUE PASSED IN IN IBM FORMAT AND RETURNED IN IEEE          
!                                                                               
!  The following is the format of the input EBCDIC REAL*4 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-24   Exponent in base 16 (Z40 = 0)                                      
!    23-0    Fractional part                                                    
!                                                                               
!                                                                               
!  The following is the format of the output ASCII REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-23   Exponent in base 2 (Z7F = 0)                                       
!    22-0    Fractional part                                                    
                                                                        !        
      IMPLICIT NONE                                                     !        
      CHARACTER*4  CHRVAL                                               !        
      CHARACTER*1  CTEMP(4)                                             !        
      REAL*4       REALVAL,REALNO,REALNUM,RFRAC                         !        
      INTEGER*4    ISIGN,IEXP,INTVAL,LOOP,ITEMP                          & !        
                   ,ZEXP/Z7F800000/,                                     & !        
                   ZSIGN/Z80000000/,POWER                               !        
      LOGICAL      BITON                                                !        
      EQUIVALENCE  (REALVAL,CHRVAL,INTVAL)                              !        
      EQUIVALENCE  (ITEMP,CTEMP(1))                                     !        
!     -----                                                                     
      REALVAL= REALNO                    !FOR MF                                
      DO LOOP = 1,4                                                     !        
         CTEMP(5 - LOOP) = CHRVAL(LOOP:LOOP)                            !        
      ENDDO                                                             !        
      INTVAL = ITEMP                                                    !        
      ISIGN = IAND(ZSIGN,INTVAL)                                        !        
      IF (ISIGN.NE.0) ISIGN = 1                                         !        
      IEXP  = IAND(ZEXP, INTVAL)                                        !        
      IEXP  = ISHFT(IEXP,-23)                                           !        
      RFRAC = 0.0                                                       !        
      POWER = -23                                                       !        
      DO LOOP = 0,22                                                    !        
         BITON = BTEST(INTVAL,LOOP)                                     !        
         IF (BITON) RFRAC = RFRAC + 2. ** POWER                         !        
         POWER = POWER + 1                                              !        
      ENDDO                                                             !        
      REALNUM = (-1.)**ISIGN * 2.**(IEXP-127) * (RFRAC+1.)              !        
      REALNO = REALNUM                                                  !        
                                                                        !        
      RETURN                                                            !        
      END                                                               !        
!****************************************************************               
!----  The following subroutine applicable on MAINFRAME only                    
!      DEBUG SUBCHK                                                             
!      END DEBUG                                                                
      SUBROUTINE IBMR8(REALNO)                                          !        
!  IEEER8 IS USED TO CONVERT A REAL NUMBER FROM IBM MAINFRAME FORMAT            
!     TO IEEE(PC) FORMAT.                                                       
!    REALNO= REAL*4 VALUE PASSED IN IN IBM FORMAT AND RETURNED IN IEEE          
!                                                                               
!  The following is the format of the input EBCDIC REAL*4 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-24   Exponent in base 16 (Z40 = 0)                                      
!    23-0    Fractional part                                                    
!                                                                               
!                                                                               
!  The following is the format of the output ASCII REAL*8 number:               
!                                                                               
!     Bit       Description                                                     
!     31     Sign (1=-, 0=+)                                                    
!    30-23   Exponent in base 2 (Z7F = 0)                                       
!    22-0    Fractional part                                                    
                                                                        !        
      IMPLICIT NONE                                                     !        
      CHARACTER*8  CHRVAL                                               !        
      CHARACTER*1  CTEMP(8)                                             !        
      REAL*8       REALVAL,REALNO,REALNUM,RFRAC                         !        
      INTEGER*4    INTIN(2),ISIGN,IEXP,IFRAC,INTVAL(2),LOOP,ITEMP(2),    & !        
                   ZEXP/Z7FF00000/,                                      & !        
                   ZSIGN/Z80000000/,LOOP2,POWER                         !        
      LOGICAL      BITON                                                !        
      EQUIVALENCE  (REALVAL,CHRVAL,INTVAL(1))                           !        
      EQUIVALENCE  (ITEMP(1),CTEMP(1))                                  !        
!     -----                                                                     
      REALVAL= REALNO                    !FOR MF                                
      DO LOOP = 1,8                                                     !        
         CTEMP(9 - LOOP) = CHRVAL(LOOP:LOOP)                            !        
      ENDDO                                                             !        
      INTVAL(1) = ITEMP(1)                                              !        
      INTVAL(2) = ITEMP(2)                                              !        
      ISIGN = IAND(ZSIGN,INTVAL(1))                                     !        
      IF (ISIGN.NE.0) ISIGN = 1                                         !        
      IEXP  = IAND(ZEXP, INTVAL(1))                                     !        
      IEXP  = ISHFT(IEXP,-20)                                           !        
      RFRAC = 0.0                                                       !        
      POWER = -14                                                       !        
      DO LOOP = 0,7                                                     !        
         IFRAC = 0                                                      !        
         DO LOOP2 = 0,3                                                 !        
            BITON = BTEST(INTVAL(2),(LOOP * 4 + LOOP2))                 !        
            IF (BITON) IFRAC = IBSET(IFRAC,LOOP2)                       !        
         ENDDO                                                          !        
         POWER = POWER + 1                                              !        
         RFRAC = RFRAC + FLOAT(IFRAC) * 16. ** POWER                    !        
      ENDDO                                                             !        
      DO LOOP = 0,4                                                     !        
         IFRAC = 0                                                      !        
         DO LOOP2 = 0,3                                                 !        
            BITON = BTEST(INTVAL(1),(LOOP * 4 + LOOP2))                 !        
            IF (BITON) IFRAC = IBSET(IFRAC,LOOP2)                       !        
         ENDDO                                                          !        
         POWER = POWER + 1                                              !        
         RFRAC = RFRAC + FLOAT(IFRAC) * 16. ** POWER                    !        
      ENDDO                                                             !        
      REALNUM = (-1.)**ISIGN * 2.**(IEXP-1023) * (RFRAC+1.)             !        
      REALNO = REALNUM                                                  !        
      RETURN                                                            !        
      END                                                               !        
!*****************************                                                  
                                                                        !        
                                                                        !        
! Joe Baumgartner
! Feb. 10, 1994                                                          
! -------------------------------------------------                             
! --- subroutine to convert rs600 integer*2 to pc integer*2                     
! --- since both use IEEE flipping the bytes is all that is needed.             
      subroutine rs2pci2(intin)                                         !        
      implicit none                                                     !        
      integer*2 int2,intin                                              !        
      character*2 char2, ctemp                                          !        
      equivalence (int2,char2)                                          !        
!     ----------------------------                                              
      int2 = intin                                                      !        
      ctemp(1:1) = char2(2:2)                                           !        
      ctemp(2:2) = char2(1:1)                                           !        
      char2(1:2) = ctemp(1:2)                                           !        
      intin = int2                                                      !        
      return                                                            !        
      end                                                               !        
! -------------------------------------------------                             
! --- subroutine to convert rs600 integer*4 to pc integer*4                     
! --- since both use IEEE flipping the bytes is all that is needed.             
      subroutine rs2pci4(intin)                                         !        
      implicit none                                                     !        
      integer*4 int4,intin                                              !        
      character*4 char4, ctemp                                          !        
      equivalence (int4,char4)                                          !        
!     ----------------------------                                              
      int4 = intin                                                      !        
      ctemp(1:1) = char4(4:4)                                           !        
      ctemp(2:2) = char4(3:3)                                           !        
      ctemp(3:3) = char4(2:2)                                           !        
      ctemp(4:4) = char4(1:1)                                           !        
      char4(1:4) = ctemp(1:4)                                           !        
      intin = int4                                                      !        
      return                                                            !        
      end                                                               !        
! -------------------------------------------------                             
! --- subroutine to convert rs600 real*4 to pc real*4                           
! --- since both use IEEE flipping the bytes is all that is needed.             
      subroutine rs2pcr4(realin)                                        !        
      implicit none                                                     !        
      real*4 real4,realin                                               !        
      character*4 char4, ctemp                                          !        
      equivalence (real4,char4)                                         !        
!     ----------------------------                                              
      real4 = realin                                                    !        
      ctemp(1:1) = char4(4:4)                                           !        
      ctemp(2:2) = char4(3:3)                                           !        
      ctemp(3:3) = char4(2:2)                                           !        
      ctemp(4:4) = char4(1:1)                                           !        
      char4(1:4) = ctemp(1:4)                                           !        
      realin = real4                                                    !        
      return                                                            !        
      end                                                               !        
! -------------------------------------------------                             
! --- subroutine to convert rs600 real*8 to pc real*8                           
! --- since both use IEEE flipping the bytes is all that is needed.             
      subroutine rs2pcr8(realin)                                        !        
      implicit none                                                     !        
      real*8 real8,realin                                               !        
      character*8 char8, ctemp                                          !        
      equivalence (real8,char8)                                         !        
!     ----------------------------                                              
      real8 = realin                                                    !        
      ctemp(1:1) = char8(8:8)                                           !        
      ctemp(2:2) = char8(7:7)                                           !        
      ctemp(3:3) = char8(6:6)                                           !        
      ctemp(4:4) = char8(5:5)                                           !        
      ctemp(5:5) = char8(4:4)                                           !        
      ctemp(6:6) = char8(3:3)                                           !        
      ctemp(7:7) = char8(2:2)                                           !        
      ctemp(8:8) = char8(1:1)                                           !        
      char8(1:8) = ctemp(1:8)                                           !        
      realin = real8                                                    !        
      return                                                            !        
      end 
!================================================================================
      subroutine wckstr(unit,row,col,string,iform,iprot,ivis)
      implicit none
! ckstr is designed to perform like wkstr in fwk1io.f which writes a string to a wk1 file, but
! instead is designed to produce a csv file 
! Arguments:
!   unit   Fortran unit number of an open direct access output file
!   row    record number
!   col    column number
!   string string to output
!
!   iform, iprot, and ivis not used but carried to avoid some editing of numerous function calls

      integer unit,row,col
      character*(*) string
      integer iform,iprot,ivis ! not used. carried for compatibility with ckstr
      integer irecl,nfields,i,is,ie,ios,k,itest
      parameter (irecl=400,nfields=49)
      character*irecl line,init
      
      
!
      integer fields(2,nfields)
      integer cnt ! number of fields found on a record
      character*100 afields(nfields)
 
! create an initialization record 
      logical once/.true./
      if(once) then
         init=' '
         line=' '
         once=.false.
         do i=1,nfields-1
           init(i:i)=','
         enddo
!         init(irecl-1:)=char(13)//char(10)
      endif
!
! read any preexisting record.  if there is none yet, goto 10 to initialize
      line=' '
      read(unit,100,rec=row,iostat=ios) line
      k=len_trim(line)
      itest=ichar(line(1:1))
      if(ios.ne.0.or.k.eq.0.or.itest.eq.0) then
        line=init
      endif
      call wcsv_field(line,nfields,fields,afields,cnt) ! determine starting and end positions of fields
      if(col.le.cnt) then
        is=fields(1,col)
        ie=fields(2,col)
        if(is.lt.len(line))then
          if(is.gt.1) then
            line=line(:is-1)//'"'//trim(string)//'"'//line(ie:)
          else
            line='"'//trim(string)//'"'//line(ie:)
          endif
        endif
!        line(irecl-1:)=char(13)//char(10)
        write(unit,100,rec=row) line
      endif   

100   format(a)
      return
      end
!================================================================================
      subroutine wckint(unit,row,col,ival,iform)
      implicit none
! ckint is designed to perform like wkint in fwk1io.f which writes an integer to a wk1 file, but
! instead is designed to produce a csv file 
! Arguments:
!   unit   Fortran unit number of an open direct access output file
!   row    record number
!   col    column number
!   ival   integer*2 value to write
!
!   iform not used but carried to avoid some editing of numerous function calls

      integer unit,row,col
      integer*2 ival
      integer iform ! not used. carried for compatibility with cknum
      integer irecl,nfields,i,is,ie,ios,k,itest
      parameter (irecl=400,nfields=49)
      character*irecl line,init
      character*7 string
      
      
!
      integer fields(2,nfields)
      integer cnt ! number of fields found on a record
      character*100 afields(nfields)
 
! create an initialization record 
      logical once/.true./
      if(once) then
         once=.false.
         do i=1,nfields-1
           init(i:i)=','
         enddo
         init(irecl-1:)=char(13)//char(10)
      endif
      
      write(string,'(i7)') ival
!
! read any preexisting record.  if there is none yet, goto 10 to initialize
      read(unit,100,rec=row,iostat=ios) line
      k=len_trim(line)
      itest=ichar(line(1:1))
      if(ios.ne.0.or.k.eq.0.or.itest.eq.0) then
        line=init
      endif
      call wcsv_field(line,nfields,fields,afields,cnt) ! determine starting and end positions of fields
      if(col.le.cnt) then
        is=fields(1,col)
        ie=fields(2,col)
        if(is.lt.len(line))then
          if(is.gt.1) then
            line=line(:is-1)//trim(string)//line(ie:)
          else
            line=trim(string)//line(ie:)
          endif
        endif
        line(irecl-1:)=char(13)//char(10)
        write(unit,100,rec=row) line
      endif   
100   format(a)
      return
      end
!================================================================================
      subroutine wcknum(unit,row,col,val,ndigit,iform ) 
      implicit none
! cknum is designed to perform like wkint in fwk1io.f which writes a double precision value to a wk1 file, but
! instead is designed to produce a csv file 
! Arguments:
!   unit   Fortran unit number of an open direct access output file
!   row    record number
!   col    column number
!   val    double precision(real*8) value to write
!   ndigit number of digits to right of decimal
!   iform not used but carried to avoid some editing of numerous function calls

      integer unit,row,col,ndigit
      real*8 val
      integer iform,iprot,ivis ! not used. carried for compatibility with ckstr
      integer irecl,nfields,i,is,ie,ios,k,itest
      parameter (irecl=400,nfields=49)
      character*irecl line,init
      character*16 string
      
      
!
      integer fields(2,nfields)
      integer cnt ! number of fields found on a record
      character*100 afields(nfields)
 
! create an initialization record 
      logical once/.true./
      if(once) then
         once=.false.
         do i=1,nfields-1
           init(i:i)=','
         enddo
         init(irecl-1:)=char(13)//char(10)
      endif
      
      write(string,'(f16.<ndigit+1>)') val
!
! read any preexisting record.  if there is none yet, goto 10 to initialize
      read(unit,100,rec=row,iostat=ios) line
      k=len_trim(line)
      itest=ichar(line(1:1))
      if(ios.ne.0.or.k.eq.0.or.itest.eq.0) then
        line=init
      endif
      call wcsv_field(line,nfields,fields,afields,cnt) ! determine starting and end positions of fields
      if(col.le.cnt) then
        is=fields(1,col)
        ie=fields(2,col)
        if(is.lt.len(line))then
          if(is.gt.1) then
            line=line(:is-1)//'"'//trim(adjustl(string))//'"'//line(ie:)
          else
            line='"'//trim(adjustl(string))//'"'//line(ie:)
          endif
        endif
        line(irecl-1:)=char(13)//char(10)
        write(unit,100,rec=row) line
      endif   
 100  format(a)
      return
      end
!==========================================================================
!  This subroutine counts the comma-delimited fields on a line
! and sets the starting and ending position of each field number 

 subroutine wcsv_field(line,maxfield,fields,afields,cnt)
 implicit none

! arguments
 integer maxfield
 integer fields(2,maxfield) ! column start, end of each comma delimited field
 character*(*) line
 character*100 afields(maxfield)
 integer cnt ! number of fields

! other
  integer ipos,icomma,iquote,line_len
 
 cnt=1
 line_len=len_trim(line)
 ipos=0
 fields=0
 if(line_len.eq.0) then
   cnt=0
   return
 endif
 do while (ipos.le.(line_len) .and. cnt .le. maxfield)   

! set field count, position, and starting position of field

   ipos=ipos+1
   fields(1,cnt)=ipos

   ! if field starts with a " find next "
   if(line(ipos:ipos).eq.'"') then
      iquote=index(line(ipos+1:line_len),'"')
      if(iquote.ne.0) then
        ipos=ipos+iquote
      endif
   endif
   ! find next comma
   icomma=index(line(ipos:line_len),',')
   if(icomma.ne.0) then
     fields(2,cnt)=ipos+icomma-2
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
     cnt=cnt+1
     ipos=ipos+icomma-1

   elseif(line(line_len:line_len).eq.'"') then  ! last field, ends in a "
     fields(2,cnt)=line_len-1
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   else                                         ! last field, ends in a character is empty 
     fields(2,cnt)=line_len
     if(fields(2,cnt).lt.fields(1,cnt)) fields(2,cnt)=fields(1,cnt)
!     cnt=cnt+1
     ipos=line_len+1
   endif

 enddo
 return
 end subroutine wcsv_field
