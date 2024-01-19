! program to read nems runlog and compile a list of runtimes from available runs
  use dfport
 implicit none
 character*127 logfile,nohup
 character*40 line,lin*127 ,head*100,starttime*8
 character*3 user
 logical lexist
 integer first_blank,nline,page,is,il,i,L
 parameter(page=50)
 integer hrs,mins,secs,cpusecs,wallsecs,part
 real cpuwall
 logical match_wild
 external match_wild
 character*3 parts(0:3)/' ','/p1','/p2','/p3'/
 character userdate*16
 character*8 datecode,scenario*16,runnumb*5,cpu*10,wall*10,maxitr*2,host*7,outpc*7,pcid*7,datec*6
 character*80 cmd,home*30,nemsjoblog*30
 logical wildcard
 integer iret
 
 call getenv("HOME",home)
 call getenv("NEMSJOBLOG",nemsjoblog)
 if(nemsjoblog.eq.' ') then
   nemsjoblog='w:/NEMSJobLog'
 endif
 do i=1,len_trim(nemsjoblog)
   if(nemsjoblog(i:i).eq.'/') then
     nemsjoblog(i:i)='\'
   endif
   
 enddo
 logfile=trim(nemsjoblog)//'\runlog'
 if(len_trim(home).gt.0) then
   cmd='copy '//trim(logfile)//' '//trim(home)//'\runlog'
   iret=system(cmd)
   if(iret.eq.0) then
     logfile=trim(home)//'\runlog'
   endif
 endif     

 head='Scenario,        Datecode, User,    Host, Maxitr, Runnumb,  CPU Time,  Wall Time,  CPU/Wall, Start Time'
 write(0,'(a\)') 'Enter date to process (mmddyy), or a 16-character-max wildcard pattern with ? and * that matches the entire run log entry:'
 read(5,'(a)') userdate
 open(8,file=logfile,status='old',readonly)

 if(index(userdate,'?').gt.0 .or. index(userdate,'*').gt.0) then
   wildcard=.true.
 else
   wildcard=.false.
 endif

 nline=0

 do while (.not. eof(8))
   read(8,'(a)',end=99) lin

  ! is=index(lin,'/',BACK=.true.)
  ! datec=lin(is+2:len_trim(lin)-1)
   
   if(index(lin,'Deleted').eq.0.and.len_trim(lin).gt.5.and. match_wild(trim(userdate),trim(lin),wildcard)) then
    ! if(wildcard) then
    !   write(6,*) 'pattern '//trim(userdate)//' found in '//trim(lin)
    ! endif
     first_blank=0
     do i=1,len_trim(lin)
       first_blank=i
       if(lin(i:i).eq.' ') exit
     enddo
     if(first_blank.eq.0) then
       cycle 
     endif

     do part=0,0  ! 3
       nohup=lin(first_blank:len_trim(lin))//trim(parts(part))//'/nohup.out'
       inquire(exist=lexist,file=nohup)
      
       if(.not. lexist .and. part.gt.0) exit
      
       if(lexist.and.index(lin,'//').gt.0) then
         user=lin(1:first_blank-1)
         is=index(lin,'//')+2 
         il=index(lin(is:),'/')-2
         outpc=lin(is:is+il)
         host=outpc ! will be replaced normally if "workdir PC" is found from runit.exe.
         datecode=' '
         scenario=' '
  ! get scenario and datecode from folder name
         is=index(lin,'/',back=.true.)
         if(is.gt.0) then
           datecode=lin(is+1:)
           lin(is:)=' '
           is=index(lin,'/',back=.true.)        
           if(is.gt.0) then  
             scenario=lin(is+1:)   
           endif
         endif
! read the nohup.out
         open(9,file=nohup,status='old',readonly,err=69)

         runnumb=' '
         maxitr=' '
         cpu=' '
         wall=' '
         starttime=' '
         pcid=' '

         do while (.not. eof(9))
           read(9,'(a)',err=79) line
           if(    index(line,'datecode=').gt.0) then
             datecode=line(12:)
           elseif(index(line,'Datecode=').gt.0) then
             datecode=line(11:)
           elseif(index(line,'scenario=').gt.0) then
             scenario=line(12:)
           elseif(index(line,'Scenario=').gt.0) then
             scenario=line(11:)
           elseif(index(line,'workdir PC=').gt.0) then
             host=line(14:)
           elseif(index(line,'PCID         =').gt.0) then
             pcid=line(17:)
           elseif(index(line,'run #').gt.0.and.index(line,'cycle').eq.0.and.index(line,'GPA').eq.0) then
             runnumb=line(17:)
           elseif(index(line,'NEMS Run ').gt.0) then ! .and.index(line,'part only').eq.0) then
	           runnumb=line(10:11)
           elseif(index(line,'StartTime').gt.0) then
             il=index(line,'=')
             starttime=line(il+2:)
           elseif(index(line,'Start time').gt.0) then
             il=index(line,': ')
             starttime=line(il+2:)
           elseif(index(line,'Host for Job ').gt.0) then
             il=index(line,'=')
             host=line(il+2:)
           elseif(index(line,'## MAXITR  =      ').gt.0) then
             maxitr=line(19:20)
           elseif(index(line,'CPU (hr:min:sec) =').gt.0) then
             cpu=line(31:38)
             cpusecs=0
             hrs=0
             mins=0
             secs=0
             read(cpu(1:2),'(i2)',err=5) hrs
             read(cpu(4:5),'(i2)',err=5) mins
             read(cpu(7:8),'(i2)',err=5) secs
5            cpusecs=secs+mins*60+hrs*3600
             
           elseif(index(line,'Wall (hr:min:sec)').gt.0) then
            
             line=line(30:)
             if(line(1:2).eq.' =') line=line(3:)
             if(line(1:3).eq.') =') line=line(4:)
             if(line(1:1).eq.'=') line=line(2:)
             wall=line(1:8)
             wallsecs=0
             read(wall(1:2),'(i2)',err=10) hrs
             read(wall(4:5),'(i2)',err=10) mins
             read(wall(7:8),'(i2)',err=10) secs
             wallsecs=secs+mins*60+hrs*3600
      
10           cpuwall=1.0
             if(wallsecs.ne.0) &
             cpuwall=real(cpusecs)/real(wallsecs)

!            if(nline.gt.page) nline=0
             if(nline.eq.0) write(6,'(a)') trim(head)
             il=len_trim(host)
             if(host.eq.'       ') then
               host=pcid
             endif
             write(6,'(a,2a,1x,2a,3x,2a,1x,2a,4x,2a,5x,2a,1x,2a,a,F6.4,10a)') &
             scenario,',',datecode,',',user,',',host,',',maxitr,',',runnumb,',',cpu,',',wall,',', &
             cpuwall, & ! f6.3
             ',','  '//starttime

             nline=nline+1


             runnumb=' '


             wall=' '





           endif
         enddo
79       continue
       endif
     enddo
   endif
69 continue
 enddo
99 continue
 end
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 
!    02110-1301  USA
!

LOGICAL FUNCTION match_wild(pattern, string, wildcard)
   ! compare given string for match to pattern which may
   ! contain wildcard characters:
   ! "?" matching any one character, and
   ! "*" matching any zero or more characters.
   ! Both strings may have trailing spaces which are ignored.
   ! Authors: Clive Page, userid: cgp  domain: le.ac.uk, 2003 (original code)
   !          Rolf Sander, 2005 (bug fixes and pattern preprocessing)
   ! Minor bug fixed by Clive Page, 2005 Nov 29, bad comment fixed 2005 Dec 2.

!    This program is free software; you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation; either version 2 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program; if not, write to the Free Software
!    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 
!    02110-1301  USA
!
   IMPLICIT NONE

   CHARACTER(LEN=*), INTENT(IN) :: pattern ! pattern may contain * and ?
   CHARACTER(LEN=*), INTENT(IN) :: string  ! string to be compared
   logical , intent(in) :: wildcard !  if pattern contains no wild card, just look for string
   INTEGER :: lenp, lenp2, lens, n, p2, p, s
   INTEGER :: n_question, n_asterisk
   

   CHARACTER(LEN=LEN(pattern)) :: pattern2

   lens = LEN_TRIM(string)
   lenp = LEN_TRIM(pattern)

! If the pattern is empty, always return true
   IF (lenp == 0) THEN
     match_wild = .TRUE.
     RETURN
   ENDIF
   if(.not. wildcard) then
     if(index(string,trim(pattern)).gt.0) then
       match_wild=.true.
     else
       match_wild=.false.
     endif
     return
   endif
! The pattern must be preprocessed. All consecutive occurences of
! one or more question marks ('?') and asterisks ('*') are sorted and
! compressed. The result is stored in pattern2.

   pattern2(:)=''
   p  = 1 ! current position in pattern
   p2 = 1 ! current position in pattern2
   DO
     IF ((pattern(p:p) == '?').OR.(pattern(p:p) == '*')) THEN
! a special character was found in the pattern
       n_question = 0
       n_asterisk = 0
       DO WHILE (p <= lenp)
         ! count the consecutive question marks and asterisks
         IF ((pattern(p:p) /= '?').AND.(pattern(p:p) /= '*')) EXIT
         IF (pattern(p:p) == '?') n_question = n_question + 1
         IF (pattern(p:p) == '*') n_asterisk = n_asterisk + 1
         p = p + 1
       ENDDO
       IF (n_question>0) THEN ! first, all the question marks
         pattern2(p2:p2+n_question-1) = REPEAT('?',n_question)
         p2 = p2 + n_question
       ENDIF
       IF (n_asterisk>0) THEN ! next, the asterisk (only one!)
         pattern2(p2:p2) = '*'
         p2 = p2 + 1
       ENDIF
     ELSE
! just a normal character
       pattern2(p2:p2) = pattern(p:p)
       p2 = p2 + 1
       p = p + 1
     ENDIF
     IF (p > lenp) EXIT
   ENDDO
!!   lenp2 = p2 - 1
   lenp2 = len_trim(pattern2)

! The modified wildcard in pattern2 is compared to the string:

   p2 = 1
   s = 1
   match_wild = .FALSE.
   DO
     IF (pattern2(p2:p2) == '?') THEN
! accept any char in string
       p2 = p2 + 1
       s = s + 1
     ELSEIF (pattern2(p2:p2) == "*") THEN
       p2 = p2 + 1
       IF (p2 > lenp2) THEN
! anything goes in rest of string
         match_wild = .TRUE.
         EXIT ! .TRUE.
       ELSE
! search string for char at p2
         n = INDEX(string(s:), pattern2(p2:p2))
         IF (n == 0) EXIT  ! .FALSE.
         s = n + s - 1
       ENDIF
     ELSEIF (pattern2(p2:p2) == string(s:s)) THEN
! single char match
       p2 = p2 + 1
       s = s + 1
     ELSE
       ! non-match
       EXIT ! .FALSE.
     ENDIF
     IF (p2 > lenp2 .AND. s > lens) THEN
! end of both pattern2 and string
       match_wild = .TRUE.
       EXIT ! .TRUE.
     ENDIF

!!     IF (s > lens .AND. (pattern2(p2:p2) == "*") .AND. p2 == lenp2) THEN
!! above line buggy since p2 can be beyond end of string pattern2 by this point. CGP

       IF (s > lens .AND. p2 == lenp) THEN
         IF(pattern2(p2:p2) == "*") THEN
! "*" at end of pattern2 represents an empty string
           match_wild = .TRUE.
           EXIT
         END IF
     ENDIF
     IF (p2 > lenp2 .OR. s > lens) THEN
! end of either pattern2 or string
       EXIT ! .FALSE.
     ENDIF
   ENDDO

END FUNCTION match_wild
