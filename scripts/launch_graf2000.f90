program launch_graf2000
use ifqwin
! launches a version of graf2000
! Compile and link via Intel visual fortran ".net" solution: 
!       l:/main/dsa/runit/launch_graf2000.sln
!       Build release version:  l:/main/dsa/runit/launch_graf2000.exe
!       1) copy launch_graf2000.exe to m:/graf2000
!       2) copy launch_graf2000.exe to m:/graf2000/graf2000.exe
!       3) copy launch_graf2000.exe to n:/default/scripts/launch_graf2000.exe
character*50 cmd, graf2000id*100
integer*4 result
logical lexist
graf2000id=' '
result = SETEXITQQ(QWIN$EXITNOPERSIST) ! prevents default exit prompt
inquire(file='graf2000_version.id',exist=lexist)
if (lexist) then
  open(8,file='graf2000_version.id',status='old',readonly)
endif
if(.not. lexist) then
  inquire(file='m:\graf2000\graf2000_version.id',exist=lexist)
  if(lexist) then
    open(8,file='m:\graf2000\graf2000_version.id',status='old',readonly)
  endif
endif
if(lexist) then
 read(8,*)
 read(8,'(a)') graf2000id
 cmd=trim(graf2000id)
 call doCreate(cmd)
endif

end
!**************************************************************************\
!*
!*  subroutine:  doCreate(cmd)
!*  calls WIN32 function CreateProcess() to run a windows executable command in
!   a sub-process.  It is 
!   similar to calling system(cmd) but runs process in the background and returns
!   immediately to calling program.
!*
!* this example drawn from the Visual Fortran Advanced Sample "PROCESS"
!* in \program files\microsoft visual studio\dv98\samples\process\
!*
!**************************************************************************/

subroutine doCreate(cmd)
use dfwin

integer, parameter :: SIZESTARTUPINFO = 68

character*(*) cmd  ! command line for new process

character*256               buffer  ! copy of cmd as c string

type (T_STARTUPINFO)            sui
type (T_PROCESS_INFORMATION)    pi

logical(4)          bret
integer f/0/ ! false

call ZeroMemory(LOC(buffer),256)
iret = lstrcpy(buffer,trim(cmd)//char(0))

!    /* set up the STARTUPINFO structure,
!     *  then call CreateProcess to try and start the new process 
!     */
sui%cb             = SIZESTARTUPINFO  ! 68 ( sizeof(StartUpInfo)
sui%lpReserved       = 0
sui%lpDesktop        = NULL
sui%lpTitle          = NULL
sui%dwX              = 0
sui%dwY              = 0
sui%dwXSize          = 0
sui%dwYSize          = 0
sui%dwXCountChars    = 0
sui%dwYCountChars    = 0
sui%dwFillAttribute  = 0
sui%dwFlags          = StartF_UseShowWindow  ! causes wShowWindow (next line) to be interpreted
sui%wShowWindow      = SW_SHOWNORMAL      ! causes window to be opened in normal state and activates it
sui%cbReserved2      = 0
sui%lpReserved2      = 0


bret = CreateProcess (                              &
                     NULL_CHARACTER,                &  ! exe name
                     buffer,                        &  ! or cmd line   
                     NULL_SECURITY_ATTRIBUTES,      &
                     NULL_SECURITY_ATTRIBUTES,      &
                     f,                             &  ! in cvf, ".false." worked here
                     CREATE_NEW_CONSOLE,            &
                     NULL,                          &
                     NULL_CHARACTER,                &
                     sui,                           &
                     pi)


end
