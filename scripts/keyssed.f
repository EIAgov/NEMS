      program keyssed
      use dflib
      implicit none

! This program performs the changes specified in the keys.sed file.
! It expects two arguments:  keys.sed and transaction.file (workfile)
! transaction file being the file it performs the changes to.
! It writes out scenario.datekey versions of the transaction file and
! the keys.sed

      character*93 keysfile, keys_outfile, keys(5000,2), keys_sub
      character*93 workfile, work_outfile, scedes_all
      character*20 found_key, scenario, datekey
      character*255 work(5000), keysline
      logical lexist
      integer keysunit, workunit, argnumber, i, j, qm, as
      integer number_of_keys, number_of_lines
      integer begin_qm, begin_as
      integer keysline_split, keysline_end, d_to_n

! check if files exist and open them

      keys=''
      work=''
      number_of_keys=0
      keysunit=20
      workunit=30
      argnumber=nargs()
      if (argnumber .lt. 2) then
        write (6,'(a)') ' No keys.sed file name provided to me.'
        goto 101
      endif
      call getarg(1,keysfile)
      lexist=.false.
      inquire(exist=lexist,file=keysfile)
      if (lexist) then
        open(unit=keysunit,file=keysfile,status='old')
      else
        write(6,'(a)')  ' Error:  could not find file keys.sed'
        goto 101
      endif
      if (argnumber .lt. 3) then
        write (6,'(a)') ' No file to perform changes on.  Therefore, I am done.'
        goto 101
      endif
      call getarg(2,workfile)
      lexist=.false.
      inquire(exist=lexist,file=workfile)
      if (lexist) then
        open(unit=workunit,file=workfile,status='old')
      else
        write(6,'(2a)')  ' Error:  could not find file ', trim(workfile)
        goto 101
      endif

! read in keys
! while reading, check against previously read keys for repetition

      do i=1,5000
        read(keysunit,'(a)',end=201) keysline
        keysline_end=len_trim(keysline)
        keysline_split=index(keysline,'=')     !  parse line, splitting where '=' is
        keys(number_of_keys+1,1)=keysline(:keysline_split-1)
        keys(number_of_keys+1,2)=keysline(keysline_split+1:keysline_end)
        if (len_trim(keys(number_of_keys+1,1)) .gt. 20) cycle
        if (trim(keys(number_of_keys+1,1)) .eq. 'nullstr') &
                 keys(number_of_keys+1,1)=''
        do j=1,number_of_keys
          if (keys(j,1) .eq. keys(number_of_keys+1,1)) exit
        enddo
        if (j .eq. (number_of_keys+1)) then
          number_of_keys=number_of_keys+1
          if (trim(keys(i,1)) .eq. 'SCEN') scenario=keys(i,2)(1:20)
          if (trim(keys(i,1)) .eq. 'DATE') datekey=keys(i,2)(1:10)
        endif
      enddo

201   continue
      keys_outfile=trim(keysfile) // '.' // trim(scenario) // '.' // trim(datekey)
      open (unit=50, file=keys_outfile, status='unknown')
      do i=1,number_of_keys
        write (50,'(3a)') trim(keys(i,1)),'=',trim(keys(i,2))
      enddo
      close(50)

      scedes_all= 'scedes.all.' // trim(scenario) // '.' // trim(datekey)
      open (unit=50, file=scedes_all, status='unknown')
      keys_sub=''
      do i=1,number_of_keys
        if (index(keys(i,2),':') .ne. 0 .and. ( &
             trim(keys(i,1)) .ne. 'OUTDIR'  .and.  &
             trim(keys(i,1)) .ne. 'LAUNCH'  .and.  &
             trim(keys(i,1)) .ne. 'CLEANUP' .and.  &
             trim(keys(i,1)) .ne. 'AIMMSLOC' ) ) then

          d_to_n=len_trim(keys(i,1))
          keys_sub(:d_to_n)=keys(i,1)(:(d_to_n-1)) // 'N'
        else
          keys_sub=keys(i,1)
        endif
        if ((trim(keys(i,2)) .ne. 'nullstr' .or. ( &
            trim(keys(i,1)) .eq. 'FMTOFF' .or. trim(keys(i,1)) .eq. 'FMTON' &
            .or. trim(keys(i,1)) .eq. 'PPY' .or. trim(keys(i,1)) .eq. 'PPN' &
            .or. trim(keys(i,1)) .eq. 'ETTDBGI' &
            .or. trim(keys(i,1)) .eq. 'ETTDBGC')) .and. &
            (trim(keys(i,2)) .ne. 'nulline' .or. ( &
            trim(keys(i,1)) .eq. 'FMTOFF' .or. trim(keys(i,1)) .eq. 'FMTON' &
            .or. trim(keys(i,1)) .eq. 'PPY' .or. trim(keys(i,1)) .eq. 'PPN' &
            .or. trim(keys(i,1)) .eq. 'ETTDBGI' &
            .or. trim(keys(i,1)) .eq. 'ETTDBGC')) .and. trim(keys(i,1)) .ne. 'OUTDIR') &
          write (50,'(3a)') trim(keys_sub),'=',trim(keys(i,2))
      enddo

! read in file to be changed

      work_outfile=trim(workfile) // '.' // trim(scenario) // '.' // trim(datekey)
      open (unit=51, file=work_outfile, status='unknown')
      do i=1,5000
        read(workunit,'(a255)',end=301) work(i)
! search for varkey by '?', then '@'
        begin_qm=1     !  in case there are keys left over, or ?s that do other things
        begin_as=1     !  in case there are keys left over, or ?s that do other things
        qm=index(work(i)(begin_qm:),'?')
recrd : do while (qm .ne. 0)
          as=index(work(i)(begin_as:),'@')
          as=as+begin_as-1       ! recalculate to position on entire line
          if ((as - qm) .le. 20 .and. (as - qm) .gt. 0) then
            found_key=work(i)(qm+1:as-1)  ! don't include '?' and '@' in search though keys
            do j=1,number_of_keys
              if (trim(keys(j,1)) .eq. trim(found_key)) then
                if (trim(keys(j,2)) .ne. 'nulline') then
                  if (qm .gt. 1) then
                    if (trim(keys(j,2)) .ne. 'nullstr') then
                      work(i)=work(i)(:qm-1) // trim(keys(j,2)) // work(i)(as+1:)
                    else
                      work(i)=work(i)(:qm-1) // work(i)(as+1:)
                    endif
                  else
                    if (trim(keys(j,2)) .ne. 'nullstr') then
                      work(i)=trim(keys(j,2)) // work(i)(as+1:)
                    else
                      work(i)=work(i)(as+1:)
                    endif
                  endif
                else
                  exit recrd    ! stop processing record - it is deleted
                endif
                exit
              endif
            enddo
            if (j .gt. number_of_keys) then
              write(6,'(3a)') ' Key ',found_key, ' not found in varkeys.'
              if ( (index(work(i)(qm+1:as),'?')) .eq. 0) then
                begin_qm=as + 1   !  search after end of key
                begin_as=as + 1   !  search after end of key
              else
                begin_qm=qm + 1     !  start next search after previous question mark
              endif
            endif
          elseif (as .eq. 0) then
            qm = 0        ! easiest way to write out line upon exiting loop
            exit recrd        ! no @ on line - read the next line
          elseif ((as - qm) .lt. 0) then
            write (6,'(2a)') '@ occurs before ?'    !  which shouldn't happen
            begin_as=as + 1   !  increment @ holder for next search
          else
            write (6,'(2a)') ' too long to be a run time option'
            begin_qm=qm+1     ! start search after previous question mark
          endif
          qm=index(work(i)(begin_qm:),'?')
          if (qm .ne. 0) qm=qm+begin_qm-1
        enddo recrd
        if (qm .eq. 0) write(51,'(a)') trim(work(i))
      enddo
301   continue
      number_of_lines=i

101   continue
      stop
      end
