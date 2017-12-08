program average_files
! Program to perform operations on sets of data files.
! Files have to have the same number of values in each line.
! USAGE:
! >average_files [command] < [list_of_filenames]
! command - can be sum, avg, med or dis (default: avg)
! list_of_filenames - file or pipe, containing list of files to be processed.
! EXAMPLE:
! >ls *.dat | average_files sum
!
! Author: Alexey Mints (minzastro at gmail.com)
use StringArray
use operators
use StringUtils
use quickSort
use comline


integer iUnitCount
character*(100) sFileName
character*(1000) sLine
integer istat
type(TStringArray) :: xA
real*8 aData(30, 70), aOut(30), fTmp, aTmp(70)
logical bMask(30)
integer iLineLength, iTmp
character*(20) sCommand
character*(1000) sFileMask
!if (iargc().gt.0) then
!  call GetArg(1, sCommand)
!else
!  sCommand = 'avg'
!endif

call clReadParams()
sCommand = clGetParamValue('-c', '--command', 'avg')
sFileMask = clGetParamValue('-f', '--files', '')

if (len(trim(sFileMask)).gt.0) then
    call EXECUTE_COMMAND_LINE('ls '//trim(sFileMask)//' > '//trim(get_filename()))
else
  if (iargc().gt.0) then
    open(40, file=trim(get_filename()), status='NEW')
    do i = 1, iargc() - iFirstFreeParameter + 1
        write(40, *) clGetFreeParam(i)
    enddo
    close(40)
  else
    write(*, *) 'Error, no file specified'
    stop
  endif
endif
open(40, file=trim(get_filename()), status='OLD')
! Read filenames and open files
iUnitCount = 0
do
  read(40, *, iostat=istat) sFileName
  if (istat.eq.0) then
    open(unit=50+iUnitCount, file=trim(sFileName), status="OLD")
    iUnitCount = iUnitCount + 1
  else
    exit
  endif
enddo
close(40, status='delete')

! Cycle through files
infinit_loop: do
  iLineLength = 0
  do i = 1,iUnitCount
    ! Reading line from then file
    read(49+i, '(a)', iostat=istat) sLine
    if (istat.ne.0) then
      exit infinit_loop ! One of files has ended - exiting.
    endif
    aData(:, i) = REAL_NAN
    ! Splitting line into values
    call TrimLeft(sLine, sLine)
    call replace_substring(sLine, achar(9), ' ') !replacing TABS with SPACEs
    xA = TStringArraySplitX(trim(sLine), ' ', .true.)
    call toRealArray(xA, 30, aData(:, i), iTmp)
    if (iTmp.gt.iLineLength) then
      iLineLength = iTmp
    endif
  enddo
  ! Doing operation with the current line values
  do i = 1, iLineLength
    bMask(1:iUnitCount) = .not.isnan(aData(i, 1:iUnitCount))
    select case (trim(sCommand))
      case ('min')
        aOut(i) = minval(aData(i, 1:iUnitCount), mask=bMask)
      case ('max')
        aOut(i) = maxval(aData(i, 1:iUnitCount), mask=bMask)
      case ('avg')
        aOut(i) = sum(aData(i, 1:iUnitCount), mask=bMask) / count(bMask)
      case ('avg_pos') !average positive values only
        bMask(1:iUnitCount) = bMask(1:iUnitCount).and.(aData(i, 1:iUnitCount).gt.0d0)
        aOut(i) = sum(aData(i, 1:iUnitCount), mask=bMask) / count(bMask)
      case ('sum')
        aOut(i) = sum(aData(i, 1:iUnitCount), mask=bMask)
      case ('dis')
        aOut(i) = sum(aData(i, 1:iUnitCount), mask=bMask) / count(bMask)
        fTmp = 0d0
        do j = 1, iUnitCount
          if (bMask(j)) then
            fTmp = fTmp + (aOut(i) - aData(i, j))**2
          endif
        enddo
        aOut(i) = dsqrt(fTmp / count(bMask))
      case ('diff')
        aOut(i) = aData(i, 2) - aData(i, 1)
      case ('diff_avg')
        if (bMask(1)) then
          aOut(i) = sum((aData(i, 2:iUnitCount) - aData(i, 1)), mask=bMask) / count(bMask)
        else
          aOut(i) = REAL_NAN
        endif
      case ('med', 'median')
        aTmp(1:count(bMask)) = pack(aData(i, 1:iUnitCount), bMask)
        call quick_sort(aTmp(1:count(bMask)), aTmp(1:count(bMask)))
        if (mod(count(bMask), 2).eq.1) then
          aOut(i) = aTmp(count(bMask)/2 + 1)
        else
          aOut(i) = 0.5*(aTmp(count(bMask)/2) + aTmp(count(bMask)/2 + 1))
        endif
      case default
        write(*,*) 'unknown command: '//trim(sCommand)
    end select
  enddo
  write(*,*) aOut(1:iLineLength)
enddo infinit_loop

! Closing files
do i=1, iUnitCount
  close(i-1)
enddo

contains

function get_filename() result(sResult) ! Returns the value of the parameter with key 's'
character*(100) sResult
  sResult = '___average_files___'//trim(getCharsFromInt(GETPID()))//'.files'  
end function get_filename

end program average_files
