program average_files
! Program to perform operations on sets of data files.
! Files have to have the same number of values in each line.
! USAGE:
! >average_files [command] < [list_of_filenames]
! command - can be sum, avg or dis (default: avg)
! list_of_filenames - file or pipe, containing list of files to be processed.
! EXAMPLE:
! >ls *.dat | average_files sum
!
! Author: Alexey Mints (minzastro at gmail.com)
use StringArray
use operators

integer iUnitCount
character*(100) sFileName
character*(1000) sLine
integer istat
type(TStringArray) :: xA
real*8 aData(30, 40), aOut(30), fTmp
logical bMask(30)
integer iLineLength, iTmp
character*(20) sCommand

if (iargc().gt.0) then
  call GetArg(1, sCommand)
else
  sCommand = 'avg'
endif


! Read filenames and open files
iUnitCount = 0
do 
  read(*,*, iostat=istat) sFileName
  if (istat.eq.0) then
    open(unit=50+iUnitCount, file=sFileName, status="OLD")
    iUnitCount = iUnitCount + 1
  else
    exit
  endif
enddo

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
      case ('avg')
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
    end select
  enddo
  write(*,*) aOut(1:iLineLength)
enddo infinit_loop

! Closing files
do i=1, iUnitCount
  close(i-1)
enddo
end program average_files
