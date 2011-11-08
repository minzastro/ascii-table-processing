program join_num
use array_works
use quickSort
use StringArray
use operators

implicit none

real*8 xData1(MAX_ROW,0:MAX_COLUMN), xData2(MAX_ROW,0:MAX_COLUMN)
logical bIsInteger(MAX_COLUMN) !Determines wether the column is integer instead of real
character*(250) sFile1, sFile2  !Filenames
character*(250) sKey, sKeyValue           !! input key & it's value
integer iArgs, i, col1, col2, ii,jj
integer iCols1, iCols2, iRows1, iRows2     !data measures
integer iIndex1(MAX_ROW), iIndex2(MAX_ROW) !indexes for the data
type (TStringArray) xArray !! For parsing comma-separated arguments

!character*(500) sFormat
character*(5) sIntFormat

iArgs = iargc()

bIsInteger(:) = .false.

i = 1
col1 = 1
col2 = 1
sFile1 = ''
sFile2 = ''

sIntFormat = 'F6.0'
sRealFormat = 'E16.5'

do while (i.le.iArgs)
  call GetArg(i, sKey)
  call GetArg(i+1, sKeyValue)
  comm_case:select case (trim(sKey))
    case('-1')
      col1 = sKeyValue
    case('-2')
      col2 = sKeyValue
    case('-j')
      col1 = sKeyValue
      col2 = sKeyValue
    case('-i')
      call Sqeeze(sKeyValue, ' ')
      call TStringArraySplit(sKeyValue, ',', xArray)
      do ii = 1, xArray%length
        jj = trim(xArray%member(ii)%chars)
        bIsInteger(jj) = .true.
      enddo
    case('-int')
      sIntFormat = trim(sKeyValue)
    case('-real')
      sRealFormat = trim(sKeyValue)
    case('--help')
      print *, 'Usage: join_num [options] file1 file2'
      print *, 'For each pair of input lines with identical (in numerical sense) join fields, write a line to'
      print *, 'standard output.  The default join field is the first, delimited'
      print *, 'by whitespace.  When FILE1 or FILE2 (not both) is -, read standard input.'
      print *, '  -i LIST           LIST provides comma-separated list of fields, that are assumed to be integer'
      print *, '  -int FORMAT       FORMAT sets output format for integer values'
      print *, '  -real FORMAT      FORMAT sets output format for real values'
      print *, '  -1 FIELD          join on this FIELD of file 1'
      print *, '  -2 FIELD          join on this FIELD of file 2'
      print *, '  -j FIELD          equivalent to `-1 FIELD -2 FIELD'
      print *, '  --help            display this help and exit'
      STOP
    case default
      if (trim(sFile1).eq.'') then
        sFile1 = sKey
      else
        sFile2 = sKey
      endif
      i = i - 1
  end select comm_case
  i = i + 2
enddo !args

call LoadFromFile(sFile1, xData1, iCols1, iRows1)
call LoadFromFile(sFile2, xData2, iCols2, iRows2)

!write(*,*) iCols1, iRows1, iCols2, iRows2

call quick_sort_index(xData1(:,col1), xData1(:,col1), iIndex1(:))
call quick_sort_index(xData2(:,col2), xData2(:,col2), iIndex2(:))

sFormat = '('
if (bIsInteger(1)) then
  sFormat = trim(sFormat)//sIntFormat
else
  sFormat = trim(sFormat)//sRealFormat
endif
do i = 2, iCols1+iCols2-1
  if (bIsInteger(i)) then
    sFormat = trim(sFormat)//',2X,'//sIntFormat
  else
    sFormat = trim(sFormat)//',2X,'//sRealFormat
  endif
enddo
sFormat = trim(sFormat) // ')'

!write(*,*) sFormat, col1, col2,iCols1, iRows1, iCols2, iRows2
jj = 1
do i = 1, iRows1
!   write(*,*) i, jj, iIndex1(i), iIndex2(jj),xData1(iIndex1(i),col1),xData2(iIndex2(jj),col2)
  do while (xData1(iIndex1(i),col1).gt.xData2(iIndex2(jj),col2))
    jj = jj + 1
    if (jj.gt.iRows2) then
      stop
    endif
  enddo
!   write(*,*) i, jj, iIndex1(i), iIndex2(jj),xData1(iIndex1(i),col1),xData2(iIndex2(jj),col2)
  if (xData1(iIndex1(i),col1).eq.xData2(iIndex2(jj),col2)) then
    write(*,sFormat) xData1(i,1:col1-1), &
                     xData1(iIndex1(i),col1), &
                     xData1(i,col1+1:iCols1), &
		     xData2(jj,1:col2-1), &
		     xData2(jj,col2+1:iCols2)
  endif
enddo

end program join_num
