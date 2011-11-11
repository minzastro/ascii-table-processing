program join_num
! Tool to join two ASCII tables by a number key.
! Comparable with the classical linux 'join' command, 
! but operates on float key values.
use array_works
use quickSort
use StringArray
use operators
use comline

implicit none

real*8 xData1(MAX_ROW,0:MAX_COLUMN), xData2(MAX_ROW,0:MAX_COLUMN)
character*(250) sFile1, sFile2  !Filenames
integer iArgs, i, col1, col2, ii,jj
integer iCols1, iCols2, iRows1, iRows2     !data measures
integer iIndex1(MAX_ROW), iIndex2(MAX_ROW) !indexes for the data
type (TStringArray) xArray !! For parsing comma-separated arguments

iArgs = iargc()


i = 1
col1 = 1
col2 = 1
sFile1 = ''
sFile2 = ''

sIntegerFormat = 'F6.0'
sRealFormat = 'E16.5'

call clReadParams()

if (clCheckParam('-j')) then
  col1 = clGetParamValue('-j')
  col2 = col1
else
  col1 = clGetParamValue('-1', 1)
  col2 = clGetParamValue('-2', 1)
endif

if (clCheckParam('-i')) then
  xArray = TStringArraySplitX(clGetParamValue('-i'), ',')
  call toIntegerArray(xArray, 10, int_columns(:), int_col_num)
else
  int_col_num = 0
endif

sIntegerFormat = clGetParamValue('-int', sIntegerFormat)
sRealFormat = clGetParamValue('-real', sRealFormat)
if (clCheckParam('--help')) then
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
endif

sFile1 = clGetFreeParam(1)
sFile2 = clGetFreeParam(2)

call LoadFromFile(sFile1, xData1, iCols1, iRows1)
call LoadFromFile(sFile2, xData2, iCols2, iRows2)

call quick_sort_index(xData1(1:iRows1,col1), xData1(1:iRows1,iCols1+1), iIndex1(1:iRows1))
call quick_sort_index(xData2(1:iRows2,col2), xData2(1:iRows2,iCols2+1), iIndex2(1:iRows2))

sFormat = '('
if (1.in.int_columns(1:int_col_num)) then
  sFormat = trim(sFormat)//sIntegerFormat
else
  sFormat = trim(sFormat)//sRealFormat
endif
do i = 2, iCols1+iCols2-1
  if (i.in.int_columns(1:int_col_num)) then
    sFormat = trim(sFormat)//',2X,'//sIntegerFormat
  else
    sFormat = trim(sFormat)//',2X,'//sRealFormat
  endif
enddo
sFormat = trim(sFormat) // ')'

jj = 1
do i = 1, iRows1
  do while (xData1(iIndex1(i), col1).gt.xData2(iIndex2(jj), col2))
    jj = jj + 1
    if (jj.gt.iRows2) then
      stop
    endif
  enddo
  if (xData1(iIndex1(i), col1).eq.xData2(iIndex2(jj), col2)) then
    write(*,sFormat) xData1(i, 1:col1-1), &
                     xData1(iIndex1(i), col1), &
                     xData1(i, col1+1:iCols1), &
		     xData2(jj, 1:col2-1), &
		     xData2(jj, col2+1:iCols2)
  endif
enddo

end program join_num
