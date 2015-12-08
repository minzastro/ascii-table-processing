program GnuOut
! Program to perform quick plots with GNUPLOT.
! Author: Alexey Mints (minzastro at gmail.com)
use datatypes
use gnuplot_module
use gnuplot_module_data
use array_works
use StringArray
use operators
use comline

implicit none

!integer, parameter :: DATA_SIZE = 1000

type(gnuplot_ctrl), pointer :: gctrl
character(len=GP_CMD_SIZE) sInit
character(len=GP_CMD_SIZE) sCommand

real*8 xData(MAX_ROW, 0:MAX_COLUMN)
integer istat, iSize

character*100 sTitle
character*(100) sOutFileName, sInFileName

integer i, j
character*(20) sKey           !! input key & it's value
character*(100) sKeyValue
character*(8) sOutType !type of output file
character*(12) sStyle

logical bLogX, bLogY, bOneToOne

type(TStringArray) xArray

  sTitle = ''
  sOutFileName = ''
  sOutType = ''
  sInit = '-background white'
  sStyle = 'linespoints'
  bLogX = .false.
  bLogY = .false.
  xcol_num = 0
  do i = 1,10
    xcol_add(i)=i
  enddo

  call clReadParams()

  sTitle = clGetParamValue('-t', '--title', sTitle)
  sOutFileName = clGetParamValue('-o', '--output', sOutFileName)
  sStyle = clGetParamValue('-s', '--style', sStyle)
  sOutType = clGetParamValue('--type', sOutType)
  sInFileName = clGetParamValue('-f', '--file', '')

  sKeyValue = clGetParamValue('-l', '--log', '')
  if (index(sKeyValue, 'x').gt.0) then
    bLogX = .true.
  endif
  if (index(sKeyValue, 'y').gt.0) then
    bLogY = .true.
  endif

  xArray = TStringArraySplitX(clGetParamValue('-x'), ',')
  call toIntegerArray(xArray, 10, xcol_add(:), xcol_num)

  if (clCheckParam('--help')) then
#include "USAGE.f90"
    stop
  endif

  call LoadFromFile(sInFileName, xData, colnum, iSize)

  bOneToOne = clCheckParam('--onetoone')

  if (clCheckParam('-a').or.clCheckParam('--auto')) then
    xcol_add(2) = xcol_add(1)
    xcol_add(1) = 0
  endif

  if (trim(sOutType).eq.'') then
    sInit = '-persist '//trim(sInit)
  endif

  gctrl=>gnuplot_init(sInit)

  if (trim(sOutType).ne.'') then
    if (trim(sOutFileName).ne.'') then
      i = gnuplot_hardcopy(gctrl, trim(sOutType), trim(sOutFileName))
    else
      print *,'Error! No output filename specified! Stopped'
      stop
    endif
  endif

  if (trim(sTitle).ne.'') then
    i = gnuplot_settitle(gctrl, trim(sTitle))
  endif

  if (bLogX) then
    i = gnuplot_setscale(gctrl, 'x', 'LOG')
  endif
  if (bLogY) then
    i = gnuplot_setscale(gctrl, 'y', 'LOG')
  endif
  gctrl%plotstyle=trim(sStyle)
  if (xcol_num.eq.0) then
    i = gnuplot_plot2d(gctrl, iSize, xData(1:iSize,1), xData(1:iSize,2),'')
  else if (xcol_num.eq.1) then
    i = gnuplot_plot2d(gctrl, iSize, xData(1:iSize,1), xData(1:iSize,xcol_add(1)),'')
  else
    do j = 2, xcol_num
      i = gnuplot_plot2d(gctrl, iSize, xData(1:iSize,xcol_add(1)), xData(1:iSize,xcol_add(j)),'')
    enddo
  end if
  if (bOneToOne) then
    gctrl%plotstyle='l'
    i = gnuplot_plot2d(gctrl, iSize, xData(1:iSize,xcol_add(1)), xData(1:iSize,xcol_add(1)),'')
  endif
  i = gnuplot_close(gctrl)
end program GnuOut
