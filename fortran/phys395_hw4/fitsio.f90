module fitsio
  implicit none

contains

  ! write array data into FITS file as sequence of image extensions
  subroutine write2fits(file, array, xx, yy, vars, coords)
    character(len=*) file, vars(:), coords
    real array(:,:,:), xx(2), yy(2)
    optional xx, yy, vars, coords

    integer i, j, status, unit
    integer :: hdus, naxis = 2, n(2), npix
    integer :: bitpix, group = 1, blocksize = -1

    ! data dimansions
    hdus = size(array,1)
    n(1) = size(array,2)
    n(2) = size(array,3)
    npix = n(1)*n(2)

    ! data format
    select case (kind(array))
    case (4); bitpix = -32
    case (8); bitpix = -64
    case default; call abort
    end select

    ! delete file if it already exists
    open(unit=1234, iostat=status, file=file, status='old')
    if (status == 0) close(1234, status='delete'); status = 0

    ! initialize FITS file
    call ftgiou(unit, status)
    call ftinit(unit, file, blocksize, status)

    ! write image extensions
    do i = 1,hdus
      call ftiimg(unit, bitpix, naxis, n, status)

      select case (kind(array))
      case (4); call ftppre(unit, group, 1, npix, array(i,:,:), status)
      case (8); call ftpprd(unit, group, 1, npix, array(i,:,:), status)
      case default; call abort
      end select

      if (present(vars)) then
        if (present(coords)) then
          call ftpkys(unit, 'EXTNAME', trim(vars(i))//coords, 'variable stored in extension', status)
        else
          call ftpkys(unit, 'EXTNAME', trim(vars(i)), 'variable stored in extension', status)
        end if
      end if
      if (present(xx)) then
        call ftpkyj(unit, 'CRPIX1', 1, 'x-axis origin pixel', status)
        call ftpkyd(unit, 'CRVAL1', real(xx(1),8), 14, 'x-axis origin coordinate', status)
        call ftpkyd(unit, 'CDELT1', real(xx(2)-xx(1),8)/real(n(1)-1,8), 14, 'x-axis increment', status)
      end if
      if (present(yy)) then
        call ftpkyj(unit, 'CRPIX2', 1, 'y-axis origin pixel', status)
        call ftpkyd(unit, 'CRVAL2', real(yy(1),8), 14, 'y-axis origin coordinate', status)
        call ftpkyd(unit, 'CDELT2', real(yy(2)-yy(1),8)/real(n(2)-1,8), 14, 'y-axis increment', status)
      end if
    end do

    ! clean up
    call ftclos(unit, status)
    call ftfiou(unit, status)
  end subroutine write2fits

end module
