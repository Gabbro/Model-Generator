module create_files
  implicit none
  integer, private                           :: i

  contains

  subroutine createFileHydr(fileHydr,nHydr,depthHydr)
    !----------------------Variables recues----------------------------------!
    character(len=30), intent(in)            :: fileHydr
    integer, intent(in)                      :: nHydr, depthHydr
    !------------------------------------------------------------------------!

    open(unit=1,file=trim(fileHydr),status="replace",action="write")

    do i=1,nHydr
      write(unit=1,fmt="(I0,a,I0)") i," ",depthHydr
    end do

    close(unit=1)

  end subroutine createFileHydr

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine createFileShot(fileShot,nShot,depthShot,typeShot)
    !----------------------Variables recues----------------------------------!
    character(len=30), intent(in)            :: fileShot
    integer, intent(in)                      :: nShot, depthShot, typeShot
    !------------------------------------------------------------------------!

    open(unit=1,file=trim(fileShot),status="replace",action="write")

    do i=1,nShot
      write(unit=1,fmt="(I0,a,I0,a,I0)") i," ",depthShot," ",typeShot
    end do

    close(unit=1)

  end subroutine createFileShot

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine createFileSea(fileSea,seaDepth,sfCosAmp,sfCosWlen,tabSeadepth)
    !----------------------Variables recues----------------------------------!
    character(len=30), intent(in)           :: fileSea
    integer, intent(in)                     :: seaDepth, sfCosAmp, sfCosWlen
    integer, dimension(:), intent(inout)    :: tabSeadepth
    !------------------------------------------------------------------------!
    integer                                 :: modelWidth
    real, parameter                         :: pi = acos(0.0)
    !------------------------------------------------------------------------!

    modelWidth = size(tabSeadepth)

    tabSeaDepth(:) = [ (int(sfCosAmp*cos((2.0*i*pi)*(sfCosWlen/ &
                       (1.0*modelWidth))))+seaDepth,i=1,modelWidth) ]

    open(unit=1,file=trim(fileSea),status="replace",action="write")

    do i=1,modelWidth
      write(unit=1,fmt="(I0,a,I0)") i," ",tabSeadepth(i)
    end do

    close(unit=1)

  end subroutine createFileSea

end module create_files

