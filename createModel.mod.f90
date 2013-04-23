module create_model_init
  implicit none
  integer, private                        :: i, j, k

  contains

  subroutine initCouches(tabModeldepth,tabModelwidth,tabSeadepth,seaVp, &
                         tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                         ratioVpVs)
    !--------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    !-----------------------------------------------------------------------!

    !---------------------Variables internes--------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth, nLayer
    !-----------------------------------------------------------------------!

    nLayer = size(tabLayerVp)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabLayerDepth(1)) = tabLayerVp(1)
      do k=2,nLayer
        tabVp(tabLayerDepth(k-1)+1:tabLayerDepth(k)) = tabLayerVp(k)
      end do
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVp(i)
        write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVs(i)
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initCouches

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

  subroutine initSmooth(tabModeldepth,tabModelwidth,tabSeadepth,seaVp, &
                        tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                        ratioVpVs,demiWindow)
    !--------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, intent(in)                 :: demiWindow
    !---------------------------------------------------------------------!

    !---------------------Variables internes--------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs, tabVtmp
    integer                           :: modelWidth, modelDepth, nLayer
    !-----------------------------------------------------------------------!

    nLayer = size(tabLayerVp)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth),tabVtmp(modelDepth))

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVtmp(1:tabSeadepth(j)) = seaVp
      tabVtmp(tabSeadepth(j)+1:tabLayerDepth(1)) = tabLayerVp(1)
      do k=2,nLayer
        tabVtmp(tabLayerDepth(k-1)+1:tabLayerDepth(k)) = tabLayerVp(k)
      end do
      tabVp(1:tabSeadepth(j)+demiWindow) = tabVtmp(1:tabSeadepth(j)+demiWindow)
      do k=tabSeadepth(j)+demiWindow+1,modelDepth-demiWindow
        tabVp(k) = sum(tabVtmp(k-demiWindow:k+demiWindow))/(2*demiWindow+1)
      end do
      tabVp(modelDepth-demiWindow+1:modelDepth) = tabVtmp(modelDepth- &
      demiWindow+1:modelDepth)
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVp(i)
        write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVs(i)
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initSmooth

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initGradient(tabModeldepth,tabModelwidth,tabSeadepth, seaVp, &
                          tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                          ratioVpVs)

    !--------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    !---------------------------------------------------------------------!

    !---------------------Variables internes--------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth
    real                              :: dv
    !-----------------------------------------------------------------------!

    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    dv = ( tabLayerVp(size(tabLayerVp)) - tabLayerVp(1) )/ &
             ( 1.0*modelDepth - 1.0*tabSeadepth(1) - 1.0)

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabSeadepth(j)+1) = tabLayerVp(1)
      tabVp(tabSeadepth(j)+2:modelDepth) = [ (tabVp(tabSeadepth(j)+1)+k*dv, &
                                             k=1,modelDepth-tabSeadepth(j)-1) ]
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVp(i)
        write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVs(i)
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initGradient

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initLentille(tabModeldepth,tabModelwidth,tabSeadepth, seaVp, &
                          tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                          ratioVpVs,rayon,anomalieV)

    !-----------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, intent(in)                 :: rayon
    real, intent(in)                    :: anomalieV
    !--------------------------------------------------------------------------!

    !---------------------Variables internes-----------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth
    real                              :: dv, dvlentille, distance, Vplentille
    real                              :: dxz
    integer                           :: centerx, centerz
    !--------------------------------------------------------------------------!

    dxz = tabModeldepth(2)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    centerx = modelWidth/2
    centerz = tabSeadepth(centerx) + (modelDepth-tabSeadepth(centerx))/2
    dv = ( tabLayerVp(size(tabLayerVp)) - tabLayerVp(1) )/ &
             ( 1.0*modelDepth - 1.0*tabSeadepth(centerx) - 1.0)
    dvlentille = anomalieV/(1.0*rayon)

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabSeadepth(j)+1) = tabLayerVp(1)
      tabVp(tabSeadepth(j)+2:modelDepth) = [ (tabVp(tabSeadepth(j)+1)+k*dv, &
                                             k=1,modelDepth-tabSeadepth(j)-1) ]
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        distance = sqrt((tabModelwidth(j)-dxz*centerx)**2 + &
        (tabModeldepth(i)-dxz*centerz)**2)
        if( distance > dxz*rayon ) then
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVp(i)
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVs(i)
        else
          Vplentille = dvlentille*(distance/dxz)+tabVp(centerz)
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille/ratioVpVs
        end if
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initLentille

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initHomLens(tabModeldepth,tabModelwidth,tabSeadepth, seaVp, &
                          tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                          ratioVpVs,rayon,anomalieV)

    !-----------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, intent(in)                 :: rayon
    real, intent(in)                    :: anomalieV
    !--------------------------------------------------------------------------!

    !---------------------Variables internes-----------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth
    real                              :: dv, dvlentille, distance, Vplentille
    real                              :: dxz
    integer                           :: centerx, centerz
    !--------------------------------------------------------------------------!

    dxz = tabModeldepth(2)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    centerx = modelWidth/2
    centerz = tabSeadepth(centerx) + (modelDepth-tabSeadepth(centerx))/2
    dv = ( tabLayerVp(size(tabLayerVp)) - tabLayerVp(1) )/ &
             ( 1.0*modelDepth - 1.0*tabSeadepth(centerx) - 1.0)
    dvlentille = anomalieV/(1.0*rayon)

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabSeadepth(j)+1) = tabLayerVp(1)
      tabVp(tabSeadepth(j)+2:modelDepth) = [ (tabVp(tabSeadepth(j)+1)+k*dv, &
                                             k=1,modelDepth-tabSeadepth(j)-1) ]
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        distance = sqrt((tabModelwidth(j)-dxz*centerx)**2 + &
        (tabModeldepth(i)-dxz*centerz)**2)
        if( distance > dxz*rayon ) then
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVp(i)
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVs(i)
        else
          Vplentille = dvlentille*(1.0*(rayon+1)-(distance/dxz))+tabVp(i)
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille/ratioVpVs
        end if
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initHomLens

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initHLensHomMedia(tabModeldepth,tabModelwidth,tabSeadepth, seaVp, &
                          tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                          ratioVpVs,rayon,anomalieV)

    !-----------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, intent(in)                 :: rayon
    real, intent(in)                    :: anomalieV
    !--------------------------------------------------------------------------!

    !---------------------Variables internes-----------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth
    real                              :: dv, dvlentille, distance, Vplentille
    real                              :: dxz
    integer                           :: centerx, centerz, nLayer
    !--------------------------------------------------------------------------!

    nLayer = size(tabLayerVp)
    dxz = tabModeldepth(2)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    centerx = modelWidth/2
    centerz = tabSeadepth(centerx) + (modelDepth-tabSeadepth(centerx))/2
    dv = ( tabLayerVp(size(tabLayerVp)) - tabLayerVp(1) )/ &
             ( 1.0*modelDepth - 1.0*tabSeadepth(centerx) - 1.0)
    dvlentille = anomalieV/(1.0*rayon)

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabLayerDepth(1)) = tabLayerVp(1)
      do k=2,nLayer
        tabVp(tabLayerDepth(k-1)+1:tabLayerDepth(k)) = tabLayerVp(k)
      end do
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        distance = sqrt((tabModelwidth(j)-dxz*centerx)**2 + &
        (tabModeldepth(i)-dxz*centerz)**2)
        if( distance > dxz*rayon ) then
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVp(i)
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVs(i)
        else
          Vplentille = dvlentille*(1.0*(rayon+1)-(distance/dxz))+tabVp(i)
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille/ratioVpVs
        end if
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initHLensHomMedia

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initCosinus(tabModeldepth,tabModelwidth,tabSeadepth,seaVp, &
                         tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                         ratioVpVs,cosampli,coswvlen)
    !--------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, dimension(:), intent(in)   :: cosampli, coswvlen
    !---------------------------------------------------------------------!

    !---------------------Variables internes--------------------------------!
    real, dimension(:), allocatable    :: tabVp, tabVs
    integer                            :: modelWidth, modelDepth, ncos
    real                               :: dv
    real, parameter                    :: pi = acos(0.0)
    integer, dimension(:), allocatable :: cosDepth
    !-----------------------------------------------------------------------!

    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    ncos = size(tabLayerVp)-1
    allocate(tabVp(modelDepth),tabVs(modelDepth),cosDepth(ncos))

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      cosDepth(:) = [ (int(cosampli(k)*cos((2.0*j*pi)*(coswvlen(k)/(1.0* &
                      modelWidth)))),k=1,ncos) ]
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:cosDepth(1)+tabLayerDepth(1)) = tabLayerVp(1)
      do k=2,ncos
        tabVp(cosDepth(k-1)+tabLayerDepth(k-1)+1:cosDepth(k)+tabLayerDepth(k) &
             ) = tabLayerVp(k)
      end do
      tabVp(cosDepth(ncos)+tabLayerDepth(ncos)+1:tabLayerDepth(ncos+1) &
           ) = tabLayerVp(ncos+1)
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVp(i)
        write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
        " ",tabModeldepth(i)," ",tabVs(i)
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initCosinus

!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!
!-----------------------------------------------------------------------------!

  subroutine initOval(tabModeldepth,tabModelwidth,tabSeadepth, seaVp, &
                          tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                          ratioVpVs,rayonH,rayonV,anomalieV)

    !-----------------------------Variables recues-----------------------------!
    real , dimension(:), intent(in)     :: tabModelwidth, tabModeldepth
    integer, dimension(:), intent(in)   :: tabSeadepth
    character(len=30), intent(in)       :: fileModelP, fileModelS
    real, dimension(:), intent(in)      :: tabLayerVp
    integer, dimension(:), intent(in)   :: tabLayerDepth
    real, intent(in)                    :: seaVp, ratioVpVs
    integer, intent(in)                 :: rayonH, rayonV
    real, intent(in)                    :: anomalieV
    !--------------------------------------------------------------------------!

    !---------------------Variables internes-----------------------------------!
    real, dimension(:), allocatable   :: tabVp, tabVs
    integer                           :: modelWidth, modelDepth
    real                              :: dv, distance, Vplentille
    real                              :: dxz
    integer                           :: centerx, centerz
    !--------------------------------------------------------------------------!

    dxz = tabModeldepth(2)
    modelDepth = size(tabModeldepth)
    modelWidth = size(tabModelwidth)
    allocate(tabVp(modelDepth),tabVs(modelDepth))

    centerx = modelWidth/2
    centerz = tabSeadepth(centerx) + (modelDepth-tabSeadepth(centerx))/2
    dv = ( tabLayerVp(size(tabLayerVp)) - tabLayerVp(1) )/ &
             ( 1.0*modelDepth - 1.0*tabSeadepth(centerx) - 1.0)

    open(unit=1,file=trim(fileModelP),status="replace",action="write")
    open(unit=2,file=trim(fileModelS),status="replace",action="write")

    do j=1,modelWidth
      tabVp(1:tabSeadepth(j)) = seaVp
      tabVp(tabSeadepth(j)+1:tabSeadepth(j)+1) = tabLayerVp(1)
      tabVp(tabSeadepth(j)+2:modelDepth) = [ (tabVp(tabSeadepth(j)+1)+k*dv, &
                                             k=1,modelDepth-tabSeadepth(j)-1) ]
      tabVs(:) = tabVp(:)/ratioVpVs
      tabVs(1:tabSeaDepth(j)) = 0.0
      do i=1,modelDepth
        distance = sqrt((tabModelwidth(j)-dxz*centerx)**2/((dxz*rayonH)**2) + &
        (tabModeldepth(i)-dxz*centerz)**2/((dxz*rayonV)**2))
        if( distance > 1 ) then
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVp(i)
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",tabVs(i)
        else
          Vplentille = anomalieV+tabVp(centerz)
          write(unit=1,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille
          write(unit=2,fmt="(F0.5,a,F0.5,a,F0.5)") tabModelwidth(j), &
          " ",tabModeldepth(i)," ",Vplentille/ratioVpVs
        end if
      end do
    end do

    close(unit=1)
    close(unit=2)

  end subroutine initOval

end module create_model_init
