program create_model
use create_files
use create_model_init
implicit none

!-----------Variables de creation de modele (de la namelist)-------------------!
integer, parameter            :: nLayer = 5
! WARNING: nLayer n'est pas dans la namelist (updated by script)
integer                       :: nHydr, depthHydr                  ! hydrophones
integer                       :: nShot, depthShot, typeShot        ! shots
integer                       :: modelWidth, modelDepth            ! model
integer                       :: seaDepth
real                          :: seaVp                   ! sea
character(len=30)             :: fileHydr, fileShot, &
                                 fileSea, fileModelP , &
                                 fileModelS  ! fichiers qui vont etre creer
real, dimension(nLayer)       :: tabLayerVp
integer, dimension(nLayer)    :: tabLayerDepth
real                          :: dxz
character(len=4)              :: startingModel, trueModel
integer                       :: demiWindow ! demi-fenetre pour le smooth model
integer                       :: rayon, rayonH, rayonV ! rayon de la lentille
real                          :: anomalieV ! anomalie de vitesse de lens
integer, dimension(nLayer-1)  :: cosampli, coswvlen
integer                       :: sfCosAmp, sfCosWlen
!------------------------------------------------------------------------------!

!-----------Tableaux dynamiques------------------------------------------------!
real , dimension(:), allocatable    :: tabModelwidth, tabModeldepth
integer, dimension(:), allocatable  :: tabSeadepth  ! model
real, dimension(:), allocatable     :: tabVp, tabVs  ! vitesses
!------------------------------------------------------------------------------!

!----------------Autres variables------------------------!
integer                :: i, j
real, parameter        :: ratioVpVs = 1.75
character(len=30)      :: initFileModP,initFileModS
!--------------------------------------------------------!

namelist/MOD8/nHydr,depthHydr,fileHydr,nShot,depthShot,typeShot,fileShot, &
              modelWidth,modelDepth,fileModelP,fileModelS, &
              seaDepth,seaVp,fileSea,dxz,tabLayerVp,tabLayerDepth, &
              startingModel,trueModel,demiWindow,rayon,anomalieV, &
              cosampli,coswvlen,sfCosAmp,sfCosWlen,rayonH,rayonV


! ouverture du fichier contenant la namelist
open(unit=1,file="model4twist",recl=200,delim="quote",action="read")
read(unit=1,nml=MOD8) ! lecture de la namelist
close(unit=1)

!---------------Creation du fichier d hydrophones-------------------------!
call createFileHydr(fileHydr,nHydr,depthHydr)
!-------------------------------------------------------------------------!

!---------------Creation du fichier de shots------------------------------!
call createFileShot(fileShot,nShot,depthShot,typeShot)
!-------------------------------------------------------------------------!


!----------Creation du fichier de profondeur de l eau---------------------!
allocate(tabSeadepth(modelWidth))
call createFileSea(fileSea,seaDepth,sfCosAmp,sfCosWlen,tabSeadepth)
!-------------------------------------------------------------------------!

!---------------Creation des modeles de vitesse P et S--------------------!
allocate(tabModelwidth(modelWidth),tabModeldepth(modelDepth))

tabModeldepth(:) = [ ((i-1)*dxz/1000,i=1,modelDepth) ]
tabModelwidth(:) = [ ((i-1)*dxz/1000,i=1,modelWidth) ]

if( trueModel == "laye" ) then
  call initCouches(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                   tabLayerDepth,fileModelP,fileModelS,ratioVpVs)
  print *, "#### Layer true model created successfully ####"
else if( trueModel == "lens" ) then
  call initLentille(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                    tabLayerDepth,fileModelP,fileModelS,ratioVpVs,rayon, &
                    anomalieV)
  print *, "#### Lens true model created successfully ####"
else if( trueModel == "hlen" ) then
  call initHomLens(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                    tabLayerDepth,fileModelP,fileModelS,ratioVpVs,rayon, &
                    anomalieV)
  print *, "#### Homogeneous lens true model created successfully ####"
else if( trueModel == "hlhm" ) then
  call initHLensHomMedia(tabModeldepth,tabModelwidth,tabSeadepth,seaVp, &
                         tabLayerVp,tabLayerDepth,fileModelP,fileModelS, &
                         ratioVpVs,rayon,anomalieV)
  print *, "#### Homogeneous lens true model created successfully ####"
else if( trueModel == "cosi" ) then
  call initCosinus(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                    tabLayerDepth,fileModelP,fileModelS,ratioVpVs, &
                    cosampli,coswvlen)
  print *, "#### Cosinus true model created successfully ####"
else if( trueModel == "smoo" ) then
  call initSmooth(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                  tabLayerDepth,fileModelP,fileModelS,ratioVpVs,demiWindow)
  print *, "#### Smoothed true model created successfully ####"
else if( trueModel == "oval") then
  call initOval(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                  tabLayerDepth,fileModelP,fileModelS,ratioVpVs,rayonH, &
                  rayonV,anomalieV)
  print *, "#### Oval model created successfully ####"
else
  print *, "WARNING: no true model created !"
end if
!-------------------------------------------------------------------------!

!---------------Creation des modeles initiaux P et S--------------------!
initFileModP = "init." // trim(fileModelP)
initFileModS = "init." // trim(fileModelS)

if( startingModel == "grad" ) then
  call initGradient(tabModeldepth,tabModelwidth,tabSeadepth,seaVp, &
                    tabLayerVp,tabLayerDepth,initFileModP,initFileModS, &
                    ratioVpVs)
  print *, "#### Gradient starting model created successfully ####"
else if( startingModel == "smoo" ) then
  call initSmooth(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                  tabLayerDepth,initFileModP,initFileModS,ratioVpVs,demiWindow)
  print *, "#### Smoothed starting model created successfully ####"
  else if( startingModel == "laye" ) then
  call initCouches(tabModeldepth,tabModelwidth,tabSeadepth,seaVp,tabLayerVp, &
                   tabLayerDepth,initFileModP,initFileModS,ratioVpVs)
  print *, "#### Layer starting model created successfully ####"
else
  print *, "WARNING: no initial model created !"
end if
!-----------------------------------------------------------------------!


end program create_model

