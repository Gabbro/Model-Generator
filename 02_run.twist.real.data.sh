#!/bin/bash

### Fonction de test des variables ###
function test_var 
{
varName=$1
varValue=$2
if [ -z "$varValue" ]
	then 
		echo "ERROR: variable not found, check the parameter file: $varName"
fi
}
######################################
varFileName=variable_list



################################
#######EXTRACT PARAMETERS#######
################################
### FILE NAMES ###
file="twt_params_real.update" #have to modified by hand
fileForw="twt_params.forw"
fileInv="twt_params.inv"
fileShowshot="02_showshot.sh"
fileClean="clean.sh"
fileCopy="03_cpfile_2.sh"
fileConvertDataHydr="06_dobs2su2segy_hydr.csh"
##################
##################
### A modifier ###
##################
### Parametre hydrophone ###
hydrDepth=5 #si negatif, pas de creation de fichier
############################
### Parametres source ###
sourType=1
sourDepth=5 #si negatif, pas de creation de fichier
#########################
### Parametres sea floor###
wbDepth=-1 #si negatif, pas de creation de fichier
sfCosAmp=0
sfCosWlen=5
###########################
### Parametres couches ###
nlayer=2
layerVel="1600,5000"
layerDepths="100,"
#layerVel="2200,2500,2800,3100,3400,5000,8000"
#layerDepths="100,120,160,200,300,500," #1. the last boundary (depth of the model) will be added automatically. NB: let the "," !!!!! #2. the first boundary is the water depth
##########################
### Nom de la namelist ###
namelistName="MOD8"
##########################
### Type des modeles ###
trueModel="none" #"none" ou "laye" ou "lens" ou "hlen" ou "hlhm" ou "cosi" ou "smoo" ou "oval" ou "ovhm"
startingModel="none" #"none" ou "grad" ou "smoo" ou "laye"
########################
### Parametre pour smooth modele ###
demiWindow=10 #demi-fenetre en point de grille pour le modele smooth
####################################
### Parametres pour lentille ###
rayon=50
anomalieV=400
################################
### Parametres pour le modele cosinus ### 
#WARNING: si 5 couches 4 valeurs doivent etre mises dans les tableaux
cosampli="0"  #1. ampli > 1 for amplification and < 1 de flatten (en points) #2. si 0 cela signifie que la limite de couche restera plate
coswvlen="5" # indiquer le nombre de periodes desirees
#########################################
rayonH=200
rayonV=10
#gradient=""
########################################################################
########################################################################
########################################################################



#put all variable names and values in variable_list ascii file
grep -E -v "^( |$)" $file > variable_list


###Global Parameters => 0 to 10###
###Forward Modelling Parameters => 11 to 33###
###Inversion Parameters => 34 to 57###
###Model Parameters => 58 to 62###
###Data Parameters => 63 to 67###
###Shot Equalisation Number => 68 to 70###
###Streamer normalisation analysis => 71 to 73###
###OBC Normalisation Analysis => 74###
###Mute Parameters => 75 to 94###

varTab=([0]="Operational mode" [1]="Streamer" [2]="Timestep increment" [3]="Grid spacing" [4]="NUM_SHOTS" [5]="SHOT_INCR" [6]="Model width" [7]="Model depth" [8]="No. of recorded timesteps" [9]="No. of iterations" [10]="Padding" [11]="P-wave starting model" [12]="S-wave starting model" [13]="Surface type" [14]="Source type" [15]="Source file" [16]="Dominant source freq." [17]="Source temporal shift" [18]="Source polarity reversal" [19]="Rcvr-depth pairs" [20]="Shot-depth pairs" [21]="Survey WB-depth pairs file" [22]="Survey Vp water" [23]="FIRST_SHOT_GLOBAL_XCOORD" [24]="FIRST_OBC_RCVR_GLOBAL_XCOORD" [25]="NUM_OBC_RCVRS" [26]="OBC_RCVR_INCR" [27]="Use expanding box strategy" [28]="Record wavefield snapshots" [29]="Timestep of first snapshot" [30]="Time increment for snapshots" [31]="Number of snapshots" [32]="Number of snapshots" [33]="Output hydrophone component" [34]="Inversion component" [35]="Boundary storage io option" [36]="Number of blocks for io option" [37]="Inversion type" [38]="P & S linked" [39]="Commutation P to S" [40]="Gradient type" [41]="Gradient dimension" [42]="Gradient top cut-off" [43]="Gradient top taper" [44]="Gradient bottom cut-off" [45]="Gradient bottom taper" [46]="Gradient shot proximety cut-off" [47]="Gradient shot taper" [48]="Gradient smoothing" [49]="Horizontal gradient smoothing" [50]="Vertical gradient smoothing" [51]="filt_data" [52]="filter source" [53]="Start filter frequency" [54]="Final filter frequency" [55]="tpr_wdth" [56]="filter steps" [57]="Gradient perturbation strength" [58]="Dimension of starting model" [59]="Dimension of true model" [60]="Output model" [61]="Log gradients" [62]="Minimum S-wave velocity" [63]="Shot increment before logging" [64]="Directory for data" [65]="Project name" [66]="FFID for first shot in inversion" [67]="FFID increment number" [68]="shot norm value" [69]="Shot-normalisation pairs in" [70]="Equalisation value" [71]="Normalisation Channel" [72]="Time minimum" [73]="Time maximum" [74]="WB time window length" [75]="Apply a top mute" [76]="Apply a bottom mute" [77]="Inner trace mute (HORZ,U)" [78]="Trace temporal top-mute (HORZ,U)" [79]="Top-mute time at zero offset (HORZ,U)" [80]="Top-mute moveout with offset (HORZ,U)" [81]="Top-mute velocity for hyp moveout (HORZ,U)" [82]="Trace temporal bottom-mute (HORZ,U)" [83]="Bottom-mute time step zero offset (HORZ,U)" [84]="Bottom-mute moveout with offset (HORZ,U)" [85]="Bottom-mute velocity for hyp moveout (HORZ,U)" [86]="INNER TRACE MUTE (VERT, V)" [87]="TRACE TEMPORAL TOP_MUTE (VERT, V)" [88]="TOP-MUTE TIME AT ZERO OFFSET (VERT, V)" [89]="TOP_MUTE MOVEOUT WITH OFFSET (VERT, V)" [90]="top-mute velocity for hyp moveout" [91]="TRACE TEMPORAL BOTTOM_MUTE (VERT, V)" [92]="BTTM-MUTE TIME STEP AT ZERO OFFSET (VERT, V)" [93]="BTTM_MUTE MOVEOUT WITH OFFSET (VERT, V)" [94]="Bottom-mute velocity for hyp moveout (VERT, V)" [95]="Gradient left cut-off" [96]="Gradient left taper" [97]="Gradient right cut-off" [98]="Gradient right taper")

for i in ${!varTab[*]}
do
varValue[$i]=`grep "${varTab[$i]}" $varFileName | awk -F"= " '{print $NF}'`
test_var "${varTab[$i]}" "${varValue[$i]}"
echo "$i => ${varTab[$i]} : ${varValue[$i]}"
done 

layerDepths=`echo "$layerDepths ${varValue[7]}"`



#################################
#######CREATE MODEL FASTLY#######
#################################

####Create namelist####
echo "&$namelistName nHydr=${varValue[25]} depthHydr=$hydrDepth fileHydr=\"${varValue[19]}\" nShot=${varValue[4]} depthShot=$sourDepth typeShot=$sourType
fileShot=\"${varValue[20]}\" modelWidth=${varValue[6]} modelDepth=${varValue[7]} fileModelP=\"${varValue[11]}\"
fileModelS=\"${varValue[12]}\" seaDepth=$wbDepth seaVp=${varValue[22]} fileSea=\"${varValue[21]}\" dxz=${varValue[3]}
tabLayerVp=$layerVel tabLayerDepth=$layerDepths startingModel=\"$startingModel\" trueModel=\"$trueModel\" demiWindow=$demiWindow
rayon=$rayon anomalieV=$anomalieV cosampli=$cosampli coswvlen=$coswvlen sfCosAmp=$sfCosAmp sfCosWlen=$sfCosWlen rayonH=$rayonH rayonV=$rayonV /" > model4twist
echo "#### namelist \"$namelistName\" created ####"

####Backup namelist####
echo "&$namelistName nHydr=${varValue[25]} depthHydr=$hydrDepth fileHydr=\"${varValue[19]}\" nShot=${varValue[4]} depthShot=$sourDepth typeShot=$sourType
fileShot=\"${varValue[20]}\" modelWidth=${varValue[6]} modelDepth=${varValue[7]} fileModelP=\"${varValue[11]}\"
fileModelS=\"${varValue[12]}\" seaDepth=$wbDepth seaVp=${varValue[22]} fileSea=\"${varValue[21]}\" dxz=${varValue[3]}
tabLayerVp=$layerVel tabLayerDepth=$layerDepths startingModel=\"$startingModel\" trueModel=\"$trueModel\" demiWindow=$demiWindow 
rayon=$rayon anomalieV=$anomalieV cosampli=$cosampli coswvlen=$coswvlen sfCosAmp=$sfCosAmp sfCosWlen=$sfCosWlen rayonH=$rayonH rayonV=$rayonV /" >> model4twist.backup
echo "#### namelist backup \"$namelistName\" created ####"

####Update Fortran file####
sed s/'^integer, parameter            :: nLayer = [0-9]*'/"integer, parameter            :: nLayer = $nlayer"/ createTwistFiles.f90 > f90.tmp
mv f90.tmp createTwistFiles.f90
sed s/'MOD[0-9]'/"$namelistName"/ createTwistFiles.f90 > f90.tmp
mv f90.tmp createTwistFiles.f90
echo "#### Fortran file \"createTwistFiles.f90\" updated ####"

####Make executable####
gfortran -c createFiles.mod.f90
gfortran -c createModel.mod.f90
gfortran -c createTwistFiles.f90
gfortran -o createTwistFiles.exe createTwistFiles.o createModel.mod.o createFiles.mod.o
rm *.o
rm *.mod
echo "#### Executable \"createTwistFiles.exe\" created ####"

####Start model creation####
#rm ${varValue[11]}
#rm ${varValue[12]}
rm init.${varValue[11]}
rm init.${varValue[12]}
echo "#### \"createTwistFiles.exe\" started ####"
./createTwistFiles.exe
echo "#### \"createTwistFiles.exe\" finished ####"

####Update Displaying Data File####
sed s/'^nrecs=[0-9]*'/"nrecs=${varValue[25]}"/ $fileShowshot > sh.tmp
mv sh.tmp $fileShowshot
sed s/'^tstep=[0-9]*'/"tstep=${varValue[8]}"/ $fileShowshot > sh.tmp
mv sh.tmp $fileShowshot
sed s/'^dt=[0-9]*.[0-9]*'/"dt=${varValue[2]}"/ $fileShowshot > sh.tmp
mv sh.tmp $fileShowshot
sed s/'^d2val=[0-9]*.[0-9]*'/"d2val=${varValue[3]}"/ $fileShowshot > sh.tmp
mv sh.tmp $fileShowshot
echo "#### Script file \"02_showshot.sh\" updated ####"
echo "NB: you have to change the shot number by hand if you want another one !"
chmod 700 $fileShowshot

####Update Data Conversion File####
sed s/'^nrecstot=[0-9]*'/"nrecstot=${varValue[25]}"/ $fileConvertDataHydr > sh.tmp
mv sh.tmp $fileConvertDataHydr
sed s/'^tstep=[0-9]*'/"tstep=${varValue[8]}"/ $fileConvertDataHydr > sh.tmp
mv sh.tmp $fileConvertDataHydr
sed s/'^tinc=[0-9].[0-9]*'/"tinc=${varValue[2]}"/ $fileConvertDataHydr > sh.tmp
mv sh.tmp $fileConvertDataHydr
sed s/'^nshots=[0-9]*'/"nshots=${varValue[4]}"/ $fileConvertDataHydr > sh.tmp
mv sh.tmp $fileConvertDataHydr
echo "#### Script file \"06_dobs2su2segy_hydr.csh\" updated ####"

#####################
####Run TWIST 2.0####
#####################

####Create Parameter File for Forward Modelling####
echo "#### Create Parameter File for Forward Modelling: $fileForw ####"
cp $file $fileForw
sed s/'^Operational mode = [0-9]'/"Operational mode = 1"/ $file > $fileForw
sed s/'^P-wave starting model = .*'/"P-wave starting model = ${varValue[11]}"/ $fileForw > forw.tmp
mv forw.tmp $fileForw
sed s/'^S-wave starting model = .*'/"S-wave starting model = ${varValue[12]}"/ $fileForw > forw.tmp
mv forw.tmp $fileForw
cp $fileForw twt_params

####Forward modelling####
echo "#### Forward modelling started ####"
#rm T002*
#./$fileClean
#/usr/lib64/openmpi/bin/mpirun -np 8 /home/gabriel/Desktop/RESEARCH/TWIST_2.0/src_grad/mpi_version/twistps_grad_mpi . # twistps_grad_mpi_tbtaper / twistps_grad_mpi
#./$fileCopy
#chmod 700 $fileConvertDataHydr
#./$fileConvertDataHydr
echo "#### Forward modelling finished ####"

####Create Parameter File for Inversion####
echo "#### Create Parameter File for Inversion: $fileInv ####"
cp $file $fileInv
sed s/'^Operational mode = [0-9]'/"Operational mode = 3"/ $file > $fileInv
sed s/'^P-wave starting model = .*'/"P-wave starting model = ${varValue[11]}"/ $fileInv > forw.tmp
mv forw.tmp $fileInv
sed s/'^S-wave starting model = .*'/"S-wave starting model = ${varValue[12]}"/ $fileInv > forw.tmp
mv forw.tmp $fileInv
cp $fileInv twt_params

####Inversion####
echo "#### Inversion started ####"
rm win*
/usr/lib64/openmpi/bin/mpirun -np 8 /home/gabriel/Desktop/RESEARCH/TWIST_2.0/src_grad/mpi_version/twistps_grad_mpi .
echo "#### Inversion finished ####"
#################################################################

#### Send mail to tell it finished ####
#mailx -s "~*_*~ INVERSION FINISHED ~*_*~" huot@ipgp.fr < model4twist
#mailx adresse << EOF 
#~s INVERSION FINISHED
#~<! echo "Your inversion finished successfully !"
#~.
#EOF
#######################################




