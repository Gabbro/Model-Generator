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
####### DEFINE PARAMETERS ######
################################
### FILE NAMES ###
file="twt_params.update" #have to be modified by hand
##################
##################
### A modifier ###
##################
### Hydrophone parameters ###
hydrDepth=5
#############################
### Source parameters ###
sourType=1
sourDepth=5
#########################
### Sea floor parameters ###
wbDepth=60
sfCosAmp=0
sfCosWlen=5
############################
### Parametres couches ###
nlayer=4
layerVel="2200,2500,3500,4200"
layerDepths="160,350,520," #1. the last boundary (depth of the model) will be added automatically. NB: let the "," !!!!! #2. the first boundary is the water depth
##########################
### Nom de la namelist ###
namelistName="MOD7"
##########################
### Type des modeles ###
trueModel="laye" #"none" ou "laye" ou "lens" ou "hlen" ou "hlhm" ou "cosi" ou "smoo"
startingModel="smoo" #"none" ou "grad" ou "smoo" ou "laye"
########################
### Parameter for smooth model ###
demiWindow=15 #demi-fenetre en point de grille pour le modele smooth
####################################
### Parametres pour lentille ###
rayon=50
anomalieV=400
################################
### Parametres pour le modele cosinus ### 
#WARNING: ex: si 5 couches 4 valeurs doivent etre mises dans les tableaux
cosampli="0,0,0"  #1. ampli > 1 for amplification and < 1 for flattening (in points) #2. si 0 cela signifie que la limite de couche restera plate
coswvlen="5,5,5" # indiquer le nombre de periodes desirees
#########################################
#gradient=""
########################################################################
########################################################################
########################################################################



################################
###### EXTRACT PARAMETERS ######
################################

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

varTab=([0]="Operational mode" [1]="Streamer" [2]="Timestep increment" [3]="Grid spacing" [4]="NUM_SHOTS" [5]="SHOT_INCR" [6]="Model width" [7]="Model depth" [8]="No. of recorded timesteps" [9]="No. of iterations" [10]="Padding" [11]="P-wave starting model" [12]="S-wave starting model" [13]="Surface type" [14]="Source type" [15]="Source file" [16]="Dominant source freq." [17]="Source temporal shift" [18]="Source polarity reversal" [19]="Rcvr-depth pairs" [20]="Shot-depth pairs" [21]="Survey WB-depth pairs file" [22]="Survey Vp water" [23]="FIRST_SHOT_GLOBAL_XCOORD" [24]="FIRST_OBC_RCVR_GLOBAL_XCOORD" [25]="NUM_OBC_RCVRS" [26]="OBC_RCVR_INCR" [27]="Use expanding box strategy" [28]="Record wavefield snapshots" [29]="Timestep of first snapshot" [30]="Time increment for snapshots" [31]="Number of snapshots" [32]="Number of snapshots" [33]="Output hydrophone component" [34]="Inversion component" [35]="Boundary storage io option" [36]="Number of blocks for io option" [37]="Inversion type" [38]="P & S linked" [39]="Commutation P to S" [40]="Gradient type" [41]="Gradient dimension" [42]="Gradient top cut-off" [43]="Gradient top taper" [44]="Gradient bottom cut-off" [45]="Gradient bottom taper" [46]="Gradient shot proximety cut-off" [47]="Gradient shot taper" [48]="Gradient smoothing" [49]="Horizontal gradient smoothing" [50]="Vertical gradient smoothing" [51]="filt_data" [52]="filter source" [53]="Start filter frequency" [54]="Final filter frequency" [55]="tpr_wdth" [56]="filter steps" [57]="Gradient perturbation strength" [58]="Dimension of starting model" [59]="Dimension of true model" [60]="Output model" [61]="Log gradients" [62]="Minimum S-wave velocity" [63]="Shot increment before logging" [64]="Directory for data" [65]="Project name" [66]="FFID for first shot in inversion" [67]="FFID increment number" [68]="shot norm value" [69]="Shot-normalisation pairs in" [70]="Equalisation value" [71]="Normalisation Channel" [72]="Time minimum" [73]="Time maximum" [74]="WB time window length" [75]="Apply a top mute" [76]="Apply a bottom mute" [77]="Inner trace mute (HORZ,U)" [78]="Trace temporal top-mute (HORZ,U)" [79]="Top-mute time at zero offset (HORZ,U)" [80]="Top-mute moveout with offset (HORZ,U)" [81]="Top-mute velocity for hyp moveout (HORZ,U)" [82]="Trace temporal bottom-mute (HORZ,U)" [83]="Bottom-mute time step zero offset (HORZ,U)" [84]="Bottom-mute moveout with offset (HORZ,U)" [85]="Bottom-mute velocity for hyp moveout (HORZ,U)" [86]="INNER TRACE MUTE (VERT, V)" [87]="TRACE TEMPORAL TOP_MUTE (VERT, V)" [88]="TOP-MUTE TIME AT ZERO OFFSET (VERT, V)" [89]="TOP_MUTE MOVEOUT WITH OFFSET (VERT, V)" [90]="top-mute velocity for hyp moveout" [91]="TRACE TEMPORAL BOTTOM_MUTE (VERT, V)" [92]="BTTM-MUTE TIME STEP AT ZERO OFFSET (VERT, V)" [93]="BTTM_MUTE MOVEOUT WITH OFFSET (VERT, V)" [94]="Bottom-mute velocity for hyp moveout (VERT, V)")

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
rayon=$rayon anomalieV=$anomalieV cosampli=$cosampli coswvlen=$coswvlen sfCosAmp=$sfCosAmp sfCosWlen=$sfCosWlen /"> model4twist
echo "#### namelist \"$namelistName\" created ####"

####Backup namelist####
echo "&$namelistName nHydr=${varValue[25]} depthHydr=$hydrDepth fileHydr=\"${varValue[19]}\" nShot=${varValue[4]} depthShot=$sourDepth typeShot=$sourType
fileShot=\"${varValue[20]}\" modelWidth=${varValue[6]} modelDepth=${varValue[7]} fileModelP=\"${varValue[11]}\"
fileModelS=\"${varValue[12]}\" seaDepth=$wbDepth seaVp=${varValue[22]} fileSea=\"${varValue[21]}\" dxz=${varValue[3]}
tabLayerVp=$layerVel tabLayerDepth=$layerDepths startingModel=\"$startingModel\" trueModel=\"$trueModel\" demiWindow=$demiWindow 
rayon=$rayon anomalieV=$anomalieV cosampli=$cosampli coswvlen=$coswvlen sfCosAmp=$sfCosAmp sfCosWlen=$sfCosWlen /" >> model4twist.backup
echo "#### namelist backup \"$namelistName\" created ####"

####Update Fortran file####
sed s/'^integer, parameter            :: nLayer = [0-9]'/"integer, parameter            :: nLayer = $nlayer"/ createTwistFiles.f90 > f90.tmp
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
rm ${varValue[11]}
rm ${varValue[12]}
rm init.${varValue[11]}
rm init.${varValue[12]}
echo "#### \"createTwistFiles.exe\" started ####"
./createTwistFiles.exe
echo "#### \"createTwistFiles.exe\" finished ####"


