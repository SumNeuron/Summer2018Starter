#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Ademxapp 1D CNN Inception Net for Instance Segmentation*)


(* ::Chapter:: *)
(*Helper Functions*)


Weave[a_List,b_List,partition_:2]:=Partition[Riffle[a,b],{partition}]


(* ::Chapter:: *)
(*Net Functions*)


AdemxappResidualCNN1DBlock[inputDimension_, name_, residualBNQ_,kernel_:3,padding_:1]:=With[{
scale=If[residualBNQ,2,1]
},Module[
{
residualBranch={
NetPort["Input"]->name<>"_plus_shared"
},
residualBNBranch={
name<>"_ramp_shared"-> name<>"_cnn1d_branch_1"->name<>"_plus_shared"
},
mainBranch = {
name<>"_bnl_shared"->name<>"_ramp_shared",
If[residualBNQ,
name<>"_ramp_shared"->name<>"_cnn1d_branch_main"-> name<>"_ramp_branch_main",
name<>"_ramp_shared"->name<>"_cnn1d_branch_main"->name<>"_bnl_main"-> name<>"_ramp_branch_main"
], 
name<>"_ramp_branch_main"->name<>"_cnn1d_branch_main_a"->name<>"_plus_shared"
},
residualBranchLayers=<||>,
residualBNBranchLayers=<|
name<>"_cnn1d_branch_1"->ConvolutionLayer[scale*First@inputDimension,{1}]
|>,
mainBranchLayers =<|
name<>"_bnl_shared"->BatchNormalizationLayer["Input"-> inputDimension],
name<>"_ramp_shared"-> Ramp,
name<>"_cnn1d_branch_main"->ConvolutionLayer[scale*First@inputDimension,{kernel},"PaddingSize"->{padding}],
If[residualBNQ,Nothing,name<>"_bnl_main"->BatchNormalizationLayer[]],
name<>"_ramp_branch_main"->ElementwiseLayer[Ramp],
name<>"_cnn1d_branch_main_a"->ConvolutionLayer[scale*First@inputDimension,{kernel},"PaddingSize"->{padding}],
name<>"_plus_shared"->ThreadingLayer[Plus]
|>,

layers, connections
},
If[residualBNQ,
layers=Join[mainBranchLayers,residualBNBranchLayers];
connections=Join[mainBranch,residualBNBranch],

layers=Join[mainBranchLayers,residualBranchLayers];
connections=Join[mainBranch,residualBranch]
];
NetGraph[layers,connections]
]]


(* ::Chapter:: *)
(*Define Net*)


net=NetGraph[
<|
"cnn"->ConvolutionLayer[64,{3},"PaddingSize"->{1}],
"pol"->PoolingLayer[{3},"PaddingSize"->{1}],

"cnn1a"->AdemxappResidualCNN1DBlock[{64,Automatic}, "1a", True,3,1],
"cnn1b"->AdemxappResidualCNN1DBlock[{64,Automatic}, "1b", True,25,12],
"cnn1c"->AdemxappResidualCNN1DBlock[{64,Automatic}, "1c", True,75,37],


"cnn1a1"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1a1", False,3,1],
"cnn1b1"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1b1", False,25,12],
"cnn1c1"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1c1", False,75,37],

"cnn1a2"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1a2", False,3,1],
"cnn1b2"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1b2", False,25,12],
"cnn1c2"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "1c2", False,75,37],

"pol1a"->PoolingLayer[{3},"PaddingSize"->{1}],
"pol1b"->PoolingLayer[{25},"PaddingSize"->{12}],
"pol1c"->PoolingLayer[{75},"PaddingSize"->{37}],


"trd1"->ThreadingLayer[Plus],

"rmp1"->Ramp,
"bnl1"->BatchNormalizationLayer[],

"cnn2a"->AdemxappResidualCNN1DBlock[{128,Automatic}, "2a", True,3,1],
"cnn2b"->AdemxappResidualCNN1DBlock[{128,Automatic}, "2b", True,25,12],
"cnn2c"->AdemxappResidualCNN1DBlock[{128,Automatic}, "2c", True,75,37],

"cnn2a1"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2a1", False,3,1],
"cnn2b1"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2b1", False,25,12],
"cnn2c1"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2c1", False,75,37],

"cnn2a2"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2a2", False,3,1],
"cnn2b2"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2b2", False,25,12],
"cnn2c2"->AdemxappResidualCNN1DBlock[{256,Automatic}, "2c2", False,75,37],



"pol2a"->PoolingLayer[{3},"PaddingSize"->{1}],
"pol2b"->PoolingLayer[{25},"PaddingSize"->{12}],
"pol2c"->PoolingLayer[{75},"PaddingSize"->{37}],

"trd2"->ThreadingLayer[Plus],

"rmp2"->Ramp,
"bnl2"->BatchNormalizationLayer[],


"cnn3a"->AdemxappResidualCNN1DBlock[{256,Automatic}, "3a", True,3,1],
"cnn3b"->AdemxappResidualCNN1DBlock[{256,Automatic}, "3b", True,25,12],
"cnn3c"->AdemxappResidualCNN1DBlock[{256,Automatic}, "3c", True,75,37],

"cnn3a1"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3a1", False,3,1],
"cnn3b1"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3b1", False,25,12],
"cnn3c1"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3c1", False,75,37],

"cnn3a2"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3a2", False,3,1],
"cnn3b2"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3b2", False,25,12],
"cnn3c2"->AdemxappResidualCNN1DBlock[{512,Automatic}, "3c2", False,75,37],


"pol3a"->PoolingLayer[{3},"PaddingSize"->{1}],
"pol3b"->PoolingLayer[{25},"PaddingSize"->{12}],
"pol3c"->PoolingLayer[{75},"PaddingSize"->{37}],

"trd3"->ThreadingLayer[Plus],

"rmp3"->Ramp,
"bnl3"->BatchNormalizationLayer[],


"dsc2"->ConvolutionLayer[256,{3},"PaddingSize"->{1}],
"drmp2"->Ramp,

"dsc4"->ConvolutionLayer[64,{3},"PaddingSize"->{1}],
"drmp4"->Ramp,

"pcnn"->ConvolutionLayer[3,{3},"PaddingSize"->{1}],
"trsp"->TransposeLayer[],
"sfmx"->SoftmaxLayer[]

|>,
{
NetPort["Input"]->"cnn"->"pol",
"pol"->"cnn1a"-> "cnn1a1"-> "cnn1a2"->"pol1a"->"trd1"->"rmp1"->"bnl1",
"pol"->"cnn1b"-> "cnn1b1"-> "cnn1b2"->"pol1b"->"trd1",
"pol"->"cnn1c"-> "cnn1c1"-> "cnn1c2"->"pol1c"->"trd1",

"bnl1"->"cnn2a"-> "cnn2a1"-> "cnn2a2"->"pol2a"->"trd2"->"rmp2"->"bnl2",
"bnl1"->"cnn2b"-> "cnn2b1"-> "cnn2b2"->"pol2b"->"trd2",
"bnl1"->"cnn2c"-> "cnn2c1"-> "cnn2c2"->"pol2c"->"trd2",

"bnl2"->"cnn3a"-> "cnn3a1"-> "cnn3a2"->"pol3a"->"trd3",
"bnl2"->"cnn3b"-> "cnn3b1"-> "cnn3b2"->"pol3b"->"trd3",
"bnl2"->"cnn3c"-> "cnn3c1"-> "cnn3c2"->"pol3c"->"trd3"->"rmp3"->"bnl3",

"bnl3"->"dsc2"->"drmp2",
"drmp2"-> "dsc4"-> "drmp4",


"drmp4"->"pcnn"->"trsp"->"sfmx"


,
"cnn1a"->"trd1",
"cnn1b"->"trd1",
"cnn1c"->"trd1",


"cnn2a"->"trd2",
"cnn2b"->"trd2",
"cnn2c"->"trd2",


"cnn3a"->"trd3",
"cnn3b"->"trd3",
"cnn3c"->"trd3"


},
"Input"->{4,300}
]


(* ::Chapter:: *)
(*Files*)


(* ::Subsubsection:: *)
(*Time*)


yearMonthDay=ToString/@Now[[1]][[1;;3]];
yearMonthDayStrings = StringJoin/@Weave[{"y","m","d"},yearMonthDay];
hourMinute=ToString/@Now[[1]][[4;;5]];
hourMinuteStrings= StringJoin/@Weave[{"h","s"},hourMinute];
timeStrings=Join[yearMonthDayStrings,hourMinuteStrings];
dateString = StringJoin[Riffle[timeStrings,"_"]]


(* ::Subsubsection:: *)
(*File Names*)


base="~/ec2";
experimentName = StringJoin[Riffle[{
"ademxapp",
"residual",
"inception",
"net_with",
"kernels",
"3,25,75",
"max_channels",
"512"
}, "_"]]

tmpDir=FileNameJoin[{base,"checkpoints_"<>dateString}];
dataDir=FileNameJoin[{base,"data"}];
resDir=FileNameJoin[{base,"results"}];
tmp = CreateDirectory[tmpDir];

trainFile = FileNameJoin[{dataDir, "train_set.mx"}];
validFile = FileNameJoin[{dataDir, "valid_set.mx"}];
testFile  = FileNameJoin[{dataDir, "test_set.mx"}];
outFile = FileNameJoin[{resDir,experimentName<>".mx"}];


(* ::Chapter:: *)
(*Load Data*)


Print["Loading Data"];


Print["\tTraining Set"];
trainingSet=Import[trainFile];
Print["\tValidation Set"];
validationSet=Import[validFile];
Print["\tTest Set"];
testSet=Import[testFile];


(* ::Chapter:: *)
(*Initialize Network*)


Print["Initializing Net"];
inet = NetInitialize[net];


(* ::Chapter:: *)
(*Train*)


Print["Begin Training"];
ntro=NetTrain[
	inet,
	trainingSet,
	All,
	ValidationSet->validationSet,
	TargetDevice->"GPU",
	LossFunction->CrossEntropyLossLayer["Probabilities"],
	Method->{"ADAM","LearningRate"->0.0001},
	BatchSize->20,
	TrainingProgressCheckpointing->{"Directory",tmp}
]
Print["Training done."];


(* ::Chapter:: *)
(*Export*)


Print["Exporting"];
Export[outFile, ntro]
