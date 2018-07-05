#!/usr/bin/env wolframscript
(* ::Package:: *)

#!/usr/bin/env wolframscript


(* ::Title:: *)
(*Detexon*)


(* ::Text:: *)
(*needs files:*)
(**)
(*~/data/train_set.mx*)
(*~/data/valid_set.mx*)
(*~/data/test_set.mx*)
(**)
(*and defaults output (OVERWRITES!)*)
(*~/results/trained_net.wlnet*)


(* ::Chapter:: *)
(*Functions*)


(* ::Subchapter::Closed:: *)
(*Neural Network Architectures*)


AdemxappResidualCNN1DBlock[inputDimension_, name_, residualBNQ_]:=
With[{scale=If[residualBNQ,2,1]}, 
Module[
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
		name<>"_cnn1d_branch_main"->ConvolutionLayer[scale*First@inputDimension,{3},"PaddingSize"->{1}],
		If[residualBNQ,Nothing,name<>"_bnl_main"->BatchNormalizationLayer[]],
		name<>"_ramp_branch_main"->ElementwiseLayer[Ramp],
		name<>"_cnn1d_branch_main_a"->ConvolutionLayer[scale*First@inputDimension,{3},"PaddingSize"->{1}],
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


(* ::Subchapter:: *)
(*Classifier Measurements*)


(* ::Section::Closed:: *)
(*GPU Version (written by Matteo)*)


GPUClassifierMeasurements[net_, set_] := Block[
  {predictions, labels, pairs, confMat},
  
  predictions = net[set[[All, 1]], TargetDevice -> "GPU"];
  labels = set[[All, 2]];
  
  pairs = Transpose@{labels, predictions};
  
  confMat = 
   Map[Count[
      pairs, #] &, {{{"Exon", "Exon"}, {"Exon", 
       "Intron"}}, {{"Intron", "Exon"}, {"Intron", "Intron"}}}, {2}];
  
  <|
   "Accuracy" -> N@Mean@Boole[SameQ @@@ pairs], 
   "ConfusionMatrix" -> 
    TableForm[confMat, 
     TableHeadings -> {{"ExonA", "IntronA"}, {"ExonP", "IntronP"}}]
   |>
  ]


(* ::Section::Closed:: *)
(*Convert Dataset to Rules for Classifier Measurements*)


cmDataset[networkData_Dataset]:=(Rule@@Normal[#][[All,2]])&/@Normal[networkData]


(* ::Chapter:: *)
(*Pipeline*)


(* ::Subchapter:: *)
(*Declare Files Data*)


base = "~/ec2";
trainFile = FileNameJoin[{base, "data", "1_10_train_set.mx"}];
validFile = FileNameJoin[{base, "data", "1_10_valid_set.mx"}];
testFile  = FileNameJoin[{base, "data","1_10_test_set.mx"}];
outFile = FileNameJoin[{base, "results", "1_10_trained_net.wlnet"}];


(* ::Subchapter:: *)
(*Load Data*)


Print["Loading Data"];


Print["\tTraining Set"];
trainingSet=Import[trainFile]; (*$ScriptCommandLine[[1]]*)
Print["\tValidation Set"];
validationSet=Import[validFile];
Print["\tTest Set"];
testSet=Import[testFile];


(* ::Subchapter:: *)
(*Define Network*)


Print["Initializing Net"];


(*inputDimension=Dimensions@First@imgs;*)
net=NetInitialize[NetGraph[
	<|
		"conv_1a"->ConvolutionLayer[64,{3},"PaddingSize"->{1}],
		"pool_1"->PoolingLayer[{3},"PaddingSize"->{1}],
		"arcb_1_upsample"-> AdemxappResidualCNN1DBlock[{64, Automatic}, "arcb_1_upsample", True],
		"arcb_1_res_a"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_1_res_a", False],
		"arcb_1_res_b"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_1_res_b", False],
		"pool_2"->PoolingLayer[{3},"PaddingSize"->{1}],
		"arcb_2_upsample"-> AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_2_upsample", True],
		"arcb_2_res_a"-> AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_2_res_a", False],
		"arcb_2_res_b"-> AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_2_res_b", False],
		"pool_3"->PoolingLayer[{3},"PaddingSize"->{1}],
		"arcb_3_upsample"-> AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_3_upsample", True],
		"arcb_3_res_a"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_a", False],
		"arcb_3_res_b"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_b", False],
		"arcb_3_res_c"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_c", False],
		"arcb_3_res_d"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_d", False],
		"arcb_3_res_e"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_e", False],
		"pool_4"->PoolingLayer[{3},"PaddingSize"->{1}],
		"arcb_4_upsample"-> AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_4_upsample", True],
		"arcb_4_res_a"-> AdemxappResidualCNN1DBlock[{1024, Automatic}, "arcb_4_res_a", False],
		"arcb_4_res_b"-> AdemxappResidualCNN1DBlock[{1024, Automatic}, "arcb_4_res_b", False],
		"pool_5"->PoolingLayer[{3},"PaddingSize"->{1}],
		"bn_6"->BatchNormalizationLayer[],
		"relu_6"->Ramp,
		"conv_6"->ConvolutionLayer[512,{3},"PaddingSize"->{1}],
		"relu_6a"->Ramp,
		"conv_6a"->ConvolutionLayer[3,{3},"PaddingSize"->{1}],
		"transpose"->TransposeLayer[],
		"softmax"->SoftmaxLayer[]
	|>, {
		"conv_1a"->"pool_1",
		"pool_1"-> "arcb_1_upsample"->"arcb_1_res_a"->"arcb_1_res_b"-> "pool_2",
		"pool_2"-> "arcb_2_upsample"->"arcb_2_res_a"->"arcb_2_res_b"-> "pool_3",
		"pool_3"->"arcb_3_upsample"->"arcb_3_res_a"->"arcb_3_res_b"->"arcb_3_res_c"->"arcb_3_res_d"->"arcb_3_res_e"-> "pool_4",
		"pool_4"->"arcb_4_upsample"->"arcb_4_res_a"->"arcb_4_res_b"-> "pool_5",
		"pool_5"-> "bn_6"->"relu_6"->"conv_6"->"relu_6a"-> "conv_6a"->"transpose"-> "softmax"
	},
	"Input"->{4,300}
]];


(* ::Subchapter:: *)
(*Train Network*)


trainedNetResultObject=
NetTrain[
	net,
	trainingSet,
	All,
	ValidationSet->validationSet,
	TargetDevice->"GPU",
	LossFunction->CrossEntropyLossLayer["Probabilities"],
	(*MaxTrainingRounds\[Rule]200,*)
	Method->{"ADAM","LearningRate"->0.0001}
	(* Output Learning Rate (not needed if set via Method Option )*)
];


Print["Save in default save file.\n Exporting Network"];
Export[outFile, trainedNetResultObject];


(* ::Subchapter:: *)
(*Measurements of Trained Network*)


(*Print["Calculating Classifier Measurments"];
Print["\tTest Set"];
cmTest=GPUClassifierMeasurements[trainedNet,cmDataset[testSet]];
aTest=cmTest["Accuracy"];
Print["\t\tAccuracy:\t"<>ToString[aTest]];
Print["\tTraining Set"];
cmTrain=GPUClassifierMeasurements[trainedNet,cmDataset[trainingSet]];
aTrain=cmTrain["Accuracy"];
Print["\t\tAccuracy:\t"<>ToString[aTrain]];*)


(* ::Subchapter:: *)
(*Export*)


(*Print["Exporting Network"];

fileName="net_test_accuracy_"<>ToString[aTest]<>"_train_accuracy_"<>ToString[aTrain]<>".wlnet"
outFile = FileNameJoin[{base, "results","nets",fileName}];
Export[outFile, trainedNet];
*)


