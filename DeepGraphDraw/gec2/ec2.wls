#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*"Deep" Graph Drawing*)


(* ::Chapter::Closed:: *)
(*Supporting Functions*)


MakeData[vertices_, edgeProbability_, graphs_]:=
Normal/@DeleteDuplicates[AdjacencyMatrix/@RandomGraph[BernoulliGraphDistribution[vertices, edgeProbability], graphs]]


PartitionData[data_, ratios_]:=
Module[
  {
    amounts = Round/@(Length@data ratios),
    training,
    validation,
    test
  },

  training = RandomSample[data, amounts[[1]]];
  validation = RandomSample[Complement[data, training], amounts[[2]]];
  test = Complement[data, validation, training];

  <|
    "TrainingData"->training,
    "ValidationData"->validation,
    "TestData"->test
  |>
]


DiscretizeList[list_List]:=
With[
  {
    sorted = DeleteDuplicates[Sort[list]]
  },
  Flatten[First@Position[sorted, #] & /@ list]
];


DiscretizeCoordinates[coordinates_]:=
Module[
  {
    x = coordinates[[;; , 1]],
    y = coordinates[[;; , 2]],
    discretizedCoordinates
  },
  Partition[ Riffle[DiscretizeList[x], DiscretizeList[y]], {2} ];
];

Options[vertexCoordinates]= {
  "GraphLayout"->"LayeredEmbedding",
  "DiscretizeCoordinates"->False
};

vertexCoordinates[graph_, OptionsPattern[]] := Module[
  {
   layout = SetProperty[graph, GraphLayout -> OptionValue["GraphLayout"]],
   coordinates
  },
  coordinates = GraphEmbedding[layout];
  If[discretizeQ, coordinates = DiscretizeCoordinates[coordinates]];
  Return[AssociationThread[VertexList[graph], coordinates]]
];


Options[MakeDataset] = {
  "NumberOfVertices"->10,
  "ProbabilityOfEdge"->0.5,
  "GraphLayout"->"LayeredEmbedding",
  "DiscretizeCoordinates"->False
};
MakeDataset[n_, OptionsPattern[]] := Module[
  {
    matrices, amGraphs, coordinates, data, inp, out
  },

  matrices = MakeData[
    OptionValue["NumberOfVertices"],
    OptionValue["ProbabilityOfEdge"],
    n
  ];

  amGraphs = AdjacencyGraph /@ matrices;

  coordinates = vertexCoordinates[
    #,
    "GraphLayout"->OptionValue["GraphLayout"],
    "DiscretizeCoordinates"->OptionValue["DiscretizeCoordinates"]
  ]& /@ amGraphs;

  inp = "Input" -> # &/@ matrices;
  out = "Output" -> # &/@ Values[coordinates];
  data = Dataset[Association@@@Partition[Riffle[inp, out], {2}]];
  data
];

MakeCoordinateOptions[coordinates_List]:=
Table[i->coordinates[[i]], {i,Length@coordinates}]


Options[CompareForcedCoordinates] = {
  "GraphLayout" -> "LayeredEmbedding",
  "PrintReadyQ"->False
}
CompareForcedCoordinates[matrix_, coordinates_, OptionsPattern[]]:=
Module[
  {
    coordinateOptions = MakeCoordinateOptions[coordinates],
    original, predicted,
    vLabels = "Name"
  },

  If[OptionValue["PrintReadyQ"], vLables = Placed["Name", Tooltip]];
  (* graph = AdjacencyGraph[matrix];
  SetProperty[graph, GraphLayout -> OptionValue["GraphLayout"]] *)
  original = AdjacencyGraph[matrix,
    GraphLayout->OptionValue["GraphLayout"],
    VertexLabels -> vLabels
  ];

  predicted = AdjacencyGraph[matrix,
    VertexCoordinates -> coordinateOptions,
    VertexStyle->Red,
    VertexShapeFunction -> "Diamond",
    EdgeStyle->Black,
    VertexLabels -> vLabels
  ];

  GraphicsRow[{original, predicted}]
]



Options[MakeSimpleQDataset] = {
  "NumberOfVertices"->10,
  "ProbabilityOfEdge"->0.5,
  "GraphLayout"->"LayeredEmbedding",
  "DiscretizeCoordinates"->False
};
MakeSimpleQDataset[n_, OptionsPattern[]] :=
Module[
  {
    matrices, smatrices, all, inp, out, data
  },
  matrices = MakeData[OptionValue["NumberOfVertices"], OptionValue["ProbabilityOfEdge"], n];
  smatrices = MakeData[OptionValue["NumberOfVertices"], OptionValue["ProbabilityOfEdge"], n];

  smatrices = Table[
  Module[
    {
      rn = RandomInteger[{1, OptionValue["NumberOfVertices"]}],
      m = sm
    },
      m[[rn,rn]]=1;
      m
    ], {sm, smatrices}
  ];
  all = Join[matrices, smatrices];
  inp = "Input" -> # & /@ all;
  out = "Output" -> # & /@ SimpleGraphQ /@AdjacencyGraph/@ all;
  data = Dataset[Association @@@ Partition[Riffle[inp, out], {2}]];
  data
];


(* ::Chapter:: *)
(*Files*)


base="~/gec2";
out="graph_net.wlnet";


(* ::Subchapter:: *)
(*Layered Embedding*)


(* ::Section::Closed:: *)
(*Make Dataset and Partition*)


n=5000000;
nov = 10;
poe = 0.5;
glo = "LayeredEmbedding";
ratios= {0.7, 0.2, .1};
Print["Making Dataset"];
ds=MakeDataset[n, 
	"NumberOfVertices"->nov,
	"ProbabilityOfEdge"->poe,
	"GraphLayout"->glo,
	"DiscretizeCoordinates"->True
];
Print["Paritioning into Training, Validation, and Test set"];	
pd=PartitionData[ds, ratios];
Export[FileNameJoin[{base,"data",
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]
	<>"_discretized"
	<>".mx"
}],pd];


(* ::Section::Closed:: *)
(*Define Linear Network*)


Print["Defining Linear Network for"<>glo];
inputDimension=Dimensions[First[First[ds]]];
net = NetInitialize[NetChain[
	{
		LinearLayer[inputDimension],
		LinearLayer[First[inputDimension]^2],
		Tanh,
		LinearLayer[First[inputDimension]^3],
		Ramp,
		LinearLayer[First[inputDimension]^3*4],
		Tanh,
		LinearLayer[First[inputDimension]^3*4],
		Ramp,
		LinearLayer[First[inputDimension]^3*8],
		Tanh,
		LinearLayer[First[inputDimension]^3*8],
		Ramp,
		LinearLayer[First[inputDimension]^3*4],
		Tanh,
		LinearLayer[First[inputDimension]^2],
		Ramp,
		LinearLayer[First[inputDimension]^3],
		Tanh,
		LinearLayer[{First[inputDimension],2}]
	},
	"Input"-> inputDimension
]];


(* ::Section::Closed:: *)
(*Train Graph*)


Print["Starting training."];
tnet=NetTrain[
	net,
	pd["TrainingData"],
	All,
	ValidationSet->pd["ValidationData"],
	TargetDevice->"GPU"
];
Print["Training done."];


(* ::Section::Closed:: *)
(*Export Net*)


Print["Exporting Linear Network for"<>glo];
netName=StringJoin[
	"num_v_",ToString[nov],
	"_prob_e_",ToString[poe],
	"_layout_",glo,
	"_n_",ToString[n],
	"_ratios_",ToString[ratios],
	"_linear"<>"_discretized"
];

netFile = FileNameJoin[{base, "results", netName<>".mx"}];
Export[netFile,tnet]
netFile = FileNameJoin[{base, "results", netName<>".wlnet"}];
Export[netFile, tnet["TrainedNet"]]


(* ::Section::Closed:: *)
(*Define CNN*)


Print["Defining CNN Network for"<>glo];
net=NetInitialize@NetChain[
{
ConvolutionLayer[128,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[256,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[512,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[1024,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[2048,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[1024,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[512,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[256,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[128,{3}],
Tanh,
ConvolutionLayer[64,{3}],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[32,{3}],
Tanh,
ConvolutionLayer[10,{3}],
Tanh
},
"Input"-> inputDimension(*,
"Output"\[Rule] {First[inputDimension],2}*)
]


(* ::Section::Closed:: *)
(*Train CNN*)


Print["Starting training."];
tnet=NetTrain[
	net,
	pd["TrainingData"],
	All,
	ValidationSet->pd["ValidationData"],
	TargetDevice->"GPU"
];
Print["Training done."];


(* ::Section::Closed:: *)
(*Export*)


Print["Exporting CNN Network for"<>glo];
netName=StringJoin[
	"num_v_",ToString[nov],
	"_prob_e_",ToString[poe],
	"_layout_",glo,
	"_n_",ToString[n],
	"_ratios_",ToString[ratios],
	"_cnn"<>"_discretized"
];
netFile = FileNameJoin[{base, "results", netName<>".mx"}];
Export[netFile,tnet]
netFile = FileNameJoin[{base, "results", netName<>".wlnet"}];
Export[netFile, tnet["TrainedNet"]]


(* ::Subchapter:: *)
(*"SpectralEmbedding"*)


(* ::Section::Closed:: *)
(*Make Dataset and Partition*)


n=5000000;
nov = 10;
poe = 0.5;
glo = "SpectralEmbedding";
ratios= {0.7, 0.2, .1};
Print["Making Dataset"];
ds=MakeDataset[n, 
	"NumberOfVertices"->nov,
	"ProbabilityOfEdge"->poe,
	"GraphLayout"->glo,
	"DiscretizeCoordinates"->True
];
Print["Paritioning into Training, Validation, and Test set"];	
pd=PartitionData[ds, ratios];
Export[FileNameJoin[{base,"data",
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_discretized"
	<>".mx"
}],pd];


(* ::Section::Closed:: *)
(*Define Linear Network*)


Print["Defining Linear Network for"<>glo];
inputDimension=Dimensions[First[First[ds]]];
net = NetInitialize[NetChain[
	{
		LinearLayer[inputDimension],
		LinearLayer[First[inputDimension]^2],
		Tanh,
		LinearLayer[First[inputDimension]^3],
		Ramp,
		LinearLayer[First[inputDimension]^3*4],
		Tanh,
		LinearLayer[First[inputDimension]^3*8],
		Tanh,
		LinearLayer[First[inputDimension]^3*8],
		Ramp,
		LinearLayer[First[inputDimension]^3*4],
		Tanh,
		LinearLayer[First[inputDimension]^2],
		Ramp,
		LinearLayer[First[inputDimension]^3],
		Tanh,
		LinearLayer[{First[inputDimension],2}]
	},
	"Input"-> inputDimension
]];


(* ::Section::Closed:: *)
(*Train Graph*)


Print["Starting training."];
tnet=NetTrain[
	net,
	pd["TrainingData"],
	All,
	ValidationSet->pd["ValidationData"],
	TargetDevice->"GPU"
];
Print["Training done."];


(* ::Section::Closed:: *)
(*Export*)


Print["Exporting Linear Network for"<>glo];
netName=StringJoin[
	"num_v_",ToString[nov],
	"_prob_e_",ToString[poe],
	"_layout_",glo,
	"_n_",ToString[n],
	"_ratios_",ToString[ratios],
	"_linear"<>"_discretized"
];

netFile = FileNameJoin[{base, "results", netName<>".mx"}];
Export[netFile,tnet]
netFile = FileNameJoin[{base, "results", netName<>".wlnet"}];
Export[netFile, tnet["TrainedNet"]]


(* ::Section::Closed:: *)
(*Define CNN*)


Print["Defining CNN Network for"<>glo];
net=NetInitialize@NetChain[
{
ConvolutionLayer[128,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[256,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[512,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[1024,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[2048,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[1024,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[512,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[256,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[128,{3}],
Tanh,
ConvolutionLayer[64,{3}],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[32,{3}],
Tanh,
ConvolutionLayer[10,{3}],
Tanh
},
"Input"-> inputDimension(*,
"Output"\[Rule] {First[inputDimension],2}*)
]


(* ::Section::Closed:: *)
(*Train CNN*)


Print["Starting training."];
tnet=NetTrain[
	net,
	pd["TrainingData"],
	All,
	ValidationSet->pd["ValidationData"],
	TargetDevice->"GPU"
];
Print["Training done."];


(* ::Section::Closed:: *)
(*Export*)


Print["Exporting CNN Network for"<>glo];
netName=StringJoin[
	"num_v_",ToString[nov],
	"_prob_e_",ToString[poe],
	"_layout_",glo,
	"_n_",ToString[n],
	"_ratios_",ToString[ratios],
	"_cnn"<>"_discretized"
];

netFile = FileNameJoin[{base, "results", netName<>".mx"}];
Export[netFile,tnet]
netFile = FileNameJoin[{base, "results", netName<>".wlnet"}];
Export[netFile, tnet["TrainedNet"]]


(* ::Chapter:: *)
(*AM for simple nets*)


(* ::Section::Closed:: *)
(*Make Data*)


n=500000;
nov = 10;
ratios= {0.7, 0.2, .1};
Print["Making Dataset"];
ds=MakeSimpleQDataset[n, "NumberOfVertices"->nov];
Print["Paritioning into Training, Validation, and Test set"];	
pd=PartitionData[ds, ratios];
Export[FileNameJoin[{base,"data",
	"num_v_"<>ToString[nov]
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_simple_q"
	<>".mx"
}],pd];


(* ::Section::Closed:: *)
(*Define Network*)


Print["Defining CNN Network for simpleq"];
inputDimension=Dimensions[First[First[ds]]];
cnn=NetInitialize@NetChain[
{
ConvolutionLayer[128,{3}, "PaddingSize"->1],
Tanh,
ConvolutionLayer[256,{3}, "PaddingSize"->1],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[128,{3}],
Tanh,
ConvolutionLayer[64,{3}],
Ramp,
BatchNormalizationLayer[],
ConvolutionLayer[32,{3}],
Tanh,
ConvolutionLayer[10,{3}],
Tanh,
AggregationLayer[Mean],
LinearLayer[2],
SoftmaxLayer[]
},
"Input"-> inputDimension,
"Output"->NetDecoder[{"Class",{True,False}}]
]


(* ::Section:: *)
(*Train net*)


tnetr=NetTrain[
	cnn,
	pd["TrainingData"],
	All,
	ValidationSet->pd["ValidationData"],
	TargetDevice->"GPU"
];


(* ::Section:: *)
(*Export*)


Print["Exporting CNN Network for simpleq"];
netName=StringJoin[
	"num_v_",ToString[nov],
	"_ratios_",ToString[ratios],
	"_cnn"<>"_simple_q"
];

netFile = FileNameJoin[{base, "results", netName<>".mx"}];
Export[netFile,tnetr]
netFile = FileNameJoin[{base, "results", netName<>".wlnet"}];
Export[netFile, tnetr["TrainedNet"]]
