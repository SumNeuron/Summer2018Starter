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


(* ::Section:: *)
(*Make Dataset and Partition*)


n=10000000;
nov = 10;
poe = 0.5;
glo = "LayeredEmbedding";
ratios= {0.7, 0.2, .1};
Print["Making Dataset"];
ds=MakeDataset[n, 
	"NumberOfVertices"->nov,
	"ProbabilityOfEdge"->poe,
	"GraphLayout"->glo,
	"DiscretizeCoordinates"->False
];
Print["Paritioning into Training, Validation, and Test set"];	
pd=PartitionData[ds, ratios];
Export[FileNameJoin[{base,"data",
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]
	<>".mx"
}],pd];


(* ::Section:: *)
(*Define Linear Network*)


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
		Ramp,
		LinearLayer[First[inputDimension]^3],
		Tanh,
		LinearLayer[{First[inputDimension],2}]
	},
	"Input"-> inputDimension
]];


(* ::Section:: *)
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


netFile = FileNameJoin[{
	base, 
	"results", 
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_linear"
	<>".mx"
}]
Export[netFile,tnet]


(* ::Section:: *)
(*Define CNN*)


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


(* ::Section:: *)
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


(* ::Section:: *)
(*Export*)


netFile = FileNameJoin[{
	base, 
	"results", 
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_cnn"
	<>".mx"
}]
Export[netFile,tnet]


(* ::Subchapter:: *)
(*"SpringElectricalEmbedding"*)


(* ::Section:: *)
(*Make Dataset and Partition*)


n=10000000;
nov = 10;
poe = 0.5;
glo = "SpringElectricalEmbedding";
ratios= {0.7, 0.2, .1};
Print["Making Dataset"];
ds=MakeDataset[n, 
	"NumberOfVertices"->nov,
	"ProbabilityOfEdge"->poe,
	"GraphLayout"->glo,
	"DiscretizeCoordinates"->False
];
Print["Paritioning into Training, Validation, and Test set"];	
pd=PartitionData[ds, ratios];
Export[FileNameJoin[{base,"data",
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]
	<>".mx"
}],pd];


(* ::Section:: *)
(*Define Linear Network*)


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
		Ramp,
		LinearLayer[First[inputDimension]^3],
		Tanh,
		LinearLayer[{First[inputDimension],2}]
	},
	"Input"-> inputDimension
]];


(* ::Section:: *)
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


netFile = FileNameJoin[{
	base, 
	"results", 
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_linear"
	<>".mx"
}]
Export[netFile,tnet]


(* ::Section:: *)
(*Define CNN*)


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


(* ::Section:: *)
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


(* ::Section:: *)
(*Export*)


netFile = FileNameJoin[{
	base, 
	"results", 
	"num_v_"<>ToString[nov]
	<>"_prob_e_"<>ToString[poe]
	<>"_layout_"<>glo
	<>"_n_"<>ToString[n]
	<>"_ratios_"<>ToString[ratios]<>"_cnn"
	<>".mx"
}]
Export[netFile,tnet]
