BeginPackage["DeepGraphDraw`"]


MakeDataset::usage="MakeDataset[n]"
MakeCoordinateOptions::usage="MakeCoordinateOptions[coordinates]"
PartitionData::usage="PartitionData[dataset, ratios]"
MakeSimpleQDataset::usage=""
CompareForcedCoordinates::usage="CompareForcedCoordinates[matrix, coordinates]"
Begin["`Private`"]
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


End[]
EndPackage[]
