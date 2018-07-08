BeginPackage["ArbitrarilySpace`"]


(* @SumNeuron I'd track this with an Association. We effectively have a LastQ test in the code, so if I'm understanding the problem right you can put If[lastQ, maxAss[#] = Max@{Lookup[maxAss, #, 0], Length@#2}]; in there and return that Association as well, *)

LevelSpacerFunction::usage=StringJoin[
  "LevelSpacerFunction[element, level, lastInLevelInstanceQ, baseSpacerSize]",
  " calculates the spacing between this element and the sequential one",
  " giventhe current level, and if the next element is in the same level or not."
];

LevelSizeFunction::usage=StringJoin[
  "LevelSizeFunction[element, level, baseObjectSize] calculates the size this",
  " element should be given the current level in relation to the baseObjectSize."
];


LevelColorFunction::usage=StringJoin[
  "LevelColorFunction[element, level, absoluteIndex, relativeIndex, maxDepth, colorGradient]",
  " calculates the color of the element given its level in relation to the maxDepth of all",
  " levels in the same dataset; this can take into account its absolute index in the dataset, ",
  " or relative index in its level instance."
];

ExtractListStructure::usage = StringJoin[
  "ExtractListStructure[list, atomQ] returns a list of tuples containing",
  " {element, absoluteIndex, level, relativeIndex, lastInLevelInstanceQ}.",
  " Treats things that match atomQ as elements."
];


PositionTuples::usage = StringJoin[
  "PositionTuples[extracted, spacingFunction, sizingFunction, colorFunction] ",
  " updates the tuples from ExtractListStructure to contain level and index based",
  " spacing, sizing, and coloring. Returns list of ",
  "{element, absoluteIndex, level, relativeIndex, lastInLevelInstanceQ, sizing, spacing, translation, color}."
]

AggregateTranslations::usage=StringJoin[
  "AggregateTranslations[a, b] takes two tuples from output of PositionTuples and",
  " accumulates the translation needed in b given a's size, post-spacing and translation."
];


ArbitrarilySpace::usage=StringJoin[
  "ArbitrarilySpace[list, sizingFunction, spacingFunciton, coloringFunction, atomQ]",
  " returns 8-ples containing each of the list elements {element, absoluteIndex,",
  " level, relativeIndex, lastInLevelInstanceQ, sizing, spacing, translation, color}."
];


ArbitrarilySpaceBarChart::usage=StringJoin[
  "ArbitrarilySpaceBarChart[ArbitrarilySpace[list]]: ",
  "Demonstration on how to use ArbitrarilySpace output for visualization."
];

Begin["`Private`"]
$BaseSpacerSize = 1;
$BaseObjectSize = 1;
$DefaultColorGradient = "DeepSeaColors";

LevelSpacerFunction[
  elementData_: {},
  level_Integer: 1,
  lastInLevelInstanceQ_: False,
  baseSpacerSize_:$BaseSpacerSize
] :=
With[
  {
   adjustedLevel = If[lastInLevelInstanceQ, level - 1, level]
   (* Last element in current instance of level,  needs spacing of previous level *)
  },
  If[adjustedLevel == 0, baseSpacerSize, baseSpacerSize / adjustedLevel]
]

LevelSizeFunction[elementData_: {}, level_Integer: 1, baseObjectSize_:$BaseObjectSize] := baseObjectSize ;

LevelColorFunction[
  elementData_: {},
  level_Integer: 1,
  absoluteIndex_Integer: 1,
  relativeIndex_Integer: 1,
  maxDepth_: 1,
  colorGradient_:$DefaultColorGradient
] := ColorData[colorGradient][level/maxDepth];


Options[ExtractListStructure] = { "AtomQ" -> Not@*ListQ };
ExtractListStructure[ll_, OptionsPattern[]] :=
Block[
  {
    i = 1,
    prev = None
  },
  Reap[
    MapIndexed[
      Function[
        If[OptionValue["AtomQ"]@#,
          If[ListQ@prev, prev[[5]] = False; Sow@prev];
          prev = {#, i++, Length@#2, Last@#2, None},
          (* else *)
          If[ListQ@prev, prev[[5]] = True; Sow@prev; prev = None]
        ];
       #
      ], ll, All
    ]
  ][[2, 1]]
];




Options[PositionTuple] = {
  "SizingFunction" -> LevelSizeFunction,
  "SpacingFunction" -> LevelSpacerFunction,
  "ColoringFunction" -> LevelColorFunction,
  "MaxDepth" -> 1
};

PositionTuple[
  tuple_List, (* a tuple from output ExtractListStructure *)
  OptionsPattern[]
] :=
With[
  {
    element = tuple[[1]],
    absoluteIndex = tuple[[2]],
    level = tuple[[3]],
    relativeIndex = tuple[[4]],
    lastInLevelInstanceQ = tuple[[5]]
  },
  Join[
    tuple,
    {
      OptionValue["SizingFunction"][element, level],
      OptionValue["SpacingFunction"][element, level, lastInLevelInstanceQ],
      0, (* aggregated spacing *)
      OptionValue["ColoringFunction"][element, level, absoluteIndex, relativeIndex, OptionValue["MaxDepth"]]
    }
  ]
]



Options[PositionTuples] = {
  "SizingFunction" -> LevelSizeFunction,
  "SpacingFunction" -> LevelSpacerFunction,
  "ColoringFunction" -> LevelColorFunction
};
PositionTuples[
  extracted_List,
  OptionsPattern[]
] :=
Module[
  {
    maxDepth = Max@extracted[[;; , 3]]
  },
  Map[
    PositionTuple[
      #,
      "SizingFunction" -> OptionValue["SizingFunction"],
      "SpacingFunction" -> OptionValue["SpacingFunction"],
      "ColoringFunction" -> OptionValue["ColoringFunction"],
      "MaxDepth" -> maxDepth
    ] &,
    extracted
  ]
]


AggregateTranslations[a_List, b_List] := Join[b[[;; -3]], { Total[a[[6 ;; 8]]], b[[-1]]}]

Options[ArbitrarilySpace] = {
  "SizingFunction" -> LevelSizeFunction,
  "SpacingFunction" -> LevelSpacerFunction,
  "ColoringFunction" -> LevelColorFunction,
  "AtomQ" -> Not@*ListQ
};
ArbitrarilySpace[
  list_List,
  OptionsPattern[]
] :=
FoldList[
  AggregateTranslations,
  PositionTuples[
    ExtractListStructure[
      list,
      "AtomQ"->OptionValue["AtomQ"]
    ],
    "SizingFunction" -> OptionValue["SizingFunction"],
    "SpacingFunction" -> OptionValue["SpacingFunction"],
    "ColoringFunction" -> OptionValue["ColoringFunction"]
  ]
]


ArbitrarilySpaceBarChart[as_List, horizontalQ_:True]:=
Map[
  With[
    {
      color = #[[-1]],
      size = #[[6]],
      data = #[[1]],
      index = #[[2]],
      level = #[[3]],
      rIndex = #[[4]],
      aggregatedSpacer = #[[-2]]
  },
  With[
    {
      rectCoord = If[horizontalQ, {size, data}, {data, size}],
      transCoord =  If[horizontalQ, {aggregatedSpacer, 0}, {0, aggregatedSpacer}]
    },

    Translate[
      {
        color,
        Tooltip[
          Rectangle[{0, 0}, rectCoord],
          StringJoin[
            "Data: "<>ToString[N[data]]<>"\n",
            "Level: "<>ToString[level]<>"\n",
            "Absolute Index: "<>ToString[index]<>"\n",
            "Relative Index: "<>ToString[rIndex]<>"\n"
          ]
        ]
      },
      transCoord
    ]
  ]
  ] &,
  as
]
End[]
EndPackage[]
