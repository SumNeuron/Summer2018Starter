{
  Offset[
    offset,
    {
      (2 radius + First@spacing) comparisonIndex  - (radius / 4),
      Min[setIndices] (2 radius + Last@spacing)
    }
  ],
  Offset[
    offset,
    {
      (2 radius + First@spacing) comparisonIndex  + (2 radius / 4),
      Max[setIndices] (2 radius + Last@spacing)
    }
  ]
} BeginPackage["UpSetChart`Graphics`IndicatorGrid`"]

Needs["UpSetChart`Utilities`"]

IndicatorGrid::usage = StringJoin[
  "IndicatorGrid[setsNames, comparisonsNames]"
]

Begin["`Private`"]
$IndicatorFilled=Black;
$IndicatorNotFilled=Lighter@Lighter@Gray;
(* Helper function for whether or not to fill the circle *)
indicateQ[comparison_, set_] := MemberQ[comparison, set];
TooltipOfGridDisk[comparison_, set_]:=
StringJoin[
  "Set: " <> ToString[set],
  "\nIntersection: " <> ToString[comparison]
];


CoordinatesOfGridDisk[comparisonIndex_, setIndex_, radius_, spacing_, offset_]:=
Offset[
  offset,
  {
    (2 radius + First@spacing) comparisonIndex,
    (2 radius + Last@spacing) setIndex
  }
]

Options[IndicatorGridDisk] = { "Offset" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisk[comparisonIndex_, setIndex_, comparisonNames_, setNames_, OptionsPattern[]] :=
Module[
  {
      coordinates = CoordinatesOfGridDisk[comparisonIndex, setIndex,
        OptionValue["Radius"],
        OptionValue["Spacing"],
        OptionValue["Offset"]
      ],
      comp = comparisonNames[[comparisonIndex]],
      set = setNames[[setIndex]],
      text, filledQ
  },
  filledQ = indicateQ[comp, set];
  text=TooltipOfGridDisk[comp, set];
  {
    If[filledQ, $IndicatorFilled, $IndicatorNotFilled],
    Tooltip[Disk[coordinates, OptionValue["Radius"]], text]
  }
]


Options[IndicatorGridDisks] = { "Offset" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisks[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridDisk[
    comparisonIndex,
    setIndex,
    comparisonsNames,
    setsNames,
    "Radius"->OptionValue["Radius"],
    "Spacing"->OptionValue["Spacing"],
    "Offset"->OptionValue["Offset"]
  ]
 , {setIndex, Length@setsNames}, {comparisonIndex, Length@comparisonsNames}
]



Options[CoordinatesOfGridLine]:={
  "Width"->Automatic
};

CoordinatesOfGridLine[comparisonIndex_, setIndices_, radius_, spacing_, offset_, OptionsPattern[]]:=
With[
  {
    width = If[ OptionValue["Width"]==Automatic, radius / 4, OptionValue["Width"] ]
  },
  {
    Offset[
      offset,
      {
        (2 radius + First@spacing) comparisonIndex  - width,
        Min[setIndices] (2 radius + Last@spacing)
      }
    ],
    Offset[
      offset,
      {
        (2 radius + First@spacing) comparisonIndex  + (2 width),
        Max[setIndices] (2 radius + Last@spacing)
      }
    ]
  }
]



Options[IndicatorGridLine] = { "Offset" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridLine[comparisonIndex_, setNames_, comparisonNames_, OptionsPattern[]] :=
Module[
  {
    set, comp,
    setIndices,
    coordinates,
    text
  },
  comp = comparisonNames[[comparisonIndex]];
  setIndices = Flatten[Position[setNames, #] & /@ comp];
  If[Length[setIndices]==0,Return[{}]];

  set = First@setIndices;
  text = TooltipOfGridDisk[comp, set];
  coordinates =
  CoordinatesOfGridLine[
    comparisonIndex,
    setIndices,
    OptionValue["Radius"],
    OptionValue["Spacing"],
    OptionValue["Offset"]
  ];

  {
    Black,
    Tooltip[Rectangle@@coordinates,text]
  }
];


Options[IndicatorGridLines] = { "Radius" -> 1, "Spacing" -> {1, 1}, "Offset"->{0, 0} };
IndicatorGridLines[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridLine[
    comparisonIndex,
    setsNames,
    comparisonsNames,
    "Radius"->OptionValue["Radius"],
    "Spacing"->OptionValue["Spacing"],
    "Offset"->OptionValue["Offset"]
  ]
, {comparisonIndex, Length@comparisonsNames}
];


Options[IndicatorGrid] = {
  "Translate" -> {0, 0},
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Offset"->{0, 0}
}
IndicatorGrid[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Translate[
  {
    IndicatorGridDisks[setsNames, comparisonsNames,
      "Radius"->OptionValue["IndicatorRadius"],
      "Spacing"->OptionValue["IndicatorSpacing"],
      "Offset"->OptionValue["Offset"]
    ],
    IndicatorGridLines[setsNames, comparisonsNames,
      "Radius"->OptionValue["IndicatorRadius"],
      "Spacing"->OptionValue["IndicatorSpacing"],
      "Offset"->OptionValue["Offset"]
    ]
  },
  OptionValue["Translate"]
]




End[]
EndPackage[]
