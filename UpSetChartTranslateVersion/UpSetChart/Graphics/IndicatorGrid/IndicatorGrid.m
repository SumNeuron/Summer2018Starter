BeginPackage["UpSetChart`Graphics`IndicatorGrid`"]

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


CoordinatesOfGridDisk[comparisonIndex_, setIndex_, radius_, spacing_]:=
{radius, radius}

TranslateOfGridDisk[comparisonIndex_, setIndex_, radius_, spacing_]:=
{
  (2 radius + First@spacing) (comparisonIndex-1),
  (2 radius + Last@spacing) (setIndex-1)
}




Options[IndicatorGridDisk] = { "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisk[comparisonIndex_, setIndex_, comparisonNames_, setNames_, OptionsPattern[]] :=
Module[
  {
    coordinates = CoordinatesOfGridDisk[
      comparisonIndex,
      setIndex,
      OptionValue["Radius"],
      OptionValue["Spacing"]
    ],
    translate = TranslateOfGridDisk[
      comparisonIndex,
      setIndex,
      OptionValue["Radius"],
      OptionValue["Spacing"]
    ],
    comp = comparisonNames[[comparisonIndex]],
    set = setNames[[setIndex]],
    text, filledQ
  },
  filledQ = indicateQ[comp, set];
  text = TooltipOfGridDisk[comp, set];

  Translate[
    {
      If[filledQ, $IndicatorFilled, $IndicatorNotFilled],
      Tooltip[Disk[coordinates, OptionValue["Radius"]], text]
    },
    translate
  ]
]


Options[IndicatorGridDisks] = { "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisks[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridDisk[
    comparisonIndex,
    setIndex,
    comparisonsNames,
    setsNames,
    "Radius"->OptionValue["Radius"],
    "Spacing"->OptionValue["Spacing"]
  ]
 , {setIndex, Length@setsNames}, {comparisonIndex, Length@comparisonsNames}
]


CoordinatesOfGridLine[comparisonIndex_, setIndices_, radius_, spacing_]:=
{
  {
    0,
    0
  },
  {
    radius,
    (Max[setIndices]-1) (2 radius) - radius / 2
  }
}

TranslateOfGridLine[comparisonIndex_, setIndices_, radius_, spacing_]:=
{
  {
    (2 radius + First@spacing) (comparisonIndex-1) + radius /2,
    (Min[setIndices]-1) (2 radius + Last@spacing) + radius / 2
  }
}


Options[IndicatorGridLine] = { "Radius" -> 1, "Spacing" -> {1, 1} };
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
    OptionValue["Spacing"]
  ];
  translate =
  TranslateOfGridLine[
    comparisonIndex,
    setIndices,
    OptionValue["Radius"],
    OptionValue["Spacing"]
  ];
  Translate[
    {
      Black,
      Tooltip[Rectangle@@coordinates,text]
    },
    translate
  ]
];


Options[IndicatorGridLines] = { "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridLines[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridLine[
    comparisonIndex,
    setsNames,
    comparisonsNames,
    "Radius"->OptionValue["Radius"],
    "Spacing"->OptionValue["Spacing"]
  ]
  , {comparisonIndex, Length@comparisonsNames}
];


Options[IndicatorGrid] = {
  "Translate" -> {0, 0},
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1}
}
IndicatorGrid[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Translate[
  {
    IndicatorGridDisks[setsNames, comparisonsNames,
      "Radius"->OptionValue["IndicatorRadius"],
      "Spacing"->OptionValue["IndicatorSpacing"]
    ],
    IndicatorGridLines[setsNames, comparisonsNames,
      "Radius"->OptionValue["IndicatorRadius"],
      "Spacing"->OptionValue["IndicatorSpacing"]
    ]
  },
  OptionValue["Translate"]
]




End[]
EndPackage[]
