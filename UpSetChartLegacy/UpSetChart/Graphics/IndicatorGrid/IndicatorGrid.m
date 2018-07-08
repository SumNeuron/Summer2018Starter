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


CoordinatesOfGridDisk[comparisonIndex_, setIndex_, radius_, translate_, spacing_]:=
{
  (2 radius + First@spacing) comparisonIndex + First@translate,
  (2 radius + Last@spacing) setIndex + Last@translate
}


Options[IndicatorGridDisk] = { "Translate" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisk[comparisonIndex_, setIndex_, comparisonNames_, setNames_, OptionsPattern[]] :=
Module[
  {
      coordinates = CoordinatesOfGridDisk[comparisonIndex, setIndex,
        OptionValue["Radius"],
        OptionValue["Translate"],
        OptionValue["Spacing"]
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


Options[IndicatorGridDisks] = { "Translate" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridDisks[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridDisk[
    comparisonIndex,
    setIndex,
    comparisonsNames,
    setsNames,
    "Radius"->OptionValue["Radius"],
    "Translate"->OptionValue["Translate"],
    "Spacing"->OptionValue["Spacing"]
  ]
 , {setIndex, Length@setsNames}, {comparisonIndex, Length@comparisonsNames}
]


CoordinatesOfGridLine[comparisonIndex_, setIndices_, radius_, translate_, spacing_]:=
{
  {
    (2 radius + First@spacing) comparisonIndex  + First@translate  - radius / 3,
    Min[setIndices] (2 radius + Last@spacing) + Last@translate
  },
  {
    (2 radius + First@spacing) comparisonIndex  + First@translate  + radius / 3 2,
    Max[setIndices] (2 radius + Last@spacing) + Last@translate
  }
}


Options[IndicatorGridLine] = { "Translate" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
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
    OptionValue["Translate"],
    OptionValue["Spacing"]
  ];

  {
    Black,
    Tooltip[Rectangle@@coordinates,text]
  }
];


Options[IndicatorGridLines] = { "Translate" -> {0, 0}, "Radius" -> 1, "Spacing" -> {1, 1} };
IndicatorGridLines[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Table[
  IndicatorGridLine[
    comparisonIndex,
    setsNames,
    comparisonsNames,
    "Radius"->OptionValue["Radius"],
    "Translate"->OptionValue["Translate"],
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
{
  IndicatorGridDisks[setsNames, comparisonsNames,
    "Radius"->OptionValue["IndicatorRadius"],
    "Translate"->OptionValue["Translate"],
    "Spacing"->OptionValue["IndicatorSpacing"]
  ],
  IndicatorGridLines[setsNames, comparisonsNames,
    "Radius"->OptionValue["IndicatorRadius"],
    "Translate"->OptionValue["Translate"],
    "Spacing"->OptionValue["IndicatorSpacing"]
  ]
}




End[]
EndPackage[]
