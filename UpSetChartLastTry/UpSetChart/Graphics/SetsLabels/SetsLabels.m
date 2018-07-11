BeginPackage["UpSetChart`Graphics`SetsLabels`"]
Needs["UpSetChart`Utilities`"]

SetsLabels::usage=StringJoin[
  "SetsLabels[setNames, maxSetCardinality]"
];

Begin["`Private`"]


CoordinatesOfSetLabel[index_, radius_, spacing_]:=
{
  0,
  (2 radius + Last@spacing) index
}


Options[SetLabel] = {
  "Radius" -> 1,
  "Spacing" -> {0, 1}
};
SetLabel[index_, setNames_, fontsize_, OptionsPattern[]] :=
Text[
  Style[setNames[[index]], fontsize],
  CoordinatesOfSetLabel[index,
    OptionValue["Radius"],
    OptionValue["Spacing"]
  ]
  , {-1,0}
]




Options[SetsLabels] = {
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0},
  "FontSize" -> 12
};
SetsLabels[setNames_, OptionsPattern[]]:=
Translate[
  Table[
    SetLabel[
      i, setNames, OptionValue["FontSize"],
      "Radius" -> OptionValue["IndicatorRadius"],
      "Spacing" -> OptionValue["IndicatorSpacing"]
    ]
    , {i, Length@setNames}
  ],
  OptionValue["Translate"]
]





End[]
EndPackage[]
