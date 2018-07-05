BeginPackage["UpSetChart`Graphics`SetsLabels`"]
Needs["UpSetChart`Utilities`"]

SetsLabels::usage=StringJoin[
  "SetsLabels[setNames, maxSetCardinality]"
];

Begin["`Private`"]


CoordinatesOfSetLabel[index_, radius_, translate_, spacing_]:=
{
  First@translate,
  (2 radius + Last@spacing) index + Last@translate
}


Options[SetLabel] = {
  "Radius" -> 1,
  "Spacing" -> {0, 1},
  "Translate" -> {0,0}
};
SetLabel[index_, setNames_, fontsize_, OptionsPattern[]] :=
Text[
  Style[setNames[[index]], fontsize],
  CoordinatesOfSetLabel[index,
    OptionValue["Radius"],
    OptionValue["Translate"],
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
Table[
  SetLabel[i, setNames, OptionValue["FontSize"],
  "Radius" -> OptionValue["IndicatorRadius"],
  "Translate" -> OptionValue["Translate"],
  "Spacing" -> OptionValue["IndicatorSpacing"]
  ]
  , {i, Length@setNames}]





End[]
EndPackage[]
