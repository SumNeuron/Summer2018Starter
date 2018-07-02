BeginPackage["UpSetChart`GraphicsUtilitiesSetsBarChart`"]
Needs["UpSetChart`Utilities`"]

SetsBarChart::usage=StringJoin[
  "SetsBarChart[",
  "sets, setNames, maxSetCardinality]"
];

Begin["`Private`"]

TooltipOfSetBar[setName_, set_]:=
StringJoin[
  "Set: " <> ToString[setName],
  "\nElements: " <> ToString[set]
];

CoordinatesOfSetBar[setIndex_, setValue_, maxSetCardinality_, radius_, translate_, spacing_]:=
{
  {
    maxSetCardinality - setValue,
    (2 radius + Last@spacing) setIndex + Last@translate - radius
  },
  {
    maxSetCardinality,
    (2 radius + Last@spacing) setIndex + Last@translate + radius
  }
}


Options[SetBar] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "Radius" -> 1,
  "Spacing" -> {1, 1},
  "Translate" -> {0,0}
};
SetBar[setIndex_, sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    value = Length[sets[setNames[[setIndex]]]],
    text = TooltipOfSetBar[setNames[[setIndex]], sets[setNames[[setIndex]]]]
  },
  {
    OptionValue["ColorFunction"][ value / maxSetCardinality ],
    Tooltip[
      Rectangle@@CoordinatesOfSetBar[setIndex, value, maxSetCardinality,
        OptionValue["Radius"],
        OptionValue["Translate"],
        OptionValue["Spacing"]
      ],
      text
    ]
  }
];


Options[SetsBarChart] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0}
};
SetsBarChart[sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {},

  Table[
    SetBar[setIndex, sets, setNames, maxSetCardinality,
      "ColorFunction" -> OptionValue["ColorFunction"],
      "Radius" -> OptionValue["IndicatorRadius"],
      "Translate" -> OptionValue["Translate"],
      "Spacing" -> OptionValue["IndicatorSpacing"]
    ]
    , {setIndex, Length@sets}
  ]
];



End[]
EndPackage[]
