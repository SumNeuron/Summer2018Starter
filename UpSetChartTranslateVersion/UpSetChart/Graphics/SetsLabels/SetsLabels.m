BeginPackage["UpSetChart`Graphics`SetsLabels`"]
Needs["UpSetChart`Utilities`"]

SetsLabels::usage=StringJoin[
  "SetsLabels[setNames, maxSetCardinality]"
];

Begin["`Private`"]


CoordinatesOfSetLabel[index_, radius_, spacing_, horizontalQ_:False]:=
With[
  {
    x = First@spacing,
    y = (2 radius + Last@spacing) (index-1) + radius
  },
  If[horizontalQ,
    {y, x},
    {x, y}
  ]
]


Options[SetLabel] = {
  "Radius" -> 1,
  "Spacing" -> {0, 1},
  "HorizontalQ" -> False
};
SetLabel[index_, setNames_, fontsize_, OptionsPattern[]] :=
With[
  {
    coords = CoordinatesOfSetLabel[
      index,
      OptionValue["Radius"],
      OptionValue["Spacing"],
      OptionValue["HorizontalQ"]
    ]
  },
  Translate[
    Text[Style[setNames[[index]], fontsize], {0,0}, {-1,0}],
    coords
  ]
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
        i,
        setNames,
        OptionValue["FontSize"],
        "Radius" -> OptionValue["IndicatorRadius"],
        "Spacing" -> OptionValue["IndicatorSpacing"]
      ]
    , {i, Length@setNames}
  ],
  OptionValue["Translate"]
]



End[]
EndPackage[]
