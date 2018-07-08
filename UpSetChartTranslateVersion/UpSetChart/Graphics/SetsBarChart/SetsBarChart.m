BeginPackage["UpSetChart`Graphics`SetsBarChart`"]

Needs["UpSetChart`Utilities`"]

SetsBarChart::usage=StringJoin[
  "SetsBarChart[",
  "sets, setNames, maxSetCardinality]"
];

Begin["`Private`"]

TooltipOfSetBar[setName_, set_]:=
StringJoin[
  "Set: " <> ToString[setName],
  "\nNumber of elements: " <> ToString[Length@set],
  "\nElements: " <> ToString[set]
];





CoordinatesOfSetBar[
  setIndex_,
  setValue_,
  maxSetCardinality_,
  radius_,
  spacing_,
  horizontalQ_:False
]:=
With[
  {
    x = 0,
    y = (2 radius + Last@spacing) (setIndex-1)
  },
  If[horizontalQ,
    {y, x},
    {x, y}
  ]
]

SizeOfSetBar[setValue_, radius_, maxSetCardinality_, horizontalQ_:False]:=
With[
  {
    x1 = maxSetCardinality-setValue,
    x2 = maxSetCardinality,
    y1= 0,
    y2= 2 radius
  },
  If[horizontalQ,
    {{y1,x1},{y2,x2}},
    {{x1,y1},{x2,y2}}
  ]
]


Options[SetBar] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "Radius" -> 1,
  "Spacing" -> {1, 1},
  "HorizontalQ"->False
};
SetBar[setIndex_, sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
With[
  {
    value = Length[sets[setNames[[setIndex]]]],
    text = TooltipOfSetBar[setNames[[setIndex]], sets[setNames[[setIndex]]]]
  },
  With[
    {
      coords = CoordinatesOfSetBar[
        setIndex, value, maxSetCardinality,
        OptionValue["Radius"],
        OptionValue["Spacing"],
        OptionValue["HorizontalQ"]
      ],
      size = SizeOfSetBar[
        value,
        OptionValue["Radius"],
        maxSetCardinality,
        OptionValue["HorizontalQ"]
      ]
    },
    Translate[
      {
        OptionValue["ColorFunction"][ value / maxSetCardinality ],
        Tooltip[
          Rectangle@@size,
          text
        ]
      },
      coords
    ]
  ]
]



Options[SetsBarChart] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0}
};
SetsBarChart[sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Translate[
  Table[
    SetBar[setIndex, sets, setNames, maxSetCardinality,
      "ColorFunction" -> OptionValue["ColorFunction"],
      "Radius" -> OptionValue["IndicatorRadius"],
      "Spacing" -> OptionValue["IndicatorSpacing"]
    ]
    , {setIndex, Length@sets}
  ],
  OptionValue["Translate"]
];



End[]

EndPackage[]
