BeginPackage["UpSetChart`Graphics`ComparisonsBarChart`"]
Needs["UpSetChart`Utilities`"]

ComparisonsBarChart::usage=StringJoin[
  "ComparisonsBarChart[",
  "sets, comparisons, maxComparisonCardinality]"
];

Begin["`Private`"]
(* Helper function for ComparisonRectangle tooltip *)
ComparisonBarTooltip[index_, comparisons_, setNames_] :=
StringJoin[
  "Unique to: "<>ToString[Keys[comparisons][[index]]],
  "\nNumber of elements: "<>ToString[Length[comparisons[[index]]]],
  "\nElements: "<>ToString[comparisons[[index]]]
];


(* Offset is needed to move over by the absolute pixels of the text labels to the left *)
CoordinatesOfComparisonBar[comparisonsIndex_, comparisonValue_, radius_, spacing_, offset_]:=
  {
    Offset[
      offset,
      {
        (2 radius + First@spacing) comparisonsIndex - radius,
        0
      }
    ],
    Offset[
      offset,
      {
        (2 radius + First@spacing) comparisonsIndex + radius,
        0 + comparisonValue
      }
  ]
}


(* Make a rectangle  *)
Options[ComparisonBar] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "Radius" -> 1,
  "Spacing" -> {1, 1},
  "Offset" -> {0, 0}
};
ComparisonBar[comparisonsIndex_, comparisons_, setNames_, maxComparisonCardinality_, OptionsPattern[]] :=
Module[
  {
    value = Length[comparisons[[comparisonsIndex]]],
    text = ComparisonBarTooltip[comparisonsIndex, comparisons, setNames]
  },
  {
    OptionValue["ColorFunction"][ value / maxComparisonCardinality ],
    Tooltip[
      Rectangle@@CoordinatesOfComparisonBar[comparisonsIndex, value,
        OptionValue["Radius"],
        OptionValue["Spacing"],
        OptionValue["Offset"]
      ],
      text
    ]
  }
];



(* Make all rectangles *)
Options[ComparisonsBarChart] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0},
  "Offset"->{0,0}
};
ComparisonsBarChart[comparisons_, maxComparisonCardinality_, setsNames_, OptionsPattern[]] :=
Translate[
  Table[
    ComparisonBar[
      comparisonIndex,
      comparisons,
      setsNames,
      maxComparisonCardinality,
      "ColorFunction" -> OptionValue["ColorFunction"],
      "Radius" -> OptionValue["IndicatorRadius"],
      "Spacing" -> OptionValue["IndicatorSpacing"],
      "Offset" -> OptionValue["Offset"]
    ]
    , {comparisonIndex, Length@comparisons}
  ],
  OptionValue["Translate"]
]



End[]
EndPackage[]
