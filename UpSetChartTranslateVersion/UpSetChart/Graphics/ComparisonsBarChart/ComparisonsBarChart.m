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



CoordinatesOfComparisonBar[comparisonsIndex_, comparisonValue_, radius_, translate_, spacing_]:=
{
  (* x = (2 r + sBIX) index - r + sBSL + sBSL;
  y = (sBIY + 2 r) Length@setNames + r + sBIG; *)
  {
    (2 radius + First@spacing) comparisonsIndex - radius + First@translate,
    Last@translate
  },
  {
    (2 radius + First@spacing) comparisonsIndex + (*2*) radius + First@translate,
    Last@translate + comparisonValue
  }
}

(* Make the rectangle  *)

Options[ComparisonBar] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "Radius" -> 1,
  "Spacing" -> {1, 1},
  "Translate" -> {0,0}
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
        OptionValue["Translate"],
        OptionValue["Spacing"]
      ],
      text
    ]
  }
];



Options[ComparisonsBarChart] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0}
};
ComparisonsBarChart[comparisons_, maxComparisonCardinality_, setsNames_, OptionsPattern[]] :=
Module[
  {},
  (* Print[options]; *)
  Table[
    ComparisonBar[comparisonIndex, comparisons, setsNames, maxComparisonCardinality,
      "ColorFunction" -> OptionValue["ColorFunction"],
      "Radius" -> OptionValue["IndicatorRadius"],
      "Translate" -> OptionValue["Translate"],
      "Spacing" -> OptionValue["IndicatorSpacing"]
    ]
    , {comparisonIndex, Length@comparisons}
  ]
];


End[]
EndPackage[]
