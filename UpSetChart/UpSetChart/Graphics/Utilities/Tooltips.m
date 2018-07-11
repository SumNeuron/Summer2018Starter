BeginPackage["UpSetChart`Graphics`Utilities`Tooltips`"]

TooltipOfGrid::usage="TooltipOfGrid[comparisonName, setName]"
TooltipOfGridLine::usage="TooltipOfGridLine[comparison]"
TooltipOfSetBar::usage="TooltipOfSetBar[setName, setData]"
TooltipOfComparisonBar::usage="TooltipOfComparisonBar[comparisonName_, comparisonData]"

Begin["`Private`"]
TooltipOfGrid[comparison_, set_] :=
StringJoin[
  "Set: " <> ToString[set],
  "\nComparison: " <> ToString[comparison]
];

TooltipOfGridLine[comparison_] :=
StringJoin[
  "Comparison: " <> ToString[comparison]
];

TooltipOfSetBar[setName_, set_] :=
StringJoin[
  "Set: " <> ToString[setName],
  "\nNumber of elements: " <> ToString[Length@set],
  "\nElements: " <> ToString[set]
];

TooltipOfComparisonBar[comparisonName_, comparison_] :=
StringJoin[
  "Unique to: " <> ToString[comparisonName],
  "\nNumber of elements: " <> ToString[Length[comparison]],
  "\nElements: " <> ToString[comparison]
];
End[]
EndPackage[]
