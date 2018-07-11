BeginPackage["UpSetChart`Graphics`"]



Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`Calculations`Utilities`"]
Needs["UpSetChart`Graphics`Utilities`"]
Needs["UpSetChart`Graphics`Dynamic`"]
(* DeclarePackage["UpSetChart`Graphics`Utilities`", {"GraphicComponents"}] *)


UpSetGraphics::usage="UpSetGraphics[sets, comparisons]"
InteractiveUpSetGraphics::usage="InteractiveUpSetGraphics[sets, comparisons]"
Begin["`Private`"]

Options[UpSetGraphics] = Join[
  {
    "ImageSize" -> Automatic
  },
  Options[GraphicComponents]
];
(* Options[GraphicComponents] = {
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "ComponentSpacing" -> {1, 1},
  "ColorGradient" -> "DeepSeaColors",
  "FontSize"->12
} *)
UpSetGraphics[sets_, comparisons_, opt: OptionsPattern[]]:= Module[
  {
    options = OverwriteOptions[{opt}, UpSetGraphics, GraphicComponents]
  },
  Graphics[GraphicComponents[sets, comparisons, options], ImageSize->OptionValue["ImageSize"]]
]

Options[InteractiveUpSetGraphics] = Join[
  {
    "ImageSize" -> {Automatic, 1}
  },
  Options[GraphicComponents]
];

InteractiveUpSetGraphics[sets_, comparisons_, opt: OptionsPattern[]]:=
With[
  {
    options = OverwriteOptions[{opt}, InteractiveUpSetGraphics, ComponentsByGroupedComparisons],
    opt2 = OverwriteOptions[{opt}, InteractiveUpSetGraphics, InteractiveUpSetComponents],
    grouped = GroupComparisonsByNumberOfSetsCompared[comparisons]
  },
  InteractiveUpSetComponents[ComponentsByGroupedComparisons[sets, grouped, options], opt2]
]


End[]
EndPackage[]
