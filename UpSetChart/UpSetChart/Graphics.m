BeginPackage["UpSetChart`Graphics`"]

Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`GraphicsUtilities`"]


UpSetGraphics::usage="UpSetGraphics[sets, comparisons]"
Begin["`Private`"]

Options[UpSetGraphics] = Options[GraphicComponents];
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
  Graphics[GraphicComponents[sets, comparisons, options]]
]


End[]
EndPackage[]
