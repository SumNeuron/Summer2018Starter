BeginPackage["UpSetChart`Graphics`"]

Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`GraphicsUtilities`"]


UpSetGraphics::usage="UpSetGraphics[sets, comparisons]"
Begin["`Private`"]

Options[UpSetGraphics] = Options[GraphicComponents];
UpSetGraphics[sets_, comparisons_, opt: OptionsPattern[]]:= Module[
  {
    options = OverwriteOptions[{opt}, UpSetGraphics, GraphicComponents]
  },
  Graphics[GraphicComponents[sets, comparisons, options]]
]


End[]
EndPackage[]
