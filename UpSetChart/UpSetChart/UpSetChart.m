BeginPackage["UpSetChart`"]

Needs["UpSetChart`Utilities`"];
Needs["UpSetChart`UniqueIntersections`"];
Needs["UpSetChart`Calculations`"];
Needs["UpSetChart`CalculationsUtilities`"];
Needs["UpSetChart`Graphics`"];


UpSetChart::usage="UpSetChart[sets]";

Begin["`Private`"]

Options[UpSetChart] = {
  "ColorGradient"->"DeepSeaColors",
  "DropEmpty"->True,
  "IntersectionSortBy" -> "Name",
  "SetSortBy" -> "Name",
  "Verbose"->True
};


UpSetChart[sets_?ListOfListsQ, opt: OptionsPattern[]]:=
UpSetChart[EnsureLabeledSets[sets], opt];

UpSetChart[sets_?LabeledSetsQ, opt: OptionsPattern[]]:=
Module[
  {
    calc, options,
    fsets, fcomp
  },


  options = OverwriteOptions[{opt}, UpSetChart, CalcThenSortAndFilter];
  calc = CalcThenSortAndFilter[sets, options]
  fsets = calc["sets"];
  fcomp = calc["comparisons"];

  options = OverwriteOptions[{opt}, UpSetChart, UpSetGraphics];
  UpSetGraphics[fsets, fcomp, options]
]
End[]
EndPackage[]
