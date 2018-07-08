BeginPackage["UpSetChart`"]

DeclarePackage["UpSetChart`Utilities`",
  {
    "EnsureLabeledSets", "LabeledSetsQ",
    "EnsureSets", "ListToSet", "ListOfStringsQ", "ListOfListsQ",
    "LabelSets", "RepeatCharacter", "NLabelsFromLibrary",
    "OverwriteOptions"
  }
];

DeclarePackage["UpSetChart`UniqueIntersections`", {"UniqueIntersections"}]
DeclarePackage["UpSetChart`Calculations`", {"CalcThenSortAndFilter"}]
DeclarePackage["UpSetChart`TestData`", {"DummyData", "RandomData"}]
DeclarePackage["UpSetChart`Graphics`", {"UpSetGraphics"}]


(* Needs["UpSetChart`UniqueIntersections`"]; *)


(* DeclarePackage["TestPackage`Component2`", {"foo2","bar2"}];
DeclarePackage["TestPackage`Component2`Subcomponent2`", {"deeperFoo2","deeperBar2"}]} *)

UpSetChart::usage="UpSetChart[sets]";

(*
UpSetChart has two main components, the calculation of the elements unique to
each comparison and the visualizaiton.

The unique intersections is calculated by UniqueIntersections.

For the UpSetChart, however, this is wrapped by UpSetChartCalculations, which also
might drop empty sets / comparisons and sort sets / comparisons.


UpSetChart consists of 4 main chart elements:
  1. a "BarChart" for the cardinality of sets
  2. a "BarChart" for the cardinality of comparisons
  3. labels for (1)
  4. a graphic based indicator for what comparison bar in (2) corresponds to.

Roughly sketching out the layout:

      (2)
(1)(3)(4)

                                       [ Comparison Card. BarChart → ]
[ Set Card. BarChart ↓ ] [ Labels ↓ ]  [ ↓       Indicators        → ]


Accordingly there are 4 functions for making these elements

(1) UpSetChartSets
(2) UpSetChartComparisons
(3) UpSetChartSetLabels
(4) UpSetChartComparisonsIndicator

so lets look at the tree of the function calls:

UpSetChart
|
| (Calculations)
|----| UpSetChartCalculations
|    |----| DropEmpty (remove empty sets)
|    |----| UniqueIntersections
|    |----| DropEmpty (remove empty comparisons)
|    |----| SortSets
|    |----| SortComparisons
|
| (Graphics)
|----| UpSetChartSets                           (1)
|    |----| SetRectangle
|
|----| UpSetChartComparisons                    (2)
|    |----| ComparisonRectangle
|         |----| TooltipComparisonRectange
|
|----| UpSetChartSetLabels                      (3)
|    |----| UpSetSetLabel
|
|----| UpSetChartComparisonsIndicator           (4)
     |----| ComparisonsIndicatorDisks
     |    |----| $Disk
     |
     |----| ComparisonsIndicatorLines
          |----| $Connector



*)

Begin["`Private`"]

Options[UpSetChart] = {
  "ColorGradient"->"DeepSeaColors",
  "DropEmpty"->True,
  "IntersectionSortBy" -> "Name",
  "SetSortBy" -> "Name",
  "Verbose"->True
};


(* UpSetChart[sets_?ListOfListsQ, opt: OptionsPattern[]]:= *)
(* UpSetChart[sets_, opt: OptionsPattern[]]:=
UpSetChart[EnsureLabeledSets[sets], opt]; *)

(* UpSetChart[sets_?LabeledSetsQ, opt: OptionsPattern[]]:= *)
UpSetChart[sets_, opt: OptionsPattern[]]:=
Module[
  {
    calc, options,
    fsets, fcomp
  },

  options = OverwriteOptions[{opt}, UpSetChart, CalcThenSortAndFilter];
  calc = CalcThenSortAndFilter[sets, options];
  fsets = calc["sets"];
  fcomp = calc["comparisons"];

  options = OverwriteOptions[{opt}, UpSetChart, UpSetGraphics];
  UpSetGraphics[fsets, fcomp, options]
]
End[]
EndPackage[]
