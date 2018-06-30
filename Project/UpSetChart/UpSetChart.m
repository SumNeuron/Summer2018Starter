BeginPackage["UpSetChart`"]

(* Needs["Helpers`"] *)

UpSetChart::usage = "UpSetChart[Association[setName->List[element1, element2,..., elementN]],...].";
UniqueIntersections::usage = StringJoin[
  "UniqueIntersections[sets, Options] calculates ",
  "the elements unique to an intersection given all passed sets."
]

Begin["`Private`"]

Options[UniqueIntersections] = {
  Verbose -> True
};

UniqueIntersections[sets_, OptionsPattern[]] := Module[
  {
    setNames,
    numberOfSets,

    comparisons,
    comparisonsGrouppedByLength,
    seenElements,
    intersections,

    setIndex,
    interesectionsOfSetIndexSets,
    elementsUniqueToInteresectionsOfSetIndexSets,

    msg
  },

  (* save recomputation *)
  setNames = Keys[sets];
  numberOfSets = Length[setNames];

  (* 2^n scales poorly *)
  msg="Hang tight! This requires " <> ToString[2^Length@setNames] <> " comparisons."
  FunctionLog[OptionValue[Verbose],msg];

  (* will test intersections *)
  comparisons = Subsets[setNames];
  comparisonsGrouppedByLength = GroupBy[comparisons, Length@# &];

  (* store seen to ensure not re-using elements *)
  seenElements = {};

  (* Null Set ({}) current not availble. *)
  elementsUniqueToComparisons = <|{} -> {}|>;

  For[setIndex = numberOfSets, setIndex > 0, setIndex--,
   (* the intersections of all comparisions with setIndex number of sets in them *)
   interesectionsOfSetIndexSets = Intersection @@@ (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex]);

   (* the intersections of all comparisions with setIndex number of sets in
   them after we removed already seenElements elements *)
   elementsUniqueToInteresectionsOfSetIndexSets = Complement[#, seenElements] & /@ interesectionsOfSetIndexSets;

   (* store the value with the name of the comparison *)
   AppendTo[
    elementsUniqueToComparisons,
    AssociationThread[
      comparisonsGrouppedByLength[setIndex],
      elementsUniqueToInteresectionsOfSetIndexSets]
    ];

   (* update seenElements *)
   seenElements = Union[seenElements, Flatten[elementsUniqueToInteresectionsOfSetIndexSets]];
   ];

  Return[
   <|
      "setNames" -> setNames,
      "comparisons" -> comparisons,
      "numberOfSets" -> numberOfSets,
      "elementsUniqueToComparisons" -> elementsUniqueToComparisons
    |>
   ];
]











upsetIntersectionRectangle[numberOfSets_, intersections_,
   comparisonIndex_, comparsions_, pointRadius_, colorScale_,
   maxValue_] := Module[
   {
    y = numberOfSets + pointRadius,
    x = comparisonIndex - pointRadius,
    c = comparsions[[comparisonIndex]],
    v = intersections[comparsions[[comparisonIndex]]],
    h = Length@intersections[comparsions[[comparisonIndex]]]
    },
   {
    colorScale[h/maxValue],
    Tooltip[
     Rectangle[{x, y}, {x + 1 - pointRadius, y + h}],
     "Unique to: " <> ToString[c] <> "\nNumber of elements: " <>
      ToString[Length@v] <> "\nElements: " <> ToString[v]
     ]
    }
   ];



   upsetSetRectangle[setIndex_, setNames_, sets_, pointRadius_,
   colorScale_, maxValue_] := Module[
   {
    y = setIndex - pointRadius,
    x = -pointRadius,
    s = setNames[[setIndex]],
    v = sets[setNames[[setIndex]]],
    w = Length@sets[setNames[[setIndex]]],
    m = Max[Length /@ Values@sets],
    d
    },
   d = m - w;
   {
    colorScale[w/maxValue],
    Tooltip[
     Rectangle[{-m + d, y}, {0, y + 1 - pointRadius}],
     "Set: " <> ToString[s] <> "\nNumber of elements: " <>
      ToString[w] <> "\nElements: " <> ToString[v]
     ]
    }
   ];

   upsetDisk[intersectionIndex_, setIndex_, comparisons_,
  setNames_, pointRadius_] := Module[
  {
   whichSet = setNames[[setIndex]],
   whichComparison = comparisons[[intersectionIndex]],
   setInComparisonQ,
   text
   },

  setInComparisonQ = MemberQ[whichComparison, whichSet];
  text = "Set: " <> ToString[whichSet] <> "\nIntersection: " <>
    ToString[whichComparison];

  {
   If[setInComparisonQ, Black, Lighter[Lighter[Gray]]],
   Tooltip[Disk[{intersectionIndex, setIndex}, pointRadius], text]
   }
  ]



UpSetChart[sets_] := Module[
  {
   uniqueIntersections = UpSetUniqueIntersections[sets],
   intersections, numberOfSets, setNames, comparisons,
   maxI, maxS, cd
   },

  intersections = uniqueIntersections["intersections"];
  numberOfSets = uniqueIntersections["numberOfSets"];
  setNames = uniqueIntersections["setNames"];
  comparisons = uniqueIntersections["comparisons"];
  maxI = Max[Length /@ Values@intersections];
  maxS = Max[Length /@ Values@sets];
  cd = ColorData["DeepSeaColors"];

  Graphics[
   {
    Table[
     upsetIntersectionRectangle[numberOfSets, intersections, i,
      comparisons, 0.3, cd, maxI],
     {i, Length@comparisons}
     ],

    Table[
     upsetDisk[i, j, comparisons, setNames, 0.3],
     {j, numberOfSets}, {i, Length@comparisons}
     ],

    Table[
     upsetSetRectangle[i, setNames, sets, 0.3, cd, maxS],
     {i, Length@setNames}
     ]
    }
   ]
  ]
End[]

EndPackage[]
