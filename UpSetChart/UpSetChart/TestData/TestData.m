BeginPackage["UpSetChart`TestData`"]

Needs["UpSetChart`Utilities`"];
DummyData[1] = <|
  "a" -> {7, 77, 53, 95, 42, 41},
  "b" -> {51, 88, 87, 67, 90, 37, 96},
  "c" -> {15, 87, 99, 6, 20, 87, 98, 68},
  "d" -> {46, 85, 6, 90},
  "e" -> {72, 97, 15, 55, 87}
|>;

DummyData[2] = <|
  "a" -> {11, 37},
  "b" -> {42, 38, 72, 54, 70, 38, 5, 34, 45, 67, 89, 59, 83, 70, 19, 17, 86, 37, 77, 85},
  "c" -> {20, 12, 22},
  "d" -> {55, 78, 59, 87, 33},
  "e" -> {61, 75, 8, 10, 16, 60, 45, 16, 13, 34, 62, 86, 97, 46, 44, 37, 3, 43},
  "f" -> {69, 78, 5, 41, 49, 24, 16, 36, 46, 51, 12, 87, 92, 35},
  "g" -> {21, 62, 82, 56, 31, 72, 4, 45, 1, 100, 74, 47, 13, 23, 88, 36, 84},
  "h" -> {15, 34, 77, 73, 20, 60, 93, 84, 56, 45, 34, 81, 16, 91, 86, 81, 41, 81, 21, 34, 72, 18, 18, 25, 36, 68},
  "i" -> {80, 39, 59, 22, 99, 69, 27, 5, 46, 9, 81, 18, 19, 27, 30, 1, 48, 34, 54},
  "j" -> {8, 80, 17, 92, 32, 68, 49, 95, 5, 67, 3, 7, 3, 46, 83, 15, 21, 12, 42, 13, 21, 91, 7, 78, 42, 67}
|>;


DummyData[3] = <|
  "Red" -> {"Strawberry", "Apple"},
  "Green" -> {"Kiwi", "Pear", "Cucumber", "Pickle"},
  "Tasty" -> {"Kiwi", "Pear"},
  "Fruit" -> {"Kiwi", "Lemon", "Banana", "Strawberry", "Pear", "Apple", "Carrot"},
  "Vegetable" -> {"Cucumber", "Pickle"}
|>;




RandomData::usage = StringJoin[
  "UpSetDummyData[nSets, mElements, r] makes nSets each with a random amount ",
  "of elements (at most mElements) where mElements are RandomInteger[{0,r}]."
]

Begin["`Private`"]
RandomData[nSets_, mElements_: 30, r_:100] :=
 EnsureLabeledSets[
  Table[RandomInteger[{0, r}], {nSets}, {RandomInteger[{1, mElements}]}]]
End[]

EndPackage[]
