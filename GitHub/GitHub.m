BeginPackage["GitHub`"]

URLBuildSubKey::usage=""
SearchRepositories::usage=""
SearchNotebooks::usage=""
URLBuildRawRequests::usage=""
DownloadRawURLs::usage=""

Begin["`Private`"]

(* GitHub's main api *)
$BaseURL = "https://api.github.com/";
(* said api's main query argument *)
$Query = "q";
(* specify which page to get *)
$Page = "page";
(* specify how many results per page to get*)
$ResultsPerPage = "per_page";
(* specify what extension file should have*)
$Extension = "extension";
(* specify what repo to look in *)
$Repository="repo";
(* specify sub url for searching for repository names *)
$SearchRepositories="search/repositories";
(* specify sub url for searching for code *)
$SearchCode="search/code";


(* GitHub API's main argument 'q' takes a list of '+' seperated
subkeys, e.g. language:<lang>+ etc.
This is just a wrapper to make it a bit easier to read the code *)
URLBuildSubKey[key_, value_String, delim_: ":", between_: "+"] :=
  key <> delim <> value;

(* map over values for a given key *)
URLBuildSubKey[key_, value_List, delim_: ":", between_: "+"] :=
  StringJoin[Riffle[(key <> delim <> # & /@ value), between]];


URLBuildSearchRepositories[languages_, page_:1, results_:1] :=  Module[
  {
    url
  },
  url = URLBuild[
    $BaseURL<>$SearchRepositories,
    {
      $Query -> URLBuildSubKey["language", languages],
      $Page -> page,
      $ResultsPerPage -> results
    }
  ];

  Return[url];
]


SearchRepositories[languages_,delay_:40]:=Module[
  {
    total,
    urls,
    results
  },
  (* make small request to see how many total results there are *)
  total=URLExecute[URLBuildSearchRepositories[languages, 1, 1], "RawJSON"]["total_count"];
  Print[ToString[total]<>" public repositories with language(s): "<>ToString[languages]];

  (* build pagenation based queries for all of said results *)
  urls=Table[
    With[
      {p=(n+99)/100},
      URLBuildSearchRepositories[languages, p, 100]
    ],{n, 1, total, 100}
  ];

  (* Print[urls]; *)
  Print[ToString[Length[urls]]<>" requests must be made with a 40 second delay."]
  Print["Total request will take "<>ToString[Round[((Length[urls] * delay) / 60)]]<>" minute(s)."];

  (* submit queries to api *)
  results = Monitor[
    Table[
      (* GitHub limits to 30 seconds between queries, we use 40 to be safe.
      Since we also called a spy query, we wait even for first iteration. *)
      Print["Getting page "<>ToString[page]<>" of "<>ToString[Length@urls]];
      (* Ideally WriteString would allow to write on same line, but doesnt seem to work... *)
      Pause[delay];
      URLExecute[urls[[page]],"RawJSON"]
    , {page, Length@urls}]
    ,ProgressIndicator[page, {0, Length@urls}]
  ];

  Return[results];
]



URLBuildNotebookFilesRequest[repository_]:=Module[
  {
    url
  },
  url = URLBuild[
    $BaseURL<>$SearchCode,
    {
      $Query-> URLBuildSubKey[$Extension, "nb"]
    }
  ]<>"+"<>URLBuildSubKey[$Repository,repository];

  Return[url];
]

(* URLBuildNotebookFilesRequest[repository_]:=Module[
  {
    url
  },
  url = URLBuild[
    $BaseURL<>$SearchCode,
    {
      $Query-> StringJoin[
        Riffle[
          {
            URLBuildSubKey[$Extension, "nb"],
            URLBuildSubKey[$Repository,repository]
          },
          "+"
        ]
      ]
    }
  ];
  url=StringReplace[url,"\\"->""];
  Return[url];
] *)


SearchNotebooks[repositories_, delay_:40]:= Module[
  {
    urls
  },
  urls = URLBuildNotebookFilesRequest[#]&/@repositories;

  (* Print[urls]; *)
  Print[ToString[Length[urls]]<>" requests must be made with a 40 second delay."]
  Print["Total request will take "<>ToString[Round[((Length[urls] * delay) / 60)]]<>" minute(s)."];

  (* submit queries to api *)
  results = Monitor[
    Table[
      (* GitHub limits to 30 seconds between queries, we use 40 to be safe.
      Since we also called a spy query, we wait even for first iteration. *)
      Print["Searching repository "<>ToString[repositories[[rIndex]]]<>" "<>ToString[rIndex]<>" of "<>ToString[Length@urls]];
      (* Ideally WriteString would allow to write on same line, but doesnt seem to work... *)
      If[rIndex != 1, Pause[delay]];
      URLExecute[urls[[rIndex]],"RawJSON"]
    (* , {rIndex, 3}] *)
    , {rIndex, Length@urls}]
    ,ProgressIndicator[rIndex, {0, Length@urls}]
  ];

  Return[results];
]

URLBuildRawRequests[notebookDataset_]:=StringReplace[Normal[notebookDataset[All, "html_url"]], "blob" -> "raw"];


DownloadRawURLs[notebookDataset_, dumpLocation_]:= Module[
  {
    urls=URLBuildRawRequests[notebookDataset],
    results
  },

  Print[ToString[Length[urls]]<>" requests must be made."]
  Monitor[
    results = Table[
      With[
        {url = urls[[uIndex]]},
        Print["Downloading file at "<>ToString[url]<>" "<>ToString[uIndex]<>" of "<>ToString[Length@urls]];
        URLDownload[
          url,
          FileNameJoin[{dumpLocation, IntegerString[Hash[url], 36]}]
        ]
      ]
    ,{uIndex, Length@urls}]
    ,ProgressIndicator[uIndex, {0, Length@urls}]
  ];
  Return[results];
];

End[]
EndPackage[]
