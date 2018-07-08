BeginPackage["Detexon`"]

ReferenceModel::usage="Ademxapp Segmentation model used for inspiration.";
DetexonNet::usage="Neural Network for Exon Detexon.";

AdemxappResidualCNN1DBlock::usage="AdemxappResidualCNN1DBlock[inputDimension, name, residualBatchNormQ]";
PadToPreserveLength::usage="Padding needed when using 1D conv to not reduce sequence length.";
AdemxappResidualCNN1DBlockChain::usage="Chain ofAdemxappResidualCNN1DBlocks.";
AdemxappResidualCNN1DInceptionBlockChain::usage="Inception model of AdemxappResidualCNN1DBlockChain.";


Begin["`Private`"]
ReferenceModel = NetModel["Ademxapp Model A1 Trained on Cityscapes Data"]
PadToPreserveLength[kernel_] := Floor[kernel/2]


AdemxappResidualCNN1DBlock[inputDimension_, name_, residualBNQ_: True, kernel_: 3, padding_: 1]:=
With[
{ scale = If[residualBNQ, 2, 1] },
  Module[
   {
      residualBranch = { NetPort["Input"] -> name <> "_plus_shared" },
      residualBNBranch = {
        name <> "_ramp_shared" -> name <> "_cnn1d_branch_1" -> name <> "_plus_shared"
      },
      mainBranch = {
        name <> "_bnl_shared" -> name <> "_ramp_shared",
        If[residualBNQ,
          name <> "_ramp_shared" ->  name <> "_cnn1d_branch_main" -> name <> "_ramp_branch_main",
          name <> "_ramp_shared" ->  name <> "_cnn1d_branch_main" -> name <> "_bnl_main" -> name <> "_ramp_branch_main"
        ],
        name <> "_ramp_branch_main" ->
        name <> "_cnn1d_branch_main_a" -> name <> "_plus_shared"
      },
      residualBranchLayers = <||>,
      residualBNBranchLayers = <|
        name <> "_cnn1d_branch_1" -> ConvolutionLayer[scale*First@inputDimension, {1}]
      |>,
      mainBranchLayers = <|
        name <> "_bnl_shared" ->  BatchNormalizationLayer["Input" -> inputDimension],
        name <> "_ramp_shared" -> Ramp,
        name <> "_cnn1d_branch_main" ->  ConvolutionLayer[scale*First@inputDimension, {kernel},  "PaddingSize" -> {padding}],

        If[residualBNQ, Nothing,  name <> "_bnl_main" -> BatchNormalizationLayer[]],

        name <> "_ramp_branch_main" -> ElementwiseLayer[Ramp],
        name <> "_cnn1d_branch_main_a" ->  ConvolutionLayer[scale*First@inputDimension, {kernel},  "PaddingSize" -> {padding}],
        name <> "_plus_shared" -> ThreadingLayer[Plus]
      |>,
      layers, connections
    },

    If[residualBNQ,
      layers = Join[mainBranchLayers, residualBNBranchLayers];
      connections = Join[mainBranch, residualBNBranch],

      layers = Join[mainBranchLayers, residualBranchLayers];
      connections = Join[mainBranch, residualBranch]
    ];
    NetGraph[layers, connections]
  ]
]



AdemxappResidualCNN1DBlockChain[inputDimension_, name_, length_: 1, kernel_: 3, padding_: 1]:=
NetChain[
  Join[
    {
      AdemxappResidualCNN1DBlock[inputDimension, name, True, kernel, padding]
    },

    With[
      {
        newInputDimension = {2 First@inputDimension,  Last@inputDimension}
      },
      Table[
        AdemxappResidualCNN1DBlock[newInputDimension, name, False, kernel, padding]
        , {length - 1}
      ]
    ]
  ]
]


AdemxappResidualCNN1DInceptionBlockChain[inputDimension_, name_, length_: 1, kernels_: {}] :=
NetGraph[
  Join[

    (* Residual chain for each kernel *)

    Table[
      name <> "_" <> ToString[k] ->
      AdemxappResidualCNN1DBlockChain[inputDimension, name, length, k, PadToPreserveLength[k]]
      , {k, kernels }
    ],

    (* Pooling layer for each kernel  *)

    Table[
      name <> "_pool_" <> ToString[k] ->
      PoolingLayer[{k}, "PaddingSize" -> PadToPreserveLength[k]]
      , {k, kernels }
    ],


    (* Batch norm for input and thread to combine output *)
    {
      name <> "_batch_norm" -> BatchNormalizationLayer[],
      name <> "_thread" -> ThreadingLayer[Plus]
    }
  ],
  Table[
    name <> "_batch_norm" -> name <> "_" <> ToString[k] ->
    name <> "_pool_" <> ToString[k] -> name <> "_thread"
    , {k, kernels}
  ]
]



(* original block chains *)
convolutionBlock::usage =
  "Basic convoultion neural network architecture modified for handing \
sequences of strings. Returns a NetChain as follows:\n\t\
ConvolutionLayer --> BatchNormalizationLayer --> ReshapeLayer --> \
ElementwiseLayer";
convolutionBlock[ "Channels" -> channels_, "Kernel" -> kernel_, "SequenceLength" -> seqLen_, convOpts___]:=
NetChain[
  {
     ConvolutionLayer[channels, kernel, convOpts],
     ReshapeLayer[{channels, seqLen, 1}],
     BatchNormalizationLayer[],
     ReshapeLayer[{channels, seqLen}],
     ElementwiseLayer[Ramp]
  }
]




residualConvolutionalChain["Channels" -> channels_,"Kernel" -> kernel_,"SequenceLength" -> seqLen_,
  "PaddingSize" -> pad_,"InitalStride" -> initStride_,"ChainLength" -> chainLen_, convOpts___]:=
Module[
  {
    nonDownsizedConvolutionBlocks = chainLen - 1,
    netGraph,
    netGraphConnections,
    downsizedConvolutionBlock
  },
  (* Create the downsized cnn block *)

  downsizedConvolutionBlock =
  convolutionBlock[
    "Channels" -> channels, "Kernel" -> kernel,
    "SequenceLength" -> seqLen, "PaddingSize" -> pad,
    "Stride" -> initStride, convOpts
  ];

  (* Start a NetGraph with the downsized cnn block *)
  netGraph = { downsizedConvolutionBlock };

  (* Table non-downsized blocks for {i, chainLen-1} and append to the netGraph *)
  Table[
    AppendTo[netGraph,
      convolutionBlock[
        "Channels" -> channels, "Kernel" -> kernel,
        "SequenceLength" -> seqLen, "PaddingSize" -> pad, "Stride" -> 1,
        convOpts
      ]
    ], {i, nonDownsizedConvolutionBlocks}
  ];

  (* Add the TreadingLayer to enable the residual architecture *)
  AppendTo[netGraph, ThreadingLayer[Plus]];

  (* Specify the residual architecture *)

  netGraphConnections =
  Flatten[Join[Table[i -> i + 1, {i, chainLen}], {1 -> chainLen + 1}]];
  (* Return NetGraph*)
  NetGraph[netGraph, netGraphConnections]
]


(* original instance segmentation attempt *)
DetexonNet = NetGraph[<|

    "conv_1a" -> ConvolutionLayer[64, {3}, "PaddingSize" -> {1}],
    "pool_1" -> PoolingLayer[{3}, "PaddingSize" -> {1}],
    "arcb_1_upsample" ->
     AdemxappResidualCNN1DBlock[{64, Automatic}, "arcb_1_upsample",
      True],
    "arcb_1_res_a" ->
     AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_1_res_a",
      False],
    "arcb_1_res_b" ->
     AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_1_res_b",
      False],
    "pool_2" -> PoolingLayer[{3}, "PaddingSize" -> {1}],
    "arcb_2_upsample" ->
     AdemxappResidualCNN1DBlock[{128, Automatic}, "arcb_2_upsample",
      True],
    "arcb_2_res_a" ->
     AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_2_res_a",
      False],
    "arcb_2_res_b" ->
     AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_2_res_b",
      False],
    "pool_3" -> PoolingLayer[{3}, "PaddingSize" -> {1}],
    "arcb_3_upsample" ->
     AdemxappResidualCNN1DBlock[{256, Automatic}, "arcb_3_upsample",
      True],
    "arcb_3_res_a" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_a",
      False],
    "arcb_3_res_b" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_b",
      False],
    "arcb_3_res_c" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_c",
      False],
    "arcb_3_res_d" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_d",
      False],
    "arcb_3_res_e" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_3_res_e",
      False],
    "pool_4" -> PoolingLayer[{3}, "PaddingSize" -> {1}],
    "arcb_4_upsample" ->
     AdemxappResidualCNN1DBlock[{512, Automatic}, "arcb_4_upsample",
      True],
    "arcb_4_res_a" ->
     AdemxappResidualCNN1DBlock[{1024, Automatic}, "arcb_4_res_a",
      False],
    "arcb_4_res_b" ->
     AdemxappResidualCNN1DBlock[{1024, Automatic}, "arcb_4_res_b",
      False],
    "pool_5" -> PoolingLayer[{3}, "PaddingSize" -> {1}],
    "bn_6" -> BatchNormalizationLayer[],
    "relu_6" -> Ramp,
    "conv_6" -> ConvolutionLayer[512, {3}, "PaddingSize" -> {1}],
    "relu_6a" -> Ramp,
    "conv_6a" -> ConvolutionLayer[3, {3}, "PaddingSize" -> {1}],
    "transpose" -> TransposeLayer[],
    "softmax" -> SoftmaxLayer[]
    |>,
   {
    "conv_1a" -> "pool_1",
    "pool_1" ->
     "arcb_1_upsample" ->
      "arcb_1_res_a" -> "arcb_1_res_b" -> "pool_2",
    "pool_2" ->
     "arcb_2_upsample" ->
      "arcb_2_res_a" -> "arcb_2_res_b" -> "pool_3",
    "pool_3" ->
     "arcb_3_upsample" ->
      "arcb_3_res_a" ->
       "arcb_3_res_b" ->
        "arcb_3_res_c" ->
         "arcb_3_res_d" -> "arcb_3_res_e" -> "pool_4",
    "pool_4" ->
     "arcb_4_upsample" ->
      "arcb_4_res_a" -> "arcb_4_res_b" -> "pool_5",
    "pool_5" ->
     "bn_6" ->
      "relu_6" ->
       "conv_6" -> "relu_6a" -> "conv_6a" -> "transpose" -> "softmax"
    },
   "Input" -> {4, 300}
   ]

End[]
EndPackage[]
