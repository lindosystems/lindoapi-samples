'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ''
 ''    LINDO API Version 13.0
 ''    Copyright (c) 2000-2018
 ''
 ''    LINDO Systems, Inc.            312.988.7422
 ''    1415 North Dayton St.          info@lindo.com
 ''    Chicago, IL 60622              http:www.lindo.com
 ''
 ''    $Id: lindo.h 2906 2020-01-11 23:47:51Z mka $
 ''
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Imports System 
Imports System.Text 
Imports System.Runtime.InteropServices 

Public Class lindo


 ' Version macros '
  Public Const  LS_MAJOR_VER_NUMBER            As Integer =         13
  Public Const  LS_MINOR_VER_NUMBER            As Integer =          0
  Public Const  LS_REV_VER_NUMBER              As Integer =        109
  Public Const  LS_VER_NUMBER                  As Integer =       1300
  Public Const  LS_BUILD_VER_NUMBER            As Integer =       4099
  Public Const  LS_MIN                         As Integer =         +1
  Public Const  LS_MAX                         As Integer =         -1
  Public Const LS_CONTYPE_GE As Integer = 71 'G'
  Public Const LS_CONTYPE_LE As Integer = 76 'L'
  Public Const LS_CONTYPE_EQ As Integer = 69 'E'
  Public Const LS_CONTYPE_FR As Integer = 78 'N'
  Public Const LS_CONETYPE_QUAD As Integer = 81 'Q'
  Public Const LS_CONETYPE_RQUAD As Integer = 82 'R'
  Public Const  LS_CONETYPE_PEXP               As Integer =        69 'E'
  Public Const  LS_CONETYPE_PPOW               As Integer =        80 'P'
  Public Const LS_VARTYPE_CONT As Integer = 67 'C'
  Public Const LS_VARTYPE_BIN As Integer = 66 'B'
  Public Const LS_VARTYPE_INT As Integer = 73 'I'
  Public Const LS_VARTYPE_SC As Integer = 83 'S'
  Public Const  LS_INFINITY                    As Double  =    1.0E+30
  Public Const  LS_BASTYPE_BAS                 As Integer =          0
  Public Const  LS_BASTYPE_ATLO                As Integer =         -1
  Public Const  LS_BASTYPE_ATUP                As Integer =         -2
  Public Const  LS_BASTYPE_FNUL                As Integer =         -3
  Public Const  LS_BASTYPE_SBAS                As Integer =         -4
  Public Const  LS_UNFORMATTED_MPS             As Integer =          0
  Public Const  LS_FORMATTED_MPS               As Integer =          1
  Public Const  LS_UNFORMATTED_MPS_COMP        As Integer =          2
  Public Const  LS_FORMATTED_MPS_COMP          As Integer =          3
  Public Const  LS_SOLUTION_OPT                As Integer =          0
  Public Const  LS_SOLUTION_MIP                As Integer =          1
  Public Const  LS_SOLUTION_OPT_IPM            As Integer =          2
  Public Const  LS_SOLUTION_OPT_OLD            As Integer =          3
  Public Const  LS_SOLUTION_MIP_OLD            As Integer =          4
  Public Const  LS_BASFILE_BIN                 As Integer =          1
  Public Const  LS_BASFILE_MPS                 As Integer =          2
  Public Const  LS_BASFILE_TXT                 As Integer =          3
  Public Const  LS_BASFILE_MTX                 As Integer =          4
  Public Const  LS_INT_TYPE                    As Integer =          4
  Public Const  LS_DOUBLE_TYPE                 As Integer =          8
  Public Const  LS_MAX_ERROR_MESSAGE_LENGTH    As Integer =       1024
  Public Const  LS_DEFAULT                     As Integer =         -1
  Public Const  LS_MAX_JOBJECTS                As Integer =        100
  Public Const  LS_PROPERTY_UNKNOWN            As Integer =          0
  Public Const  LS_PROPERTY_CONST              As Integer =          1
  Public Const  LS_PROPERTY_LINEAR             As Integer =          2
  Public Const  LS_PROPERTY_CONVEX             As Integer =          3
  Public Const  LS_PROPERTY_CONCAVE            As Integer =          4
  Public Const  LS_PROPERTY_QUASI_CONVEX       As Integer =          5
  Public Const  LS_PROPERTY_QUASI_CONCAVE      As Integer =          6
  Public Const  LS_PROPERTY_MAX                As Integer =          7
  Public Const  LS_PROPERTY_MONO_INCREASE      As Integer =          8
  Public Const  LS_PROPERTY_MONO_DECREASE      As Integer =          9

 ' bitmasks for LScopyModel '
  Public Const  LS_RAW_COPY                    As Integer =          0
  Public Const  LS_DEEP_COPY                   As Integer =          1
  Public Const  LS_TIME_COPY                   As Integer =          2
  Public Const  LS_STOC_COPY                   As Integer =          4
  Public Const  LS_SNGSTG_COPY                 As Integer =          8

 ' Time frames in seconds '
  Public Const  LSSEC01                        As Integer =          1
  Public Const  LSSEC02                        As Integer =          2
  Public Const  LSSEC03                        As Integer =          3
  Public Const  LSSEC04                        As Integer =          4
  Public Const  LSSEC05                        As Integer =          5
  Public Const  LSSEC06                        As Integer =          6
  Public Const  LSSEC10                        As Integer =         10
  Public Const  LSSEC15                        As Integer =         15
  Public Const  LSSEC20                        As Integer =         20
  Public Const  LSSEC30                        As Integer =         30
  Public Const  LSMIN01                        As Integer =         60
  Public Const  LSMIN02                        As Integer =        120
  Public Const  LSMIN03                        As Integer =        180
  Public Const  LSMIN05                        As Integer =        300
  Public Const  LSMIN06                        As Integer =        600
  Public Const  LSMIN10                        As Integer =        600
  Public Const  LSMIN15                        As Integer =        900
  Public Const  LSMIN20                        As Integer =       1200
  Public Const  LSMIN30                        As Integer =       1800
  Public Const  LSHOUR01                       As Integer =       3600
  Public Const  LSHOUR02                       As Integer =       7200
  Public Const  LSHOUR03                       As Integer =      10800
  Public Const  LSHOUR05                       As Integer =      18000
  Public Const  LSHOUR06                       As Integer =      21600
  Public Const  LSHOUR08                       As Integer =      28800
  Public Const  LSHOUR12                       As Integer =      43200
  Public Const  LSDAY                          As Integer =      86400
  Public Const  LSWEEK                         As Integer =     604800
  Public Const  LSMONTH                        As Integer =    2592000
  Public Const  LSQUARTER                      As Integer =    7776000
  Public Const  LSYEAR                         As Integer =   31104000

 ' Days of week '
  Public Const  LSSUNDAY                       As Integer =          0
  Public Const  LSMONDAY                       As Integer =          1
  Public Const  LSTUESDAY                      As Integer =          2
  Public Const  LSWEDNESDAY                    As Integer =          3
  Public Const  LSTHURSDAY                     As Integer =          4
  Public Const  LSFRIDAY                       As Integer =          5
  Public Const  LSSATURDAY                     As Integer =          6

 ' bitmask for components '
  Public Const  LS_DATA_CORE                   As Integer =          1
  Public Const  LS_DATA_TIME                   As Integer =          2
  Public Const  LS_DATA_STOC                   As Integer =          4
  Public Const  LS_DATA_FILE                   As Integer =          8

 ' external solvers '
  Public Const  LS_XSOLVER_MSKLP               As Integer =          1
  Public Const  LS_XSOLVER_GRBLP               As Integer =          2
  Public Const  LS_XSOLVER_GRBCL               As Integer =          3
  Public Const  LS_XSOLVER_GRBMIP              As Integer =          4
  Public Const  LS_XSOLVER_CPXLP               As Integer =          5
  Public Const  LS_XSOLVER_CPXMIP              As Integer =          6
  Public Const  LS_XSOLVER_OSI                 As Integer =          7
  Public Const  LS_XSOLVER_CLP                 As Integer =          8
  Public Const  LS_XSOLVER_MSK                 As Integer =          9
  Public Const  LS_XSOLVER_COI                 As Integer =         10
  Public Const  LS_XSOLVER_SOP                 As Integer =         11

 ' Solution or model status (1-20) '
  Public Const  LS_STATUS_OPTIMAL              As Integer =          1
  Public Const  LS_STATUS_BASIC_OPTIMAL        As Integer =          2
  Public Const  LS_STATUS_INFEASIBLE           As Integer =          3
  Public Const  LS_STATUS_UNBOUNDED            As Integer =          4
  Public Const  LS_STATUS_FEASIBLE             As Integer =          5
  Public Const  LS_STATUS_INFORUNB             As Integer =          6
  Public Const  LS_STATUS_NEAR_OPTIMAL         As Integer =          7
  Public Const  LS_STATUS_LOCAL_OPTIMAL        As Integer =          8
  Public Const  LS_STATUS_LOCAL_INFEASIBLE     As Integer =          9
  Public Const  LS_STATUS_CUTOFF               As Integer =         10
  Public Const  LS_STATUS_NUMERICAL_ERROR      As Integer =         11
  Public Const  LS_STATUS_UNKNOWN              As Integer =         12
  Public Const  LS_STATUS_UNLOADED             As Integer =         13
  Public Const  LS_STATUS_LOADED               As Integer =         14
  Public Const  LS_STATUS_BOUNDED              As Integer =         15

    ' General parameters (1021 - 1099) '
  Public Const  LS_IPARAM_OBJSENSE             As Integer =       1022
  Public Const  LS_DPARAM_CALLBACKFREQ         As Integer =       1023
  Public Const  LS_DPARAM_OBJPRINTMUL          As Integer =       1024
  Public Const  LS_IPARAM_CHECK_FOR_ERRORS     As Integer =       1025
  Public Const  LS_IPARAM_ALLOW_CNTRLBREAK     As Integer =       1026
  Public Const  LS_IPARAM_DECOMPOSITION_TYPE   As Integer =       1027
  Public Const  LS_IPARAM_LP_SCALE             As Integer =       1029
  Public Const  LS_IPARAM_LP_ITRLMT            As Integer =       1030
  Public Const  LS_IPARAM_SPLEX_PPRICING       As Integer =       1031
  Public Const  LS_IPARAM_SPLEX_REFACFRQ       As Integer =       1032
  Public Const  LS_IPARAM_BARRIER_SOLVER       As Integer =       1033
  Public Const  LS_IPARAM_PROB_TO_SOLVE        As Integer =       1034
  Public Const  LS_IPARAM_LP_PRINTLEVEL        As Integer =       1035
  Public Const  LS_IPARAM_MPS_OBJ_WRITESTYLE   As Integer =       1036
  Public Const  LS_IPARAM_SPLEX_DPRICING       As Integer =       1037
  Public Const  LS_IPARAM_SOL_REPORT_STYLE     As Integer =       1038
  Public Const  LS_IPARAM_INSTRUCT_LOADTYPE    As Integer =       1039
  Public Const  LS_IPARAM_SPLEX_DUAL_PHASE     As Integer =       1040
  Public Const  LS_IPARAM_LP_PRELEVEL          As Integer =       1041
  Public Const  LS_IPARAM_STRING_LENLMT        As Integer =       1042
  Public Const  LS_IPARAM_USE_NAMEDATA         As Integer =       1043
  Public Const  LS_IPARAM_SPLEX_USE_EXTERNAL   As Integer =       1044
  Public Const  LS_DPARAM_LP_ITRLMT            As Integer =       1045
  Public Const  LS_IPARAM_COPY_MODE            As Integer =       1046
  Public Const  LS_IPARAM_SBD_NUM_THREADS      As Integer =       1047
  Public Const  LS_IPARAM_NUM_THREADS          As Integer =       1048
  Public Const  LS_IPARAM_MULTITHREAD_MODE     As Integer =       1049
  Public Const  LS_IPARAM_FIND_BLOCK           As Integer =       1050
  Public Const  LS_IPARAM_PROFILER_LEVEL       As Integer =       1051
  Public Const  LS_IPARAM_INSTRUCT_READMODE    As Integer =       1052
  Public Const  LS_IPARAM_INSTRUCT_SUBOUT      As Integer =       1053
  Public Const  LS_IPARAM_SOLPOOL_LIM          As Integer =       1054
  Public Const  LS_IPARAM_FIND_SYMMETRY_LEVEL  As Integer =       1055
  Public Const  LS_IPARAM_FIND_SYMMETRY_PRINT_LEVEL As Integer =       1056
  Public Const  LS_IPARAM_TUNER_PRINT_LEVEL    As Integer =       1057

    ' Generic solver parameters (1251 - 1500) '
  Public Const  LS_IPARAM_SOLVER_IUSOL         As Integer =       1251
  Public Const  LS_IPARAM_SOLVER_TIMLMT        As Integer =       1252
  Public Const  LS_DPARAM_SOLVER_CUTOFFVAL     As Integer =       1253
  Public Const  LS_DPARAM_SOLVER_FEASTOL       As Integer =       1254
  Public Const  LS_IPARAM_SOLVER_RESTART       As Integer =       1255
  Public Const  LS_IPARAM_SOLVER_IPMSOL        As Integer =       1256
  Public Const  LS_DPARAM_SOLVER_OPTTOL        As Integer =       1257
  Public Const  LS_IPARAM_SOLVER_USECUTOFFVAL  As Integer =       1258
  Public Const  LS_IPARAM_SOLVER_PRE_ELIM_FILL As Integer =       1259
  Public Const  LS_DPARAM_SOLVER_TIMLMT        As Integer =       1260
  Public Const  LS_IPARAM_SOLVER_CONCURRENT_OPTMODE As Integer =       1261
  Public Const  LS_DPARAM_SOLVER_PERT_FEASTOL  As Integer =       1262
  Public Const  LS_IPARAM_SOLVER_PARTIALSOL_LEVEL As Integer =       1263
  Public Const  LS_IPARAM_SOLVER_MODE          As Integer =       1264

    ' Advanced parameters for the simplex method (4000 - 41++) '
  Public Const  LS_DPARAM_LP_MIN_FEASTOL       As Integer =       4060
  Public Const  LS_DPARAM_LP_MAX_FEASTOL       As Integer =       4061
  Public Const  LS_DPARAM_LP_MIN_OPTTOL        As Integer =       4062
  Public Const  LS_DPARAM_LP_MAX_OPTTOL        As Integer =       4063
  Public Const  LS_DPARAM_LP_MIN_PIVTOL        As Integer =       4064
  Public Const  LS_DPARAM_LP_MAX_PIVTOL        As Integer =       4065
  Public Const  LS_DPARAM_LP_AIJ_ZEROTOL       As Integer =       4066
  Public Const  LS_DPARAM_LP_PIV_ZEROTOL       As Integer =       4067
  Public Const  LS_DPARAM_LP_PIV_BIGTOL        As Integer =       4068
  Public Const  LS_DPARAM_LP_BIGM              As Integer =       4069
  Public Const  LS_DPARAM_LP_BNDINF            As Integer =       4070
  Public Const  LS_DPARAM_LP_INFINITY          As Integer =       4071
  Public Const  LS_IPARAM_LP_PPARTIAL          As Integer =       4072
  Public Const  LS_IPARAM_LP_DPARTIAL          As Integer =       4073
  Public Const  LS_IPARAM_LP_DRATIO            As Integer =       4074
  Public Const  LS_IPARAM_LP_PRATIO            As Integer =       4075
  Public Const  LS_IPARAM_LP_RATRANGE          As Integer =       4076
  Public Const  LS_IPARAM_LP_DPSWITCH          As Integer =       4077
  Public Const  LS_IPARAM_LP_PALLOC            As Integer =       4078
  Public Const  LS_IPARAM_LP_PRTFG             As Integer =       4079
  Public Const  LS_IPARAM_LP_OPRFREE           As Integer =       4080
  Public Const  LS_IPARAM_LP_SPRINT_SUB        As Integer =       4081
  Public Const  LS_IPARAM_LP_PERTMODE          As Integer =       4082
  Public Const  LS_IPARAM_LP_PCOLAL_FACTOR     As Integer =       4083
  Public Const  LS_IPARAM_LP_MAXMERGE          As Integer =       4084
  Public Const  LS_DPARAM_LP_PERTFACT          As Integer =       4085
  Public Const  LS_DPARAM_LP_DYNOBJFACT        As Integer =       4086
  Public Const  LS_IPARAM_LP_DYNOBJMODE        As Integer =       4087
  Public Const  LS_DPARAM_LP_MERGEFACT         As Integer =       4088
  Public Const  LS_IPARAM_LP_UMODE             As Integer =       4089
  Public Const  LS_IPARAM_LP_SPRINT_MAXPASS    As Integer =       4090
  Public Const  LS_IPARAM_LP_SPRINT_COLFACT    As Integer =       4091

    ' Advanced parameters for LU decomposition (4800 - 4+++) '
  Public Const  LS_IPARAM_LU_NUM_CANDITS       As Integer =       4800
  Public Const  LS_IPARAM_LU_MAX_UPDATES       As Integer =       4801
  Public Const  LS_IPARAM_LU_PRINT_LEVEL       As Integer =       4802
  Public Const  LS_IPARAM_LU_UPDATE_TYPE       As Integer =       4803
  Public Const  LS_IPARAM_LU_MODE              As Integer =       4804
  Public Const  LS_IPARAM_LU_PIVMOD            As Integer =       4806
  Public Const  LS_DPARAM_LU_EPS_DIAG          As Integer =       4900
  Public Const  LS_DPARAM_LU_EPS_NONZ          As Integer =       4901
  Public Const  LS_DPARAM_LU_EPS_PIVABS        As Integer =       4902
  Public Const  LS_DPARAM_LU_EPS_PIVREL        As Integer =       4903
  Public Const  LS_DPARAM_LU_INI_RCOND         As Integer =       4904
  Public Const  LS_DPARAM_LU_SPVTOL_UPDATE     As Integer =       4905
  Public Const  LS_DPARAM_LU_SPVTOL_FTRAN      As Integer =       4906
  Public Const  LS_DPARAM_LU_SPVTOL_BTRAN      As Integer =       4907
  Public Const  LS_IPARAM_LU_CORE              As Integer =       4908

    ' Parameters for the IPM method (3000 - 3+++) '
  Public Const  LS_DPARAM_IPM_TOL_INFEAS       As Integer =       3150
  Public Const  LS_DPARAM_IPM_TOL_PATH         As Integer =       3151
  Public Const  LS_DPARAM_IPM_TOL_PFEAS        As Integer =       3152
  Public Const  LS_DPARAM_IPM_TOL_REL_STEP     As Integer =       3153
  Public Const  LS_DPARAM_IPM_TOL_PSAFE        As Integer =       3154
  Public Const  LS_DPARAM_IPM_TOL_DFEAS        As Integer =       3155
  Public Const  LS_DPARAM_IPM_TOL_DSAFE        As Integer =       3156
  Public Const  LS_DPARAM_IPM_TOL_MU_RED       As Integer =       3157
  Public Const  LS_DPARAM_IPM_BASIS_REL_TOL_S  As Integer =       3158
  Public Const  LS_DPARAM_IPM_BASIS_TOL_S      As Integer =       3159
  Public Const  LS_DPARAM_IPM_BASIS_TOL_X      As Integer =       3160
  Public Const  LS_DPARAM_IPM_BI_LU_TOL_REL_PIV As Integer =       3161
  Public Const  LS_DPARAM_IPM_CO_TOL_INFEAS    As Integer =       3162
  Public Const  LS_DPARAM_IPM_CO_TOL_PFEAS     As Integer =       3163
  Public Const  LS_DPARAM_IPM_CO_TOL_DFEAS     As Integer =       3164
  Public Const  LS_DPARAM_IPM_CO_TOL_MU_RED    As Integer =       3165
  Public Const  LS_IPARAM_IPM_MAX_ITERATIONS   As Integer =       3166
  Public Const  LS_IPARAM_IPM_OFF_COL_TRH      As Integer =       3167
  Public Const  LS_IPARAM_IPM_NUM_THREADS      As Integer =       3168
  Public Const  LS_IPARAM_IPM_CHECK_CONVEXITY  As Integer =       3169

    ' Nonlinear programming (NLP) parameters (2500 - 25++) '
  Public Const  LS_IPARAM_NLP_SOLVE_AS_LP      As Integer =       2500
  Public Const  LS_IPARAM_NLP_SOLVER           As Integer =       2501
  Public Const  LS_IPARAM_NLP_SUBSOLVER        As Integer =       2502
  Public Const  LS_IPARAM_NLP_PRINTLEVEL       As Integer =       2503
  Public Const  LS_DPARAM_NLP_PSTEP_FINITEDIFF As Integer =       2504
  Public Const  LS_IPARAM_NLP_DERIV_DIFFTYPE   As Integer =       2505
  Public Const  LS_DPARAM_NLP_FEASTOL          As Integer =       2506
  Public Const  LS_DPARAM_NLP_REDGTOL          As Integer =       2507
  Public Const  LS_IPARAM_NLP_USE_CRASH        As Integer =       2508
  Public Const  LS_IPARAM_NLP_USE_STEEPEDGE    As Integer =       2509
  Public Const  LS_IPARAM_NLP_USE_SLP          As Integer =       2510
  Public Const  LS_IPARAM_NLP_USE_SELCONEVAL   As Integer =       2511
  Public Const  LS_IPARAM_NLP_PRELEVEL         As Integer =       2512
  Public Const  LS_IPARAM_NLP_ITRLMT           As Integer =       2513
  Public Const  LS_IPARAM_NLP_LINEARZ          As Integer =       2514
  Public Const  LS_IPARAM_NLP_LINEARITY        As Integer =       2515
  Public Const  LS_IPARAM_NLP_STARTPOINT       As Integer =       2516
  Public Const  LS_IPARAM_NLP_CONVEXRELAX      As Integer =       2517
  Public Const  LS_IPARAM_NLP_CR_ALG_REFORM    As Integer =       2518
  Public Const  LS_IPARAM_NLP_QUADCHK          As Integer =       2519
  Public Const  LS_IPARAM_NLP_AUTODERIV        As Integer =       2520
  Public Const  LS_IPARAM_NLP_MAXLOCALSEARCH   As Integer =       2521
  Public Const  LS_IPARAM_NLP_CONVEX           As Integer =       2522
  Public Const  LS_IPARAM_NLP_CONOPT_VER       As Integer =       2523
  Public Const  LS_IPARAM_NLP_USE_LINDO_CRASH  As Integer =       2524
  Public Const  LS_IPARAM_NLP_STALL_ITRLMT     As Integer =       2525
  Public Const  LS_IPARAM_NLP_AUTOHESS         As Integer =       2526
  Public Const  LS_IPARAM_NLP_FEASCHK          As Integer =       2527
  Public Const  LS_DPARAM_NLP_ITRLMT           As Integer =       2528
  Public Const  LS_IPARAM_NLP_MAXSUP           As Integer =       2529
  Public Const  LS_IPARAM_NLP_MSW_SOLIDX       As Integer =       2530
  Public Const  LS_IPARAM_NLP_ITERS_PER_LOGLINE As Integer =       2531
  Public Const  LS_IPARAM_NLP_MAX_RETRY        As Integer =       2532
  Public Const  LS_IPARAM_NLP_MSW_NORM         As Integer =       2533
  Public Const  LS_IPARAM_NLP_MSW_POPSIZE      As Integer =       2534
  Public Const  LS_IPARAM_NLP_MSW_MAXPOP       As Integer =       2535
  Public Const  LS_IPARAM_NLP_MSW_MAXNOIMP     As Integer =       2536
  Public Const  LS_IPARAM_NLP_MSW_FILTMODE     As Integer =       2537
  Public Const  LS_DPARAM_NLP_MSW_POXDIST_THRES As Integer =       2538
  Public Const  LS_DPARAM_NLP_MSW_EUCDIST_THRES As Integer =       2539
  Public Const  LS_DPARAM_NLP_MSW_XNULRAD_FACTOR As Integer =       2540
  Public Const  LS_DPARAM_NLP_MSW_XKKTRAD_FACTOR As Integer =       2541
  Public Const  LS_IPARAM_NLP_MAXLOCALSEARCH_TREE As Integer =       2542
  Public Const  LS_IPARAM_NLP_MSW_NUM_THREADS  As Integer =       2543
  Public Const  LS_IPARAM_NLP_MSW_RG_SEED      As Integer =       2544
  Public Const  LS_IPARAM_NLP_MSW_PREPMODE     As Integer =       2545
  Public Const  LS_IPARAM_NLP_MSW_RMAPMODE     As Integer =       2546
  Public Const  LS_IPARAM_NLP_XSMODE           As Integer =       2547
  Public Const  LS_DPARAM_NLP_MSW_OVERLAP_RATIO As Integer =       2548
  Public Const  LS_DPARAM_NLP_INF              As Integer =       2549
  Public Const  LS_IPARAM_NLP_IPM2GRG          As Integer =       2550
  Public Const  LS_IPARAM_NLP_USE_SDP          As Integer =       2551
  Public Const  LS_IPARAM_NLP_LINEARZ_WB_CONSISTENT As Integer =       2552
  Public Const  LS_DPARAM_NLP_CUTOFFOBJ        As Integer =       2553
  Public Const  LS_IPARAM_NLP_USECUTOFFOBJ     As Integer =       2554
  Public Const  	LS_IPARAM_NLP_CONIC_REFORM    As Integer =       2555

    ' Mixed integer programming (MIP) parameters (5000 - 5+++) '
  Public Const  LS_IPARAM_MIP_TIMLIM           As Integer =       5300
  Public Const  LS_IPARAM_MIP_AOPTTIMLIM       As Integer =       5301
  Public Const  LS_IPARAM_MIP_LSOLTIMLIM       As Integer =       5302
  Public Const  LS_IPARAM_MIP_PRELEVEL         As Integer =       5303
  Public Const  LS_IPARAM_MIP_NODESELRULE      As Integer =       5304
  Public Const  LS_DPARAM_MIP_INTTOL           As Integer =       5305
  Public Const  LS_DPARAM_MIP_RELINTTOL        As Integer =       5306
  Public Const  LS_DPARAM_MIP_RELOPTTOL        As Integer =       5307
  Public Const  LS_DPARAM_MIP_PEROPTTOL        As Integer =       5308
  Public Const  LS_IPARAM_MIP_MAXCUTPASS_TOP   As Integer =       5309
  Public Const  LS_IPARAM_MIP_MAXCUTPASS_TREE  As Integer =       5310
  Public Const  LS_DPARAM_MIP_ADDCUTPER        As Integer =       5311
  Public Const  LS_DPARAM_MIP_ADDCUTPER_TREE   As Integer =       5312
  Public Const  LS_IPARAM_MIP_MAXNONIMP_CUTPASS As Integer =       5313
  Public Const  LS_IPARAM_MIP_CUTLEVEL_TOP     As Integer =       5314
  Public Const  LS_IPARAM_MIP_CUTLEVEL_TREE    As Integer =       5315
  Public Const  LS_IPARAM_MIP_CUTTIMLIM        As Integer =       5316
  Public Const  LS_IPARAM_MIP_CUTDEPTH         As Integer =       5317
  Public Const  LS_IPARAM_MIP_CUTFREQ          As Integer =       5318
  Public Const  LS_IPARAM_MIP_HEULEVEL         As Integer =       5319
  Public Const  LS_IPARAM_MIP_PRINTLEVEL       As Integer =       5320
  Public Const  LS_IPARAM_MIP_PREPRINTLEVEL    As Integer =       5321
  Public Const  LS_DPARAM_MIP_CUTOFFOBJ        As Integer =       5322
  Public Const  LS_IPARAM_MIP_USECUTOFFOBJ     As Integer =       5323
  Public Const  LS_IPARAM_MIP_STRONGBRANCHLEVEL As Integer =       5324
  Public Const  LS_IPARAM_MIP_TREEREORDERLEVEL As Integer =       5325
  Public Const  LS_IPARAM_MIP_BRANCHDIR        As Integer =       5326
  Public Const  LS_IPARAM_MIP_TOPOPT           As Integer =       5327
  Public Const  LS_IPARAM_MIP_REOPT            As Integer =       5328
  Public Const  LS_IPARAM_MIP_SOLVERTYPE       As Integer =       5329
  Public Const  LS_IPARAM_MIP_KEEPINMEM        As Integer =       5330
  Public Const  LS_IPARAM_MIP_BRANCHRULE       As Integer =       5331
  Public Const  LS_DPARAM_MIP_REDCOSTFIX_CUTOFF As Integer =       5332
  Public Const  LS_DPARAM_MIP_ADDCUTOBJTOL     As Integer =       5333
  Public Const  LS_IPARAM_MIP_HEUMINTIMLIM     As Integer =       5334
  Public Const  LS_IPARAM_MIP_BRANCH_PRIO      As Integer =       5335
  Public Const  LS_IPARAM_MIP_SCALING_BOUND    As Integer =       5336
  Public Const  LS_DPARAM_MIP_PSEUDOCOST_WEIGT As Integer =       5337
  Public Const  LS_DPARAM_MIP_LBIGM            As Integer =       5338
  Public Const  LS_DPARAM_MIP_DELTA            As Integer =       5339
  Public Const  LS_IPARAM_MIP_DUAL_SOLUTION    As Integer =       5340
  Public Const  LS_IPARAM_MIP_BRANCH_LIMIT     As Integer =       5341
  Public Const  LS_DPARAM_MIP_ITRLIM           As Integer =       5342
  Public Const  LS_IPARAM_MIP_AGGCUTLIM_TOP    As Integer =       5343
  Public Const  LS_IPARAM_MIP_AGGCUTLIM_TREE   As Integer =       5344
  Public Const  LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_ITER As Integer =       5345
  Public Const  LS_IPARAM_MIP_ANODES_SWITCH_DF As Integer =       5346
  Public Const  LS_DPARAM_MIP_ABSOPTTOL        As Integer =       5347
  Public Const  LS_DPARAM_MIP_MINABSOBJSTEP    As Integer =       5348
  Public Const  LS_IPARAM_MIP_PSEUDOCOST_RULE  As Integer =       5349
  Public Const  LS_IPARAM_MIP_ENUM_HEUMODE     As Integer =       5350
  Public Const  LS_IPARAM_MIP_PRELEVEL_TREE    As Integer =       5351
  Public Const  LS_DPARAM_MIP_REDCOSTFIX_CUTOFF_TREE As Integer =       5352
  Public Const  LS_IPARAM_MIP_USE_INT_ZERO_TOL As Integer =       5353
  Public Const  LS_IPARAM_MIP_USE_CUTS_HEU     As Integer =       5354
  Public Const  LS_DPARAM_MIP_BIGM_FOR_INTTOL  As Integer =       5355
  Public Const  LS_IPARAM_MIP_STRONGBRANCHDONUM As Integer =       5366
  Public Const  LS_IPARAM_MIP_MAKECUT_INACTIVE_COUNT As Integer =       5367
  Public Const  LS_IPARAM_MIP_PRE_ELIM_FILL    As Integer =       5368
  Public Const  LS_IPARAM_MIP_HEU_MODE         As Integer =       5369
  Public Const  LS_DPARAM_MIP_TIMLIM           As Integer =       5370
  Public Const  LS_DPARAM_MIP_AOPTTIMLIM       As Integer =       5371
  Public Const  LS_DPARAM_MIP_LSOLTIMLIM       As Integer =       5372
  Public Const  LS_DPARAM_MIP_CUTTIMLIM        As Integer =       5373
  Public Const  LS_DPARAM_MIP_HEUMINTIMLIM     As Integer =       5374
  Public Const  LS_IPARAM_MIP_FP_MODE          As Integer =       5375
  Public Const  LS_DPARAM_MIP_FP_WEIGHT        As Integer =       5376
  Public Const  LS_IPARAM_MIP_FP_OPT_METHOD    As Integer =       5377
  Public Const  LS_DPARAM_MIP_FP_TIMLIM        As Integer =       5378
  Public Const  LS_IPARAM_MIP_FP_ITRLIM        As Integer =       5379
  Public Const  LS_IPARAM_MIP_FP_HEU_MODE      As Integer =       5380
  Public Const  LS_DPARAM_MIP_OBJ_THRESHOLD    As Integer =       5381
  Public Const  LS_IPARAM_MIP_LOCALBRANCHNUM   As Integer =       5382
  Public Const  LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_TIME As Integer =       5383
  Public Const  LS_DPARAM_MIP_ITRLIM_SIM       As Integer =       5384
  Public Const  LS_DPARAM_MIP_ITRLIM_NLP       As Integer =       5385
  Public Const  LS_DPARAM_MIP_ITRLIM_IPM       As Integer =       5386
  Public Const  LS_IPARAM_MIP_MAXNUM_MIP_SOL_STORAGE As Integer =       5387
  Public Const  LS_IPARAM_MIP_CONCURRENT_TOPOPTMODE As Integer =       5388
  Public Const  LS_IPARAM_MIP_CONCURRENT_REOPTMODE As Integer =       5389
  Public Const  LS_IPARAM_MIP_PREHEU_LEVEL     As Integer =       5390
  Public Const  LS_IPARAM_MIP_PREHEU_PRE_LEVEL As Integer =       5391
  Public Const  LS_IPARAM_MIP_PREHEU_PRINT_LEVEL As Integer =       5392
  Public Const  LS_IPARAM_MIP_PREHEU_TC_ITERLIM As Integer =       5393
  Public Const  LS_IPARAM_MIP_PREHEU_DFE_VSTLIM As Integer =       5394
  Public Const  LS_IPARAM_MIP_PREHEU_VAR_SEQ   As Integer =       5395
  Public Const  LS_IPARAM_MIP_USE_PARTIALSOL_LEVEL As Integer =       5396
  Public Const  LS_IPARAM_MIP_GENERAL_MODE     As Integer =       5397
  Public Const  LS_IPARAM_MIP_NUM_THREADS      As Integer =       5398
  Public Const  LS_IPARAM_MIP_POLISH_NUM_BRANCH_NEXT As Integer =       5399
  Public Const  LS_IPARAM_MIP_POLISH_MAX_BRANCH_COUNT As Integer =       5400
  Public Const  LS_DPARAM_MIP_POLISH_ALPHA_TARGET As Integer =       5401
  Public Const  LS_IPARAM_MIP_CONCURRENT_STRATEGY As Integer =       5402
  Public Const  LS_DPARAM_MIP_BRANCH_TOP_VAL_DIFF_WEIGHT As Integer =       5403
  Public Const  LS_IPARAM_MIP_BASCUTS_DONUM    As Integer =       5404
  Public Const  LS_IPARAM_MIP_PARA_SUB         As Integer =       5405
  Public Const  LS_DPARAM_MIP_PARA_RND_ITRLMT  As Integer =       5406
  Public Const  LS_DPARAM_MIP_PARA_INIT_NODE   As Integer =       5407
  Public Const  LS_IPARAM_MIP_PARA_ITR_MODE    As Integer =       5408
  Public Const  LS_IPARAM_MIP_PARA_FP          As Integer =       5409
  Public Const  LS_IPARAM_MIP_PARA_FP_MODE     As Integer =       5410
  Public Const  LS_IPARAM_MIP_HEU_DROP_OBJ     As Integer =       5411
  Public Const  LS_DPARAM_MIP_ABSCUTTOL        As Integer =       5412
  Public Const  LS_IPARAM_MIP_PERSPECTIVE_REFORM As Integer =       5413
  Public Const  LS_IPARAM_MIP_TREEREORDERMODE  As Integer =       5414
  Public Const  LS_IPARAM_MIP_XSOLVER          As Integer =       5415
  Public Const  LS_IPARAM_MIP_BNB_TRY_BNP      As Integer =       5416
  Public Const  LS_IPARAM_MIP_KBEST_USE_GOP    As Integer =       5417
  Public Const  LS_IPARAM_MIP_SYMMETRY_MODE    As Integer =       5418
  Public Const  LS_IPARAM_MIP_ALLDIFF_METHOD   As Integer =       5419
  Public Const  LS_IPARAM_MIP_SOLLIM           As Integer =       5420

    ' Global optimization (GOP) parameters (6000 - 6+++) '
  Public Const  LS_DPARAM_GOP_RELOPTTOL        As Integer =       6400
  Public Const  LS_DPARAM_GOP_FLTTOL           As Integer =       6401
  Public Const  LS_DPARAM_GOP_BOXTOL           As Integer =       6402
  Public Const  LS_DPARAM_GOP_WIDTOL           As Integer =       6403
  Public Const  LS_DPARAM_GOP_DELTATOL         As Integer =       6404
  Public Const  LS_DPARAM_GOP_BNDLIM           As Integer =       6405
  Public Const  LS_IPARAM_GOP_TIMLIM           As Integer =       6406
  Public Const  LS_IPARAM_GOP_OPTCHKMD         As Integer =       6407
  Public Const  LS_IPARAM_GOP_BRANCHMD         As Integer =       6408
  Public Const  LS_IPARAM_GOP_MAXWIDMD         As Integer =       6409
  Public Const  LS_IPARAM_GOP_PRELEVEL         As Integer =       6410
  Public Const  LS_IPARAM_GOP_POSTLEVEL        As Integer =       6411
  Public Const  LS_IPARAM_GOP_BBSRCHMD         As Integer =       6412
  Public Const  LS_IPARAM_GOP_DECOMPPTMD       As Integer =       6413
  Public Const  LS_IPARAM_GOP_ALGREFORMMD      As Integer =       6414
  Public Const  LS_IPARAM_GOP_RELBRNDMD        As Integer =       6415
  Public Const  LS_IPARAM_GOP_PRINTLEVEL       As Integer =       6416
  Public Const  LS_IPARAM_GOP_BNDLIM_MODE      As Integer =       6417
  Public Const  LS_IPARAM_GOP_BRANCH_LIMIT     As Integer =       6418
  Public Const  LS_IPARAM_GOP_CORELEVEL        As Integer =       6419
  Public Const  LS_IPARAM_GOP_OPT_MODE         As Integer =       6420
  Public Const  LS_IPARAM_GOP_HEU_MODE         As Integer =       6421
  Public Const  LS_IPARAM_GOP_SUBOUT_MODE      As Integer =       6422
  Public Const  LS_IPARAM_GOP_USE_NLPSOLVE     As Integer =       6423
  Public Const  LS_IPARAM_GOP_LSOLBRANLIM      As Integer =       6424
  Public Const  LS_IPARAM_GOP_LPSOPT           As Integer =       6425
  Public Const  LS_DPARAM_GOP_TIMLIM           As Integer =       6426
  Public Const  LS_DPARAM_GOP_BRANCH_LIMIT     As Integer =       6427
  Public Const  LS_IPARAM_GOP_QUADMD           As Integer =       6428
  Public Const  LS_IPARAM_GOP_LIM_MODE         As Integer =       6429
  Public Const  LS_DPARAM_GOP_ITRLIM           As Integer =       6430
  Public Const  LS_DPARAM_GOP_ITRLIM_SIM       As Integer =       6431
  Public Const  LS_DPARAM_GOP_ITRLIM_IPM       As Integer =       6432
  Public Const  LS_DPARAM_GOP_ITRLIM_NLP       As Integer =       6433
  Public Const  LS_DPARAM_GOP_ABSOPTTOL        As Integer =       6434
  Public Const  LS_DPARAM_GOP_PEROPTTOL        As Integer =       6435
  Public Const  LS_DPARAM_GOP_AOPTTIMLIM       As Integer =       6436
  Public Const  LS_IPARAM_GOP_LINEARZ          As Integer =       6437
  Public Const  LS_IPARAM_GOP_NUM_THREADS      As Integer =       6438
  Public Const  LS_IPARAM_GOP_MULTILINEAR      As Integer =       6439
  Public Const  LS_DPARAM_GOP_OBJ_THRESHOLD    As Integer =       6440
  Public Const  LS_IPARAM_GOP_QUAD_METHOD      As Integer =       6441
  Public Const  LS_IPARAM_GOP_SOLLIM           As Integer =       6442
  Public Const  LS_IPARAM_GOP_CMINLP           As Integer =       6443
  Public Const  LS_IPARAM_GOP_CONIC_REFORM     As Integer =       6644

    ' License information parameters '
  Public Const  LS_IPARAM_LIC_CONSTRAINTS      As Integer =        500
  Public Const  LS_IPARAM_LIC_VARIABLES        As Integer =        501
  Public Const  LS_IPARAM_LIC_INTEGERS         As Integer =        502
  Public Const  LS_IPARAM_LIC_NONLINEARVARS    As Integer =        503
  Public Const  LS_IPARAM_LIC_GOP_INTEGERS     As Integer =        504
  Public Const  LS_IPARAM_LIC_GOP_NONLINEARVARS As Integer =        505
  Public Const  LS_IPARAM_LIC_DAYSTOEXP        As Integer =        506
  Public Const  LS_IPARAM_LIC_DAYSTOTRIALEXP   As Integer =        507
  Public Const  LS_IPARAM_LIC_NONLINEAR        As Integer =        508
  Public Const  LS_IPARAM_LIC_EDUCATIONAL      As Integer =        509
  Public Const  LS_IPARAM_LIC_RUNTIME          As Integer =        510
  Public Const  LS_IPARAM_LIC_NUMUSERS         As Integer =        511
  Public Const  LS_IPARAM_LIC_BARRIER          As Integer =        512
  Public Const  LS_IPARAM_LIC_GLOBAL           As Integer =        513
  Public Const  LS_IPARAM_LIC_PLATFORM         As Integer =        514
  Public Const  LS_IPARAM_LIC_MIP              As Integer =        515
  Public Const  LS_IPARAM_LIC_SP               As Integer =        516
  Public Const  LS_IPARAM_LIC_CONIC            As Integer =        517
  Public Const  LS_IPARAM_LIC_RESERVED1        As Integer =        519

    ' Model analysis parameters (1500 - 15++) '
  Public Const  LS_IPARAM_IIS_ANALYZE_LEVEL    As Integer =       1550
  Public Const  LS_IPARAM_IUS_ANALYZE_LEVEL    As Integer =       1551
  Public Const  LS_IPARAM_IIS_TOPOPT           As Integer =       1552
  Public Const  LS_IPARAM_IIS_REOPT            As Integer =       1553
  Public Const  LS_IPARAM_IIS_USE_SFILTER      As Integer =       1554
  Public Const  LS_IPARAM_IIS_PRINT_LEVEL      As Integer =       1555
  Public Const  LS_IPARAM_IIS_INFEAS_NORM      As Integer =       1556
  Public Const  LS_IPARAM_IIS_ITER_LIMIT       As Integer =       1557
  Public Const  LS_DPARAM_IIS_ITER_LIMIT       As Integer =       1558
  Public Const  LS_IPARAM_IIS_TIME_LIMIT       As Integer =       1559
  Public Const  LS_IPARAM_IIS_METHOD           As Integer =       1560
  Public Const  LS_IPARAM_IIS_USE_EFILTER      As Integer =       1561
  Public Const  LS_IPARAM_IIS_USE_GOP          As Integer =       1562
  Public Const  LS_IPARAM_IIS_NUM_THREADS      As Integer =       1563
  Public Const  LS_IPARAM_IIS_GETMODE          As Integer =       1564

    ' Output log format parameter '
  Public Const  LS_IPARAM_FMT_ISSQL            As Integer =       1590

    '! Common sample size per stochastic parameter. '
  Public Const  LS_IPARAM_STOC_NSAMPLE_SPAR    As Integer =       6600

    '! Common sample size per stage.  '
  Public Const  LS_IPARAM_STOC_NSAMPLE_STAGE   As Integer =       6601

    '! Seed to initialize the random number generator. '
  Public Const  LS_IPARAM_STOC_RG_SEED         As Integer =       6602

    '! Stochastic optimization method to solve the model. '
  Public Const  LS_IPARAM_STOC_METHOD          As Integer =       6603

    '! Reoptimization method to solve the node-models. '
  Public Const  LS_IPARAM_STOC_REOPT           As Integer =       6604

    '! Optimization method to solve the root problem. '
  Public Const  LS_IPARAM_STOC_TOPOPT          As Integer =       6605

    '! Iteration limit for stochastic solver. '
  Public Const  LS_IPARAM_STOC_ITER_LIM        As Integer =       6606

    '! Print level to display progress information during optimization '
  Public Const  LS_IPARAM_STOC_PRINT_LEVEL     As Integer =       6607

    '! Type of deterministic equivalent '
  Public Const  LS_IPARAM_STOC_DETEQ_TYPE      As Integer =       6608

    '! Flag to enabledisable calculation of EVPI. '
  Public Const  LS_IPARAM_STOC_CALC_EVPI       As Integer =       6609

    '! Flag to restrict sampling to continuous stochastic parameters only or not.'
  Public Const  LS_IPARAM_STOC_SAMP_CONT_ONLY  As Integer =       6611

    '! Bucket size in Benders decomposition '
  Public Const  LS_IPARAM_STOC_BUCKET_SIZE     As Integer =       6612

    '! Maximum number of scenarios before forcing automatic sampling '
  Public Const  LS_IPARAM_STOC_MAX_NUMSCENS    As Integer =       6613

    '! Stage beyond which node-models are shared '
  Public Const  LS_IPARAM_STOC_SHARE_BEGSTAGE  As Integer =       6614

    '! Print level to display progress information during optimization of node models '
  Public Const  LS_IPARAM_STOC_NODELP_PRELEVEL As Integer =       6615

    '! Time limit for stochastic solver.'
  Public Const  LS_DPARAM_STOC_TIME_LIM        As Integer =       6616

    '! Relative optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. '
  Public Const  LS_DPARAM_STOC_RELOPTTOL       As Integer =       6617

    '! Absolute optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. '
  Public Const  LS_DPARAM_STOC_ABSOPTTOL       As Integer =       6618

    '! Internal mask '
  Public Const  LS_IPARAM_STOC_DEBUG_MASK      As Integer =       6619

    '! Sampling method for variance reduction. '
  Public Const  LS_IPARAM_STOC_VARCONTROL_METHOD As Integer =       6620

    '! Correlation type associated with correlation matrix.  '
  Public Const  LS_IPARAM_STOC_CORRELATION_TYPE As Integer =       6621

    '! Warm start basis for wait-see model  .  '
  Public Const  LS_IPARAM_STOC_WSBAS           As Integer =       6622

    '! Outer loop iteration limit for ALD  .  '
  Public Const  LS_IPARAM_STOC_ALD_OUTER_ITER_LIM As Integer =       6623

    '! Inner loop iteration limit for ALD  .  '
  Public Const  LS_IPARAM_STOC_ALD_INNER_ITER_LIM As Integer =       6624

    '! Dual feasibility tolerance for ALD  .  '
  Public Const  LS_DPARAM_STOC_ALD_DUAL_FEASTOL As Integer =       6625

    '! Primal feasibility tolerance for ALD  .  '
  Public Const  LS_DPARAM_STOC_ALD_PRIMAL_FEASTOL As Integer =       6626

    '! Dual step length for ALD  .  '
  Public Const  LS_DPARAM_STOC_ALD_DUAL_STEPLEN As Integer =       6627

    '! Primal step length for ALD  .  '
  Public Const  LS_DPARAM_STOC_ALD_PRIMAL_STEPLEN As Integer =       6628

    '! Order nontemporal models or not.  '
  Public Const  LS_IPARAM_CORE_ORDER_BY_STAGE  As Integer =       6629

    '! Node name format.  '
  Public Const  LS_SPARAM_STOC_FMT_NODE_NAME   As Integer =       6630

    '! Scenario name format.  '
  Public Const  LS_SPARAM_STOC_FMT_SCENARIO_NAME As Integer =       6631

    '! Flag to specify whether stochastic parameters in MPI will be mapped as LP matrix elements.  '
  Public Const  LS_IPARAM_STOC_MAP_MPI2LP      As Integer =       6632

    '! Flag to enable or disable autoaggregation '
  Public Const  LS_IPARAM_STOC_AUTOAGGR        As Integer =       6633

    '! Benchmark scenario to compare EVPI and EVMU against'
  Public Const  LS_IPARAM_STOC_BENCHMARK_SCEN  As Integer =       6634

    '! Value to truncate infinite bounds at non-leaf nodes '
  Public Const  LS_DPARAM_STOC_INFBND          As Integer =       6635

    '! Flag to use add-instructions mode when building deteq '
  Public Const  LS_IPARAM_STOC_ADD_MPI         As Integer =       6636

    ' Flag to enable elimination of fixed variables from deteq MPI '
  Public Const  LS_IPARAM_STOC_ELIM_FXVAR      As Integer =       6637

    '! RHS value of objective cut in SBD master problem.  '
  Public Const  LS_DPARAM_STOC_SBD_OBJCUTVAL   As Integer =       6638

    '! Flag to enable objective cut in SBD master problem.  '
  Public Const  LS_IPARAM_STOC_SBD_OBJCUTFLAG  As Integer =       6639

    '! Maximum number of candidate solutions to generate at SBD root '
  Public Const  LS_IPARAM_STOC_SBD_NUMCANDID   As Integer =       6640

    '! Big-M value for linearization and penalty functions '
  Public Const  LS_DPARAM_STOC_BIGM            As Integer =       6641

    '! Name data level '
  Public Const  LS_IPARAM_STOC_NAMEDATA_LEVEL  As Integer =       6642

    '! Max cuts to generate for master problem '
  Public Const  LS_IPARAM_STOC_SBD_MAXCUTS     As Integer =       6643

    '! Optimization method to solve the deteq problem. '
  Public Const  LS_IPARAM_STOC_DEQOPT          As Integer =       6644

    '! Subproblem formulation to use in DirectSearch. '
  Public Const  LS_IPARAM_STOC_DS_SUBFORM      As Integer =       6645

    '! Primal-step tolerance '
  Public Const  LS_DPARAM_STOC_REL_PSTEPTOL    As Integer =       6646

    '! Dual-step tolerance '
  Public Const  LS_DPARAM_STOC_REL_DSTEPTOL    As Integer =       6647

    '! Number of parallel threads '
  Public Const  LS_IPARAM_STOC_NUM_THREADS     As Integer =       6648

    '! Number of deteq blocks '
  Public Const  LS_IPARAM_STOC_DETEQ_NBLOCKS   As Integer =       6649

    '! Bitmask to enable methods for solving the nearest correlation matrix (NCM) subproblem '
  Public Const  LS_IPARAM_SAMP_NCM_METHOD      As Integer =       7701

    '! Objective cutoff (target) value to stop the nearest correlation matrix (NCM) subproblem '
  Public Const  LS_DPARAM_SAMP_NCM_CUTOBJ      As Integer =       7702

    '! Flag to enabledisable sparse mode in NCM computations '
  Public Const  LS_IPARAM_SAMP_NCM_DSTORAGE    As Integer =       7703

    '! Correlation matrix diagonal shift increment '
  Public Const  LS_DPARAM_SAMP_CDSINC          As Integer =       7704

    '! Flag to enable scaling of raw sample data '
  Public Const  LS_IPARAM_SAMP_SCALE           As Integer =       7705

    '! Iteration limit for NCM method '
  Public Const  LS_IPARAM_SAMP_NCM_ITERLIM     As Integer =       7706

    '! Optimality tolerance for NCM method '
  Public Const  LS_DPARAM_SAMP_NCM_OPTTOL      As Integer =       7707

    '! Number of parallel threads '
  Public Const  LS_IPARAM_SAMP_NUM_THREADS     As Integer =       7708

    '! Buffer size for random number generators '
  Public Const  LS_IPARAM_SAMP_RG_BUFFER_SIZE  As Integer =       7709

    'bound size when subproblem is unbounded'
  Public Const  LS_DPARAM_BNP_INFBND           As Integer =       8010

    'branch and bound type'
  Public Const  LS_IPARAM_BNP_LEVEL            As Integer =       8011

    'print level'
  Public Const  LS_IPARAM_BNP_PRINT_LEVEL      As Integer =       8012

    'box size for BOXSTEP method'
  Public Const  LS_DPARAM_BNP_BOX_SIZE         As Integer =       8013

    'number of threads in bnp'
  Public Const  LS_IPARAM_BNP_NUM_THREADS      As Integer =       8014

    'relative optimality tolerance for subproblems'
  Public Const  LS_DPARAM_BNP_SUB_ITRLMT       As Integer =       8015

    'method for finding block structure'
  Public Const  LS_IPARAM_BNP_FIND_BLK         As Integer =       8016

    'pre level'
  Public Const  LS_IPARAM_BNP_PRELEVEL         As Integer =       8017

    'column limit'
  Public Const  LS_DPARAM_BNP_COL_LMT          As Integer =       8018

    'time limit for bnp'
  Public Const  LS_DPARAM_BNP_TIMLIM           As Integer =       8019

    'simplex limit for bnp'
  Public Const  LS_DPARAM_BNP_ITRLIM_SIM       As Integer =       8020

    'ipm limit for bnp'
  Public Const  LS_DPARAM_BNP_ITRLIM_IPM       As Integer =       8021

    'branch limit for bnp'
  Public Const  LS_IPARAM_BNP_BRANCH_LIMIT     As Integer =       8022

    'iteration limit for bnp'
  Public Const  LS_DPARAM_BNP_ITRLIM           As Integer =       8023

    '  Probability of crossover for continuous variables '
  Public Const  LS_DPARAM_GA_CXOVER_PROB       As Integer =       8501

    '  Spreading factor for crossover '
  Public Const  LS_DPARAM_GA_XOVER_SPREAD      As Integer =       8502

    '  Probability of crossover for integer variables '
  Public Const  LS_DPARAM_GA_IXOVER_PROB       As Integer =       8503

    '  Probability of mutation for continuous variables '
  Public Const  LS_DPARAM_GA_CMUTAT_PROB       As Integer =       8504

    '  Spreading factor for mutation '
  Public Const  LS_DPARAM_GA_MUTAT_SPREAD      As Integer =       8505

    '  Probability of mutation for integer variables '
  Public Const  LS_DPARAM_GA_IMUTAT_PROB       As Integer =       8506

    '  Numeric zero tolerance in GA '
  Public Const  LS_DPARAM_GA_TOL_ZERO          As Integer =       8507

    '  Primal feasibility tolerance in GA '
  Public Const  LS_DPARAM_GA_TOL_PFEAS         As Integer =       8508

    '  Numeric infinity in GA '
  Public Const  LS_DPARAM_GA_INF               As Integer =       8509

    '  Infinity threshold for finite bounds in GA '
  Public Const  LS_DPARAM_GA_INFBND            As Integer =       8510

    '  Alpha parameter in Blending Alpha Crossover method '
  Public Const  LS_DPARAM_GA_BLXA              As Integer =       8511

    '  Beta parameter in Blending Alpha-Beta Crossover method '
  Public Const  LS_DPARAM_GA_BLXB              As Integer =       8512

    '  Method of crossover for continuous variables '
  Public Const  LS_IPARAM_GA_CXOVER_METHOD     As Integer =       8513

    '  Method of crossover for integer variables '
  Public Const  LS_IPARAM_GA_IXOVER_METHOD     As Integer =       8514

    '  Method of mutation for continuous variables '
  Public Const  LS_IPARAM_GA_CMUTAT_METHOD     As Integer =       8515

    '  Method of mutation for integer variables '
  Public Const  LS_IPARAM_GA_IMUTAT_METHOD     As Integer =       8516

    '  RNG seed for GA '
  Public Const  LS_IPARAM_GA_SEED              As Integer =       8517

    '  Number of generations in GA '
  Public Const  LS_IPARAM_GA_NGEN              As Integer =       8518

    '  Population size in GA '
  Public Const  LS_IPARAM_GA_POPSIZE           As Integer =       8519

    '  Print level to log files '
  Public Const  LS_IPARAM_GA_FILEOUT           As Integer =       8520

    '  Print level for GA '
  Public Const  LS_IPARAM_GA_PRINTLEVEL        As Integer =       8521

    '  Flag to specify whether an optimum individual will be injected '
  Public Const  LS_IPARAM_GA_INJECT_OPT        As Integer =       8522

    '  Number of threads in GA '
  Public Const  LS_IPARAM_GA_NUM_THREADS       As Integer =       8523

    '  Objective function sense '
  Public Const  LS_IPARAM_GA_OBJDIR            As Integer =       8524

    '  Target objective function value '
  Public Const  LS_DPARAM_GA_OBJSTOP           As Integer =       8525

    '  Migration probability  '
  Public Const  LS_DPARAM_GA_MIGRATE_PROB      As Integer =       8526

    '  Search space or search mode  '
  Public Const  LS_IPARAM_GA_SSPACE            As Integer =       8527

    ' Version info '
  Public Const  LS_IPARAM_VER_MAJOR            As Integer =        990
  Public Const  LS_IPARAM_VER_MINOR            As Integer =        991
  Public Const  LS_IPARAM_VER_BUILD            As Integer =        992
  Public Const  LS_IPARAM_VER_REVISION         As Integer =        993

    ' Last card for parameters '
  Public Const  LS_IPARAM_VER_NUMBER           As Integer =        999

 ' Math operator codes (1000-1500) '
  Public Const  EP_NO_OP                       As Integer =       0000
  Public Const  EP_PLUS                        As Integer =       1001
  Public Const  EP_MINUS                       As Integer =       1002
  Public Const  EP_MULTIPLY                    As Integer =       1003
  Public Const  EP_DIVIDE                      As Integer =       1004
  Public Const  EP_POWER                       As Integer =       1005
  Public Const  EP_EQUAL                       As Integer =       1006
  Public Const  EP_NOT_EQUAL                   As Integer =       1007
  Public Const  EP_LTOREQ                      As Integer =       1008
  Public Const  EP_GTOREQ                      As Integer =       1009
  Public Const  EP_LTHAN                       As Integer =       1010
  Public Const  EP_GTHAN                       As Integer =       1011
  Public Const  EP_AND                         As Integer =       1012
  Public Const  EP_OR                          As Integer =       1013
  Public Const  EP_NOT                         As Integer =       1014
  Public Const  EP_PERCENT                     As Integer =       1015
  Public Const  EP_POSATE                      As Integer =       1016
  Public Const  EP_NEGATE                      As Integer =       1017
  Public Const  EP_ABS                         As Integer =       1018
  Public Const  EP_SQRT                        As Integer =       1019
  Public Const  EP_LOG                         As Integer =       1020
  Public Const  EP_LN                          As Integer =       1021
  Public Const  EP_PI                          As Integer =       1022
  Public Const  EP_SIN                         As Integer =       1023
  Public Const  EP_COS                         As Integer =       1024
  Public Const  EP_TAN                         As Integer =       1025
  Public Const  EP_ATAN2                       As Integer =       1026
  Public Const  EP_ATAN                        As Integer =       1027
  Public Const  EP_ASIN                        As Integer =       1028
  Public Const  EP_ACOS                        As Integer =       1029
  Public Const  EP_EXP                         As Integer =       1030
  Public Const  EP_MOD                         As Integer =       1031
  Public Const  EP_FALSE                       As Integer =       1032
  Public Const  EP_TRUE                        As Integer =       1033
  Public Const  EP_IF                          As Integer =       1034
  Public Const  EP_PSN                         As Integer =       1035
  Public Const  EP_PSL                         As Integer =       1036
  Public Const  EP_LGM                         As Integer =       1037
  Public Const  EP_SIGN                        As Integer =       1038
  Public Const  EP_FLOOR                       As Integer =       1039
  Public Const  EP_FPA                         As Integer =       1040
  Public Const  EP_FPL                         As Integer =       1041
  Public Const  EP_PEL                         As Integer =       1042
  Public Const  EP_PEB                         As Integer =       1043
  Public Const  EP_PPS                         As Integer =       1044
  Public Const  EP_PPL                         As Integer =       1045
  Public Const  EP_PTD                         As Integer =       1046
  Public Const  EP_PCX                         As Integer =       1047
  Public Const  EP_WRAP                        As Integer =       1048
  Public Const  EP_PBNO                        As Integer =       1049
  Public Const  EP_PFS                         As Integer =       1050
  Public Const  EP_PFD                         As Integer =       1051
  Public Const  EP_PHG                         As Integer =       1052
  Public Const  EP_RAND                        As Integer =       1053
  Public Const  EP_USER                        As Integer =       1054
  Public Const  EP_SUM                         As Integer =       1055
  Public Const  EP_AVG                         As Integer =       1056
  Public Const  EP_MIN                         As Integer =       1057
  Public Const  EP_MAX                         As Integer =       1058
  Public Const  EP_NPV                         As Integer =       1059
  Public Const  EP_VAND                        As Integer =       1060
  Public Const  EP_VOR                         As Integer =       1061
  Public Const  EP_PUSH_NUM                    As Integer =       1062
  Public Const  EP_PUSH_VAR                    As Integer =       1063
  Public Const  EP_NORMDENS                    As Integer =       1064
  Public Const  EP_NORMINV                     As Integer =       1065
  Public Const  EP_TRIAINV                     As Integer =       1066
  Public Const  EP_EXPOINV                     As Integer =       1067
  Public Const  EP_UNIFINV                     As Integer =       1068
  Public Const  EP_MULTINV                     As Integer =       1069
  Public Const  EP_USRCOD                      As Integer =       1070
  Public Const  EP_SUMPROD                     As Integer =       1071
  Public Const  EP_SUMIF                       As Integer =       1072
  Public Const  EP_VLOOKUP                     As Integer =       1073
  Public Const  EP_VPUSH_NUM                   As Integer =       1074
  Public Const  EP_VPUSH_VAR                   As Integer =       1075
  Public Const  EP_VMULT                       As Integer =       1076
  Public Const  EP_SQR                         As Integer =       1077
  Public Const  EP_SINH                        As Integer =       1078
  Public Const  EP_COSH                        As Integer =       1079
  Public Const  EP_TANH                        As Integer =       1080
  Public Const  EP_ASINH                       As Integer =       1081
  Public Const  EP_ACOSH                       As Integer =       1082
  Public Const  EP_ATANH                       As Integer =       1083
  Public Const  EP_LOGB                        As Integer =       1084
  Public Const  EP_LOGX                        As Integer =       1085
  Public Const  EP_LNX                         As Integer =       1086
  Public Const  EP_TRUNC                       As Integer =       1087
  Public Const  EP_NORMSINV                    As Integer =       1088
  Public Const  EP_INT                         As Integer =       1089
  Public Const  EP_PUSH_STR                    As Integer =       1090
  Public Const  EP_VPUSH_STR                   As Integer =       1091
  Public Const  EP_PUSH_SPAR                   As Integer =       1092
  Public Const  EP_NORMPDF                     As Integer =       1093
  Public Const  EP_NORMCDF                     As Integer =       1094
  Public Const  EP_LSQ                         As Integer =       1095
  Public Const  EP_LNPSNX                      As Integer =       1096
  Public Const  EP_LNCPSN                      As Integer =       1097
  Public Const  EP_XEXPNAX                     As Integer =       1098
  Public Const  EP_XNEXPMX                     As Integer =       1099

    ' Probability density functions '
  Public Const  EP_PBT                         As Integer =       1100
  Public Const  EP_PBTINV                      As Integer =       1101
  Public Const  EP_PBNINV                      As Integer =       1102
  Public Const  EP_PCC                         As Integer =       1103
  Public Const  EP_PCCINV                      As Integer =       1104
  Public Const  EP_PCXINV                      As Integer =       1105
  Public Const  EP_EXPN                        As Integer =       1106
  Public Const  EP_PFDINV                      As Integer =       1107
  Public Const  EP_PGA                         As Integer =       1108
  Public Const  EP_PGAINV                      As Integer =       1109
  Public Const  EP_PGE                         As Integer =       1110
  Public Const  EP_PGEINV                      As Integer =       1111
  Public Const  EP_PGU                         As Integer =       1112
  Public Const  EP_PGUINV                      As Integer =       1113
  Public Const  EP_PHGINV                      As Integer =       1114
  Public Const  EP_PLA                         As Integer =       1115
  Public Const  EP_PLAINV                      As Integer =       1116
  Public Const  EP_PLG                         As Integer =       1117
  Public Const  EP_PLGINV                      As Integer =       1118
  Public Const  EP_LGT                         As Integer =       1119
  Public Const  EP_LGTINV                      As Integer =       1120
  Public Const  EP_LGNM                        As Integer =       1121
  Public Const  EP_LGNMINV                     As Integer =       1122
  Public Const  EP_NGBN                        As Integer =       1123
  Public Const  EP_NGBNINV                     As Integer =       1124
  Public Const  EP_NRM                         As Integer =       1125
  Public Const  EP_PPT                         As Integer =       1126
  Public Const  EP_PPTINV                      As Integer =       1127
  Public Const  EP_PPSINV                      As Integer =       1128
  Public Const  EP_PTDINV                      As Integer =       1129
  Public Const  EP_TRIAN                       As Integer =       1130
  Public Const  EP_UNIFM                       As Integer =       1131
  Public Const  EP_PWB                         As Integer =       1132
  Public Const  EP_PWBINV                      As Integer =       1133
  Public Const  EP_NRMINV                      As Integer =       1134
  Public Const  EP_TRIANINV                    As Integer =       1135
  Public Const  EP_EXPNINV                     As Integer =       1136
  Public Const  EP_UNIFMINV                    As Integer =       1137
  Public Const  EP_MLTNMINV                    As Integer =       1138
  Public Const  EP_BTDENS                      As Integer =       1139
  Public Const  EP_BNDENS                      As Integer =       1140
  Public Const  EP_CCDENS                      As Integer =       1141
  Public Const  EP_CXDENS                      As Integer =       1142
  Public Const  EP_EXPDENS                     As Integer =       1143
  Public Const  EP_FDENS                       As Integer =       1144
  Public Const  EP_GADENS                      As Integer =       1145
  Public Const  EP_GEDENS                      As Integer =       1146
  Public Const  EP_GUDENS                      As Integer =       1147
  Public Const  EP_HGDENS                      As Integer =       1148
  Public Const  EP_LADENS                      As Integer =       1149
  Public Const  EP_LGDENS                      As Integer =       1150
  Public Const  EP_LGTDENS                     As Integer =       1151
  Public Const  EP_LGNMDENS                    As Integer =       1152
  Public Const  EP_NGBNDENS                    As Integer =       1153
  Public Const  EP_NRMDENS                     As Integer =       1154
  Public Const  EP_PTDENS                      As Integer =       1155
  Public Const  EP_PSDENS                      As Integer =       1156
  Public Const  EP_TDENS                       As Integer =       1157
  Public Const  EP_TRIADENS                    As Integer =       1158
  Public Const  EP_UNIFDENS                    As Integer =       1159
  Public Const  EP_WBDENS                      As Integer =       1160
  Public Const  EP_RADIANS                     As Integer =       1161
  Public Const  EP_DEGREES                     As Integer =       1162
  Public Const  EP_ROUND                       As Integer =       1163
  Public Const  EP_ROUNDUP                     As Integer =       1164
  Public Const  EP_ROUNDDOWN                   As Integer =       1165
  Public Const  EP_ERF                         As Integer =       1166
  Public Const  EP_PBN                         As Integer =       1167
  Public Const  EP_PBB                         As Integer =       1168
  Public Const  EP_PBBINV                      As Integer =       1169
  Public Const  EP_BBDENS                      As Integer =       1170
  Public Const  EP_PSS                         As Integer =       1171
  Public Const  EP_SSDENS                      As Integer =       1172
  Public Const  EP_SSINV                       As Integer =       1173
  Public Const  EP_POSD                        As Integer =       1174
  Public Const  EP_SETS                        As Integer =       1175
  Public Const  EP_CARD                        As Integer =       1176
  Public Const  EP_STDEV                       As Integer =       1177
  Public Const  EP_LMTD                        As Integer =       1178
  Public Const  EP_RLMTD                       As Integer =       1179
  Public Const  EP_LOGIT                       As Integer =       1180
  Public Const  EP_ALLDIFF                     As Integer =       1181
  Public Const  EP_SIGNPOWER                   As Integer =       1182
  Public Const  EP_QUADPROD                    As Integer =       1183
  Public Const  EP_ATAN2R                      As Integer =       1184
  Public Const  EP_XPOWDIVAB                   As Integer =       1185
  Public Const  EP_LOGABEXPX                   As Integer =       1186

 ' Model statistics (11001-11199)'
  Public Const  LS_IINFO_NUM_NONZ_OBJ          As Integer =      11001
  Public Const  LS_IINFO_NUM_SEMICONT          As Integer =      11002
  Public Const  LS_IINFO_NUM_SETS              As Integer =      11003
  Public Const  LS_IINFO_NUM_SETS_NNZ          As Integer =      11004
  Public Const  LS_IINFO_NUM_QCP_CONS          As Integer =      11005
  Public Const  LS_IINFO_NUM_CONT_CONS         As Integer =      11006
  Public Const  LS_IINFO_NUM_INT_CONS          As Integer =      11007
  Public Const  LS_IINFO_NUM_BIN_CONS          As Integer =      11008
  Public Const  LS_IINFO_NUM_QCP_VARS          As Integer =      11009
  Public Const  LS_IINFO_NUM_CONS              As Integer =      11010
  Public Const  LS_IINFO_NUM_VARS              As Integer =      11011
  Public Const  LS_IINFO_NUM_NONZ              As Integer =      11012
  Public Const  LS_IINFO_NUM_BIN               As Integer =      11013
  Public Const  LS_IINFO_NUM_INT               As Integer =      11014
  Public Const  LS_IINFO_NUM_CONT              As Integer =      11015
  Public Const  LS_IINFO_NUM_QC_NONZ           As Integer =      11016
  Public Const  LS_IINFO_NUM_NLP_NONZ          As Integer =      11017
  Public Const  LS_IINFO_NUM_NLPOBJ_NONZ       As Integer =      11018
  Public Const  LS_IINFO_NUM_RDCONS            As Integer =      11019
  Public Const  LS_IINFO_NUM_RDVARS            As Integer =      11020
  Public Const  LS_IINFO_NUM_RDNONZ            As Integer =      11021
  Public Const  LS_IINFO_NUM_RDINT             As Integer =      11022
  Public Const  LS_IINFO_LEN_VARNAMES          As Integer =      11023
  Public Const  LS_IINFO_LEN_CONNAMES          As Integer =      11024
  Public Const  LS_IINFO_NUM_NLP_CONS          As Integer =      11025
  Public Const  LS_IINFO_NUM_NLP_VARS          As Integer =      11026
  Public Const  LS_IINFO_NUM_SUF_ROWS          As Integer =      11027
  Public Const  LS_IINFO_NUM_IIS_ROWS          As Integer =      11028
  Public Const  LS_IINFO_NUM_SUF_BNDS          As Integer =      11029
  Public Const  LS_IINFO_NUM_IIS_BNDS          As Integer =      11030
  Public Const  LS_IINFO_NUM_SUF_COLS          As Integer =      11031
  Public Const  LS_IINFO_NUM_IUS_COLS          As Integer =      11032
  Public Const  LS_IINFO_NUM_CONES             As Integer =      11033
  Public Const  LS_IINFO_NUM_CONE_NONZ         As Integer =      11034
  Public Const  LS_IINFO_LEN_CONENAMES         As Integer =      11035
  Public Const  LS_DINFO_INST_VAL_MIN_COEF     As Integer =      11036
  Public Const  LS_IINFO_INST_VARNDX_MIN_COEF  As Integer =      11037
  Public Const  LS_IINFO_INST_CONNDX_MIN_COEF  As Integer =      11038
  Public Const  LS_DINFO_INST_VAL_MAX_COEF     As Integer =      11039
  Public Const  LS_IINFO_INST_VARNDX_MAX_COEF  As Integer =      11040
  Public Const  LS_IINFO_INST_CONNDX_MAX_COEF  As Integer =      11041
  Public Const  LS_IINFO_NUM_VARS_CARD         As Integer =      11042
  Public Const  LS_IINFO_NUM_VARS_SOS1         As Integer =      11043
  Public Const  LS_IINFO_NUM_VARS_SOS2         As Integer =      11044
  Public Const  LS_IINFO_NUM_VARS_SOS3         As Integer =      11045
  Public Const  LS_IINFO_NUM_VARS_SCONT        As Integer =      11046
  Public Const  LS_IINFO_NUM_CONS_L            As Integer =      11047
  Public Const  LS_IINFO_NUM_CONS_E            As Integer =      11048
  Public Const  LS_IINFO_NUM_CONS_G            As Integer =      11049
  Public Const  LS_IINFO_NUM_CONS_R            As Integer =      11050
  Public Const  LS_IINFO_NUM_CONS_N            As Integer =      11051
  Public Const  LS_IINFO_NUM_VARS_LB           As Integer =      11052
  Public Const  LS_IINFO_NUM_VARS_UB           As Integer =      11053
  Public Const  LS_IINFO_NUM_VARS_LUB          As Integer =      11054
  Public Const  LS_IINFO_NUM_VARS_FR           As Integer =      11055
  Public Const  LS_IINFO_NUM_VARS_FX           As Integer =      11056
  Public Const  LS_IINFO_NUM_INST_CODES        As Integer =      11057
  Public Const  LS_IINFO_NUM_INST_REAL_NUM     As Integer =      11058
  Public Const  LS_IINFO_NUM_SPARS             As Integer =      11059
  Public Const  LS_IINFO_NUM_PROCS             As Integer =      11060
  Public Const  LS_IINFO_NUM_POSDS             As Integer =      11061
  Public Const  LS_IINFO_NUM_SUF_INTS          As Integer =      11062
  Public Const  LS_IINFO_NUM_IIS_INTS          As Integer =      11063
  Public Const  LS_IINFO_NUM_OBJPOOL           As Integer =      11064
  Public Const  LS_IINFO_NUM_SOLPOOL           As Integer =      11065
  Public Const  LS_IINFO_NUM_ALLDIFF           As Integer =      11066
  Public Const  LS_IINFO_MAX_RNONZ             As Integer =      11067
  Public Const  LS_IINFO_MAX_CNONZ             As Integer =      11068
  Public Const  LS_DINFO_AVG_RNONZ             As Integer =      11069
  Public Const  LS_DINFO_AVG_CNONZ             As Integer =      11070

 ' LP and NLP related info (11200-11299)'
  Public Const  LS_IINFO_METHOD                As Integer =      11200
  Public Const  LS_DINFO_POBJ                  As Integer =      11201
  Public Const  LS_DINFO_DOBJ                  As Integer =      11202
  Public Const  LS_DINFO_PINFEAS               As Integer =      11203
  Public Const  LS_DINFO_DINFEAS               As Integer =      11204
  Public Const  LS_IINFO_MODEL_STATUS          As Integer =      11205
  Public Const  LS_IINFO_PRIMAL_STATUS         As Integer =      11206
  Public Const  LS_IINFO_DUAL_STATUS           As Integer =      11207
  Public Const  LS_IINFO_BASIC_STATUS          As Integer =      11208
  Public Const  LS_IINFO_BAR_ITER              As Integer =      11209
  Public Const  LS_IINFO_SIM_ITER              As Integer =      11210
  Public Const  LS_IINFO_NLP_ITER              As Integer =      11211
  Public Const  LS_IINFO_ELAPSED_TIME          As Integer =      11212
  Public Const  LS_DINFO_MSW_POBJ              As Integer =      11213
  Public Const  LS_IINFO_MSW_PASS              As Integer =      11214
  Public Const  LS_IINFO_MSW_NSOL              As Integer =      11215
  Public Const  LS_IINFO_IPM_STATUS            As Integer =      11216
  Public Const  LS_DINFO_IPM_POBJ              As Integer =      11217
  Public Const  LS_DINFO_IPM_DOBJ              As Integer =      11218
  Public Const  LS_DINFO_IPM_PINFEAS           As Integer =      11219
  Public Const  LS_DINFO_IPM_DINFEAS           As Integer =      11220
  Public Const  LS_IINFO_NLP_CALL_FUN          As Integer =      11221
  Public Const  LS_IINFO_NLP_CALL_DEV          As Integer =      11222
  Public Const  LS_IINFO_NLP_CALL_HES          As Integer =      11223
  Public Const  LS_IINFO_CONCURRENT_OPTIMIZER  As Integer =      11224
  Public Const  LS_IINFO_LEN_STAGENAMES        As Integer =      11225
  Public Const  LS_DINFO_BAR_ITER              As Integer =      11226
  Public Const  LS_DINFO_SIM_ITER              As Integer =      11227
  Public Const  LS_DINFO_NLP_ITER              As Integer =      11228
  Public Const  LS_IINFO_BAR_THREADS           As Integer =      11229
  Public Const  LS_IINFO_NLP_THREADS           As Integer =      11230
  Public Const  LS_IINFO_SIM_THREADS           As Integer =      11231
  Public Const  LS_DINFO_NLP_THRIMBL           As Integer =      11232
  Public Const  LS_SINFO_NLP_THREAD_LOAD       As Integer =      11233
  Public Const  LS_SINFO_BAR_THREAD_LOAD       As Integer =      11234
  Public Const  LS_SINFO_SIM_THREAD_LOAD       As Integer =      11235
  Public Const  LS_SINFO_ARCH                  As Integer =      11236
  Public Const  LS_IINFO_ARCH_ID               As Integer =      11237
  Public Const  LS_IINFO_MSW_BESTRUNIDX        As Integer =      11238
  Public Const  LS_DINFO_ACONDEST              As Integer =      11239
  Public Const  LS_DINFO_BCONDEST              As Integer =      11240
  Public Const  LS_IINFO_LPTOOL                As Integer =      11241
  Public Const  LS_SINFO_MODEL_TYPE            As Integer =      11242
  Public Const  LS_IINFO_NLP_LINEARITY         As Integer =      11243

 ' MIP and MINLP related info (11300-11400) '
  Public Const  LS_IINFO_MIP_STATUS            As Integer =      11300
  Public Const  LS_DINFO_MIP_OBJ               As Integer =      11301
  Public Const  LS_DINFO_MIP_BESTBOUND         As Integer =      11302
  Public Const  LS_IINFO_MIP_SIM_ITER          As Integer =      11303
  Public Const  LS_IINFO_MIP_BAR_ITER          As Integer =      11304
  Public Const  LS_IINFO_MIP_NLP_ITER          As Integer =      11305
  Public Const  LS_IINFO_MIP_BRANCHCOUNT       As Integer =      11306
  Public Const  LS_IINFO_MIP_NEWIPSOL          As Integer =      11307
  Public Const  LS_IINFO_MIP_LPCOUNT           As Integer =      11308
  Public Const  LS_IINFO_MIP_ACTIVENODES       As Integer =      11309
  Public Const  LS_IINFO_MIP_LTYPE             As Integer =      11310
  Public Const  LS_IINFO_MIP_AOPTTIMETOSTOP    As Integer =      11311
  Public Const  LS_IINFO_MIP_NUM_TOTAL_CUTS    As Integer =      11312
  Public Const  LS_IINFO_MIP_GUB_COVER_CUTS    As Integer =      11313
  Public Const  LS_IINFO_MIP_FLOW_COVER_CUTS   As Integer =      11314
  Public Const  LS_IINFO_MIP_LIFT_CUTS         As Integer =      11315
  Public Const  LS_IINFO_MIP_PLAN_LOC_CUTS     As Integer =      11316
  Public Const  LS_IINFO_MIP_DISAGG_CUTS       As Integer =      11317
  Public Const  LS_IINFO_MIP_KNAPSUR_COVER_CUTS As Integer =      11318
  Public Const  LS_IINFO_MIP_LATTICE_CUTS      As Integer =      11319
  Public Const  LS_IINFO_MIP_GOMORY_CUTS       As Integer =      11320
  Public Const  LS_IINFO_MIP_COEF_REDC_CUTS    As Integer =      11321
  Public Const  LS_IINFO_MIP_GCD_CUTS          As Integer =      11322
  Public Const  LS_IINFO_MIP_OBJ_CUT           As Integer =      11323
  Public Const  LS_IINFO_MIP_BASIS_CUTS        As Integer =      11324
  Public Const  LS_IINFO_MIP_CARDGUB_CUTS      As Integer =      11325
  Public Const  LS_IINFO_MIP_CLIQUE_CUTS       As Integer =      11326
  Public Const  LS_IINFO_MIP_CONTRA_CUTS       As Integer =      11327
  Public Const  LS_IINFO_MIP_GUB_CONS          As Integer =      11328
  Public Const  LS_IINFO_MIP_GLB_CONS          As Integer =      11329
  Public Const  LS_IINFO_MIP_PLANTLOC_CONS     As Integer =      11330
  Public Const  LS_IINFO_MIP_DISAGG_CONS       As Integer =      11331
  Public Const  LS_IINFO_MIP_SB_CONS           As Integer =      11332
  Public Const  LS_IINFO_MIP_IKNAP_CONS        As Integer =      11333
  Public Const  LS_IINFO_MIP_KNAP_CONS         As Integer =      11334
  Public Const  LS_IINFO_MIP_NLP_CONS          As Integer =      11335
  Public Const  LS_IINFO_MIP_CONT_CONS         As Integer =      11336
  Public Const  LS_DINFO_MIP_TOT_TIME          As Integer =      11347
  Public Const  LS_DINFO_MIP_OPT_TIME          As Integer =      11348
  Public Const  LS_DINFO_MIP_HEU_TIME          As Integer =      11349
  Public Const  LS_IINFO_MIP_SOLSTATUS_LAST_BRANCH As Integer =      11350
  Public Const  LS_DINFO_MIP_SOLOBJVAL_LAST_BRANCH As Integer =      11351
  Public Const  LS_IINFO_MIP_HEU_LEVEL         As Integer =      11352
  Public Const  LS_DINFO_MIP_PFEAS             As Integer =      11353
  Public Const  LS_DINFO_MIP_INTPFEAS          As Integer =      11354
  Public Const  LS_IINFO_MIP_WHERE_IN_CODE     As Integer =      11355
  Public Const  LS_IINFO_MIP_FP_ITER           As Integer =      11356
  Public Const  LS_DINFO_MIP_FP_SUMFEAS        As Integer =      11357
  Public Const  LS_DINFO_MIP_RELMIPGAP         As Integer =      11358
  Public Const  LS_DINFO_MIP_ROOT_OPT_TIME     As Integer =      11359
  Public Const  LS_DINFO_MIP_ROOT_PRE_TIME     As Integer =      11360
  Public Const  LS_IINFO_MIP_ROOT_METHOD       As Integer =      11361
  Public Const  LS_DINFO_MIP_SIM_ITER          As Integer =      11362
  Public Const  LS_DINFO_MIP_BAR_ITER          As Integer =      11363
  Public Const  LS_DINFO_MIP_NLP_ITER          As Integer =      11364
  Public Const  LS_IINFO_MIP_TOP_RELAX_IS_NON_CONVEX As Integer =      11365
  Public Const  LS_DINFO_MIP_FP_TIME           As Integer =      11366
  Public Const  LS_IINFO_MIP_THREADS           As Integer =      11367
  Public Const  LS_SINFO_MIP_THREAD_LOAD       As Integer =      11368
  Public Const  LS_DINFO_MIP_ABSGAP            As Integer =      11369
  Public Const  LS_DINFO_MIP_RELGAP            As Integer =      11370
  Public Const  LS_IINFO_MIP_SOFTKNAP_CUTS     As Integer =      11371
  Public Const  LS_IINFO_MIP_LP_ROUND_CUTS     As Integer =      11372
  Public Const  LS_IINFO_MIP_PERSPECTIVE_CUTS  As Integer =      11373
  Public Const  LS_IINFO_MIP_STRATEGY_MASK     As Integer =      11374

 ' GOP related info (11601-11699) '
  Public Const  LS_DINFO_GOP_OBJ               As Integer =      11600
  Public Const  LS_IINFO_GOP_SIM_ITER          As Integer =      11601
  Public Const  LS_IINFO_GOP_BAR_ITER          As Integer =      11602
  Public Const  LS_IINFO_GOP_NLP_ITER          As Integer =      11603
  Public Const  LS_DINFO_GOP_BESTBOUND         As Integer =      11604
  Public Const  LS_IINFO_GOP_STATUS            As Integer =      11605
  Public Const  LS_IINFO_GOP_LPCOUNT           As Integer =      11606
  Public Const  LS_IINFO_GOP_NLPCOUNT          As Integer =      11607
  Public Const  LS_IINFO_GOP_MIPCOUNT          As Integer =      11608
  Public Const  LS_IINFO_GOP_NEWSOL            As Integer =      11609
  Public Const  LS_IINFO_GOP_BOX               As Integer =      11610
  Public Const  LS_IINFO_GOP_BBITER            As Integer =      11611
  Public Const  LS_IINFO_GOP_SUBITER           As Integer =      11612
  Public Const  LS_IINFO_GOP_MIPBRANCH         As Integer =      11613
  Public Const  LS_IINFO_GOP_ACTIVEBOXES       As Integer =      11614
  Public Const  LS_IINFO_GOP_TOT_TIME          As Integer =      11615
  Public Const  LS_IINFO_GOP_MAXDEPTH          As Integer =      11616
  Public Const  LS_DINFO_GOP_PFEAS             As Integer =      11617
  Public Const  LS_DINFO_GOP_INTPFEAS          As Integer =      11618
  Public Const  LS_DINFO_GOP_SIM_ITER          As Integer =      11619
  Public Const  LS_DINFO_GOP_BAR_ITER          As Integer =      11620
  Public Const  LS_DINFO_GOP_NLP_ITER          As Integer =      11621
  Public Const  LS_DINFO_GOP_LPCOUNT           As Integer =      11622
  Public Const  LS_DINFO_GOP_NLPCOUNT          As Integer =      11623
  Public Const  LS_DINFO_GOP_MIPCOUNT          As Integer =      11624
  Public Const  LS_DINFO_GOP_BBITER            As Integer =      11625
  Public Const  LS_DINFO_GOP_SUBITER           As Integer =      11626
  Public Const  LS_DINFO_GOP_MIPBRANCH         As Integer =      11627
  Public Const  LS_DINFO_GOP_FIRST_TIME        As Integer =      11628
  Public Const  LS_DINFO_GOP_BEST_TIME         As Integer =      11629
  Public Const  LS_DINFO_GOP_TOT_TIME          As Integer =      11630
  Public Const  LS_IINFO_GOP_THREADS           As Integer =      11631
  Public Const  LS_SINFO_GOP_THREAD_LOAD       As Integer =      11632
  Public Const  LS_DINFO_GOP_ABSGAP            As Integer =      11633
  Public Const  LS_DINFO_GOP_RELGAP            As Integer =      11634

    ' Progress info during callbacks '
  Public Const  LS_DINFO_SUB_OBJ               As Integer =      11700
  Public Const  LS_DINFO_SUB_PINF              As Integer =      11701
  Public Const  LS_DINFO_CUR_OBJ               As Integer =      11702
  Public Const  LS_IINFO_CUR_ITER              As Integer =      11703
  Public Const  LS_DINFO_CUR_BEST_BOUND        As Integer =      11704
  Public Const  LS_IINFO_CUR_STATUS            As Integer =      11705
  Public Const  LS_IINFO_CUR_LP_COUNT          As Integer =      11706
  Public Const  LS_IINFO_CUR_BRANCH_COUNT      As Integer =      11707
  Public Const  LS_IINFO_CUR_ACTIVE_COUNT      As Integer =      11708
  Public Const  LS_IINFO_CUR_NLP_COUNT         As Integer =      11709
  Public Const  LS_IINFO_CUR_MIP_COUNT         As Integer =      11710
  Public Const  LS_IINFO_CUR_CUT_COUNT         As Integer =      11711
  Public Const  LS_DINFO_CUR_ITER              As Integer =      11712

 ' Model generation progress info (1800+)'
  Public Const  LS_DINFO_GEN_PERCENT           As Integer =      11800
  Public Const  LS_IINFO_GEN_NONZ_TTL          As Integer =      11801
  Public Const  LS_IINFO_GEN_NONZ_NL           As Integer =      11802
  Public Const  LS_IINFO_GEN_ROW_NL            As Integer =      11803
  Public Const  LS_IINFO_GEN_VAR_NL            As Integer =      11804

 ' IIS-IUS info '
  Public Const  LS_IINFO_IIS_BAR_ITER          As Integer =      11850
  Public Const  LS_IINFO_IIS_SIM_ITER          As Integer =      11851
  Public Const  LS_IINFO_IIS_NLP_ITER          As Integer =      11852
  Public Const  LS_DINFO_IIS_BAR_ITER          As Integer =      11853
  Public Const  LS_DINFO_IIS_SIM_ITER          As Integer =      11854
  Public Const  LS_DINFO_IIS_NLP_ITER          As Integer =      11855
  Public Const  LS_IINFO_IIS_TOT_TIME          As Integer =      11856
  Public Const  LS_IINFO_IIS_ACT_NODE          As Integer =      11857
  Public Const  LS_IINFO_IIS_LPCOUNT           As Integer =      11858
  Public Const  LS_IINFO_IIS_NLPCOUNT          As Integer =      11859
  Public Const  LS_IINFO_IIS_MIPCOUNT          As Integer =      11860
  Public Const  LS_IINFO_IIS_THREADS           As Integer =      11861
  Public Const  LS_SINFO_IIS_THREAD_LOAD       As Integer =      11862
  Public Const  LS_IINFO_IIS_STATUS            As Integer =      11863
  Public Const  LS_IINFO_IUS_BAR_ITER          As Integer =      11875
  Public Const  LS_IINFO_IUS_SIM_ITER          As Integer =      11876
  Public Const  LS_IINFO_IUS_NLP_ITER          As Integer =      11877
  Public Const  LS_DINFO_IUS_BAR_ITER          As Integer =      11878
  Public Const  LS_DINFO_IUS_SIM_ITER          As Integer =      11879
  Public Const  LS_DINFO_IUS_NLP_ITER          As Integer =      11880
  Public Const  LS_IINFO_IUS_TOT_TIME          As Integer =      11881
  Public Const  LS_IINFO_IUS_ACT_NODE          As Integer =      11882
  Public Const  LS_IINFO_IUS_LPCOUNT           As Integer =      11883
  Public Const  LS_IINFO_IUS_NLPCOUNT          As Integer =      11884
  Public Const  LS_IINFO_IUS_MIPCOUNT          As Integer =      11885
  Public Const  LS_IINFO_IUS_THREADS           As Integer =      11886
  Public Const  LS_SINFO_IUS_THREAD_LOAD       As Integer =      11887
  Public Const  LS_IINFO_IUS_STATUS            As Integer =      11888

 ' Presolve info    '
  Public Const  LS_IINFO_PRE_NUM_RED           As Integer =      11900
  Public Const  LS_IINFO_PRE_TYPE_RED          As Integer =      11901
  Public Const  LS_IINFO_PRE_NUM_RDCONS        As Integer =      11902
  Public Const  LS_IINFO_PRE_NUM_RDVARS        As Integer =      11903
  Public Const  LS_IINFO_PRE_NUM_RDNONZ        As Integer =      11904
  Public Const  LS_IINFO_PRE_NUM_RDINT         As Integer =      11905

 ' Error info '
  Public Const  LS_IINFO_ERR_OPTIM             As Integer =      11999

    ' IIS Profiler '
  Public Const  LS_DINFO_PROFILE_BASE          As Integer =      12000
  Public Const  LS_DINFO_PROFILE_IIS_FIND_NEC_ROWS As Integer =      12050
  Public Const  LS_DINFO_PROFILE_IIS_FIND_NEC_COLS As Integer =      12051
  Public Const  LS_DINFO_PROFILE_IIS_FIND_SUF_ROWS As Integer =      12052
  Public Const  LS_DINFO_PROFILE_IIS_FIND_SUF_COLS As Integer =      12053

    ' MIP Profiler '
  Public Const  LS_DINFO_PROFILE_MIP_ROOT_LP   As Integer =      12101
  Public Const  LS_DINFO_PROFILE_MIP_TOTAL_LP  As Integer =      12102
  Public Const  LS_DINFO_PROFILE_MIP_LP_SIM_PRIMAL As Integer =      12103
  Public Const  LS_DINFO_PROFILE_MIP_LP_SIM_DUAL As Integer =      12104
  Public Const  LS_DINFO_PROFILE_MIP_LP_SIM_BARRIER As Integer =      12105
  Public Const  LS_DINFO_PROFILE_MIP_PRE_PROCESS As Integer =      12106
  Public Const  LS_DINFO_PROFILE_MIP_FEA_PUMP  As Integer =      12107
  Public Const  LS_DINFO_PROFILE_MIP_TOP_HEURISTIC As Integer =      12108
  Public Const  LS_DINFO_PROFILE_MIP_BNB_HEURISTIC As Integer =      12109
  Public Const  LS_DINFO_PROFILE_MIP_BNB_MAIN_LOOP As Integer =      12110
  Public Const  LS_DINFO_PROFILE_MIP_BNB_SUB_LOOP As Integer =      12111
  Public Const  LS_DINFO_PROFILE_MIP_BNB_BEFORE_BEST As Integer =      12112
  Public Const  LS_DINFO_PROFILE_MIP_BNB_AFTER_BEST As Integer =      12113
  Public Const  LS_DINFO_PROFILE_MIP_TOP_CUT   As Integer =      12114
  Public Const  LS_DINFO_PROFILE_MIP_BNB_CUT   As Integer =      12115
  Public Const  LS_DINFO_PROFILE_MIP_LP_NON_BNB_LOOP As Integer =      12116
  Public Const  LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_MAIN As Integer =      12117
  Public Const  LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_SUB As Integer =      12118
  Public Const  LS_DINFO_PROFILE_MIP_NODE_PRESOLVE As Integer =      12119
  Public Const  LS_DINFO_PROFILE_MIP_BNB_BRANCHING As Integer =      12120
  Public Const  LS_DINFO_PROFILE_MIP_BNB_BRANCHING_MAIN As Integer =      12121
  Public Const  LS_DINFO_PROFILE_MIP_BNB_BRANCHING_SUB As Integer =      12122

    ' GOP Profiler '
  Public Const  LS_DINFO_PROFILE_GOP_SUB_LP_SOLVER As Integer =      12251
  Public Const  LS_DINFO_PROFILE_GOP_SUB_NLP_SOLVER As Integer =      12252
  Public Const  LS_DINFO_PROFILE_GOP_SUB_MIP_SOLVER As Integer =      12253
  Public Const  LS_DINFO_PROFILE_GOP_SUB_GOP_SOLVER As Integer =      12254
  Public Const  LS_DINFO_PROFILE_GOP_CONS_PROP_LP As Integer =      12255
  Public Const  LS_DINFO_PROFILE_GOP_CONS_PROP_NLP As Integer =      12256
  Public Const  LS_DINFO_PROFILE_GOP_VAR_MIN_MAX As Integer =      12257

 ' Misc info '
  Public Const  LS_SINFO_MODEL_FILENAME        As Integer =      12950
  Public Const  LS_SINFO_MODEL_SOURCE          As Integer =      12951
  Public Const  LS_IINFO_MODEL_TYPE            As Integer =      12952
  Public Const  LS_SINFO_CORE_FILENAME         As Integer =      12953
  Public Const  LS_SINFO_STOC_FILENAME         As Integer =      12954
  Public Const  LS_SINFO_TIME_FILENAME         As Integer =      12955
  Public Const  LS_IINFO_ASSIGNED_MODEL_TYPE   As Integer =      12956
  Public Const  LS_IINFO_NZCINDEX              As Integer =      12957
  Public Const  LS_IINFO_NZRINDEX              As Integer =      12958
  Public Const  LS_IINFO_NZCRANK               As Integer =      12959
  Public Const  LS_IINFO_NZRRANK               As Integer =      12960

   '! Expected value of the objective function.  '
  Public Const  LS_DINFO_STOC_EVOBJ            As Integer =      13201

    '! Expected value of perfect information.  '
  Public Const  LS_DINFO_STOC_EVPI             As Integer =      13202

    '! Primal infeasibility of the first stage solution.  '
  Public Const  LS_DINFO_STOC_PINFEAS          As Integer =      13203

    '! Dual infeasibility of the first stage solution.  '
  Public Const  LS_DINFO_STOC_DINFEAS          As Integer =      13204

    '! Relative optimality gap at current solution.  '
  Public Const  LS_DINFO_STOC_RELOPT_GAP       As Integer =      13205

    '! Absolute optimality gap at current solution.  '
  Public Const  LS_DINFO_STOC_ABSOPT_GAP       As Integer =      13206

    '! Number of simplex iterations performed.  '
  Public Const  LS_IINFO_STOC_SIM_ITER         As Integer =      13207

    '! Number of barrier iterations performed.  '
  Public Const  LS_IINFO_STOC_BAR_ITER         As Integer =      13208

    '! Number of nonlinear iterations performed.  '
  Public Const  LS_IINFO_STOC_NLP_ITER         As Integer =      13209

    '! Number of stochastic parameters in the RHS.  '
  Public Const  LS_IINFO_NUM_STOCPAR_RHS       As Integer =      13210

    '! Number of stochastic parameters in the objective function.  '
  Public Const  LS_IINFO_NUM_STOCPAR_OBJ       As Integer =      13211

    '! Number of stochastic parameters in the lower bound.  '
  Public Const  LS_IINFO_NUM_STOCPAR_LB        As Integer =      13212

    '! Number of stochastic parameters in the upper bound.  '
  Public Const  LS_IINFO_NUM_STOCPAR_UB        As Integer =      13213

    '! Number of stochastic parameters in the instructions constituting the objective.  '
  Public Const  LS_IINFO_NUM_STOCPAR_INSTR_OBJS As Integer =      13214

    '! Number of stochastic parameters in the instructions constituting the constraints.  '
  Public Const  LS_IINFO_NUM_STOCPAR_INSTR_CONS As Integer =      13215

    '! Number of stochastic parameters in the LP matrix.  '
  Public Const  LS_IINFO_NUM_STOCPAR_AIJ       As Integer =      13216

    '! Total time elapsed in seconds to solve the model  '
  Public Const  LS_DINFO_STOC_TOTAL_TIME       As Integer =      13217

    '! Status of the SP model.  '
  Public Const  LS_IINFO_STOC_STATUS           As Integer =      13218

    '! Stage of the specified node.  '
  Public Const  LS_IINFO_STOC_STAGE_BY_NODE    As Integer =      13219

    '! Number of scenarios (integer) in the scenario tree. '
  Public Const  LS_IINFO_STOC_NUM_SCENARIOS    As Integer =      13220

    '! Number of scenarios (double) in the scenario tree. '
  Public Const  LS_DINFO_STOC_NUM_SCENARIOS    As Integer =      13221

    '! Number of stages in the model. '
  Public Const  LS_IINFO_STOC_NUM_STAGES       As Integer =      13222

    '! Number of nodes in the scenario tree (integer). '
  Public Const  LS_IINFO_STOC_NUM_NODES        As Integer =      13223

    '! Number of nodes in the scenario tree (double). '
  Public Const  LS_DINFO_STOC_NUM_NODES        As Integer =      13224

    '! Number of nodes that belong to specified stage in the scenario tree (integer). '
  Public Const  LS_IINFO_STOC_NUM_NODES_STAGE  As Integer =      13225

    '! Number of nodes that belong to specified stage in the scenario tree (double). '
  Public Const  LS_DINFO_STOC_NUM_NODES_STAGE  As Integer =      13226

    '! Number of node-models created or to be created. '
  Public Const  LS_IINFO_STOC_NUM_NODE_MODELS  As Integer =      13227

    '! Column offset in DEQ of the first variable associated with the specified node.  '
  Public Const  LS_IINFO_STOC_NUM_COLS_BEFORE_NODE As Integer =      13228

    '! Row offset in DEQ of the first variable associated with the specified node. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_BEFORE_NODE As Integer =      13229

    '! Total number of columns in the implicit DEQ (integer). '
  Public Const  LS_IINFO_STOC_NUM_COLS_DETEQI  As Integer =      13230

    '! Total number of columns in the implicit DEQ (double). '
  Public Const  LS_DINFO_STOC_NUM_COLS_DETEQI  As Integer =      13231

    '! Total number of rows in the implicit DEQ (integer). '
  Public Const  LS_IINFO_STOC_NUM_ROWS_DETEQI  As Integer =      13232

    '! Total number of rows in the implicit DEQ (double). '
  Public Const  LS_DINFO_STOC_NUM_ROWS_DETEQI  As Integer =      13233

    '! Total number of columns in the explict DEQ (integer). '
  Public Const  LS_IINFO_STOC_NUM_COLS_DETEQE  As Integer =      13234

    '! Total number of columns in the explict DEQ (double). '
  Public Const  LS_DINFO_STOC_NUM_COLS_DETEQE  As Integer =      13235

    '! Total number of rows in the explict DEQ (integer). '
  Public Const  LS_IINFO_STOC_NUM_ROWS_DETEQE  As Integer =      13236

    '! Total number of rows in the explict DEQ (double). '
  Public Const  LS_DINFO_STOC_NUM_ROWS_DETEQE  As Integer =      13237

    '! Total number of columns in non-anticipativity block. '
  Public Const  LS_IINFO_STOC_NUM_COLS_NAC     As Integer =      13238

    '! Total number of rows in non-anticipativity block. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_NAC     As Integer =      13239

    '! Total number of columns in core model. '
  Public Const  LS_IINFO_STOC_NUM_COLS_CORE    As Integer =      13240

    '! Total number of rows in core model. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_CORE    As Integer =      13241

    '! Total number of columns in core model in the specified stage. '
  Public Const  LS_IINFO_STOC_NUM_COLS_STAGE   As Integer =      13242

    '! Total number of rows in core model in the specified stage. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_STAGE   As Integer =      13243

    '! Total number of feasibility cuts generated during NBD iterations. '
  Public Const  LS_IINFO_STOC_NUM_NBF_CUTS     As Integer =      13244

    '! Total number of optimality cuts generated during NBD iterations. '
  Public Const  LS_IINFO_STOC_NUM_NBO_CUTS     As Integer =      13245

    '! Distribution type of the sample '
  Public Const  LS_IINFO_DIST_TYPE             As Integer =      13246

    '! Sample size. '
  Public Const  LS_IINFO_SAMP_SIZE             As Integer =      13247

    '! Sample mean. '
  Public Const  LS_DINFO_SAMP_MEAN             As Integer =      13248

    '! Sample standard deviation. '
  Public Const  LS_DINFO_SAMP_STD              As Integer =      13249

    '! Sample skewness. '
  Public Const  LS_DINFO_SAMP_SKEWNESS         As Integer =      13250

    '! Sample kurtosis. '
  Public Const  LS_DINFO_SAMP_KURTOSIS         As Integer =      13251

    '! Total number of quadratic constraints in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_CONS_DETEQE As Integer =      13252

    '! Total number of continuous constraints in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_CONS_DETEQE As Integer =      13253

    '! Total number of constraints with general integer variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_CONS_DETEQE As Integer =      13254

    '! Total number of constraints with binary variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_CONS_DETEQE As Integer =      13255

    '! Total number of quadratic variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_VARS_DETEQE As Integer =      13256

    '! Total number of nonzeros in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NONZ_DETEQE  As Integer =      13259

    '! Total number of binaries in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_DETEQE   As Integer =      13260

    '! Total number of general integer variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_DETEQE   As Integer =      13261

    '! Total number of continuous variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_DETEQE  As Integer =      13262

    '! Total number of quadratic nonzeros in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QC_NONZ_DETEQE As Integer =      13263

    '! Total number of nonlinear nonzeros in the constraints of explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_NONZ_DETEQE As Integer =      13264

    '! Total number of nonlinear nonzeros in the objective function of explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQE As Integer =      13265

    '! Total number of quadratic constraints in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_CONS_DETEQI As Integer =      13266

    '! Total number of continuous constraints in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_CONS_DETEQI As Integer =      13267

    '! Total number of constraints with general integer variables in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_CONS_DETEQI As Integer =      13268

    '! Total number of constraints with binary variables in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_CONS_DETEQI As Integer =      13269

    '! Total number of quadratic variables in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_VARS_DETEQI As Integer =      13270

    '! Total number of nonzeros in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NONZ_DETEQI  As Integer =      13271

    '! Total number of binaries in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_DETEQI   As Integer =      13272

    '! Total number of general integer variables in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_DETEQI   As Integer =      13273

    '! Total number of continuous variables in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_DETEQI  As Integer =      13274

    '! Total number of quadratic nonzeros in the implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QC_NONZ_DETEQI As Integer =      13275

    '! Total number of nonlinear nonzeros in the constraints of implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_NONZ_DETEQI As Integer =      13276

    '! Total number of nonlinear nonzeros in the objective function of implicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQI As Integer =      13277

    '! Total number of block events. '
  Public Const  LS_IINFO_STOC_NUM_EVENTS_BLOCK As Integer =      13278

    '! Total number of independent events with a discrete distribution. '
  Public Const  LS_IINFO_STOC_NUM_EVENTS_DISCRETE As Integer =      13279

    '! Total number of independent events with a parametric distribution. '
  Public Const  LS_IINFO_STOC_NUM_EVENTS_PARAMETRIC As Integer =      13280

    '! Total number of events loaded explictly as a scenario '
  Public Const  LS_IINFO_STOC_NUM_EXPLICIT_SCENARIOS As Integer =      13281

    '! Index of a node's parent'
  Public Const  LS_IINFO_STOC_PARENT_NODE      As Integer =      13282

    '! Index of a node's eldest child'
  Public Const  LS_IINFO_STOC_ELDEST_CHILD_NODE As Integer =      13283

    '! Total number of childs a node has '
  Public Const  LS_IINFO_STOC_NUM_CHILD_NODES  As Integer =      13284

    '! Number of stochastic parameters in the instruction list.  '
  Public Const  LS_IINFO_NUM_STOCPAR_INSTR     As Integer =      13285

    '! The index of the scenario which is infeasible or unbounded.  '
  Public Const  LS_IINFO_INFORUNB_SCEN_IDX     As Integer =      13286

    '! Expected value of modeling uncertainity.  '
  Public Const  LS_DINFO_STOC_EVMU             As Integer =      13287

    '! Expected value of wait-and-see model's objective.  '
  Public Const  LS_DINFO_STOC_EVWS             As Integer =      13288

    '! Expected value of the objective based on average model's first-stage optimal decisions.  '
  Public Const  LS_DINFO_STOC_EVAVR            As Integer =      13289

    '! Number of arguments of a distribution sample.  '
  Public Const  LS_IINFO_DIST_NARG             As Integer =      13290

    '! Variance reductioncontrol method used in generating the sample.  '
  Public Const  LS_IINFO_SAMP_VARCONTROL_METHOD As Integer =      13291

    '! Total number of nonlinear variables in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_VARS_DETEQE As Integer =      13292

    '! Total number of nonlinear constraints in the explicit deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_CONS_DETEQE As Integer =      13293

    '! Best lower bound on expected value of the objective function.  '
  Public Const  LS_DINFO_STOC_EVOBJ_LB         As Integer =      13294

    '! Best upper bound on expected value of the objective function.  '
  Public Const  LS_DINFO_STOC_EVOBJ_UB         As Integer =      13295

    '! Expected value of average model's objective.  '
  Public Const  LS_DINFO_STOC_AVROBJ           As Integer =      13296

    '! Sample median. '
  Public Const  LS_DINFO_SAMP_MEDIAN           As Integer =      13297

    '! Distribution (population) median. '
  Public Const  LS_DINFO_DIST_MEDIAN           As Integer =      13298

    '! Number of chance-constraints. '
  Public Const  LS_IINFO_STOC_NUM_CC           As Integer =      13299

    '! Number of rows in chance-constraints. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_CC      As Integer =      13300

    '! Internal. '
  Public Const  LS_IINFO_STOC_ISCBACK          As Integer =      13301

    '! Total number of LPs solved. '
  Public Const  LS_IINFO_STOC_LP_COUNT         As Integer =      13302

    '! Total number of NLPs solved. '
  Public Const  LS_IINFO_STOC_NLP_COUNT        As Integer =      13303

    '! Total number of MILPs solved. '
  Public Const  LS_IINFO_STOC_MIP_COUNT        As Integer =      13304

    '! Time elapsed in seconds in the optimizer (excluding setup)  '
  Public Const  LS_DINFO_STOC_OPT_TIME         As Integer =      13305

    '! Difference between underlying sample's correlation (S) and target correlation (T) loaded.  '
  Public Const  LS_DINFO_SAMP_CORRDIFF_ST      As Integer =      13306

    '! Difference between underlying sample's induced correlation (C) and target correlation (T) loaded.  '
  Public Const  LS_DINFO_SAMP_CORRDIFF_CT      As Integer =      13307

    '! Difference between underlying sample's correlation (S) and induced correlation (C).  '
  Public Const  LS_DINFO_SAMP_CORRDIFF_SC      As Integer =      13308

    '! Number of rows with equality type in chance-constraints. '
  Public Const  LS_IINFO_STOC_NUM_EQROWS_CC    As Integer =      13309

    '! Number of stochastic rows'
  Public Const  LS_IINFO_STOC_NUM_ROWS         As Integer =      13310

    '! Number of chance sets violated over all scenarios '
  Public Const  LS_IINFO_STOC_NUM_CC_VIOLATED  As Integer =      13311

    '! Total number of columns in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_COLS_DETEQC  As Integer =      13312

    '! Total number of rows in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_ROWS_DETEQC  As Integer =      13313

    '! Total number of quadratic constraints in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_CONS_DETEQC As Integer =      13314

    '! Total number of continuous constraints in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_CONS_DETEQC As Integer =      13315

    '! Total number of constraints with general integer variables in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_CONS_DETEQC As Integer =      13316

    '! Total number of constraints with binary variables in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_CONS_DETEQC As Integer =      13317

    '! Total number of quadratic variables in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QCP_VARS_DETEQC As Integer =      13318

    '! Total number of nonzeros in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NONZ_DETEQC  As Integer =      13319

    '! Total number of binaries in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_BIN_DETEQC   As Integer =      13320

    '! Total number of general integer variables in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_INT_DETEQC   As Integer =      13321

    '! Total number of continuous variables in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_CONT_DETEQC  As Integer =      13322

    '! Total number of quadratic nonzeros in the chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_QC_NONZ_DETEQC As Integer =      13323

    '! Total number of nonlinear nonzeros in the constraints of chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_NONZ_DETEQC As Integer =      13324

    '! Total number of nonlinear nonzeros in the objective function of chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQC As Integer =      13325

    '! Total number of nonlinear constraints in the constraints of chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_CONS_DETEQC As Integer =      13326

    '! Total number of nonlinear variables in the constraints of chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NLP_VARS_DETEQC As Integer =      13327

    '! Total number of nonzeros in the objective of chance deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQC As Integer =      13328

    '! Total number of nonzeros in the objective of explict deterministic equivalent. '
  Public Const  LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQE As Integer =      13329

    '! p-level for chance constraint '
  Public Const  LS_DINFO_STOC_CC_PLEVEL        As Integer =      13340

    '! Number of parallel threads used '
  Public Const  LS_IINFO_STOC_THREADS          As Integer =      13341

    '! Work imbalance across threads '
  Public Const  LS_DINFO_STOC_THRIMBL          As Integer =      13342

    '! Number of EQ type stochastic rows'
  Public Const  LS_IINFO_STOC_NUM_EQROWS       As Integer =      13343

    '! Thread workloads '
  Public Const  LS_SINFO_STOC_THREAD_LOAD      As Integer =      13344

    '! Number of buckets '
  Public Const  LS_IINFO_STOC_NUM_BUCKETS      As Integer =      13345

    'BNP information'
  Public Const  LS_IINFO_BNP_SIM_ITER          As Integer =      14000
  Public Const  LS_IINFO_BNP_LPCOUNT           As Integer =      14001
  Public Const  LS_IINFO_BNP_NUMCOL            As Integer =      14002
  Public Const  LS_DINFO_BNP_BESTBOUND         As Integer =      14003
  Public Const  LS_DINFO_BNP_BESTOBJ           As Integer =      14004

 ' Error codes (2001-2299) '
  Public Const  LSERR_NO_ERROR                 As Integer =       0000
  Public Const  LSERR_OUT_OF_MEMORY            As Integer =       2001
  Public Const  LSERR_CANNOT_OPEN_FILE         As Integer =       2002
  Public Const  LSERR_BAD_MPS_FILE             As Integer =       2003
  Public Const  LSERR_BAD_CONSTRAINT_TYPE      As Integer =       2004
  Public Const  LSERR_BAD_MODEL                As Integer =       2005
  Public Const  LSERR_BAD_SOLVER_TYPE          As Integer =       2006
  Public Const  LSERR_BAD_OBJECTIVE_SENSE      As Integer =       2007
  Public Const  LSERR_BAD_MPI_FILE             As Integer =       2008
  Public Const  LSERR_INFO_NOT_AVAILABLE       As Integer =       2009
  Public Const  LSERR_ILLEGAL_NULL_POINTER     As Integer =       2010
  Public Const  LSERR_UNABLE_TO_SET_PARAM      As Integer =       2011
  Public Const  LSERR_INDEX_OUT_OF_RANGE       As Integer =       2012
  Public Const  LSERR_ERRMSG_FILE_NOT_FOUND    As Integer =       2013
  Public Const  LSERR_VARIABLE_NOT_FOUND       As Integer =       2014
  Public Const  LSERR_INTERNAL_ERROR           As Integer =       2015
  Public Const  LSERR_ITER_LIMIT               As Integer =       2016
  Public Const  LSERR_TIME_LIMIT               As Integer =       2017
  Public Const  LSERR_NOT_CONVEX               As Integer =       2018
  Public Const  LSERR_NUMERIC_INSTABILITY      As Integer =       2019
  Public Const  LSERR_STEP_TOO_SMALL           As Integer =       2021
  Public Const  LSERR_USER_INTERRUPT           As Integer =       2023
  Public Const  LSERR_PARAMETER_OUT_OF_RANGE   As Integer =       2024
  Public Const  LSERR_ERROR_IN_INPUT           As Integer =       2025
  Public Const  LSERR_TOO_SMALL_LICENSE        As Integer =       2026
  Public Const  LSERR_NO_VALID_LICENSE         As Integer =       2027
  Public Const  LSERR_NO_METHOD_LICENSE        As Integer =       2028
  Public Const  LSERR_NOT_SUPPORTED            As Integer =       2029
  Public Const  LSERR_MODEL_ALREADY_LOADED     As Integer =       2030
  Public Const  LSERR_MODEL_NOT_LOADED         As Integer =       2031
  Public Const  LSERR_INDEX_DUPLICATE          As Integer =       2032
  Public Const  LSERR_INSTRUCT_NOT_LOADED      As Integer =       2033
  Public Const  LSERR_OLD_LICENSE              As Integer =       2034
  Public Const  LSERR_NO_LICENSE_FILE          As Integer =       2035
  Public Const  LSERR_BAD_LICENSE_FILE         As Integer =       2036
  Public Const  LSERR_MIP_BRANCH_LIMIT         As Integer =       2037
  Public Const  LSERR_GOP_FUNC_NOT_SUPPORTED   As Integer =       2038
  Public Const  LSERR_GOP_BRANCH_LIMIT         As Integer =       2039
  Public Const  LSERR_BAD_DECOMPOSITION_TYPE   As Integer =       2040
  Public Const  LSERR_BAD_VARIABLE_TYPE        As Integer =       2041
  Public Const  LSERR_BASIS_BOUND_MISMATCH     As Integer =       2042
  Public Const  LSERR_BASIS_COL_STATUS         As Integer =       2043
  Public Const  LSERR_BASIS_INVALID            As Integer =       2044
  Public Const  LSERR_BASIS_ROW_STATUS         As Integer =       2045
  Public Const  LSERR_BLOCK_OF_BLOCK           As Integer =       2046
  Public Const  LSERR_BOUND_OUT_OF_RANGE       As Integer =       2047
  Public Const  LSERR_COL_BEGIN_INDEX          As Integer =       2048
  Public Const  LSERR_COL_INDEX_OUT_OF_RANGE   As Integer =       2049
  Public Const  LSERR_COL_NONZCOUNT            As Integer =       2050
  Public Const  LSERR_INVALID_ERRORCODE        As Integer =       2051
  Public Const  LSERR_ROW_INDEX_OUT_OF_RANGE   As Integer =       2052
  Public Const  LSERR_TOTAL_NONZCOUNT          As Integer =       2053
  Public Const  LSERR_MODEL_NOT_LINEAR         As Integer =       2054
  Public Const  LSERR_CHECKSUM                 As Integer =       2055
  Public Const  LSERR_USER_FUNCTION_NOT_FOUND  As Integer =       2056
  Public Const  LSERR_TRUNCATED_NAME_DATA      As Integer =       2057
  Public Const  LSERR_ILLEGAL_STRING_OPERATION As Integer =       2058
  Public Const  LSERR_STRING_ALREADY_LOADED    As Integer =       2059
  Public Const  LSERR_STRING_NOT_LOADED        As Integer =       2060
  Public Const  LSERR_STRING_LENGTH_LIMIT      As Integer =       2061
  Public Const  LSERR_DATA_TERM_EXIST          As Integer =       2062
  Public Const  LSERR_NOT_SORTED_ORDER         As Integer =       2063
  Public Const  LSERR_INST_MISS_ELEMENTS       As Integer =       2064
  Public Const  LSERR_INST_TOO_SHORT           As Integer =       2065
  Public Const  LSERR_INST_INVALID_BOUND       As Integer =       2066
  Public Const  LSERR_INST_SYNTAX_ERROR        As Integer =       2067
  Public Const  LSERR_COL_TOKEN_NOT_FOUND      As Integer =       2068
  Public Const  LSERR_ROW_TOKEN_NOT_FOUND      As Integer =       2069
  Public Const  LSERR_NAME_TOKEN_NOT_FOUND     As Integer =       2070
  Public Const  LSERR_NOT_LSQ_MODEL            As Integer =       2071
  Public Const  LSERR_INCOMPATBLE_DECOMPOSITION As Integer =       2072
  Public Const  LSERR_NO_MULTITHREAD_SUPPORT   As Integer =       2073
  Public Const  LSERR_INVALID_PARAMID          As Integer =       2074
  Public Const  LSERR_INVALID_NTHREADS         As Integer =       2075
  Public Const  LSERR_COL_LIMIT                As Integer =       2076
  Public Const  LSERR_QCDATA_NOT_LOADED        As Integer =       2077
  Public Const  LSERR_NO_QCDATA_IN_ROW         As Integer =       2078
  Public Const  LSERR_CLOCK_SETBACK            As Integer =       2079
  Public Const  LSERR_XSOLVER_LOAD             As Integer =       2080
  Public Const  LSERR_XSOLVER_NO_FILENAME      As Integer =       2081
  Public Const  LSERR_XSOLVER_ALREADY_LOADED   As Integer =       2082
  Public Const  LSERR_XSOLVER_FUNC_NOT_INSTALLED As Integer =       2083
  Public Const  LSERR_XSOLVER_LIB_NOT_INSTALLED As Integer =       2084
  Public Const  LSERR_ZLIB_LOAD                As Integer =       2085
  Public Const  LSERR_XSOLVER_ENV_NOT_CREATED  As Integer =       2086
  Public Const  LSERR_SOLPOOL_EMPTY            As Integer =       2087
  Public Const  LSERR_SOLPOOL_FULL             As Integer =       2088
  Public Const  LSERR_SOL_LIMIT                As Integer =       2089
  Public Const  LSERR_TUNER_NOT_SETUP          As Integer =       2090

    ' Error in LDLt factorization '
  Public Const  LSERR_LDL_FACTORIZATION        As Integer =       2201

    ' Empty column detected in LDLt factorization '
  Public Const  LSERR_LDL_EMPTY_COL            As Integer =       2202

    ' Matrix data is invalid or has bad input in LDLt factorization '
  Public Const  LSERR_LDL_BAD_MATRIX_DATA      As Integer =       2203

    ' Invalid matrix or vector dimension '
  Public Const  LSERR_LDL_INVALID_DIM          As Integer =       2204

    ' Matrix or vector is empty '
  Public Const  LSERR_LDL_EMPTY_MATRIX         As Integer =       2205

    ' Matrix is not symmetric '
  Public Const  LSERR_LDL_MATRIX_NOTSYM        As Integer =       2206

    ' Matrix has zero diagonal '
  Public Const  LSERR_LDL_ZERO_DIAG            As Integer =       2207

    ' Invalid permutation '
  Public Const  LSERR_LDL_INVALID_PERM         As Integer =       2208

    ' Duplicate elements detected in LDLt factorization '
  Public Const  LSERR_LDL_DUPELEM              As Integer =       2209

    ' Detected rank deficiency in LDLt factorization '
  Public Const  LSERR_LDL_RANK                 As Integer =       2210

   '! Core MPS filemodel has an error '
  Public Const  LSERR_BAD_SMPS_CORE_FILE       As Integer =       2301

    '! Time filemodel has an error '
  Public Const  LSERR_BAD_SMPS_TIME_FILE       As Integer =       2302

    '! Stoc filemodel has an error '
  Public Const  LSERR_BAD_SMPS_STOC_FILE       As Integer =       2303

    '! Core MPI filemodel has an error '
  Public Const  LSERR_BAD_SMPI_CORE_FILE       As Integer =       2304

    '! Stoc file associated with Core MPI file has an error '
  Public Const  LSERR_BAD_SMPI_STOC_FILE       As Integer =       2305

    '! Unable to open Core file '
  Public Const  LSERR_CANNOT_OPEN_CORE_FILE    As Integer =       2306

    '! Unable to open Time file '
  Public Const  LSERR_CANNOT_OPEN_TIME_FILE    As Integer =       2307

    '! Unable to open Stoc file '
  Public Const  LSERR_CANNOT_OPEN_STOC_FILE    As Integer =       2308

    '! Stochastic modeldata has not been loaded yet. '
  Public Const  LSERR_STOC_MODEL_NOT_LOADED    As Integer =       2309

    '! Stochastic parameter specified in Stoc file has not been found . '
  Public Const  LSERR_STOC_SPAR_NOT_FOUND      As Integer =       2310

    '! Stochastic parameter specified in Time file has not been found . '
  Public Const  LSERR_TIME_SPAR_NOT_FOUND      As Integer =       2311

    '! Specified scenario index is out of sequence '
  Public Const  LSERR_SCEN_INDEX_OUT_OF_SEQUENCE As Integer =       2312

    '! Stochastic modeldata has already been loaded. '
  Public Const  LSERR_STOC_MODEL_ALREADY_PARSED As Integer =       2313

    '! Specified scenario CDF is invalid, e.g. scenario probabilities don't sum to 1.0'
  Public Const  LSERR_STOC_INVALID_SCENARIO_CDF As Integer =       2314

    '! No stochastic parameters was found in the Core file '
  Public Const  LSERR_CORE_SPAR_NOT_FOUND      As Integer =       2315

    '! Number of stochastic parameters found in Core file don't match to that of Time file '
  Public Const  LSERR_CORE_SPAR_COUNT_MISMATCH As Integer =       2316

    '! Specified stochastic parameter index is invalid '
  Public Const  LSERR_CORE_INVALID_SPAR_INDEX  As Integer =       2317

    '! A stochastic parameter was not expected in Time file. '
  Public Const  LSERR_TIME_SPAR_NOT_EXPECTED   As Integer =       2318

    '! Number of stochastic parameters found in Time file don't match to that of Stoc file '
  Public Const  LSERR_TIME_SPAR_COUNT_MISMATCH As Integer =       2319

    '! Specified stochastic parameter doesn't have a valid outcome value '
  Public Const  LSERR_CORE_SPAR_VALUE_NOT_FOUND As Integer =       2320

    '! Requested information is unavailable '
  Public Const  LSERR_INFO_UNAVAILABLE         As Integer =       2321

    '! Core file doesn't have a valid bound name tag '
  Public Const  LSERR_STOC_MISSING_BNDNAME     As Integer =       2322

    '! Core file doesn't have a valid objective name tag '
  Public Const  LSERR_STOC_MISSING_OBJNAME     As Integer =       2323

    '! Core file doesn't have a valid right-hand-side name tag '
  Public Const  LSERR_STOC_MISSING_RHSNAME     As Integer =       2324

    '! Core file doesn't have a valid range name tag '
  Public Const  LSERR_STOC_MISSING_RNGNAME     As Integer =       2325

    '! Stoc file doesn't have an expected token name. '
  Public Const  LSERR_MISSING_TOKEN_NAME       As Integer =       2326

    '! Stoc file doesn't have a 'ROOT' token to specify a root scenario '
  Public Const  LSERR_MISSING_TOKEN_ROOT       As Integer =       2327

    '! Node model is unbounded '
  Public Const  LSERR_STOC_NODE_UNBOUNDED      As Integer =       2328

    '! Node model is infeasible '
  Public Const  LSERR_STOC_NODE_INFEASIBLE     As Integer =       2329

    '! Stochastic model has too many scenarios to solve with specified solver '
  Public Const  LSERR_STOC_TOO_MANY_SCENARIOS  As Integer =       2330

    '! One or more node-models have irrecoverable numerical problems '
  Public Const  LSERR_STOC_BAD_PRECISION       As Integer =       2331

    '! Specified aggregation structure is not compatible with model's stage structure '
  Public Const  LSERR_CORE_BAD_AGGREGATION     As Integer =       2332

    '! Event tree is either not initialized yet or was too big to create '
  Public Const  LSERR_STOC_NULL_EVENT_TREE     As Integer =       2333

    '! Specified stage index is invalid '
  Public Const  LSERR_CORE_BAD_STAGE_INDEX     As Integer =       2334

    '! Specified algorithmmethod is invalid or not supported '
  Public Const  LSERR_STOC_BAD_ALGORITHM       As Integer =       2335

    '! Specified number of stages in Core model is invalid '
  Public Const  LSERR_CORE_BAD_NUMSTAGES       As Integer =       2336

    '! Underlying model has an invalid temporal order '
  Public Const  LSERR_TIME_BAD_TEMPORAL_ORDER  As Integer =       2337

    '! Number of stages specified in Time structure is invalid '
  Public Const  LSERR_TIME_BAD_NUMSTAGES       As Integer =       2338

    '! Core and Time data are inconsistent '
  Public Const  LSERR_CORE_TIME_MISMATCH       As Integer =       2339

    '! Specified stochastic structure has an invalid CDF '
  Public Const  LSERR_STOC_INVALID_CDF         As Integer =       2340

    '! Specified distribution type is invalid or not supported. '
  Public Const  LSERR_BAD_DISTRIBUTION_TYPE    As Integer =       2341

    '! Scale parameter for specified distribution is out of range. '
  Public Const  LSERR_DIST_SCALE_OUT_OF_RANGE  As Integer =       2342

    '! Shape parameter for specified distribution is out of range. '
  Public Const  LSERR_DIST_SHAPE_OUT_OF_RANGE  As Integer =       2343

    '! Specified probabability value is invalid '
  Public Const  LSERR_DIST_INVALID_PROBABILITY As Integer =       2344

    '! Derivative information is unavailable '
  Public Const  LSERR_DIST_NO_DERIVATIVE       As Integer =       2345

    '! Specified standard deviation is invalid '
  Public Const  LSERR_DIST_INVALID_SD          As Integer =       2346

    '! Specified value is invalid '
  Public Const  LSERR_DIST_INVALID_X           As Integer =       2347

    '! Specified parameters are invalid for the given distribution. '
  Public Const  LSERR_DIST_INVALID_PARAMS      As Integer =       2348

    '! Iteration limit has been reached during a root finding operation '
  Public Const  LSERR_DIST_ROOTER_ITERLIM      As Integer =       2349

    '! Given array is out of bounds '
  Public Const  LSERR_ARRAY_OUT_OF_BOUNDS      As Integer =       2350

    '! Limiting PDF does not exist '
  Public Const  LSERR_DIST_NO_PDF_LIMIT        As Integer =       2351

    '! A random number generator is not set. '
  Public Const  LSERR_RG_NOT_SET               As Integer =       2352

    '! Distribution function value was truncated during calculations '
  Public Const  LSERR_DIST_TRUNCATED           As Integer =       2353

    '! Stoc file has a parameter value missing '
  Public Const  LSERR_STOC_MISSING_PARAM_TOKEN As Integer =       2354

    '! Distribution has invalid number of parameters '
  Public Const  LSERR_DIST_INVALID_NUMPARAM    As Integer =       2355

    '! Core filemodel is not in temporal order '
  Public Const  LSERR_CORE_NOT_IN_TEMPORAL_ORDER As Integer =       2357

    '! Specified sample size is invalid '
  Public Const  LSERR_STOC_INVALID_SAMPLE_SIZE As Integer =       2358

    '! Node probability cannot be computed due to presence of continuous stochastic parameters '
  Public Const  LSERR_STOC_NOT_DISCRETE        As Integer =       2359

    '! Event tree exceeds the maximum number of scenarios allowed to attempt an exact solution.'
  Public Const  LSERR_STOC_SCENARIO_LIMIT      As Integer =       2360

    '! Specified correlation type is invalid '
  Public Const  LSERR_DIST_BAD_CORRELATION_TYPE As Integer =       2361

    '! Number of stages in the model is not set yet. '
  Public Const  LSERR_TIME_NUMSTAGES_NOT_SET   As Integer =       2362

    '! Model already contains a sampled tree '
  Public Const  LSERR_STOC_SAMPLE_ALREADY_LOADED As Integer =       2363

    '! Stochastic events are not loaded yet '
  Public Const  LSERR_STOC_EVENTS_NOT_LOADED   As Integer =       2364

    '! Stochastic tree already initialized '
  Public Const  LSERR_STOC_TREE_ALREADY_INIT   As Integer =       2365

    '! Random number generator seed not initialized '
  Public Const  LSERR_RG_SEED_NOT_SET          As Integer =       2366

    '! All sample points in the sample has been used. Resampling may be required. '
  Public Const  LSERR_STOC_OUT_OF_SAMPLE_POINTS As Integer =       2367

    '! Sampling is not supported for models with explicit scenarios. '
  Public Const  LSERR_STOC_SCENARIO_SAMPLING_NOT_SUPPORTED As Integer =       2368

    '! Sample points are not yet generated for a stochastic parameter. '
  Public Const  LSERR_STOC_SAMPLE_NOT_GENERATED As Integer =       2369

    '! Sample points are already generated for a stochastic parameter. '
  Public Const  LSERR_STOC_SAMPLE_ALREADY_GENERATED As Integer =       2370

    '! Sample sizes selected are too small. '
  Public Const  LSERR_STOC_SAMPLE_SIZE_TOO_SMALL As Integer =       2371

    '! A random number generator is already set. '
  Public Const  LSERR_RG_ALREADY_SET           As Integer =       2372

    '! Sampling is not allowed for blockjoint distributions. '
  Public Const  LSERR_STOC_BLOCK_SAMPLING_NOT_SUPPORTED As Integer =       2373

    '! No stochastic parameters were assigned to one of the stages. '
  Public Const  LSERR_EMPTY_SPAR_STAGE         As Integer =       2374

    '! No rows were assigned to one of the stages. '
  Public Const  LSERR_EMPTY_ROW_STAGE          As Integer =       2375

    '! No columns were assigned to one of the stages. '
  Public Const  LSERR_EMPTY_COL_STAGE          As Integer =       2376

    '! Default sample sizes per stoc.pars and stage are in conflict. '
  Public Const  LSERR_STOC_CONFLICTING_SAMP_SIZES As Integer =       2377

    '! Empty scenario data '
  Public Const  LSERR_STOC_EMPTY_SCENARIO_DATA As Integer =       2378

    '! A correlation structure has not been induced yet '
  Public Const  LSERR_STOC_CORRELATION_NOT_INDUCED As Integer =       2379

    '! A discrete PDF table has not been loaded '
  Public Const  LSERR_STOC_PDF_TABLE_NOT_LOADED As Integer =       2380

    '! No continously distributed random parameters are found '
  Public Const  LSERR_STOC_NO_CONTINUOUS_SPAR_FOUND As Integer =       2381

    '! One or more rows already belong to another chance constraint '
  Public Const  LSERR_STOC_ROW_ALREADY_IN_CC   As Integer =       2382

    '! No chance-constraints were loaded '
  Public Const  LSERR_STOC_CC_NOT_LOADED       As Integer =       2383

    '! Cut limit has been reached '
  Public Const  LSERR_STOC_CUT_LIMIT           As Integer =       2384

    '! GA object has not been initialized yet '
  Public Const  LSERR_STOC_GA_NOT_INIT         As Integer =       2385

    '! There exists stochastic rows not loaded to any chance constraints yet.'
  Public Const  LSERR_STOC_ROWS_NOT_LOADED_IN_CC As Integer =       2386

    '! Specified sample is already assigned as the source for the target sample. '
  Public Const  LSERR_SAMP_ALREADY_SOURCE      As Integer =       2387

    '! No user-defined distribution function has been set for the specified sample. '
  Public Const  LSERR_SAMP_USERFUNC_NOT_SET    As Integer =       2388

    '! Specified sample does not support the function call or it is incompatible with the argument list. '
  Public Const  LSERR_SAMP_INVALID_CALL        As Integer =       2389

    '! Mapping stochastic instructions leads to multiple occurrences in matrix model. '
  Public Const  LSERR_STOC_MAP_MULTI_SPAR      As Integer =       2390

    '! Two or more stochastic instructions maps to the same position in matrix model. '
  Public Const  LSERR_STOC_MAP_SAME_SPAR       As Integer =       2391

    '! A stochastic parameter was not expected in the objective function. '
  Public Const  LSERR_STOC_SPAR_NOT_EXPECTED_OBJ As Integer =       2392

    '! One of the distribution parameters of the specified sample was not set. '
  Public Const  LSERR_DIST_PARAM_NOT_SET       As Integer =       2393

    '! Specified stochastic input is invalid. '
  Public Const  LSERR_STOC_INVALID_INPUT       As Integer =       2394

    ' Error codes for the sprint method. '
  Public Const  LSERR_SPRINT_MISSING_TAG_ROWS  As Integer =       2577
  Public Const  LSERR_SPRINT_MISSING_TAG_COLS  As Integer =       2578
  Public Const  LSERR_SPRINT_MISSING_TAG_RHS   As Integer =       2579
  Public Const  LSERR_SPRINT_MISSING_TAG_ENDATA As Integer =       2580
  Public Const  LSERR_SPRINT_MISSING_VALUE_ROW As Integer =       2581
  Public Const  LSERR_SPRINT_EXTRA_VALUE_ROW   As Integer =       2582
  Public Const  LSERR_SPRINT_MISSING_VALUE_COL As Integer =       2583
  Public Const  LSERR_SPRINT_EXTRA_VALUE_COL   As Integer =       2584
  Public Const  LSERR_SPRINT_MISSING_VALUE_RHS As Integer =       2585
  Public Const  LSERR_SPRINT_EXTRA_VALUE_RHS   As Integer =       2586
  Public Const  LSERR_SPRINT_MISSING_VALUE_BOUND As Integer =       2587
  Public Const  LSERR_SPRINT_EXTRA_VALUE_BOUND As Integer =       2588
  Public Const  LSERR_SPRINT_INTEGER_VARS_IN_MPS As Integer =       2589
  Public Const  LSERR_SPRINT_BINARY_VARS_IN_MPS As Integer =       2590
  Public Const  LSERR_SPRINT_SEMI_CONT_VARS_IN_MPS As Integer =       2591
  Public Const  LSERR_SPRINT_UNKNOWN_TAG_BOUNDS As Integer =       2592
  Public Const  LSERR_SPRINT_MULTIPLE_OBJ_ROWS As Integer =       2593
  Public Const  LSERR_SPRINT_COULD_NOT_SOLVE_SUBPROBLEM As Integer =       2594
  Public Const  LSERR_COULD_NOT_WRITE_TO_FILE  As Integer =       2595
  Public Const  LSERR_COULD_NOT_READ_FROM_FILE As Integer =       2596
  Public Const  LSERR_READING_PAST_EOF         As Integer =       2597

    ' Error codes associated with scripting 2700-2750 '
  Public Const  LSERR_SCRIPT                   As Integer =       2700

    '! @} '
  Public Const  LSERR_LAST_ERROR               As Integer =       2598

 ' Callback locations '
  Public Const  LSLOC_PRIMAL                   As Integer =          0
  Public Const  LSLOC_DUAL                     As Integer =          1
  Public Const  LSLOC_BARRIER                  As Integer =          2
  Public Const  LSLOC_CROSSOVER                As Integer =          3
  Public Const  LSLOC_CONOPT                   As Integer =          4
  Public Const  LSLOC_MIP                      As Integer =          5
  Public Const  LSLOC_LOCAL_OPT                As Integer =          6
  Public Const  LSLOC_GEN_START                As Integer =          7
  Public Const  LSLOC_GEN_PROCESSING           As Integer =          8
  Public Const  LSLOC_GEN_END                  As Integer =          9
  Public Const  LSLOC_GOP                      As Integer =         10
  Public Const  LSLOC_EXIT_SOLVER              As Integer =         11
  Public Const  LSLOC_PRESOLVE                 As Integer =         12
  Public Const  LSLOC_MSW                      As Integer =         13
  Public Const  LSLOC_FUNC_CALC                As Integer =         14
  Public Const  LSLOC_IISIUS                   As Integer =         15
  Public Const  LSLOC_SP                       As Integer =         16
  Public Const  LSLOC_GEN_SP_START             As Integer =         17
  Public Const  LSLOC_GEN_SP                   As Integer =         18
  Public Const  LSLOC_GEN_SP_END               As Integer =         19
  Public Const  LSLOC_SP_WS                    As Integer =         20
  Public Const  LSLOC_LSQ                      As Integer =         21
  Public Const  LSLOC_SP_WS_START              As Integer =         22
  Public Const  LSLOC_SP_WS_END                As Integer =         23
  Public Const  LSLOC_SP_BENCH_START           As Integer =         24
  Public Const  LSLOC_SP_BENCH_END             As Integer =         25
  Public Const  LSLOC_BNP                      As Integer =         26
  Public Const  LS_METHOD_FREE                 As Integer =          0
  Public Const  LS_METHOD_PSIMPLEX             As Integer =          1
  Public Const  LS_METHOD_DSIMPLEX             As Integer =          2
  Public Const  LS_METHOD_BARRIER              As Integer =          3
  Public Const  LS_METHOD_NLP                  As Integer =          4
  Public Const  LS_METHOD_MIP                  As Integer =          5
  Public Const  LS_METHOD_MULTIS               As Integer =          6
  Public Const  LS_METHOD_GOP                  As Integer =          7
  Public Const  LS_METHOD_IIS                  As Integer =          8
  Public Const  LS_METHOD_IUS                  As Integer =          9
  Public Const  LS_METHOD_SBD                  As Integer =         10
  Public Const  LS_METHOD_SPRINT               As Integer =         11
  Public Const  LS_METHOD_GA                   As Integer =         12
  Public Const  LS_STRATEGY_USER               As Integer =          0
  Public Const  LS_STRATEGY_PRIMIP             As Integer =          1
  Public Const  LS_STRATEGY_NODEMIP            As Integer =          2
  Public Const  LS_STRATEGY_HEUMIP             As Integer =          3
  Public Const  LS_NMETHOD_FREE                As Integer =          4
  Public Const  LS_NMETHOD_LSQ                 As Integer =          5
  Public Const  LS_NMETHOD_QP                  As Integer =          6
  Public Const  LS_NMETHOD_CONOPT              As Integer =          7
  Public Const  LS_NMETHOD_SLP                 As Integer =          8
  Public Const  LS_NMETHOD_MSW_GRG             As Integer =          9
  Public Const  LS_NMETHOD_IPOPT               As Integer =         10
  Public Const  LS_PROB_SOLVE_FREE             As Integer =          0
  Public Const  LS_PROB_SOLVE_PRIMAL           As Integer =          1
  Public Const  LS_PROB_SOLVE_DUAL             As Integer =          2
  Public Const  LS_BAR_METHOD_FREE             As Integer =          4
  Public Const  LS_BAR_METHOD_INTPNT           As Integer =          5
  Public Const  LS_BAR_METHOD_CONIC            As Integer =          6
  Public Const  LS_BAR_METHOD_QCONE            As Integer =          7
  Public Const  LSSOL_BASIC_PRIMAL             As Integer =         11
  Public Const  LSSOL_BASIC_DUAL               As Integer =         12
  Public Const  LSSOL_BASIC_SLACK              As Integer =         13
  Public Const  LSSOL_BASIC_REDCOST            As Integer =         14
  Public Const  LSSOL_INTERIOR_PRIMAL          As Integer =         15
  Public Const  LSSOL_INTERIOR_DUAL            As Integer =         16
  Public Const  LSSOL_INTERIOR_SLACK           As Integer =         17
  Public Const  LSSOL_INTERIOR_REDCOST         As Integer =         18

    ' linear programs                          '
  Public Const  LS_LP                          As Integer =         10

    ' quadratic programs                       '
  Public Const  LS_QP                          As Integer =         11

    ' conic programs                           '
  Public Const  LS_SOCP                        As Integer =         12

    ' semidefinite programs                    '
  Public Const  LS_SDP                         As Integer =         13

    ' nonlinear programs                       '
  Public Const  LS_NLP                         As Integer =         14

    ' mixed-integer linear programs            '
  Public Const  LS_MILP                        As Integer =         15

    ' mixed-integer quadratic programs         '
  Public Const  LS_MIQP                        As Integer =         16

    ' mixed-integer conic programs             '
  Public Const  LS_MISOCP                      As Integer =         17

    ' mixed-integer semidefinite programs      '
  Public Const  LS_MISDP                       As Integer =         18

    ' mixed-integer nonlinear programs         '
  Public Const  LS_MINLP                       As Integer =         19

    ' convex QP '
  Public Const  LS_CONVEX_QP                   As Integer =         20

    'convex NLP '
  Public Const  LS_CONVEX_NLP                  As Integer =         21

    'convex MIQP '
  Public Const  LS_CONVEX_MIQP                 As Integer =         22

    'convex MINLP '
  Public Const  LS_CONVEX_MINLP                As Integer =         23

    ' undetermined   '
  Public Const  LS_UNDETERMINED                As Integer =         -1
  Public Const  LS_LINK_BLOCKS_FREE            As Integer =          0
  Public Const  LS_LINK_BLOCKS_SELF            As Integer =          1
  Public Const  LS_LINK_BLOCKS_NONE            As Integer =          2
  Public Const  LS_LINK_BLOCKS_COLS            As Integer =          3
  Public Const  LS_LINK_BLOCKS_ROWS            As Integer =          4
  Public Const  LS_LINK_BLOCKS_BOTH            As Integer =          5
  Public Const  LS_LINK_BLOCKS_MATRIX          As Integer =          6

 ' Controls the way objective function and
 ' objective sense are printed when writing
 ' LS_MAX type problems in MPS format.
 '
  Public Const  LS_MPS_USE_MAX_NOTE            As Integer =          0
  Public Const  LS_MPS_USE_MAX_CARD            As Integer =          1
  Public Const  LS_MPS_USE_MAX_FLIP            As Integer =          2

 ' Finite differences methods '
  Public Const  LS_DERIV_FREE                  As Integer =          0
  Public Const  LS_DERIV_FORWARD_DIFFERENCE    As Integer =          1
  Public Const  LS_DERIV_BACKWARD_DIFFERENCE   As Integer =          2
  Public Const  LS_DERIV_CENTER_DIFFERENCE     As Integer =          3

 ' MIP Sets
 '  SOS1: S={x_1,...,x_p}  only one x_j can be different from zero
 '  SOS2: S={x_1,...,x_p}  at most two x_j can be different from zero
 '                         and  when they are they have to be adjacent
 '  SOS3: S={x_1,...,x_p}  @sum(j: x_j      )  = 1;  x_j >=0,
 '  CARD: S={x_1,...,x_p}  @sum(j: sign(x_j)) <= k;  x_j >=0
 '
  Public Const  LS_MIP_SET_CARD                As Integer =          4
  Public Const  LS_MIP_SET_SOS1                As Integer =          1
  Public Const  LS_MIP_SET_SOS2                As Integer =          2
  Public Const  LS_MIP_SET_SOS3                As Integer =          3
  Public Const  LS_QTERM_NONE                  As Integer =          0
  Public Const  LS_QTERM_INDEF                 As Integer =          1
  Public Const  LS_QTERM_POSDEF                As Integer =          2
  Public Const  LS_QTERM_NEGDEF                As Integer =          3
  Public Const  LS_QTERM_POS_SEMIDEF           As Integer =          4
  Public Const  LS_QTERM_NEG_SEMIDEF           As Integer =          5

 ' Bit masks for general MIP mode. Use sums
 ' to enable a collection of available levels.
 '
  Public Const  LS_MIP_MODE_NO_TIME_EVENTS     As Integer =          2
  Public Const  LS_MIP_MODE_FAST_FEASIBILITY   As Integer =          4
  Public Const  LS_MIP_MODE_FAST_OPTIMALITY    As Integer =          8
  Public Const  LS_MIP_MODE_NO_BRANCH_CUTS     As Integer =         16
  Public Const  LS_MIP_MODE_NO_LP_BARRIER      As Integer =         32

 ' Bit mask for cut generation levels. Use sums to
 ' enable a collection of available cuts.
 '
  Public Const  LS_MIP_GUB_COVER_CUTS          As Integer =          2
  Public Const  LS_MIP_FLOW_COVER_CUTS         As Integer =          4
  Public Const  LS_MIP_LIFT_CUTS               As Integer =          8
  Public Const  LS_MIP_PLAN_LOC_CUTS           As Integer =         16
  Public Const  LS_MIP_DISAGG_CUTS             As Integer =         32
  Public Const  LS_MIP_KNAPSUR_COVER_CUTS      As Integer =         64
  Public Const  LS_MIP_LATTICE_CUTS            As Integer =        128
  Public Const  LS_MIP_GOMORY_CUTS             As Integer =        256
  Public Const  LS_MIP_COEF_REDC_CUTS          As Integer =        512
  Public Const  LS_MIP_GCD_CUTS                As Integer =       1024
  Public Const  LS_MIP_OBJ_CUT                 As Integer =       2048
  Public Const  LS_MIP_BASIS_CUTS              As Integer =       4096
  Public Const  LS_MIP_CARDGUB_CUTS            As Integer =       8192
  Public Const  LS_MIP_DISJUN_CUTS             As Integer =      16384

 ' Bit masks for MIP preprocessing levels. Use sums
 ' to enable a collection of available levels.
 '
  Public Const  LS_MIP_PREP_SPRE               As Integer =          2
  Public Const  LS_MIP_PREP_PROB               As Integer =          4
  Public Const  LS_MIP_PREP_COEF               As Integer =          8
  Public Const  LS_MIP_PREP_ELIM               As Integer =         16
  Public Const  LS_MIP_PREP_DUAL               As Integer =         32
  Public Const  LS_MIP_PREP_DBACK              As Integer =         64
  Public Const  LS_MIP_PREP_BINROWS            As Integer =        128
  Public Const  LS_MIP_PREP_AGGROWS            As Integer =        256
  Public Const  LS_MIP_PREP_COEF_LIFTING       As Integer =        512
  Public Const  LS_MIP_PREP_MAXPASS            As Integer =       1024
  Public Const  LS_MIP_PREP_SIMROW             As Integer =       2048

 ' Bit masks for solver preprocessing levels. Use sums
 ' to enable a collection of available levels.
 '
  Public Const  LS_SOLVER_PREP_SPRE            As Integer =          2
  Public Const  LS_SOLVER_PREP_PFOR            As Integer =          4
  Public Const  LS_SOLVER_PREP_DFOR            As Integer =          8
  Public Const  LS_SOLVER_PREP_ELIM            As Integer =         16
  Public Const  LS_SOLVER_PREP_DCOL            As Integer =         32
  Public Const  LS_SOLVER_PREP_DROW            As Integer =         64
  Public Const  LS_SOLVER_PREP_CONE            As Integer =        128
  Public Const  LS_SOLVER_PREP_MAXPASS         As Integer =       1024
  Public Const  LS_SOLVER_PREP_DECOMP          As Integer =       4096
  Public Const  LS_SOLVER_PREP_LOWMEM          As Integer =       8192
  Public Const  LS_SOLVER_PREP_EXTERNAL        As Integer =      16384

 ' Bit masks for IIS & IUS analysis levels. Use sums to
 ' enable a collection of available levels.
 '
  Public Const  LS_NECESSARY_ROWS              As Integer =          1
  Public Const  LS_NECESSARY_COLS              As Integer =          2
  Public Const  LS_SUFFICIENT_ROWS             As Integer =          4
  Public Const  LS_SUFFICIENT_COLS             As Integer =          8
  Public Const  LS_IIS_INTS                    As Integer =         16
  Public Const  LS_IISRANK_LTF                 As Integer =         32
  Public Const  LS_IISRANK_DECOMP              As Integer =         64
  Public Const  LS_IISRANK_NNZ                 As Integer =        128
  Public Const  LS_IISLIMIT_MIS                As Integer =        256
  Public Const  LS_IIS_MASK_IISCOLS            As Integer =        512
  Public Const  LS_IIS_SETS                    As Integer =       1024

 ' Infeasibility norms for IIS finder '
  Public Const  LS_IIS_NORM_FREE               As Integer =          0
  Public Const  LS_IIS_NORM_ONE                As Integer =          1
  Public Const  LS_IIS_NORM_INFINITY           As Integer =          2

 ' IIS methods '
  Public Const  LS_IIS_DEFAULT                 As Integer =          0
  Public Const  LS_IIS_DEL_FILTER              As Integer =          1
  Public Const  LS_IIS_ADD_FILTER              As Integer =          2
  Public Const  LS_IIS_GBS_FILTER              As Integer =          3
  Public Const  LS_IIS_DFBS_FILTER             As Integer =          4
  Public Const  LS_IIS_FSC_FILTER              As Integer =          5
  Public Const  LS_IIS_ELS_FILTER              As Integer =          6

 'codes for IINFO_MIP_WHERE_IN_CODE'
  Public Const  LS_MIP_IN_PRESOLVE             As Integer =          0
  Public Const  LS_MIP_IN_FP_MODE              As Integer =          1
  Public Const  LS_MIP_IN_HEU_MODE             As Integer =          2
  Public Const  LS_MIP_IN_ENUM                 As Integer =          3
  Public Const  LS_MIP_IN_CUT_ADD_TOP          As Integer =          4
  Public Const  LS_MIP_IN_BANDB                As Integer =          6

    ''
    ' @ingroup LSstocOptDataTypes
    '/
    '! Stochastic parameter is an instruction code  '/
    Public Const LS_JCOL_INST As Integer = -8
    '! Stochastic parameter is a RHS upper bound (reserved for future use)'/
    Public Const LS_JCOL_RUB As Integer = -7
    '! Stochastic parameter is a RHS lower bound (reserved for future use)'/
    Public Const LS_JCOL_RLB As Integer = -6
    '! Stochastic parameter is a RHS value (belongs to RHS column)      '/
    Public Const LS_JCOL_RHS As Integer = -5
    '! Stochastic parameter is an objective coefficient (belongs to OBJ row)   '/
    Public Const LS_IROW_OBJ As Integer = -4
    '! Stochastic parameter is a variable lower bound (belongs to LO row) '/
    Public Const LS_IROW_VUB As Integer = -3
    '! Stochastic parameter is a variable upper bound (belongs to UP row) '/
    Public Const LS_IROW_VLB As Integer = -2
    '! Stochastic parameter is a variable fixed bound (belongs to FX row) '/
    Public Const LS_IROW_VFX As Integer = -1
    '! Stochastic parameter is an LP matrix entry. '/
  Public Const  LS_IMAT_AIJ                    As Integer =          0

   ' discrete distributions '
  Public Const  LSDIST_TYPE_BINOMIAL           As Integer =        701
  Public Const  LSDIST_TYPE_DISCRETE           As Integer =        702
  Public Const  LSDIST_TYPE_DISCRETE_BLOCK     As Integer =        703
  Public Const  LSDIST_TYPE_GEOMETRIC          As Integer =        705
  Public Const  LSDIST_TYPE_POISSON            As Integer =        706
  Public Const  LSDIST_TYPE_LOGARITHMIC        As Integer =        707
  Public Const  LSDIST_TYPE_HYPER_GEOMETRIC    As Integer =        708
  Public Const  LSDIST_TYPE_LINTRAN_BLOCK      As Integer =        709
  Public Const  LSDIST_TYPE_SUB_BLOCK          As Integer =        710
  Public Const  LSDIST_TYPE_SUB                As Integer =        711
  Public Const  LSDIST_TYPE_USER               As Integer =        712

    ' continuous distributions '
  Public Const  LSDIST_TYPE_BETA               As Integer =        801
  Public Const  LSDIST_TYPE_CAUCHY             As Integer =        802
  Public Const  LSDIST_TYPE_CHI_SQUARE         As Integer =        803
  Public Const  LSDIST_TYPE_EXPONENTIAL        As Integer =        804
  Public Const  LSDIST_TYPE_F_DISTRIBUTION     As Integer =        805
  Public Const  LSDIST_TYPE_GAMMA              As Integer =        806
  Public Const  LSDIST_TYPE_GUMBEL             As Integer =        807
  Public Const  LSDIST_TYPE_LAPLACE            As Integer =        808
  Public Const  LSDIST_TYPE_LOGNORMAL          As Integer =        809
  Public Const  LSDIST_TYPE_LOGISTIC           As Integer =        810
  Public Const  LSDIST_TYPE_NORMAL             As Integer =        811
  Public Const  LSDIST_TYPE_PARETO             As Integer =        812
  Public Const  LSDIST_TYPE_STABLE_PARETIAN    As Integer =        813
  Public Const  LSDIST_TYPE_STUDENTS_T         As Integer =        814
  Public Const  LSDIST_TYPE_TRIANGULAR         As Integer =        815
  Public Const  LSDIST_TYPE_UNIFORM            As Integer =        816
  Public Const  LSDIST_TYPE_WEIBULL            As Integer =        817
  Public Const  LSDIST_TYPE_WILCOXON           As Integer =        818
  Public Const  LSDIST_TYPE_BETABINOMIAL       As Integer =        819
  Public Const  LSDIST_TYPE_SYMMETRICSTABLE    As Integer =        820

 ' supported operations modifying the core. '
  Public Const  LS_REPLACE                     As Integer =          0
  Public Const  LS_ADD                         As Integer =          1
  Public Const  LS_SUB                         As Integer =          2
  Public Const  LS_MULTIPLY                    As Integer =          3
  Public Const  LS_DIVIDE                      As Integer =          4

 ' scenario indices for special cases '
  Public Const  LS_SCEN_ROOT                   As Integer =         -1
  Public Const  LS_SCEN_AVRG                   As Integer =         -2
  Public Const  LS_SCEN_MEDIAN                 As Integer =         -3
  Public Const  LS_SCEN_USER                   As Integer =         -4
  Public Const  LS_SCEN_NONE                   As Integer =         -5

 ' warmstart rule in optimizing wait-see model '
  Public Const  LS_WSBAS_FREE                  As Integer =         -1
  Public Const  LS_WSBAS_NONE                  As Integer =          0
  Public Const  LS_WSBAS_AVRG                  As Integer =          1
  Public Const  LS_WSBAS_LAST                  As Integer =          2

   '! Solve with the method chosen by the solver. '
  Public Const  LS_METHOD_STOC_FREE            As Integer =         -1

    '! Solve the deterministic equivalent (DETEQ).  '
  Public Const  LS_METHOD_STOC_DETEQ           As Integer =          0

    '! Solve with the Nested Benders Decomposition (NBD) method. '
  Public Const  LS_METHOD_STOC_NBD             As Integer =          1

    '! Solve with the Augmented Lagrangian Decomposition (ALD) method. '
  Public Const  LS_METHOD_STOC_ALD             As Integer =          2

    '! Solve with the Heuristic-Search (HS) method. '
  Public Const  LS_METHOD_STOC_HS              As Integer =          4

 '
 ' @ingroup LSstocOptDeteqType
 '
  Public Const  LS_DETEQ_FREE                  As Integer =         -1
  Public Const  LS_DETEQ_IMPLICIT              As Integer =          0
  Public Const  LS_DETEQ_EXPLICIT              As Integer =          1
  Public Const  LS_DETEQ_CHANCE                As Integer =          2

 ' Distribution functions '
  Public Const  LS_USER                        As Integer =          0
  Public Const  LS_PDF                         As Integer =          1
  Public Const  LS_CDF                         As Integer =          2
  Public Const  LS_CDFINV                      As Integer =          3
  Public Const  LS_PDFDIFF                     As Integer =          4

 ' Correlation types '
  Public Const  LS_CORR_TARGET                 As Integer =         -1
  Public Const  LS_CORR_LINEAR                 As Integer =          0
  Public Const  LS_CORR_PEARSON                As Integer =          0
  Public Const  LS_CORR_KENDALL                As Integer =          1
  Public Const  LS_CORR_SPEARMAN               As Integer =          2

 ' Sampling types '
  Public Const  LS_MONTECARLO                  As Integer =          0
  Public Const  LS_LATINSQUARE                 As Integer =          1
  Public Const  LS_ANTITHETIC                  As Integer =          2

 ' Random number generator algorithms '
  Public Const  LS_RANDGEN_FREE                As Integer =         -1
  Public Const  LS_RANDGEN_SYSTEM              As Integer =          0
  Public Const  LS_RANDGEN_LINDO1              As Integer =          1
  Public Const  LS_RANDGEN_LINDO2              As Integer =          2
  Public Const  LS_RANDGEN_LIN1                As Integer =          3
  Public Const  LS_RANDGEN_MULT1               As Integer =          4
  Public Const  LS_RANDGEN_MULT2               As Integer =          5
  Public Const  LS_RANDGEN_MERSENNE            As Integer =          6

 ' NCM methods '
  Public Const  LS_NCM_GA                      As Integer =          2
  Public Const  LS_NCM_ALTP                    As Integer =          4
  Public Const  LS_NCM_L2NORM_CONE             As Integer =          8
  Public Const  LS_NCM_L2NORM_NLP              As Integer =         16

 ' pointer types used '
  Public Const  LS_PTR_ENV                     As Integer =          0
  Public Const  LS_PTR_MODEL                   As Integer =          1
  Public Const  LS_PTR_SAMPLE                  As Integer =          2
  Public Const  LS_PTR_RG                      As Integer =          3

 ' multithreading mode '
  Public Const  LS_MTMODE_FREE                 As Integer =         -1
  Public Const  LS_MTMODE_EXPLCT               As Integer =          0
  Public Const  LS_MTMODE_PPCC                 As Integer =          1
  Public Const  LS_MTMODE_PP                   As Integer =          2
  Public Const  LS_MTMODE_CCPP                 As Integer =          3
  Public Const  LS_MTMODE_CC                   As Integer =          4

 ' Output file types created by the Sprint code'
  Public Const  LS_SPRINT_OUTPUT_FILE_FREE     As Integer =          0
  Public Const  LS_SPRINT_OUTPUT_FILE_BIN      As Integer =          1
  Public Const  LS_SPRINT_OUTPUT_FILE_TXT      As Integer =          2

   '! scan for basic solutions for pool '
  Public Const  LS_SOLVER_MODE_POOLBAS         As Integer =          1

   '! scan for edge solutions for pool '
  Public Const  LS_SOLVER_MODE_POOLEDGE        As Integer =          2

   '! scan for integer basic solutions '
  Public Const  LS_SOLVER_MODE_INTBAS          As Integer =          4

 ' Equivalences '
  Public Const  LS_IINFO_OBJSENSE              As Integer = LS_IPARAM_OBJSENSE
  Public Const  LS_IINFO_VER_MAJOR             As Integer = LS_IPARAM_VER_MAJOR
  Public Const  LS_IINFO_VER_MINOR             As Integer = LS_IPARAM_VER_MINOR
  Public Const  LS_IINFO_VER_BUILD             As Integer = LS_IPARAM_VER_BUILD
  Public Const  LS_IINFO_VER_REVISION          As Integer = LS_IPARAM_VER_REVISION

 ' Conic vs Second-Order-Cone equivalence'
  Public Const  LS_CONIC                       As Integer =    LS_SOCP
  Public Const  LS_MICONIC                     As Integer =  LS_MISOCP

 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 '                   Conversion to version 1.x                       '
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

 ' old parameter names, changed in 12.x '
  Public Const  LS_INT_PARAMETER_TYPE          As Integer = LS_INT_TYPE
  Public Const  LS_DOUBLE_PARAMETER_TYPE       As Integer = LS_DOUBLE_TYPE

 ' old parameter names, changed in 8.x '
  Public Const  LS_IPARAM_NLP_MSW_MAXREF       As Integer = LS_IPARAM_NLP_MSW_POPSIZE
  Public Const  LS_IPARAM_STOC_DEBUG_LEVEL     As Integer = LS_IPARAM_STOC_DEBUG_MASK

 ' old parameter names changed in 6.x '
  Public Const  LS_SPRINT_OUTPUT_FILE_DEFAULT  As Integer = LS_SPRINT_OUTPUT_FILE_FREE

 ' old parameter names changed in 5.x '
  Public Const  LS_IPARAM_SPLEX_SCALE          As Integer = LS_IPARAM_LP_SCALE
  Public Const  LS_IPARAM_SPLEX_ITRLMT         As Integer = LS_IPARAM_LP_ITRLMT
  Public Const  LS_IPARAM_MIP_USE_ENUM_HEU     As Integer = LS_IPARAM_MIP_ENUM_HEUMODE
  Public Const  LS_IPARAM_SOLVER_USE_CONCURRENT_OPT As Integer = LS_IPARAM_SOLVER_CONCURRENT_OPTMODE
  Public Const  LS_IPARAM_GOP_USEBNDLIM        As Integer = LS_IPARAM_GOP_BNDLIM_MODE

 ' old parameter names changed in 4.x or older'
  Public Const  LSLOC_BANDB                    As Integer =  LSLOC_MIP
  Public Const  LS_IPARAM_ITRLMT               As Integer = LS_IPARAM_SPLEX_ITRLMT
  Public Const  LS_IPARAM_PRICING              As Integer = LS_IPARAM_SPLEX_PPRICING
  Public Const  LS_IPARAM_SCALE                As Integer = LS_IPARAM_SPLEX_SCALE
  Public Const  LS_IPARAM_TIMLMT               As Integer = LS_IPARAM_SOLVER_TIMLMT
  Public Const  LS_DPARAM_CUTOFFVAL            As Integer = LS_DPARAM_SOLVER_CUTOFFVAL
  Public Const  LS_IPARAM_RESTART              As Integer = LS_IPARAM_SOLVER_RESTART
  Public Const  LS_DPARAM_FEASTOL              As Integer = LS_DPARAM_SOLVER_FEASTOL
  Public Const  LS_IPARAM_IUSOL                As Integer = LS_IPARAM_SOLVER_IUSOL
  Public Const  LS_IPARAM_MIPTIMLIM            As Integer = LS_IPARAM_MIP_TIMLIM
  Public Const  LS_IPARAM_MIPAOPTTIMLIM        As Integer = LS_IPARAM_MIP_AOPTTIMLIM
  Public Const  LS_IPARAM_MIPPRELEVEL          As Integer = LS_IPARAM_MIP_PRELEVEL
  Public Const  LS_IPARAM_MIPNODESELRULE       As Integer = LS_IPARAM_MIP_NODESELRULE
  Public Const  LS_DPARAM_MIPINTTOL            As Integer = LS_DPARAM_MIP_INTTOL
  Public Const  LS_DPARAM_MIPRELINTTOL         As Integer = LS_DPARAM_MIP_RELINTTOL
  Public Const  LS_DPARAM_MIP_OPTTOL           As Integer = LS_DPARAM_MIP_RELOPTTOL
  Public Const  LS_DPARAM_MIPOPTTOL            As Integer = LS_DPARAM_MIP_OPTTOL
  Public Const  LS_DPARAM_MIPPEROPTTOL         As Integer = LS_DPARAM_MIP_PEROPTTOL
  Public Const  LS_IPARAM_MIPMAXCUTPASS        As Integer = LS_IPARAM_MIP_MAXCUTPASS_TOP
  Public Const  LS_DPARAM_MIPADDCUTPER         As Integer = LS_DPARAM_MIP_ADDCUTPER
  Public Const  LS_IPARAM_MIPCUTLEVEL          As Integer = LS_IPARAM_MIP_CUTLEVEL_TOP
  Public Const  LS_IPARAM_MIPHEULEVEL          As Integer = LS_IPARAM_MIP_HEULEVEL
  Public Const  LS_IPARAM_MIPPRINTLEVEL        As Integer = LS_IPARAM_MIP_PRINTLEVEL
  Public Const  LS_IPARAM_MIPPREPRINTLEVEL     As Integer = LS_IPARAM_MIP_PREPRINTLEVEL
  Public Const  LS_DPARAM_MIPCUTOFFOBJ         As Integer = LS_DPARAM_MIP_CUTOFFOBJ
  Public Const  LS_IPARAM_MIPSTRONGBRANCHLEVEL As Integer = LS_IPARAM_MIP_STRONGBRANCHLEVEL
  Public Const  LS_IPARAM_MIPBRANCHDIR         As Integer = LS_IPARAM_MIP_BRANCHDIR
  Public Const  LS_IPARAM_MIPTOPOPT            As Integer = LS_IPARAM_MIP_TOPOPT
  Public Const  LS_IPARAM_MIPREOPT             As Integer = LS_IPARAM_MIP_REOPT
  Public Const  LS_IPARAM_MIPSOLVERTYPE        As Integer = LS_IPARAM_MIP_SOLVERTYPE
  Public Const  LS_IPARAM_MIPKEEPINMEM         As Integer = LS_IPARAM_MIP_KEEPINMEM
  Public Const  LS_DPARAM_MIP_REDCOSTFIXING_CUTOFF As Integer = LS_DPARAM_MIP_REDCOSTFIX_CUTOFF
  Public Const  LS_IPARAM_NLPPRINTLEVEL        As Integer = LS_IPARAM_NLP_PRINTLEVEL
  Public Const  LS_IPARAM_LPPRINTLEVEL         As Integer = LS_IPARAM_LP_PRINTLEVEL
  Public Const  LS_IPARAM_NLPSOLVER            As Integer = LS_IPARAM_NLP_SOLVER
  Public Const  LS_IPARAM_MODEL_CONVEX_FLAG    As Integer = LS_IPARAM_NLP_CONVEX
  Public Const  LS_IPARAM_NLP_SOLVEASLP        As Integer = LS_IPARAM_NLP_SOLVE_AS_LP
  Public Const  LS_DINFO_MIPBESTBOUND          As Integer = LS_DINFO_MIP_BESTBOUND
  Public Const  LS_IINFO_MIPBRANCHCOUNT        As Integer = LS_IINFO_MIP_BRANCHCOUNT
  Public Const  LS_IINFO_MIPSTATUS             As Integer = LS_IINFO_MIP_STATUS
  Public Const  LS_IINFO_MIPNEWIPSOL           As Integer = LS_IINFO_MIP_NEWIPSOL
  Public Const  LS_IINFO_MIPLPCOUNT            As Integer = LS_IINFO_MIP_LPCOUNT
  Public Const  LS_IINFO_MIPACTIVENODES        As Integer = LS_IINFO_MIP_ACTIVENODES
  Public Const  LS_IINFO_MIPLTYPE              As Integer = LS_IINFO_MIP_LTYPE
  Public Const  LS_IINFO_MIPAOPTTIMETOSTOP     As Integer = LS_IINFO_MIP_AOPTTIMETOSTOP
  Public Const  LS_DINFO_MIPOBJ                As Integer = LS_DINFO_MIP_OBJ
  Public Const  LS_IPARAM_BARRIER_PROB_TO_SOLVE As Integer = LS_IPARAM_PROB_TO_SOLVE
  Public Const  LS_IINFO_STATUS                As Integer = LS_IINFO_PRIMAL_STATUS
  Public Const  LS_GOPSOLSTAT_GLOBAL_OPTIMAL   As Integer = LS_STATUS_OPTIMAL
  Public Const  LS_GOPSOLSTAT_LOCAL_OPTIMAL    As Integer = LS_STATUS_LOCAL_OPTIMAL
  Public Const  LS_GOPSOLSTAT_INFEASIBLE       As Integer = LS_STATUS_INFEASIBLE
  Public Const  LS_GOPSOLSTAT_TOPUNBOUNDED     As Integer = LS_STATUS_UNBOUNDED
  Public Const  LS_GOPSOLSTAT_FEASIBLE         As Integer = LS_STATUS_FEASIBLE
  Public Const  LS_GOPSOLSTAT_UNKNOWN          As Integer = LS_STATUS_UNKNOWN
  Public Const  LS_GOPSOLSTAT_NUMERICAL_ERROR  As Integer = LS_STATUS_NUMERICAL_ERROR
  Public Const  LS_IIS_NORM_NONE               As Integer = LS_IIS_NORM_FREE
  Public Const  LS_IPARAM_STOC_SAMPLING_METHOD As Integer = LS_IPARAM_STOC_VARCONTROL_METHOD
  Public Const  LS_DPARAM_GOP_OPTTOL           As Integer = LS_DPARAM_GOP_RELOPTTOL

 ' old operator names '
  Public Const  EP_EXT_AND                     As Integer =    EP_VAND
  Public Const  EP_EXT_OR                      As Integer =     EP_VOR
  Public Const  EP_MULTMULT                    As Integer =   EP_VMULT
  Public Const  EP_PUSH_SVAR                   As Integer = EP_PUSH_SPAR

 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Structure Creation and Deletion Routines (4)                      '
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LScreateEnv _
 Lib "lindo12_0.dll" _
   (                                   ByRef  pnErrorcode       As      Integer         , _
                                       ByVal  pszPassword       As       String         ) As Integer


 Public Declare Function LScreateModel _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByRef  pnErrorcode       As      Integer         ) As Integer


 Public Declare Function LSdeleteEnv _
 Lib "lindo12_0.dll" _
   (                                   ByRef         pEnv       As       IntPtr         ) As Integer


 Public Declare Function LSdeleteModel _
 Lib "lindo12_0.dll" _
   (                                   ByRef       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSloadLicenseString _
 Lib "lindo12_0.dll" _
   (                                   ByVal     pszFname       As       String         , _
                                       ByVal  pachLicense       As StringBuilder         ) As Integer


 Public Declare Function LSgetVersionInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal   pachVernum       As StringBuilder         , _
                                       ByVal pachBuildDate       As StringBuilder         ) As Integer


 Public Declare Function LScopyParam _
 Lib "lindo12_0.dll" _
   (                                   ByVal  sourceModel       As      Integer         , _
                                       ByVal  targetModel       As      Integer         , _
                                       ByVal  mSolverType       As      Integer         ) As Integer


 Public Declare Function LSsetXSolverLibrary _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal    mVendorId       As      Integer         , _
                                       ByVal    szLibrary       As       String         ) As Integer


 Public Declare Function LSgetXSolverLibrary _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal    mVendorId       As      Integer         , _
                                       ByVal   szLibrary        As     StringBuilder) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Model I-O Routines (13)                                            '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSreadMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSwriteMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSreadLINDOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteLINDOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadLINDOStream _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    pszStream       As       String         , _
                                       ByVal   nStreamLen       As      Integer         ) As Integer


 Public Declare Function LSwriteLINGOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteDualMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         , _
                                       ByVal    nObjSense       As      Integer         ) As Integer


 Public Declare Function LSwriteDualLINDOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal    nObjSense       As      Integer         ) As Integer


 Public Declare Function LSwriteSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteNLSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteSolutionOfType _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSwriteIIS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteIUS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadMPIFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteMPIFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteMPXFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal        mMask       As      Integer         ) As Integer


 Public Declare Function LSwriteWithSetsAndSC _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSreadBasis _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSwriteBasis _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSwriteVarPriorities _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal        nMode       As      Integer         ) As Integer


 Public Declare Function LSreadLPFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadLPStream _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    pszStream       As       String         , _
                                       ByVal   nStreamLen       As      Integer         ) As Integer


 Public Declare Function LSreadSDPAFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadCBFFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadMPXFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadMPXStream _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    pszStream       As       String         , _
                                       ByVal   nStreamLen       As      Integer         ) As Integer


 Public Declare Function LSreadNLFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Error Handling Routines (3)                                        '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSgetErrorMessage _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nErrorcode       As      Integer         , _
                                       ByVal  pachMessage       As StringBuilder         ) As Integer


 Public Declare Function LSgetFileError _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef    pnLinenum       As      Integer         , _
                                       ByVal  pachLinetxt       As StringBuilder         ) As Integer


 Public Declare Function LSgetErrorRowIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef        piRow       As      Integer         ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Routines for Setting and Retrieving Parameter Values (14)          '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSsetModelParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal      pvValue       As       Object         ) As Integer


 Public Declare Function LSgetModelParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal      pvValue       As       Object         ) As Integer


    Public Declare Function LSsetModelParameter _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nParameter As Integer, _
                                          ByRef pvValue As Double) As Integer


    Public Declare Function LSgetModelParameter _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nParameter As Integer, _
                                          ByRef pvValue As Double) As Integer


 Public Declare Function LSsetEnvParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                          ByRef pvValue As Integer) As Integer


    Public Declare Function LSgetEnvParameter _
    Lib "lindo12_0.dll" _
      (ByVal pEnv As IntPtr, _
                                          ByVal nParameter As Integer, _
                                          ByRef pvValue As Integer) As Integer

    Public Declare Function LSsetEnvParameter _
    Lib "lindo12_0.dll" _
      (ByVal pEnv As IntPtr, _
                                          ByVal nParameter As Integer, _
                                          ByRef pvValue As Double) As Integer


 Public Declare Function LSgetEnvParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                          ByRef pvValue As Double) As Integer


 Public Declare Function LSsetModelDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal         dVal       As       Double         ) As Integer


 Public Declare Function LSgetModelDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef        pdVal       As       Double         ) As Integer


 Public Declare Function LSsetModelIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal         nVal       As      Integer         ) As Integer


 Public Declare Function LSgetModelIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef        pnVal       As      Integer         ) As Integer


 Public Declare Function LSsetEnvDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal         dVal       As       Double         ) As Integer


 Public Declare Function LSgetEnvDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef        pdVal       As       Double         ) As Integer


 Public Declare Function LSsetEnvIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByVal         nVal       As      Integer         ) As Integer


 Public Declare Function LSgetEnvIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef        pnVal       As      Integer         ) As Integer


 Public Declare Function LSreadModelParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSreadEnvParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteModelParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteEnvParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteParameterAsciiDoc _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal  pszFileName       As       String         ) As Integer


 Public Declare Function LSgetIntParameterRange _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef     pnValMIN       As      Integer         , _
                                       ByRef     pnValMAX       As      Integer         ) As Integer


 Public Declare Function LSgetDouParameterRange _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nParameter       As      Integer         , _
                                       ByRef     pdValMIN       As       Double         , _
                                       ByRef     pdValMAX       As       Double         ) As Integer


 Public Declare Function LSgetParamShortDesc _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       nParam       As      Integer         , _
                                       ByVal pachDescription       As StringBuilder         ) As Integer


 Public Declare Function LSgetParamLongDesc _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       nParam       As      Integer         , _
                                       ByVal pachDescription       As StringBuilder         ) As Integer


 Public Declare Function LSgetParamMacroName _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       nParam       As      Integer         , _
                                       ByVal    pachParam       As StringBuilder         ) As Integer


 Public Declare Function LSgetParamMacroID _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal      szParam       As       String         , _
                                       ByRef  pnParamType       As      Integer         , _
                                       ByRef      pnParam       As      Integer         ) As Integer


 Public Declare Function LSgetQCEigs _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal    pachWhich       As StringBuilder         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padEigval       As       Double ()      , _
                                       ByRef   *padEigvec       As       Double         , _
                                       ByVal      nEigval       As      Integer         , _
                                       ByVal          ncv       As      Integer         , _
                                       ByVal         dTol       As       Double         , _
                                       ByVal     nMaxIter       As      Integer         ) As Integer


 Public Declare Function LSgetEigs _
 Lib "lindo12_0.dll" _
   (                                   ByVal         nDim       As      Integer         , _
                                          ByVal chUL As Byte, _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padD       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padV       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetEigg _
 Lib "lindo12_0.dll" _
   (                                   ByVal         nDim       As      Integer         , _
                                          ByVal chJOBV As Byte, _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padWR       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padWI       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padVRR       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padVRI       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padVLR       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padVLI       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixTranspose _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padAT       As       Double ()      ) As Integer


 Public Declare Function LSgetMatrixInverse _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padAinv       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixInverseSY _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                          ByVal chUpLo As Byte, _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padAinv       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixLUFactor _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         panP       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixQRFactor _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padQ       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padR       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixDeterminant _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padDet       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixCholFactor _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                          ByVal chUpLo As Byte, _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padUL       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixSVDFactor _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nRows       As      Integer         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padS       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padVT       As       Double ()      , _
                                       ByRef       pnInfo       As      Integer         ) As Integer


 Public Declare Function LSgetMatrixFSolve _
 Lib "lindo12_0.dll" _
   (                                   ByVal       szuplo       As       String         , _
                                       ByVal      sztrans       As       String         , _
                                       ByVal       szdiag       As       String         , _
                                       ByVal        nRows       As      Integer         , _
                                       ByVal       dAlpha       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSgetMatrixBSolve _
 Lib "lindo12_0.dll" _
   (                                   ByVal       szuplo       As       String         , _
                                       ByVal      sztrans       As       String         , _
                                       ByVal       szdiag       As       String         , _
                                       ByVal        nRows       As      Integer         , _
                                       ByVal       dAlpha       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSgetMatrixSolve _
 Lib "lindo12_0.dll" _
   (                                   ByVal       szside       As       String         , _
                                       ByVal       szuplo       As       String         , _
                                       ByVal      sztrans       As       String         , _
                                       ByVal       szdiag       As       String         , _
                                       ByVal        nRows       As      Integer         , _
                                       ByVal         nRHS       As      Integer         , _
                                       ByVal       dAlpha       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padA       As       Double ()      , _
                                       ByVal         nLDA       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
                                       ByVal         nLDB       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer

  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' Model Loading Routines (9)                                        '
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSloadLPData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
                                       ByVal        nVars       As      Integer         , _
                                       ByVal    dObjSense       As      Integer         , _
                                       ByVal    dObjConst       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padC       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
                                       ByVal  pszConTypes       As       String         , _
                                       ByVal        nAnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padAcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSloadQCData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQCnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiQCrows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSloadConeData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCone       As      Integer         , _
                                       ByVal pszConeTypes       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padConeAlpha       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiConebegcone       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiConecols       As      Integer ()      ) As Integer


 Public Declare Function LSloadPOSDData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nPOSD       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiPOSDdim       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiPOSDbeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDrowndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDcolndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDvarndx       As      Integer ()      ) As Integer


 Public Declare Function LSloadALLDIFFData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     nALLDIFF       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffDim       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiAlldiffL       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiAlldiffU       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffVar       As      Integer ()      ) As Integer


 Public Declare Function LSloadSETSData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nSETS       As      Integer         , _
                                       ByVal  pszSETStype       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiCARDnum       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiSETSbegcol       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiSETScols       As      Integer ()      ) As Integer


 Public Declare Function LSloadSemiContData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      nSCVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSloadVarType _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszVarTypes       As       String         ) As Integer


 Public Declare Function LSloadNameData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszTitle       As       String         , _
                                       ByVal   pszObjName       As       String         , _
                                       ByVal   pszRhsName       As       String         , _
                                       ByVal   pszRngName       As       String         , _
                                       ByVal   pszBndname       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszConNames       As       String ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszVarNames       As       String ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszConeNames       As       String ()      ) As Integer


 Public Declare Function LSloadNLPData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiNLPcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panNLPcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padNLPcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiNLProws       As      Integer ()      , _
                                       ByVal      nNLPobj       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiNLPobj       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNLPobj       As       Double ()      ) As Integer


 Public Declare Function LSloadNLPDense _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
                                       ByVal        nVars       As      Integer         , _
                                       ByVal    dObjSense       As      Integer         , _
                                       ByVal  pszConTypes       As       String         , _
                                       ByVal  pszVarTypes       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padX0       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSloadInstruct _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
                                       ByVal        nObjs       As      Integer         , _
                                       ByVal        nVars       As      Integer         , _
                                       ByVal     nNumbers       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panObjSense       As      Integer ()      , _
                                       ByVal   pszConType       As       String         , _
                                       ByVal   pszVarType       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panInstruct       As      Integer ()      , _
                                       ByVal    nInstruct       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNumVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padVarVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiObjBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panObjLen       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiConBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panConLen       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padLB       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padUB       As       Double ()      ) As Integer


 Public Declare Function LSaddInstruct _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
                                       ByVal        nObjs       As      Integer         , _
                                       ByVal        nVars       As      Integer         , _
                                       ByVal     nNumbers       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panObjSense       As      Integer ()      , _
                                       ByVal   pszConType       As       String         , _
                                       ByVal   pszVarType       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panInstruct       As      Integer ()      , _
                                       ByVal    nInstruct       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNumVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padVarVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiObjBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panObjLen       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiConBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panConLen       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padLB       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padUB       As       Double ()      ) As Integer


 Public Declare Function LSloadStringData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     nStrings       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszStringData       As       String ()      ) As Integer


 Public Declare Function LSloadString _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    pszString       As       String         ) As Integer


 Public Declare Function LSbuildStringData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSdeleteStringData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSdeleteString _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSgetStringValue _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      iString       As      Integer         , _
                                       ByRef      pdValue       As       Double         ) As Integer


 Public Declare Function LSgetConstraintProperty _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      ndxCons       As      Integer         , _
                                       ByRef   pnConptype       As      Integer         ) As Integer


 Public Declare Function LSsetConstraintProperty _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      ndxCons       As      Integer         , _
                                       ByVal    nConptype       As      Integer         ) As Integer


 Public Declare Function LSgetGOPVariablePriority _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       ndxVar       As      Integer         , _
                                       ByRef   pnPriority       As      Integer         ) As Integer


 Public Declare Function LSsetGOPVariablePriority _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       ndxVar       As      Integer         , _
                                       ByVal    nPriority       As      Integer         ) As Integer


 Public Declare Function LSloadMultiStartSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         ) As Integer


 Public Declare Function LSloadGASolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         ) As Integer


 Public Declare Function LSaddQCShift _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal       dShift       As       Double         ) As Integer


 Public Declare Function LSgetQCShift _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByRef      pdShift       As       Double         ) As Integer


 Public Declare Function LSresetQCShift _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         ) As Integer


 Public Declare Function LSaddObjPool _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padC       As       Double ()      , _
                                       ByVal    mObjSense       As      Integer         , _
                                       ByVal        mRank       As      Integer         , _
                                       ByVal   dRelOptTol       As       Double         ) As Integer


 Public Declare Function LSremObjPool _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nObjIndex       As      Integer         ) As Integer


 Public Declare Function LSfreeObjPool _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSsetObjPoolInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nObjIndex       As      Integer         , _
                                       ByVal        mInfo       As      Integer         , _
                                       ByVal       dValue       As       Double         ) As Integer


 Public Declare Function LSgetObjPoolNumSol _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nObjIndex       As      Integer         , _
                                       ByRef      pNumSol       As      Integer         ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Solver Initialization Routines (6)                                 '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSloadBasis _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panCstatus       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panRstatus       As      Integer ()      ) As Integer


 Public Declare Function LSloadVarPriorities _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panCprior       As      Integer ()      ) As Integer


 Public Declare Function LSreadVarPriorities _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSloadVarStartPoint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSloadVarStartPointPartial _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSloadMIPVarStartPoint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSloadMIPVarStartPointPartial _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiPrimal       As      Integer ()      ) As Integer


 Public Declare Function LSreadVarStartPoint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSloadBlockStructure _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nBlock       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panRblock       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panCblock       As      Integer ()      , _
                                       ByVal        nType       As      Integer         ) As Integer


 Public Declare Function LSloadIISPriorities _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panRprior       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panCprior       As      Integer ()      ) As Integer


 Public Declare Function LSloadSolutionAt _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nObjIndex       As      Integer         , _
                                       ByVal    nSolIndex       As      Integer         ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Optimization Routines (3)                                          '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSoptimize _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      nMethod       As      Integer         , _
                                       ByRef  pnSolStatus       As      Integer         ) As Integer


 Public Declare Function LSsolveMIP _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef pnMIPSolStatus       As      Integer         ) As Integer


 Public Declare Function LSsolveGOP _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef pnGOPSolStatus       As      Integer         ) As Integer


 Public Declare Function LSoptimizeQP _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef pnQPSolStatus       As      Integer         ) As Integer


 Public Declare Function LScheckConvexity _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSsolveSBD _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      nStages       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panRowStage       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panColStage       As      Integer ()      , _
                                       ByRef     pnStatus       As      Integer         ) As Integer


 Public Declare Function LSsolveMipBnp _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nBlock       As      Integer         , _
                                       ByVal     pszFname       As       String         , _
                                       ByRef     pnStatus       As      Integer         ) As Integer

  ' query general model and solver information '


 Public Declare Function LSgetInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef pvResult As Integer) As Integer

    Public Declare Function LSgetInfo _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByRef pvResult As Double) As Integer


 Public Declare Function LSgetProfilerInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     mContext       As      Integer         , _
                                       ByRef      pnCalls       As      Integer         , _
                                       ByRef pdElapsedTime       As       Double         ) As Integer


 Public Declare Function LSgetProfilerContext _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     mContext       As      Integer         ) As Integer

  ' query continous models '


 Public Declare Function LSgetPrimalSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSgetDualSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padDual       As       Double ()      ) As Integer


 Public Declare Function LSgetReducedCosts _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padRedcosts       As       Double ()      ) As Integer


 Public Declare Function LSgetReducedCostsCone _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padRedcosts       As       Double ()      ) As Integer


 Public Declare Function LSgetSlacks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padSlacks       As       Double ()      ) As Integer


 Public Declare Function LSgetBasis _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panCstatus       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panRstatus       As      Integer ()      ) As Integer


 Public Declare Function LSgetSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nWhich       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padVal       As       Double ()      ) As Integer


 Public Declare Function LSgetNextBestSol _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef  pnModStatus       As      Integer         ) As Integer

  ' query integer models '


 Public Declare Function LSgetMIPPrimalSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSgetMIPDualSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padDual       As       Double ()      ) As Integer


 Public Declare Function LSgetMIPReducedCosts _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padRedcosts       As       Double ()      ) As Integer


 Public Declare Function LSgetMIPSlacks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padSlacks       As       Double ()      ) As Integer


 Public Declare Function LSgetMIPBasis _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panCstatus       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panRstatus       As      Integer ()      ) As Integer


 Public Declare Function LSgetNextBestMIPSol _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef pnIntModStatus       As      Integer         ) As Integer


 Public Declare Function LSgetKBestMIPSols _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         , _
                                       ByRef pfMIPCallback       As      Integer         , _
                                       ByVal     pvCbData       As       Object         , _
                                       ByVal     nMaxSols       As      Integer         ) As Integer

  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  ' Model Query Routines (13)                                         '
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSgetLPData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef   pdObjSense       As      Integer         , _
                                       ByRef   pdObjConst       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padC       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
                                       ByVal pachConTypes       As StringBuilder         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padAcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSgetQCData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiQCrows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetQCDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iCon       As      Integer         , _
                                       ByRef      pnQCnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetVarType _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal pachVarTypes       As StringBuilder         ) As Integer


 Public Declare Function LSgetVarStartPoint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSgetVarStartPointPartial _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      panCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSgetMIPVarStartPointPartial _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      panCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiPrimal       As      Integer ()      ) As Integer


 Public Declare Function LSgetMIPVarStartPoint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer


 Public Declare Function LSgetSETSData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      piNsets       As      Integer         , _
                                       ByRef       piNtnz       As      Integer         , _
                                       ByVal  pachSETtype       As StringBuilder         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiCardnum       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       paiNnz       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiBegset       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiVarndx       As      Integer ()      ) As Integer


 Public Declare Function LSgetSETSDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iSet       As      Integer         , _
                                       ByVal  pachSETType       As StringBuilder         , _
                                       ByRef    piCardnum       As      Integer         , _
                                       ByRef        piNnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiVarndx       As      Integer ()      ) As Integer


 Public Declare Function LSsetSETSStatei _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iSet       As      Integer         , _
                                       ByVal       mState       As      Integer         ) As Integer


 Public Declare Function LSgetSemiContData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      piNvars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiVarndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSgetALLDIFFData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef   pinALLDIFF       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffDim       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiAlldiffL       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiAlldiffU       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffVar       As      Integer ()      ) As Integer


 Public Declare Function LSgetALLDIFFDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     iALLDIFF       As      Integer         , _
                                       ByRef piAlldiffDim       As      Integer         , _
                                       ByRef   piAlldiffL       As      Integer         , _
                                       ByRef   piAlldiffU       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiAlldiffVar       As      Integer ()      ) As Integer


 Public Declare Function LSgetPOSDData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      pinPOSD       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiPOSDdim       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiPOSDnnz       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiPOSDbeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDrowndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDcolndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDvarndx       As      Integer ()      ) As Integer


 Public Declare Function LSgetPOSDDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iPOSD       As      Integer         , _
                                       ByRef    piPOSDdim       As      Integer         , _
                                       ByRef    piPOSDnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDrowndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDcolndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiPOSDvarndx       As      Integer ()      ) As Integer


 Public Declare Function LSgetNameData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    pachTitle       As StringBuilder         , _
                                       ByVal  pachObjName       As StringBuilder         , _
                                       ByVal  pachRhsName       As StringBuilder         , _
                                       ByVal  pachRngName       As StringBuilder         , _
                                       ByVal  pachBndname       As StringBuilder         , _
                                          ByVal pachConNames As StringBuilder, _
                                       ByVal pachConNameData       As StringBuilder         , _
                                          ByVal pachVarNames As StringBuilder, _
                                       ByVal pachVarNameData       As StringBuilder         ) As Integer


 Public Declare Function LSgetLPVariableDataj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iVar       As      Integer         , _
                                       ByVal  pachVartype       As StringBuilder         , _
                                       ByRef          pdC       As       Double         , _
                                       ByRef          pdL       As       Double         , _
                                       ByRef          pdU       As       Double         , _
                                       ByRef       pnAnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padAcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetVariableNamej _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iVar       As      Integer         , _
                                       ByVal  pachVarName       As StringBuilder         ) As Integer


 Public Declare Function LSgetVariableIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   pszVarName       As       String         , _
                                       ByRef        piVar       As      Integer         ) As Integer


 Public Declare Function LSgetConstraintNamei _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iCon       As      Integer         , _
                                       ByVal  pachConName       As StringBuilder         ) As Integer


 Public Declare Function LSgetConstraintIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   pszConName       As       String         , _
                                       ByRef        piCon       As      Integer         ) As Integer


 Public Declare Function LSgetConstraintDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iCon       As      Integer         , _
                                       ByVal  pachConType       As StringBuilder         , _
                                       ByVal    pachIsNlp       As StringBuilder         , _
                                       ByRef          pdB       As       Double         ) As Integer


 Public Declare Function LSgetLPConstraintDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iCon       As      Integer         , _
                                       ByVal  pachConType       As StringBuilder         , _
                                       ByRef          pdB       As       Double         , _
                                       ByRef        pnNnz       As      Integer         , _
                                       ByRef        piVar       As      Integer         , _
                                       ByRef      pdAcoef       As       Double         ) As Integer


 Public Declare Function LSgetConeNamei _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iCone       As      Integer         , _
                                       ByVal pachConeName       As StringBuilder         ) As Integer


 Public Declare Function LSgetConeIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszConeName       As       String         , _
                                       ByRef       piCone       As      Integer         ) As Integer


 Public Declare Function LSgetConeDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iCone       As      Integer         , _
                                       ByVal pachConeType       As StringBuilder         , _
                                       ByRef  pdConeAlpha       As       Double         , _
                                       ByRef        piNnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      ) As Integer


 Public Declare Function LSgetNLPData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiNLPcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panNLPcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padNLPcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiNLProws       As      Integer ()      , _
                                       ByRef     pnNLPobj       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiNLPobj       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNLPobj       As       Double ()      , _
                                       ByVal pachNLPConTypes       As StringBuilder         ) As Integer


 Public Declare Function LSgetNLPConstraintDatai _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iCon       As      Integer         , _
                                       ByRef        pnNnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiNLPcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padNLPcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetNLPVariableDataj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iVar       As      Integer         , _
                                       ByRef        pnNnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panNLProws       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padNLPcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetNLPObjectiveData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef  pnNLPobjnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiNLPobj       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNLPobj       As       Double ()      ) As Integer


 Public Declare Function LSgetDualModel _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   pDualModel       As      Integer         ) As Integer


 Public Declare Function LSgetInstruct _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef   pnObjSense       As      Integer         , _
                                       ByVal  pachConType       As StringBuilder         , _
                                       ByVal  pachVarType       As StringBuilder         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      panCode       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padNumVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padVarVal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panObjBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panObjLength       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panConBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panConLength       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padLwrBnd       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padUprBnd       As       Double ()      ) As Integer


 Public Declare Function LScalinfeasMIPsolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef   pdIntPfeas       As       Double         , _
                                       ByRef  pbConsPfeas       As       Double         , _
                                       ByRef pdPrimalMipsol       As       Double         ) As Integer


 Public Declare Function LSgetRoundMIPsolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padPrimalRound       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padObjRound       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padPfeasRound       As       Double ()      , _
                                       ByRef     pnstatus       As      Integer         , _
                                       ByVal     iUseOpti       As      Integer         ) As Integer


 Public Declare Function LSgetDuplicateColumns _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal   nCheckVals       As      Integer         , _
                                       ByRef       pnSets       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiSetsBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      ) As Integer


 Public Declare Function LSgetRangeData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padR       As       Double ()      ) As Integer


 Public Declare Function LSgetJac _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef  pnJnonzeros       As      Integer         , _
                                       ByRef    pnJobjnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiJrows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiJcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padJcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSgetHess _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef  pnHnonzeros       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiHrows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiHcol1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiHcol2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padHcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 '  Model Modification Routines (22)                                  '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSaddConstraints _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  nNumaddcons       As      Integer         , _
                                       ByVal  pszConTypes       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszConNames       As       String ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padAcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      ) As Integer


 Public Declare Function LSaddVariables _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  nNumaddvars       As      Integer         , _
                                       ByVal  pszVarTypes       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszVarNames       As       String ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padAcoef       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padC       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSaddCones _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCone       As      Integer         , _
                                       ByVal pszConeTypes       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padConeAlpha       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszConenames       As       String ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiConebegcol       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiConecols       As      Integer ()      ) As Integer


 Public Declare Function LSaddSETS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nSETS       As      Integer         , _
                                       ByVal  pszSETStype       As       String         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiCARDnum       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiSETSbegcol       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiSETScols       As      Integer ()      ) As Integer


 Public Declare Function LSaddQCterms _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  nQCnonzeros       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiQCconndx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSdeleteConstraints _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteCones _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nCones       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiCones       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteSETS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nSETS       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiSETS       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteSemiContVars _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      nSCVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiSCVars       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteVariables _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteQCterms _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      ) As Integer


 Public Declare Function LSdeleteAj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iVar1       As      Integer         , _
                                       ByVal        nRows       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiRows       As      Integer ()      ) As Integer


 Public Declare Function LSmodifyLowerBounds _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      ) As Integer


 Public Declare Function LSmodifyUpperBounds _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSmodifyRHS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      ) As Integer


 Public Declare Function LSmodifyObjective _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padC       As       Double ()      ) As Integer


 Public Declare Function LSmodifyObjConstant _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    dObjConst       As       Double         ) As Integer


 Public Declare Function LSmodifyAj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iVar1       As      Integer         , _
                                       ByVal        nRows       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiRows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padAj       As       Double ()      ) As Integer


 Public Declare Function LSmodifyCone _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    cConeType       As      String          , _
                                       ByVal     iConeNum       As      Integer         , _
                                       ByVal     iConeNnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  paiConeCols       As      Integer ()      , _
                                       ByVal   dConeAlpha       As       Double         ) As Integer


 Public Declare Function LSmodifySET _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     cSETtype       As      String          , _
                                       ByVal      iSETnum       As      Integer         , _
                                       ByVal      iSETnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiSETcols       As      Integer ()      ) As Integer


 Public Declare Function LSmodifySemiContVars _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      nSCVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiSCVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      ) As Integer


 Public Declare Function LSmodifyConstraintType _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
                                       ByVal  pszConTypes       As       String         ) As Integer


 Public Declare Function LSmodifyVariableType _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nVars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
                                       ByVal  pszVarTypes       As       String         ) As Integer


 Public Declare Function LSaddNLPAj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iVar1       As      Integer         , _
                                       ByVal        nRows       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiRows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal        padAj       As       Double ()      ) As Integer


 Public Declare Function LSaddNLPobj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padColj       As       Double ()      ) As Integer


 Public Declare Function LSdeleteNLPobj _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCols       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      ) As Integer

 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 '   Model & Solution Analysis Routines (10)                         '
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSgetConstraintRanges _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padDec       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padInc       As       Double ()      ) As Integer


 Public Declare Function LSgetObjectiveRanges _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padDec       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padInc       As       Double ()      ) As Integer


 Public Declare Function LSgetBoundRanges _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padDec       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal       padInc       As       Double ()      ) As Integer


 Public Declare Function LSgetBestBounds _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padBestL       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padBestU       As       Double ()      ) As Integer


 Public Declare Function LSfindIIS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nLevel       As      Integer         ) As Integer


 Public Declare Function LSfindIUS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nLevel       As      Integer         ) As Integer


 Public Declare Function LSfindBlockStructure _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nBlock       As      Integer         , _
                                       ByVal        nType       As      Integer         ) As Integer


 Public Declare Function LSdisplayBlockStructure _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      pnBlock       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColPos       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowPos       As      Integer ()      ) As Integer


 Public Declare Function LSgetIIS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      pnSuf_r       As      Integer         , _
                                       ByRef      pnIIS_r       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
                                       ByRef      pnSuf_c       As      Integer         , _
                                       ByRef      pnIIS_c       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      panBnds       As      Integer ()      ) As Integer


 Public Declare Function LSgetIISInts _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef    pnSuf_int       As      Integer         , _
                                       ByRef    pnIIS_int       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      ) As Integer


 Public Declare Function LSgetIISSETs _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef    pnSuf_set       As      Integer         , _
                                       ByRef    pnIIS_set       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiSets       As      Integer ()      ) As Integer


 Public Declare Function LSgetIUS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef        pnSuf       As      Integer         , _
                                       ByRef        pnIUS       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiVars       As      Integer ()      ) As Integer


 Public Declare Function LSgetBlockStructure _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      pnBlock       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panRblock       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panCblock       As      Integer ()      , _
                                       ByRef       pnType       As      Integer         ) As Integer


 Public Declare Function LSfindSymmetry _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                          ByRef pnerrorcode As Integer) As IntPtr


 Public Declare Function LSdeleteSymmetry _
 Lib "lindo12_0.dll" _
      (ByRef pSymInfo As IntPtr) As Integer


 Public Declare Function LSgetOrbitInfo _
 Lib "lindo12_0.dll" _
      (ByVal pSymInfo As IntPtr, _
                                       ByRef pnNumGenerators       As      Integer         , _
                                       ByRef pnNumOfOrbits       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  panOrbitBeg       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panOrbits       As      Integer ()      ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Advanced Routines (6)                                              '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSdoBTRAN _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef        pcYnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         paiY       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padY       As       Double ()      , _
                                       ByRef        pcXnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         paiX       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSdoFTRAN _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef        pcYnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         paiY       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padY       As       Double ()      , _
                                       ByRef        pcXnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         paiX       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer

  ' function and gradient evaluations '


 Public Declare Function LScalcObjFunc _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
                                       ByRef     pdObjval       As       Double         ) As Integer


 Public Declare Function LScalcConFunc _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padSlacks       As       Double ()      ) As Integer


 Public Declare Function LScalcObjGrad _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
                                       ByVal     nParList       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiParList       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padParGrad       As       Double ()      ) As Integer


 Public Declare Function LScalcConGrad _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         irow       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
                                       ByVal     nParList       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiParList       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padParGrad       As       Double ()      ) As Integer


 Public Declare Function LScheckQterms _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiType       As      Integer ()      ) As Integer


 Public Declare Function LSrepairQterms _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiType       As      Integer ()      ) As Integer


 Public Declare Function LScomputeFunction _
 Lib "lindo12_0.dll" _
   (                                   ByVal         inst       As      Integer         , _
                                       ByRef      pdInput       As       Double         , _
                                       ByRef     pdOutput       As       Double         ) As Integer


 Public Declare Function LSfindLtf _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColPos       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowPos       As      Integer ()      ) As Integer


 Public Declare Function LSapplyLtf _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowIdx       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewColPos       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panNewRowPos       As      Integer ()      , _
                                       ByVal        nMode       As      Integer         ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' Callback Management Routines (9)                                   '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    ' Delegates 
    Public Delegate Function typCallback _
                       (ByVal nModel As IntPtr, _
                        ByVal loc As Integer, _
                        ByVal nvCbData As IntPtr) As Integer

    Public Delegate Function typMIPCallback _
                       (ByVal nModel As IntPtr, _
                        ByVal nvCbData As IntPtr, _
                        ByVal dObj As Double, _
                        ByRef adPrimal As Double) As Integer


    Public Delegate Function typGOPCallback _
                       (ByVal nModel As IntPtr, _
                        ByVal nvCbData As IntPtr, _
                        ByVal dObj As Double, _
                        ByRef adPrimal As Double) As Integer

    ' Delegates 
    Public Delegate Function typStrategy _
                       (ByVal nModel As IntPtr, _
                        ByVal nRunId As Integer, _
                        ByVal nvCbData As IntPtr) As Integer

    Public Delegate Function typFuncalc _
                       (ByVal nModel As IntPtr, _
                        ByVal nvCbData As IntPtr, _
                        ByVal nRow As Integer, _
                        ByVal adX As IntPtr, _
                        ByVal nJDiff As Integer, _
                        ByVal dXJBase As Double, _
                        ByRef adFuncVal As Double, _
                        ByVal pReserved As Integer) As Integer

    Public Delegate Function typGradcalc _
                       (ByVal nModel As IntPtr, _
                        ByVal nvCbData As IntPtr, _
                        ByVal nRow As Integer, _
                        ByVal adX As IntPtr, _
                        ByRef adLB As Double, _
                        ByRef adUB As Double, _
                        ByVal nNewPnt As Integer, _
                        ByVal nNPar As Integer, _
                        ByRef aiPartial As Integer, _
                        ByRef adPartial As Double) As Integer


    Public Delegate Function typUsercalc _
                       (ByVal nModel As IntPtr, _
                        ByVal nArgs As Integer, _
                        ByRef pdValues As Double, _
                        ByVal nvCbData As IntPtr, _
                        ByRef adFuncVal As Double) As Integer

    Public Delegate Sub typPrintEnvLog _
                       (ByVal nEnv As IntPtr, _
                        ByVal szStr As String, _
                        ByVal nvCbData As IntPtr)

    Public Delegate Sub typPrintModelLog _
                       (ByVal nModel As IntPtr, _
                        ByVal szStr As String, _
                        ByVal nvCbData As IntPtr)

    Public Delegate Function typUserPdf _
                       (ByVal nSample As IntPtr, _
                        ByVal nFuncType As Integer, _
                        ByRef dInput As Double, _
                        ByVal pdOutput As IntPtr, _
                        ByVal nvCbData As IntPtr) As Integer

    ' set various callback                      
    Public Declare Function LSsetCallback _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfCallback As typCallback, _
                        ByRef nvCbData As Object) As Integer


 Public Declare Function LSsetCallback _
       Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                           ByVal nfCallback As typCallback, _
                           ByVal nvCbData As IntPtr) As Integer

    Public Declare Function LSsetMIPCCStrategy _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfCallback As typStrategy, _
                        ByRef nvCbData As Object) As Integer

    Public Declare Function LSsetMIPCallback _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfMIPCallback As typMIPCallback, _
                        ByRef nvCbData As Object) As Integer


 Public Declare Function LSsetMIPCallback _
       Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                           ByVal nfMIPCallback As typMIPCallback, _
                           ByVal nvCbData As IntPtr) As Integer

    Public Declare Function LSsetEnvLogfunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                       ByVal locFunc As typPrintEnvLog, _
                       ByRef nvPrData As Object) As Integer

    Public Declare Function LSsetEnvLogfunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                       ByVal locFunc As typPrintEnvLog, _
                       ByVal nvPrData As IntPtr) As Integer

    Public Declare Function LSsetEnvLogfunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                       ByVal locFunc As typPrintEnvLog, _
                       ByVal nvPrData As Integer) As Integer


    Public Declare Function LSsetModelLogfunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                       ByVal locFunc As typPrintModelLog, _
                       ByRef nvPrData As Object) As Integer

    Public Declare Function LSsetModelLogfunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                       ByVal locFunc As typPrintModelLog, _
                       ByVal nvPrData As IntPtr) As Integer


    Public Declare Function LSgetCallbackInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                          ByVal nLocation As Integer, _
                                          ByVal nQuery As Integer, _
                                          ByRef nvValue As Double) As Integer


    Public Declare Function LSgetCallbackInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                          ByVal nLocation As Integer, _
                                          ByVal nQuery As Integer, _
                                          ByRef nvValue As Integer) As Integer

    Public Declare Function LSgetMIPCallbackInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                          ByVal nQuery As Integer, _
                                          ByRef nvValue As Double) As Integer


    Public Declare Function LSgetMIPCallbackInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                          ByVal nQuery As Integer, _
                                          ByRef nvValue As Integer) As Integer


    Public Declare Function LSgetProgressInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nLocation       As      Integer         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef nvValue As Double) As Integer


 Public Declare Function LSgetProgressInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nLocation       As      Integer         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef nvValue As Integer) As Integer

    ' function evaluation routines for NLP solvers '/

    ' overloaded set of LSsetGradcalc
    Public Declare Function LSsetGradcalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfGrad_func As typGradcalc, _
                        ByRef nvUserData As Object, _
                        ByVal nLenUseGrad As Integer, _
                        ByRef nUseGrad As Integer) As Integer


 Public Declare Function LSsetGradcalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfGrad_func As typGradcalc, _
                        ByVal nvCbData As IntPtr, _
                                       ByVal  nLenUseGrad       As      Integer         , _
                        ByRef nUseGrad As Integer) As Integer


    ' overloaded set of LSsetFuncalc


 Public Declare Function LSsetFuncalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfFunc As typFuncalc, _
                        ByRef nvFData As Object) As Integer

    Public Declare Function LSsetFuncalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfFunc As typFuncalc, _
                        ByVal nvCbData As IntPtr) As Integer


    ' overloaded set of LSsetUsercalc
    Public Declare Function LSsetUsercalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfFunc As typUsercalc, _
                        ByRef nvFData As Object) As Integer


 Public Declare Function LSsetUsercalc _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfFunc As typUsercalc, _
                        ByVal nvCbData As IntPtr) As Integer


    Public Declare Function LSsetEnvExitFunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                        ByVal pfExitFunc As Object, _
                        ByVal nvCbData As Object) As Integer


 Public Declare Function LSsetEnvExitFunc _
    Lib "lindo12_0.dll" (ByVal nEnv As IntPtr, _
                        ByVal pfExitFunc As Object, _
                        ByVal nvCbData As IntPtr) As Integer



    Public Declare Function LSsetGOPCallback _
    Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                        ByVal nfMIPCallback As typGOPCallback, _
                        ByRef nvCbData As Object) As Integer


 Public Declare Function LSsetGOPCallback _
       Lib "lindo12_0.dll" (ByVal nModel As IntPtr, _
                           ByVal nfMIPCallback As typGOPCallback, _
                           ByVal nvCbData As IntPtr) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 '  Memory Related Routines (7)                                       '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSfreeSolverMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSfreeHashMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSfreeSolutionMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSfreeMIPSolutionMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSfreeGOPSolutionMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSsetProbAllocSizes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal n_vars_alloc       As      Integer         , _
                                       ByVal n_cons_alloc       As      Integer         , _
                                       ByVal   n_QC_alloc       As      Integer         , _
                                       ByVal n_Annz_alloc       As      Integer         , _
                                       ByVal n_Qnnz_alloc       As      Integer         , _
                                       ByVal n_NLPnnz_alloc       As      Integer         ) As Integer


 Public Declare Function LSsetProbNameAllocSizes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal n_varname_alloc       As      Integer         , _
                                       ByVal n_rowname_alloc       As      Integer         ) As Integer


 Public Declare Function LSaddEmptySpacesAcolumns _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiColnnz       As      Integer ()      ) As Integer


 Public Declare Function LSaddEmptySpacesNLPAcolumns _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiColnnz       As      Integer ()      ) As Integer

 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ' new  functions from version 5.+                                    '
 ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



 ' basic IO routines '


 Public Declare Function LSwriteDeteqMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszFilename       As       String         , _
                                       ByVal      nFormat       As      Integer         , _
                                       ByVal        iType       As      Integer         ) As Integer


 Public Declare Function LSwriteDeteqLINDOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszFilename       As       String         , _
                                       ByVal        iType       As      Integer         ) As Integer


 Public Declare Function LSwriteSMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszCorefile       As       String         , _
                                       ByVal  pszTimefile       As       String         , _
                                       ByVal  pszStocfile       As       String         , _
                                       ByVal    nCoretype       As      Integer         ) As Integer


 Public Declare Function LSreadSMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszCorefile       As       String         , _
                                       ByVal  pszTimefile       As       String         , _
                                       ByVal  pszStocfile       As       String         , _
                                       ByVal    nCoretype       As      Integer         ) As Integer


 Public Declare Function LSwriteSMPIFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszCorefile       As       String         , _
                                       ByVal  pszTimefile       As       String         , _
                                       ByVal  pszStocfile       As       String         ) As Integer


 Public Declare Function LSreadSMPIFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal  pszCorefile       As       String         , _
                                       ByVal  pszTimefile       As       String         , _
                                       ByVal  pszStocfile       As       String         ) As Integer


 Public Declare Function LSwriteScenarioSolutionFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteNodeSolutionFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteScenarioMPIFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal     pszFname       As       String         ) As Integer


 Public Declare Function LSwriteScenarioMPSFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal     pszFname       As       String         , _
                                       ByVal      nFormat       As      Integer         ) As Integer


 Public Declare Function LSwriteScenarioLINDOFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal     pszFname       As       String         ) As Integer

 ' parameter routines '


 Public Declare Function LSsetModelStocDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iPar       As      Integer         , _
                                       ByVal         dVal       As       Double         ) As Integer


 Public Declare Function LSgetModelStocDouParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iPar       As      Integer         , _
                                       ByRef        pdVal       As       Double         ) As Integer


 Public Declare Function LSsetModelStocIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iPar       As      Integer         , _
                                       ByVal         iVal       As      Integer         ) As Integer


 Public Declare Function LSgetModelStocIntParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iPar       As      Integer         , _
                                       ByRef        piVal       As      Integer         ) As Integer

 ' general query routines '


 Public Declare Function LSgetScenarioIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      pszName       As       String         , _
                                       ByRef      pnIndex       As      Integer         ) As Integer


 Public Declare Function LSgetStageIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      pszName       As       String         , _
                                       ByRef      pnIndex       As      Integer         ) As Integer


 Public Declare Function LSgetStocParIndex _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      pszName       As       String         , _
                                       ByRef      pnIndex       As      Integer         ) As Integer


 Public Declare Function LSgetStocParName _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         , _
                                       ByVal     pachName       As StringBuilder         ) As Integer


 Public Declare Function LSgetScenarioName _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         , _
                                       ByVal     pachName       As StringBuilder         ) As Integer


 Public Declare Function LSgetStageName _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         , _
                                       ByVal     pachName       As StringBuilder         ) As Integer


 Public Declare Function LSgetStocInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                       ByVal       nParam       As      Integer         , _
                                          ByRef pResult As Double) As Integer

    Public Declare Function LSgetStocInfo _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByVal nParam As Integer, _
                                          ByRef pResult As Integer) As Integer


 Public Declare Function LSgetStocCCPInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                       ByVal nScenarioIndex       As      Integer         , _
                                       ByVal    nCPPIndex       As      Integer         , _
                                          ByRef pvResult As Double) As Integer


    Public Declare Function LSgetStocCCPInfo _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByVal nScenarioIndex As Integer, _
                                          ByVal nCPPIndex As Integer, _
                                          ByRef pvResult As Integer) As Integer


 Public Declare Function LSloadSampleSizes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panSampleSize       As      Integer ()      ) As Integer


 Public Declare Function LSloadConstraintStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panStage       As      Integer ()      ) As Integer


 Public Declare Function LSloadVariableStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panStage       As      Integer ()      ) As Integer


 Public Declare Function LSloadStageData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    numStages       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panRstage       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panCstage       As      Integer ()      ) As Integer


 Public Declare Function LSloadStocParData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panSparStage       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padSparValue       As       Double ()      ) As Integer


 Public Declare Function LSloadStocParNames _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nSvars       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paszSVarNames       As       String ()      ) As Integer


 Public Declare Function LSgetDeteqModel _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     iDeqType       As      Integer         , _
                                       ByRef  pnErrorCode       As      Integer         ) As Integer


 Public Declare Function LSaggregateStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panScheme       As      Integer ()      , _
                                       ByVal      nLength       As      Integer         ) As Integer


 Public Declare Function LSgetStageAggScheme _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panScheme       As      Integer ()      , _
                                       ByRef     pnLength       As      Integer         ) As Integer


 Public Declare Function LSdeduceStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nMaxStage       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panRowStagse       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panColStages       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panSparStage       As      Integer ()      ) As Integer

 ' optimization routines '


 Public Declare Function LSsolveSP _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef     pnStatus       As      Integer         ) As Integer


 Public Declare Function LSsolveHS _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal nSearchMethod       As      Integer         , _
                                       ByRef     pnStatus       As      Integer         ) As Integer

 ' solution access routines '


 Public Declare Function LSgetScenarioObjective _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByRef         pObj       As       Double         ) As Integer


 Public Declare Function LSgetNodePrimalSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSgetNodeDualSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padY       As       Double ()      ) As Integer


 Public Declare Function LSgetNodeReducedCost _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      ) As Integer


 Public Declare Function LSgetNodeSlacks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padY       As       Double ()      ) As Integer


 Public Declare Function LSgetScenarioPrimalSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      , _
                                       ByRef         pObj       As       Double         ) As Integer


 Public Declare Function LSgetScenarioReducedCost _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padD       As       Double ()      ) As Integer


 Public Declare Function LSgetScenarioDualSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padY       As       Double ()      ) As Integer


 Public Declare Function LSgetScenarioSlacks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padS       As       Double ()      ) As Integer


 Public Declare Function LSgetNodeListByScenario _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiNodes       As      Integer ()      , _
                                       ByRef      pnNodes       As      Integer         ) As Integer


 Public Declare Function LSgetProbabilityByScenario _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByRef       pdProb       As       Double         ) As Integer


 Public Declare Function LSgetProbabilityByNode _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iNode       As      Integer         , _
                                       ByRef       pdProb       As       Double         ) As Integer


 Public Declare Function LSgetStocParData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    paiStages       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      ) As Integer

 ' load stochastic data '


 Public Declare Function LSaddDiscreteBlocks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iStage       As      Integer         , _
                                       ByVal  nRealzBlock       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padProb       As       Double ()      , _
                                       ByRef     pakStart       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiRows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiStvs       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
                                       ByVal  nModifyRule       As      Integer         ) As Integer


 Public Declare Function LSaddScenario _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByVal  iParentScen       As      Integer         , _
                                       ByVal       iStage       As      Integer         , _
                                       ByVal        dProb       As       Double         , _
                                       ByVal       nElems       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiRows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiStvs       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
                                       ByVal  nModifyRule       As      Integer         ) As Integer


 Public Declare Function LSaddDiscreteIndep _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal         jCol       As      Integer         , _
                                       ByVal         iStv       As      Integer         , _
                                       ByVal nRealizations       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padProbs       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
                                       ByVal  nModifyRule       As      Integer         ) As Integer


 Public Declare Function LSaddParamDistIndep _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal         jCol       As      Integer         , _
                                       ByVal         iStv       As      Integer         , _
                                       ByVal    nDistType       As      Integer         , _
                                       ByVal      nParams       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padParams       As       Double ()      , _
                                       ByVal  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSaddUserDist _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal         jCol       As      Integer         , _
                                       ByVal         iStv       As      Integer         , _
                                       ByRef   pfUserFunc       As      Integer         , _
                                       ByVal     nSamples       As      Integer         , _
                                       ByRef    paSamples       As      Integer         , _
                                       ByVal   pvUserData       As       Object         , _
                                       ByVal  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSaddChanceConstraint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iSense       As      Integer         , _
                                       ByVal        nCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
                                       ByVal     dPrLevel       As       Double         , _
                                       ByVal   dObjWeight       As       Double         ) As Integer


 Public Declare Function LSsetNumStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    numStages       As      Integer         ) As Integer


 Public Declare Function LSgetStocParOutcomes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padProbs       As       Double ()      ) As Integer


 Public Declare Function LSloadCorrelationMatrix _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         nDim       As      Integer         , _
                                       ByVal    nCorrType       As      Integer         , _
                                       ByVal       nQCnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetCorrelationMatrix _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        iFlag       As      Integer         , _
                                       ByVal    nCorrType       As      Integer         , _
                                       ByRef      pnQCnnz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   paiQCcols2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSgetStocParSample _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iStv       As      Integer         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal         jCol       As      Integer         , _
                                       ByRef  pnErrorCode       As      Integer         ) As Integer


 Public Declare Function LSgetDiscreteBlocks _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iEvent       As      Integer         , _
                                       ByRef    nDistType       As      Integer         , _
                                       ByRef       iStage       As      Integer         , _
                                       ByRef  nRealzBlock       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padProbs       As       Double ()      , _
                                       ByRef  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSgetDiscreteBlockOutcomes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iEvent       As      Integer         , _
                                       ByVal       iRealz       As      Integer         , _
                                       ByRef       nRealz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiStvs       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      ) As Integer


 Public Declare Function LSgetDiscreteIndep _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iEvent       As      Integer         , _
                                       ByRef    nDistType       As      Integer         , _
                                       ByRef       iStage       As      Integer         , _
                                       ByRef         iRow       As      Integer         , _
                                       ByRef         jCol       As      Integer         , _
                                       ByRef         iStv       As      Integer         , _
                                       ByRef nRealizations       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padProbs       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
                                       ByRef  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSgetParamDistIndep _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       iEvent       As      Integer         , _
                                       ByRef    nDistType       As      Integer         , _
                                       ByRef       iStage       As      Integer         , _
                                       ByRef         iRow       As      Integer         , _
                                       ByRef         jCol       As      Integer         , _
                                       ByRef         iStv       As      Integer         , _
                                       ByRef      nParams       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padParams       As       Double ()      , _
                                       ByRef  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSgetScenario _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                       ByRef  iParentScen       As      Integer         , _
                                       ByRef iBranchStage       As      Integer         , _
                                       ByRef       pdProb       As       Double         , _
                                       ByRef       nRealz       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiArows       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiAcols       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiStvs       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
                                       ByRef  iModifyRule       As      Integer         ) As Integer


 Public Declare Function LSgetChanceConstraint _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      iChance       As      Integer         , _
                                       ByRef      piSense       As      Integer         , _
                                       ByRef       pnCons       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      paiCons       As      Integer ()      , _
                                       ByRef       pdProb       As       Double         , _
                                       ByRef  pdObjWeight       As       Double         ) As Integer


 Public Declare Function LSgetSampleSizes _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panSampleSize       As      Integer ()      ) As Integer


 Public Declare Function LSgetConstraintStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panStage       As      Integer ()      ) As Integer


 Public Declare Function LSgetVariableStages _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     panStage       As      Integer ()      ) As Integer


 Public Declare Function LSgetStocRowIndices _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     paiSrows       As      Integer ()      ) As Integer


 Public Declare Function LSsetStocParRG _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal         iStv       As      Integer         , _
                                       ByVal         iRow       As      Integer         , _
                                       ByVal         jCol       As      Integer         , _
                                       ByVal          pRG       As       IntPtr         ) As Integer


 Public Declare Function LSgetScenarioModel _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    jScenario       As      Integer         , _
                                          ByRef pnErrorcode As Integer) As IntPtr

 ' memory routines '


 Public Declare Function LSfreeStocMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer


 Public Declare Function LSfreeStocHashMemory _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer

 ' stochastic parameter routines '


 Public Declare Function LSgetModelStocParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef pvResult As Integer) As Integer


    Public Declare Function LSgetModelStocParameter _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByRef pvResult As Double) As Integer


 Public Declare Function LSsetModelStocParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef pvResult As Integer) As Integer

    Public Declare Function LSsetModelStocParameter _
    Lib "lindo12_0.dll" _
      (ByVal pModel As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByRef pvResult As Double) As Integer


 Public Declare Function LSsetEnvStocParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                          ByVal nQuery As Integer, _
                                          ByRef pvResult As Integer) As Integer

    Public Declare Function LSsetEnvStocParameter _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                          ByVal nQuery As Integer, _
                                          ByRef pvResult As Double) As Integer

 ' Public Functions '


 Public Declare Function LSsampCreate _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal    nDistType       As      Integer         , _
                                          ByRef nErrorCode As Integer) As IntPtr


 Public Declare Function LSsampDelete _
 Lib "lindo12_0.dll" _
   (                                   ByRef      pSample       As       IntPtr         ) As Integer


 Public Declare Function LSsampSetUserDistr _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                          ByVal pUserFunc As typUserPdf, _
                                          ByVal pUserData As IntPtr) As Integer

    Public Declare Function LSsampSetUserDistr _
    Lib "lindo12_0.dll" _
      (ByVal pSample As IntPtr, _
                                          ByVal pUserFunc As typUserPdf, _
                                          ByVal pUserData As Object) As Integer


 Public Declare Function LSsampSetDistrParam _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         , _
                                       ByVal       dValue       As       Double         ) As Integer


 Public Declare Function LSsampGetDistrParam _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal       nIndex       As      Integer         , _
                                       ByRef      pdValue       As       Double         ) As Integer


 Public Declare Function LSsampEvalDistr _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal    nFuncType       As      Integer         , _
                                       ByVal        dXval       As       Double         , _
                                       ByRef     pdResult       As       Double         ) As Integer


 Public Declare Function LSsampEvalDistrLI _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal    nFuncType       As      Integer         , _
                                       ByVal        dXval       As       Double         , _
                                       ByRef     pdResult       As       Double         ) As Integer


 Public Declare Function LSsampEvalUserDistr _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal    nFuncType       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padXval       As       Double ()      , _
                                       ByVal         nDim       As      Integer         , _
                                       ByRef     pdResult       As       Double         ) As Integer


 Public Declare Function LSsampSetRG _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                          ByVal pRG As IntPtr) As Integer


 Public Declare Function LSsampGenerate _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal      nMethod       As      Integer         , _
                                       ByVal        nSize       As      Integer         ) As Integer


 Public Declare Function LSsampGetPointsPtr _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByRef   pnSampSize       As      Integer         , _
                                          ByRef pdXval As IntPtr) As Integer


 Public Declare Function LSsampGetPoints _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByRef   pnSampSize       As      Integer         , _
       <MarshalAs(UnmanagedType.LPArray)> ByVal pdXval As Double()) As Integer


 Public Declare Function LSsampLoadPoints _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal    nSampSize       As      Integer         , _
       <MarshalAs(UnmanagedType.LPArray)> ByVal pdXval As Double()) As Integer


 Public Declare Function LSsampGetCIPointsPtr _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByRef   pnSampSize       As      Integer         , _
                                          ByRef pdXval As IntPtr) As Integer


 Public Declare Function LSsampGetCIPoints _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByRef   pnSampSize       As      Integer         , _
       <MarshalAs(UnmanagedType.LPArray)> ByVal pdXval As Double()) As Integer


 Public Declare Function LSsampInduceCorrelation _
 Lib "lindo12_0.dll" _
   (                                   ByRef     paSample       As      Integer         , _
                                       ByVal         nDim       As      Integer         , _
                                       ByVal    nCorrType       As      Integer         , _
                                       ByVal  nQCnonzeros       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSsampGetCorrelationMatrix _
 Lib "lindo12_0.dll" _
   (                                   ByRef     paSample       As      Integer         , _
                                       ByVal         nDim       As      Integer         , _
                                       ByVal        iFlag       As      Integer         , _
                                       ByVal    nCorrType       As      Integer         , _
                                       ByRef  nQCnonzeros       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx1       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal paiQCvarndx2       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padQCcoef       As       Double ()      ) As Integer


 Public Declare Function LSsampLoadDiscretePdfTable _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal         nLen       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padProb       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      ) As Integer


 Public Declare Function LSsampGetDiscretePdfTable _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByRef        pnLen       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padProb       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      ) As Integer


 Public Declare Function LSsampGetInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal       nQuery       As      Integer         , _
                                          ByRef pResult As Double) As Integer

    Public Declare Function LSsampGetInfo _
    Lib "lindo12_0.dll" _
      (ByVal pSample As IntPtr, _
                                          ByVal nQuery As Integer, _
                                          ByRef pResult As Integer) As Integer


 Public Declare Function LSsampAddUserFuncArg _
 Lib "lindo12_0.dll" _
   (                                   ByVal      pSample       As       IntPtr         , _
                                       ByVal pSampleSource       As       IntPtr         ) As Integer


 Public Declare Function LSregress _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nNdim       As      Integer         , _
                                       ByVal        nPdim       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padU       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padX       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padB       As       Double ()      , _
                                       ByRef         pdB0       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal         padR       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal     padstats       As       Double ()      ) As Integer

 ' Public functions '


 Public Declare Function LScreateRG _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal      nMethod       As      Integer         ) As Integer


 Public Declare Function LScreateRGMT _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal      nMethod       As      Integer         ) As Integer


 Public Declare Function LSgetDoubleRV _
 Lib "lindo12_0.dll" _
      (ByVal pRG As IntPtr) As Double


 Public Declare Function LSgetInt32RV _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByVal         iLow       As      Integer         , _
                                       ByVal        iHigh       As      Integer         ) As Integer


 Public Declare Function LSsetRGSeed _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByVal        nSeed       As      Integer         ) As Integer


 Public Declare Function LSdisposeRG _
 Lib "lindo12_0.dll" _
   (                                   ByRef         ppRG       As       IntPtr         ) As Integer


 Public Declare Function LSsetDistrParamRG _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByVal       iParam       As      Integer         , _
                                       ByVal       dParam       As       Double         ) As Integer


 Public Declare Function LSsetDistrRG _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByVal    nDistType       As      Integer         ) As Integer


 Public Declare Function LSgetDistrRV _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                          ByRef pResult As Double) As Integer

    Public Declare Function LSgetDistrRV _
    Lib "lindo12_0.dll" _
      (ByVal pRG As IntPtr, _
                                          ByRef pResult As Integer) As Integer


 Public Declare Function LSgetInitSeed _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         ) As Integer


 Public Declare Function LSgetRGNumThreads _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByRef    pnThreads       As      Integer         ) As Integer


 Public Declare Function LSfillRGBuffer _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         ) As Integer


 Public Declare Function LSgetRGBufferPtr _
 Lib "lindo12_0.dll" _
   (                                   ByVal          pRG       As       IntPtr         , _
                                       ByRef  pnBufferLen       As      Integer         ) As Integer


 ' Auxiliary functions '


 Public Declare Function LSgetHistogram _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal    nSampSize       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padVals       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padWeights       As       Double ()      , _
                                       ByVal     dHistLow       As       Double         , _
                                       ByVal    dHistHigh       As       Double         , _
                                       ByRef       pnBins       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal panBinCounts       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padBinProbs       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padBinLow       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   padBinHigh       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padBinLeftEdge       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padBinRightEdge       As       Double ()      ) As Integer


 Public Declare Function LSsampGetCorrDiff _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef     paSample       As      Integer         , _
                                       ByVal         nDim       As      Integer         , _
                                       ByVal    nDiffType       As      Integer         , _
                                       ByRef      pdNorm1       As       Double         , _
                                       ByRef      pdNorm2       As       Double         , _
                                       ByRef pdVecNormInf       As       Double         ) As Integer


 Public Declare Function LSgetNnzData _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal        mStat       As      Integer         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    panOutput       As      Integer ()      ) As Integer

 ' Public functions '


 Public Declare Function LSsolveFileLP _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal szFileNameMPS       As       String         , _
                                       ByVal szFileNameSol       As       String         , _
                                       ByVal nNoOfColsEvaluatedPerSet       As      Integer         , _
                                       ByVal nNoOfColsSelectedPerSet       As      Integer         , _
                                       ByVal nTimeLimitSec       As      Integer         , _
                                       ByRef pnSolStatusParam       As      Integer         , _
                                       ByRef pnNoOfConsMps       As      Integer         , _
                                       ByRef pnNoOfColsMps       As      Integer         , _
                                       ByRef  pnErrorLine       As      Integer         ) As Integer


 Public Declare Function LSreadSolutionFileLP _
 Lib "lindo12_0.dll" _
   (                                   ByVal szFileNameSol       As       String         , _
                                       ByVal  nFileFormat       As      Integer         , _
                                       ByVal nBeginIndexPrimalSol       As      Integer         , _
                                       ByVal nEndIndexPrimalSol       As      Integer         , _
                                       ByRef  pnSolStatus       As      Integer         , _
                                       ByRef   pdObjValue       As       Double         , _
                                       ByRef   pnNoOfCons       As      Integer         , _
                                       ByRef   plNoOfCols       As      Integer         , _
                                       ByRef pnNoOfColsEvaluated       As      Integer         , _
                                       ByRef pnNoOfIterations       As      Integer         , _
                                       ByRef pdTimeTakenInSeconds       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padPrimalValues       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal padDualValues       As       Double ()      ) As Integer

 ' Documented Public functions '


 Public Declare Function LSdateDiffSecs _
 Lib "lindo12_0.dll" _
   (                                   ByVal         nYr1       As      Integer         , _
                                       ByVal        nMon1       As      Integer         , _
                                       ByVal        nDay1       As      Integer         , _
                                       ByVal         nHr1       As      Integer         , _
                                       ByVal        nMin1       As      Integer         , _
                                       ByVal        dSec1       As       Double         , _
                                       ByVal         nYr2       As      Integer         , _
                                       ByVal        nMon2       As      Integer         , _
                                       ByVal        nDay2       As      Integer         , _
                                       ByVal         nHr2       As      Integer         , _
                                       ByVal        nMin2       As      Integer         , _
                                       ByVal        dSec2       As       Double         , _
                                       ByRef    pdSecdiff       As       Double         ) As Integer


 Public Declare Function LSdateYmdhms _
 Lib "lindo12_0.dll" _
   (                                   ByVal     dSecdiff       As       Double         , _
                                       ByVal         nYr1       As      Integer         , _
                                       ByVal        nMon1       As      Integer         , _
                                       ByVal        nDay1       As      Integer         , _
                                       ByVal         nHr1       As      Integer         , _
                                       ByVal        nMin1       As      Integer         , _
                                       ByVal        dSec1       As       Double         , _
                                       ByRef        pnYr2       As      Integer         , _
                                       ByRef       pnMon2       As      Integer         , _
                                       ByRef       pnDay2       As      Integer         , _
                                       ByRef        pnHr2       As      Integer         , _
                                       ByRef       pnMin2       As      Integer         , _
                                       ByRef       pdSec2       As       Double         , _
                                       ByRef        pnDow       As      Integer         ) As Integer


 Public Declare Function LSdateToday _
 Lib "lindo12_0.dll" _
   (                                   ByRef        pnYr1       As      Integer         , _
                                       ByRef       pnMon1       As      Integer         , _
                                       ByRef       pnDay1       As      Integer         , _
                                       ByRef        pnHr1       As      Integer         , _
                                       ByRef       pnMin1       As      Integer         , _
                                       ByRef       pdSec1       As       Double         , _
                                       ByRef        pnDow       As      Integer         ) As Integer

 ' Undocumented Public functions '


 Public Declare Function LSdateMakeDate _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nYYYY       As      Integer         , _
                                       ByVal          nMM       As      Integer         , _
                                       ByVal          nDD       As      Integer         ) As Integer


 Public Declare Function LSdateMakeTime _
 Lib "lindo12_0.dll" _
   (                                   ByVal          nHH       As      Integer         , _
                                       ByVal          nMM       As      Integer         , _
                                       ByVal          dSS       As       Double         ) As Integer


 Public Declare Function LSdateSetBaseDate _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nYYYY       As      Integer         , _
                                       ByVal          nMM       As      Integer         , _
                                       ByVal          nDD       As      Integer         ) As Integer


 Public Declare Function LSdateScalarSec _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateScalarSecInverse _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dSSEC       As       Double         , _
                                       ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         ) As Integer


 Public Declare Function LSdateScalarHour _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateScalarHourInverse _
 Lib "lindo12_0.dll" _
   (                                   ByVal       dSHOUR       As       Double         , _
                                       ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         ) As Integer


 Public Declare Function LSdateJulianSec _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateJulianSecInverse _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dJSEC       As       Double         , _
                                       ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         ) As Integer


 Public Declare Function LSdateJulianHour _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateJulianHourInverse _
 Lib "lindo12_0.dll" _
   (                                   ByVal       dJHOUR       As       Double         , _
                                       ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         ) As Integer


 Public Declare Function LSdateDiff _
 Lib "lindo12_0.dll" _
   (                                   ByVal       nDate1       As      Integer         , _
                                       ByVal       dTime1       As       Double         , _
                                       ByVal       nDate2       As      Integer         , _
                                       ByVal       dTime2       As       Double         , _
                                       ByRef       pnDays       As      Integer         , _
                                       ByRef       pdSecs       As       Double         ) As Integer


 Public Declare Function LSdateNow _
 Lib "lindo12_0.dll" _
   (                                   ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         ) As Integer


 Public Declare Function LSdateIsLeapYear _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nYear       As      Integer         ) As Integer


 Public Declare Function LSdateJulianDay _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateDayOfWeek _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateWeekOfYear _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateQuarterOfYear _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateDayOfYear _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateNextWeekDay _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdatePrevWeekDay _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateNextMonth _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateDateToDays _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateDaysToDate _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDays       As      Integer         ) As Integer


 Public Declare Function LSdateTimeToSecs _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateSecsToTime _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dSecs       As       Double         ) As Integer


 Public Declare Function LSdateFutureDate _
 Lib "lindo12_0.dll" _
   (                                   ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         , _
                                       ByVal        nDays       As      Integer         , _
                                       ByVal        dSecs       As      Integer         ) As Integer


 Public Declare Function LSdatePastDate _
 Lib "lindo12_0.dll" _
   (                                   ByRef       pnDate       As      Integer         , _
                                       ByRef       pdTime       As       Double         , _
                                       ByVal        nDays       As      Integer         , _
                                       ByVal        dSecs       As       Double         ) As Integer


 Public Declare Function LSdateIsValidDate _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateIsValidTime _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateIsDateFuture _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateIsDatePast _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         , _
                                       ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateYear _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateMonth _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateDay _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateHour _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateMinute _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateSecond _
 Lib "lindo12_0.dll" _
   (                                   ByVal        dTime       As       Double         ) As Integer


 Public Declare Function LSdateWeekOfMonth _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateLocalTimeStamp _
 Lib "lindo12_0.dll" _
   (                                   ByVal szTimeBuffer       As       String         ) As Integer


 Public Declare Function LSdateDateNum _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nDate       As      Integer         ) As Integer


 Public Declare Function LSdateMakeDateNum _
 Lib "lindo12_0.dll" _
   (                                   ByVal        nYYYY       As      Integer         , _
                                       ByVal          nMM       As      Integer         , _
                                       ByVal          nDD       As      Integer         ) As Integer

 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 ''
 ''    Tuner Functions
 ''
 ''    LINDO API Version 13.0
 ''    Copyright (c) 2019-2020
 ''
 ''    LINDO Systems, Inc.            312.988.7422
 ''    1415 North Dayton St.          info@lindo.com
 ''    Chicago, IL 60622              http:www.lindo.com
 ''
 ''    $Id: lindo.h 2906 2020-01-11 23:47:51Z mka $
 ''
 '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


 Public Declare Function LSrunTuner _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         ) As Integer


 Public Declare Function LSrunTunerFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   szJsonFile       As       String         ) As Integer


 Public Declare Function LSrunTunerString _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal szJsonString       As       String         ) As Integer


 Public Declare Function LSloadTunerConfigString _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal szJsonString       As       String         ) As Integer


 Public Declare Function LSloadTunerConfigFile _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal   szJsonFile       As       String         ) As Integer


 Public Declare Function LSclearTuner _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         ) As Integer


 Public Declare Function LSresetTuner _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         ) As Integer


 Public Declare Function LSprintTuner _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         ) As Integer


 Public Declare Function LSsetTunerOption _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal        szKey       As       String         , _
                                       ByVal         dval       As       Double         ) As Integer


 Public Declare Function LSgetTunerOption _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal        szkey       As       String         , _
                                       ByRef        pdval       As       Double         ) As Integer


 Public Declare Function LSgetTunerResult _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal        szkey       As       String         , _
                                       ByVal    jInstance       As      Integer         , _
                                       ByVal      kConfig       As      Integer         , _
                                       ByRef        pdval       As       Double         ) As Integer


 Public Declare Function LSgetTunerSpace _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panParamId       As      Integer ()      , _
                                       ByRef     numParam       As      Integer         ) As Integer


 Public Declare Function LSwriteTunerConfigString _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal szJsonString       As       String         , _
                                       ByVal   szJsonFile       As       String         ) As Integer


 Public Declare Function LSgetTunerConfigString _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal szJsonString       As       String         ) As Integer


 Public Declare Function LSwriteTunerParameters _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       szFile       As       String         , _
                                       ByVal    jInstance       As      Integer         , _
                                       ByVal   mCriterion       As      Integer         ) As Integer


 Public Declare Function LSaddTunerInstance _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       szFile       As       String         ) As Integer


 Public Declare Function LSaddTunerZStatic _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal     jGroupId       As      Integer         , _
                                       ByVal       iParam       As      Integer         , _
                                       ByVal       dValue       As       Double         ) As Integer


 Public Declare Function LSaddTunerZDynamic _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal       iParam       As      Integer         ) As Integer


 Public Declare Function LSaddTunerOption _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal        szKey       As       String         , _
                                       ByVal       dValue       As       Double         ) As Integer


 Public Declare Function LSdisplayTunerResults _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         ) As Integer

  ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetLicenseInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef    pnMaxcons       As      Integer         , _
                                       ByRef    pnMaxvars       As      Integer         , _
                                       ByRef pnMaxintvars       As      Integer         , _
                                       ByRef  pnReserved1       As      Integer         , _
                                       ByRef  pnDaystoexp       As      Integer         , _
                                       ByRef pnDaystotrialexp       As      Integer         , _
                                       ByRef pnNlpAllowed       As      Integer         , _
                                       ByRef      pnUsers       As      Integer         , _
                                       ByRef pnBarAllowed       As      Integer         , _
                                       ByRef    pnRuntime       As      Integer         , _
                                       ByRef pnEdulicense       As      Integer         , _
                                       ByVal     pachText       As StringBuilder         ) As Integer

  ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetDimensions _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef       pnVars       As      Integer         , _
                                       ByRef       pnCons       As      Integer         , _
                                       ByRef      pnCones       As      Integer         , _
                                       ByRef       pnAnnz       As      Integer         , _
                                       ByRef      pnQCnnz       As      Integer         , _
                                       ByRef    pnConennz       As      Integer         , _
                                       ByRef     pnNLPnnz       As      Integer         , _
                                       ByRef  pnNLPobjnnz       As      Integer         , _
                                       ByRef pnVarNamelen       As      Integer         , _
                                       ByRef pnConNamelen       As      Integer         , _
                                       ByRef pnConeNamelen       As      Integer         ) As Integer

  ' Deprecated, use LSsolveMIP() '


 Public Declare Function LSbnbSolve _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal     pszFname       As       String         ) As Integer

  ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetDualMIPsolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal      padDual       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal  padRedcosts       As       Double ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panCstatus       As      Integer ()      , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal   panRstatus       As      Integer ()      ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetMIPSolutionStatus _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef     pnStatus       As      Integer         ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetSolutionStatus _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef      nStatus       As      Integer         ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetObjective _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef     pdObjval       As       Double         ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetSolutionInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef     pnMethod       As      Integer         , _
                                       ByRef    pnElapsed       As      Integer         , _
                                       ByRef    pnSpxiter       As      Integer         , _
                                       ByRef    pnBariter       As      Integer         , _
                                       ByRef    pnNlpiter       As      Integer         , _
                                       ByRef pnPrimStatus       As      Integer         , _
                                       ByRef pnDualStatus       As      Integer         , _
                                       ByRef  pnBasStatus       As      Integer         , _
                                       ByRef    pdPobjval       As       Double         , _
                                       ByRef    pdDobjval       As       Double         , _
                                       ByRef    pdPinfeas       As       Double         , _
                                       ByRef    pdDinfeas       As       Double         ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetMIPSolution _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef    pdPobjval       As       Double         , _
    <MarshalAs(UnmanagedType.LPArray)> ByVal    padPrimal       As       Double ()      ) As Integer

 ' Deprecated,  use LSgetInfo() '


 Public Declare Function LSgetCurrentMIPSolutionInfo _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByRef  pnMIPstatus       As      Integer         , _
                                       ByRef  pdMIPobjval       As       Double         , _
                                       ByRef  pdBestbound       As       Double         , _
                                       ByRef    pdSpxiter       As       Double         , _
                                       ByRef    pdBariter       As       Double         , _
                                       ByRef    pdNlpiter       As       Double         , _
                                       ByRef      pnLPcnt       As      Integer         , _
                                       ByRef  pnBranchcnt       As      Integer         , _
                                       ByRef  pnActivecnt       As      Integer         , _
                                       ByRef  pnCons_prep       As      Integer         , _
                                       ByRef  pnVars_prep       As      Integer         , _
                                       ByRef  pnAnnz_prep       As      Integer         , _
                                       ByRef   pnInt_prep       As      Integer         , _
                                       ByRef pnCut_contra       As      Integer         , _
                                       ByRef    pnCut_obj       As      Integer         , _
                                       ByRef    pnCut_gub       As      Integer         , _
                                       ByRef   pnCut_lift       As      Integer         , _
                                       ByRef   pnCut_flow       As      Integer         , _
                                       ByRef pnCut_gomory       As      Integer         , _
                                       ByRef    pnCut_gcd       As      Integer         , _
                                       ByRef pnCut_clique       As      Integer         , _
                                       ByRef pnCut_disagg       As      Integer         , _
                                       ByRef pnCut_planloc       As      Integer         , _
                                       ByRef pnCut_latice       As      Integer         , _
                                       ByRef   pnCut_coef       As      Integer         ) As Integer

 ' Command Line Parser '


 Public Declare Function LSgetCLOpt _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByVal        nArgc       As      Integer         , _
                                          ByVal pszArgv As String, _
                                       ByVal       pszOpt       As       String         ) As Integer


 Public Declare Function LSgetCLOptArg _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                          ByVal pszOptArg As String) As Integer


 Public Declare Function LSgetCLOptInd _
 Lib "lindo12_0.dll" _
   (                                   ByVal         pEnv       As       IntPtr         , _
                                       ByRef     pnOptInd       As      Integer         ) As Integer


 Public Declare Function LSsolveExternally _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         , _
                                       ByVal      mSolver       As      Integer         , _
                                       ByVal      nMethod       As      Integer         , _
                                       ByVal  nFileFormat       As      Integer         , _
                                       ByRef     pnStatus       As      Integer         ) As Integer


 Public Declare Function LSgetMasterModel _
 Lib "lindo12_0.dll" _
   (                                   ByVal       pModel       As       IntPtr         ) As Integer
End Class
