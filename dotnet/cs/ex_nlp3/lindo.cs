/*********************************************************************
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2000-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: lindo.cs 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/

using System;
using System.Text;
using System.Runtime.InteropServices;

public class lindo
{

 /*     Define LSWIN64 to enable x64 compatibility 
  */
 
#if LSWIN64
  public const string   LINDO_DLL                         = "lindo64_13_0.dll";
#else  
  public const string   LINDO_DLL                         = "lindo13_0.dll";
#endif  

 /*********************************************************************
 *                        Constant Definitions                       *
 *********************************************************************/
       public const int    LS_MAJOR_VER_NUMBER            =         13;
       public const int    LS_MINOR_VER_NUMBER            =          0;
       public const int    LS_REV_VER_NUMBER              =        108;
       public const int    LS_VER_NUMBER                  =       1300;
       public const int    LS_BUILD_VER_NUMBER            =       4099;
       public const int    LS_MIN                         =         +1;
       public const int    LS_MAX                         =         -1;
       public const int    LS_CONTYPE_GE                  =        'G';
       public const int    LS_CONTYPE_LE                  =        'L';
       public const int    LS_CONTYPE_EQ                  =        'E';
       public const int    LS_CONTYPE_FR                  =        'N';
       public const int    LS_CONETYPE_QUAD               =        'Q';
       public const int    LS_CONETYPE_RQUAD              =        'R';
       public const int    LS_CONETYPE_PEXP               =        'E';
       public const int    LS_CONETYPE_PPOW               =        'P';
       public const int    LS_VARTYPE_CONT                =        'C';
       public const int    LS_VARTYPE_BIN                 =        'B';
       public const int    LS_VARTYPE_INT                 =        'I';
       public const int    LS_VARTYPE_SC                  =        'S';
       public const double LS_INFINITY                    =    1.0E+30;
       public const int    LS_BASTYPE_BAS                 =          0;
       public const int    LS_BASTYPE_ATLO                =         -1;
       public const int    LS_BASTYPE_ATUP                =         -2;
       public const int    LS_BASTYPE_FNUL                =         -3;
       public const int    LS_BASTYPE_SBAS                =         -4;
       public const int    LS_UNFORMATTED_MPS             =          0;
       public const int    LS_FORMATTED_MPS               =          1;
       public const int    LS_UNFORMATTED_MPS_COMP        =          2;
       public const int    LS_FORMATTED_MPS_COMP          =          3;
       public const int    LS_SOLUTION_OPT                =          0;
       public const int    LS_SOLUTION_MIP                =          1;
       public const int    LS_SOLUTION_OPT_IPM            =          2;
       public const int    LS_SOLUTION_OPT_OLD            =          3;
       public const int    LS_SOLUTION_MIP_OLD            =          4;
       public const int    LS_BASFILE_BIN                 =          1;
       public const int    LS_BASFILE_MPS                 =          2;
       public const int    LS_BASFILE_TXT                 =          3;
       public const int    LS_BASFILE_MTX                 =          4;
       public const int    LS_INT_TYPE                    =          4;
       public const int    LS_DOUBLE_TYPE                 =          8;
       public const int    LS_MAX_ERROR_MESSAGE_LENGTH    =       1024;
       public const int    LS_DEFAULT                     =         -1;
       public const int    LS_MAX_JOBJECTS                =        100;
       public const int    LS_PROPERTY_UNKNOWN            =          0;
       public const int    LS_PROPERTY_CONST              =          1;
       public const int    LS_PROPERTY_LINEAR             =          2;
       public const int    LS_PROPERTY_CONVEX             =          3;
       public const int    LS_PROPERTY_CONCAVE            =          4;
       public const int    LS_PROPERTY_QUASI_CONVEX       =          5;
       public const int    LS_PROPERTY_QUASI_CONCAVE      =          6;
       public const int    LS_PROPERTY_MAX                =          7;
       public const int    LS_PROPERTY_MONO_INCREASE      =          8;
       public const int    LS_PROPERTY_MONO_DECREASE      =          9;

 /* bitmasks for LScopyModel */
       public const int    LS_RAW_COPY                    =          0;
       public const int    LS_DEEP_COPY                   =          1;
       public const int    LS_TIME_COPY                   =          2;
       public const int    LS_STOC_COPY                   =          4;
       public const int    LS_SNGSTG_COPY                 =          8;

 /* Time frames in seconds */
       public const int    LSSEC01                        =          1;
       public const int    LSSEC02                        =          2;
       public const int    LSSEC03                        =          3;
       public const int    LSSEC04                        =          4;
       public const int    LSSEC05                        =          5;
       public const int    LSSEC06                        =          6;
       public const int    LSSEC10                        =         10;
       public const int    LSSEC15                        =         15;
       public const int    LSSEC20                        =         20;
       public const int    LSSEC30                        =         30;
       public const int    LSMIN01                        =         60;
       public const int    LSMIN02                        =        120;
       public const int    LSMIN03                        =        180;
       public const int    LSMIN05                        =        300;
       public const int    LSMIN06                        =        600;
       public const int    LSMIN10                        =        600;
       public const int    LSMIN15                        =        900;
       public const int    LSMIN20                        =       1200;
       public const int    LSMIN30                        =       1800;
       public const int    LSHOUR01                       =       3600;
       public const int    LSHOUR02                       =       7200;
       public const int    LSHOUR03                       =      10800;
       public const int    LSHOUR05                       =      18000;
       public const int    LSHOUR06                       =      21600;
       public const int    LSHOUR08                       =      28800;
       public const int    LSHOUR12                       =      43200;
       public const int    LSDAY                          =      86400;
       public const int    LSWEEK                         =     604800;
       public const int    LSMONTH                        =    2592000;
       public const int    LSQUARTER                      =    7776000;
       public const int    LSYEAR                         =   31104000;

 /* Days of week */
       public const int    LSSUNDAY                       =          0;
       public const int    LSMONDAY                       =          1;
       public const int    LSTUESDAY                      =          2;
       public const int    LSWEDNESDAY                    =          3;
       public const int    LSTHURSDAY                     =          4;
       public const int    LSFRIDAY                       =          5;
       public const int    LSSATURDAY                     =          6;

 /* bitmask for components */
       public const int    LS_DATA_CORE                   =          1;
       public const int    LS_DATA_TIME                   =          2;
       public const int    LS_DATA_STOC                   =          4;
       public const int    LS_DATA_FILE                   =          8;

 /* external solvers */
       public const int    LS_XSOLVER_MSKLP               =          1;
       public const int    LS_XSOLVER_GRBLP               =          2;
       public const int    LS_XSOLVER_GRBCL               =          3;
       public const int    LS_XSOLVER_GRBMIP              =          4;
       public const int    LS_XSOLVER_CPXLP               =          5;
       public const int    LS_XSOLVER_CPXMIP              =          6;
       public const int    LS_XSOLVER_OSI                 =          7;
       public const int    LS_XSOLVER_CLP                 =          8;
       public const int    LS_XSOLVER_MSK                 =          9;
       public const int    LS_XSOLVER_COI                 =         10;
       public const int    LS_XSOLVER_SOP                 =         11;

 /* Solution or model status (1-20) */
       public const int    LS_STATUS_OPTIMAL              =          1;
       public const int    LS_STATUS_BASIC_OPTIMAL        =          2;
       public const int    LS_STATUS_INFEASIBLE           =          3;
       public const int    LS_STATUS_UNBOUNDED            =          4;
       public const int    LS_STATUS_FEASIBLE             =          5;
       public const int    LS_STATUS_INFORUNB             =          6;
       public const int    LS_STATUS_NEAR_OPTIMAL         =          7;
       public const int    LS_STATUS_LOCAL_OPTIMAL        =          8;
       public const int    LS_STATUS_LOCAL_INFEASIBLE     =          9;
       public const int    LS_STATUS_CUTOFF               =         10;
       public const int    LS_STATUS_NUMERICAL_ERROR      =         11;
       public const int    LS_STATUS_UNKNOWN              =         12;
       public const int    LS_STATUS_UNLOADED             =         13;
       public const int    LS_STATUS_LOADED               =         14;
       public const int    LS_STATUS_BOUNDED              =         15;

    /* General parameters (1021 - 1099) */
       public const int    LS_IPARAM_OBJSENSE             =       1022;
       public const int    LS_DPARAM_CALLBACKFREQ         =       1023;
       public const int    LS_DPARAM_OBJPRINTMUL          =       1024;
       public const int    LS_IPARAM_CHECK_FOR_ERRORS     =       1025;
       public const int    LS_IPARAM_ALLOW_CNTRLBREAK     =       1026;
       public const int    LS_IPARAM_DECOMPOSITION_TYPE   =       1027;
       public const int    LS_IPARAM_LP_SCALE             =       1029;
       public const int    LS_IPARAM_LP_ITRLMT            =       1030;
       public const int    LS_IPARAM_SPLEX_PPRICING       =       1031;
       public const int    LS_IPARAM_SPLEX_REFACFRQ       =       1032;
       public const int    LS_IPARAM_BARRIER_SOLVER       =       1033;
       public const int    LS_IPARAM_PROB_TO_SOLVE        =       1034;
       public const int    LS_IPARAM_LP_PRINTLEVEL        =       1035;
       public const int    LS_IPARAM_MPS_OBJ_WRITESTYLE   =       1036;
       public const int    LS_IPARAM_SPLEX_DPRICING       =       1037;
       public const int    LS_IPARAM_SOL_REPORT_STYLE     =       1038;
       public const int    LS_IPARAM_INSTRUCT_LOADTYPE    =       1039;
       public const int    LS_IPARAM_SPLEX_DUAL_PHASE     =       1040;
       public const int    LS_IPARAM_LP_PRELEVEL          =       1041;
       public const int    LS_IPARAM_STRING_LENLMT        =       1042;
       public const int    LS_IPARAM_USE_NAMEDATA         =       1043;
       public const int    LS_IPARAM_SPLEX_USE_EXTERNAL   =       1044;
       public const int    LS_DPARAM_LP_ITRLMT            =       1045;
       public const int    LS_IPARAM_COPY_MODE            =       1046;
       public const int    LS_IPARAM_SBD_NUM_THREADS      =       1047;
       public const int    LS_IPARAM_NUM_THREADS          =       1048;
       public const int    LS_IPARAM_MULTITHREAD_MODE     =       1049;
       public const int    LS_IPARAM_FIND_BLOCK           =       1050;
       public const int    LS_IPARAM_PROFILER_LEVEL       =       1051;
       public const int    LS_IPARAM_INSTRUCT_READMODE    =       1052;
       public const int    LS_IPARAM_INSTRUCT_SUBOUT      =       1053;
       public const int    LS_IPARAM_SOLPOOL_LIM          =       1054;
       public const int    LS_IPARAM_FIND_SYMMETRY_LEVEL  =       1055;
       public const int    LS_IPARAM_FIND_SYMMETRY_PRINT_LEVEL =       1056;
       public const int    LS_IPARAM_TUNER_PRINT_LEVEL    =       1057;

    /* Generic solver parameters (1251 - 1500) */
       public const int    LS_IPARAM_SOLVER_IUSOL         =       1251;
       public const int    LS_IPARAM_SOLVER_TIMLMT        =       1252;
       public const int    LS_DPARAM_SOLVER_CUTOFFVAL     =       1253;
       public const int    LS_DPARAM_SOLVER_FEASTOL       =       1254;
       public const int    LS_IPARAM_SOLVER_RESTART       =       1255;
       public const int    LS_IPARAM_SOLVER_IPMSOL        =       1256;
       public const int    LS_DPARAM_SOLVER_OPTTOL        =       1257;
       public const int    LS_IPARAM_SOLVER_USECUTOFFVAL  =       1258;
       public const int    LS_IPARAM_SOLVER_PRE_ELIM_FILL =       1259;
       public const int    LS_DPARAM_SOLVER_TIMLMT        =       1260;
       public const int    LS_IPARAM_SOLVER_CONCURRENT_OPTMODE =       1261;
       public const int    LS_DPARAM_SOLVER_PERT_FEASTOL  =       1262;
       public const int    LS_IPARAM_SOLVER_PARTIALSOL_LEVEL =       1263;
       public const int    LS_IPARAM_SOLVER_MODE          =       1264;

    /* Advanced parameters for the simplex method (4000 - 41++) */
       public const int    LS_DPARAM_LP_MIN_FEASTOL       =       4060;
       public const int    LS_DPARAM_LP_MAX_FEASTOL       =       4061;
       public const int    LS_DPARAM_LP_MIN_OPTTOL        =       4062;
       public const int    LS_DPARAM_LP_MAX_OPTTOL        =       4063;
       public const int    LS_DPARAM_LP_MIN_PIVTOL        =       4064;
       public const int    LS_DPARAM_LP_MAX_PIVTOL        =       4065;
       public const int    LS_DPARAM_LP_AIJ_ZEROTOL       =       4066;
       public const int    LS_DPARAM_LP_PIV_ZEROTOL       =       4067;
       public const int    LS_DPARAM_LP_PIV_BIGTOL        =       4068;
       public const int    LS_DPARAM_LP_BIGM              =       4069;
       public const int    LS_DPARAM_LP_BNDINF            =       4070;
       public const int    LS_DPARAM_LP_INFINITY          =       4071;
       public const int    LS_IPARAM_LP_PPARTIAL          =       4072;
       public const int    LS_IPARAM_LP_DPARTIAL          =       4073;
       public const int    LS_IPARAM_LP_DRATIO            =       4074;
       public const int    LS_IPARAM_LP_PRATIO            =       4075;
       public const int    LS_IPARAM_LP_RATRANGE          =       4076;
       public const int    LS_IPARAM_LP_DPSWITCH          =       4077;
       public const int    LS_IPARAM_LP_PALLOC            =       4078;
       public const int    LS_IPARAM_LP_PRTFG             =       4079;
       public const int    LS_IPARAM_LP_OPRFREE           =       4080;
       public const int    LS_IPARAM_LP_SPRINT_SUB        =       4081;
       public const int    LS_IPARAM_LP_PERTMODE          =       4082;
       public const int    LS_IPARAM_LP_PCOLAL_FACTOR     =       4083;
       public const int    LS_IPARAM_LP_MAXMERGE          =       4084;
       public const int    LS_DPARAM_LP_PERTFACT          =       4085;
       public const int    LS_DPARAM_LP_DYNOBJFACT        =       4086;
       public const int    LS_IPARAM_LP_DYNOBJMODE        =       4087;
       public const int    LS_DPARAM_LP_MERGEFACT         =       4088;
       public const int    LS_IPARAM_LP_UMODE             =       4089;
       public const int    LS_IPARAM_LP_SPRINT_MAXPASS    =       4090;
       public const int    LS_IPARAM_LP_SPRINT_COLFACT    =       4091;

    /* Advanced parameters for LU decomposition (4800 - 4+++) */
       public const int    LS_IPARAM_LU_NUM_CANDITS       =       4800;
       public const int    LS_IPARAM_LU_MAX_UPDATES       =       4801;
       public const int    LS_IPARAM_LU_PRINT_LEVEL       =       4802;
       public const int    LS_IPARAM_LU_UPDATE_TYPE       =       4803;
       public const int    LS_IPARAM_LU_MODE              =       4804;
       public const int    LS_IPARAM_LU_PIVMOD            =       4806;
       public const int    LS_DPARAM_LU_EPS_DIAG          =       4900;
       public const int    LS_DPARAM_LU_EPS_NONZ          =       4901;
       public const int    LS_DPARAM_LU_EPS_PIVABS        =       4902;
       public const int    LS_DPARAM_LU_EPS_PIVREL        =       4903;
       public const int    LS_DPARAM_LU_INI_RCOND         =       4904;
       public const int    LS_DPARAM_LU_SPVTOL_UPDATE     =       4905;
       public const int    LS_DPARAM_LU_SPVTOL_FTRAN      =       4906;
       public const int    LS_DPARAM_LU_SPVTOL_BTRAN      =       4907;
       public const int    LS_IPARAM_LU_CORE              =       4908;

    /* Parameters for the IPM method (3000 - 3+++) */
       public const int    LS_DPARAM_IPM_TOL_INFEAS       =       3150;
       public const int    LS_DPARAM_IPM_TOL_PATH         =       3151;
       public const int    LS_DPARAM_IPM_TOL_PFEAS        =       3152;
       public const int    LS_DPARAM_IPM_TOL_REL_STEP     =       3153;
       public const int    LS_DPARAM_IPM_TOL_PSAFE        =       3154;
       public const int    LS_DPARAM_IPM_TOL_DFEAS        =       3155;
       public const int    LS_DPARAM_IPM_TOL_DSAFE        =       3156;
       public const int    LS_DPARAM_IPM_TOL_MU_RED       =       3157;
       public const int    LS_DPARAM_IPM_BASIS_REL_TOL_S  =       3158;
       public const int    LS_DPARAM_IPM_BASIS_TOL_S      =       3159;
       public const int    LS_DPARAM_IPM_BASIS_TOL_X      =       3160;
       public const int    LS_DPARAM_IPM_BI_LU_TOL_REL_PIV =       3161;
       public const int    LS_DPARAM_IPM_CO_TOL_INFEAS    =       3162;
       public const int    LS_DPARAM_IPM_CO_TOL_PFEAS     =       3163;
       public const int    LS_DPARAM_IPM_CO_TOL_DFEAS     =       3164;
       public const int    LS_DPARAM_IPM_CO_TOL_MU_RED    =       3165;
       public const int    LS_IPARAM_IPM_MAX_ITERATIONS   =       3166;
       public const int    LS_IPARAM_IPM_OFF_COL_TRH      =       3167;
       public const int    LS_IPARAM_IPM_NUM_THREADS      =       3168;
       public const int    LS_IPARAM_IPM_CHECK_CONVEXITY  =       3169;

    /* Nonlinear programming (NLP) parameters (2500 - 25++) */
       public const int    LS_IPARAM_NLP_SOLVE_AS_LP      =       2500;
       public const int    LS_IPARAM_NLP_SOLVER           =       2501;
       public const int    LS_IPARAM_NLP_SUBSOLVER        =       2502;
       public const int    LS_IPARAM_NLP_PRINTLEVEL       =       2503;
       public const int    LS_DPARAM_NLP_PSTEP_FINITEDIFF =       2504;
       public const int    LS_IPARAM_NLP_DERIV_DIFFTYPE   =       2505;
       public const int    LS_DPARAM_NLP_FEASTOL          =       2506;
       public const int    LS_DPARAM_NLP_REDGTOL          =       2507;
       public const int    LS_IPARAM_NLP_USE_CRASH        =       2508;
       public const int    LS_IPARAM_NLP_USE_STEEPEDGE    =       2509;
       public const int    LS_IPARAM_NLP_USE_SLP          =       2510;
       public const int    LS_IPARAM_NLP_USE_SELCONEVAL   =       2511;
       public const int    LS_IPARAM_NLP_PRELEVEL         =       2512;
       public const int    LS_IPARAM_NLP_ITRLMT           =       2513;
       public const int    LS_IPARAM_NLP_LINEARZ          =       2514;
       public const int    LS_IPARAM_NLP_LINEARITY        =       2515;
       public const int    LS_IPARAM_NLP_STARTPOINT       =       2516;
       public const int    LS_IPARAM_NLP_CONVEXRELAX      =       2517;
       public const int    LS_IPARAM_NLP_CR_ALG_REFORM    =       2518;
       public const int    LS_IPARAM_NLP_QUADCHK          =       2519;
       public const int    LS_IPARAM_NLP_AUTODERIV        =       2520;
       public const int    LS_IPARAM_NLP_MAXLOCALSEARCH   =       2521;
       public const int    LS_IPARAM_NLP_CONVEX           =       2522;
       public const int    LS_IPARAM_NLP_CONOPT_VER       =       2523;
       public const int    LS_IPARAM_NLP_USE_LINDO_CRASH  =       2524;
       public const int    LS_IPARAM_NLP_STALL_ITRLMT     =       2525;
       public const int    LS_IPARAM_NLP_AUTOHESS         =       2526;
       public const int    LS_IPARAM_NLP_FEASCHK          =       2527;
       public const int    LS_DPARAM_NLP_ITRLMT           =       2528;
       public const int    LS_IPARAM_NLP_MAXSUP           =       2529;
       public const int    LS_IPARAM_NLP_MSW_SOLIDX       =       2530;
       public const int    LS_IPARAM_NLP_ITERS_PER_LOGLINE =       2531;
       public const int    LS_IPARAM_NLP_MAX_RETRY        =       2532;
       public const int    LS_IPARAM_NLP_MSW_NORM         =       2533;
       public const int    LS_IPARAM_NLP_MSW_POPSIZE      =       2534;
       public const int    LS_IPARAM_NLP_MSW_MAXPOP       =       2535;
       public const int    LS_IPARAM_NLP_MSW_MAXNOIMP     =       2536;
       public const int    LS_IPARAM_NLP_MSW_FILTMODE     =       2537;
       public const int    LS_DPARAM_NLP_MSW_POXDIST_THRES =       2538;
       public const int    LS_DPARAM_NLP_MSW_EUCDIST_THRES =       2539;
       public const int    LS_DPARAM_NLP_MSW_XNULRAD_FACTOR =       2540;
       public const int    LS_DPARAM_NLP_MSW_XKKTRAD_FACTOR =       2541;
       public const int    LS_IPARAM_NLP_MAXLOCALSEARCH_TREE =       2542;
       public const int    LS_IPARAM_NLP_MSW_NUM_THREADS  =       2543;
       public const int    LS_IPARAM_NLP_MSW_RG_SEED      =       2544;
       public const int    LS_IPARAM_NLP_MSW_PREPMODE     =       2545;
       public const int    LS_IPARAM_NLP_MSW_RMAPMODE     =       2546;
       public const int    LS_IPARAM_NLP_XSMODE           =       2547;
       public const int    LS_DPARAM_NLP_MSW_OVERLAP_RATIO =       2548;
       public const int    LS_DPARAM_NLP_INF              =       2549;
       public const int    LS_IPARAM_NLP_IPM2GRG          =       2550;
       public const int    LS_IPARAM_NLP_USE_SDP          =       2551;
       public const int    LS_IPARAM_NLP_LINEARZ_WB_CONSISTENT =       2552;
       public const int    LS_DPARAM_NLP_CUTOFFOBJ        =       2553;
       public const int    LS_IPARAM_NLP_USECUTOFFOBJ     =       2554;
       public const int    	LS_IPARAM_NLP_CONIC_REFORM    =       2555;

    /* Mixed integer programming (MIP) parameters (5000 - 5+++) */
       public const int    LS_IPARAM_MIP_TIMLIM           =       5300;
       public const int    LS_IPARAM_MIP_AOPTTIMLIM       =       5301;
       public const int    LS_IPARAM_MIP_LSOLTIMLIM       =       5302;
       public const int    LS_IPARAM_MIP_PRELEVEL         =       5303;
       public const int    LS_IPARAM_MIP_NODESELRULE      =       5304;
       public const int    LS_DPARAM_MIP_INTTOL           =       5305;
       public const int    LS_DPARAM_MIP_RELINTTOL        =       5306;
       public const int    LS_DPARAM_MIP_RELOPTTOL        =       5307;
       public const int    LS_DPARAM_MIP_PEROPTTOL        =       5308;
       public const int    LS_IPARAM_MIP_MAXCUTPASS_TOP   =       5309;
       public const int    LS_IPARAM_MIP_MAXCUTPASS_TREE  =       5310;
       public const int    LS_DPARAM_MIP_ADDCUTPER        =       5311;
       public const int    LS_DPARAM_MIP_ADDCUTPER_TREE   =       5312;
       public const int    LS_IPARAM_MIP_MAXNONIMP_CUTPASS =       5313;
       public const int    LS_IPARAM_MIP_CUTLEVEL_TOP     =       5314;
       public const int    LS_IPARAM_MIP_CUTLEVEL_TREE    =       5315;
       public const int    LS_IPARAM_MIP_CUTTIMLIM        =       5316;
       public const int    LS_IPARAM_MIP_CUTDEPTH         =       5317;
       public const int    LS_IPARAM_MIP_CUTFREQ          =       5318;
       public const int    LS_IPARAM_MIP_HEULEVEL         =       5319;
       public const int    LS_IPARAM_MIP_PRINTLEVEL       =       5320;
       public const int    LS_IPARAM_MIP_PREPRINTLEVEL    =       5321;
       public const int    LS_DPARAM_MIP_CUTOFFOBJ        =       5322;
       public const int    LS_IPARAM_MIP_USECUTOFFOBJ     =       5323;
       public const int    LS_IPARAM_MIP_STRONGBRANCHLEVEL =       5324;
       public const int    LS_IPARAM_MIP_TREEREORDERLEVEL =       5325;
       public const int    LS_IPARAM_MIP_BRANCHDIR        =       5326;
       public const int    LS_IPARAM_MIP_TOPOPT           =       5327;
       public const int    LS_IPARAM_MIP_REOPT            =       5328;
       public const int    LS_IPARAM_MIP_SOLVERTYPE       =       5329;
       public const int    LS_IPARAM_MIP_KEEPINMEM        =       5330;
       public const int    LS_IPARAM_MIP_BRANCHRULE       =       5331;
       public const int    LS_DPARAM_MIP_REDCOSTFIX_CUTOFF =       5332;
       public const int    LS_DPARAM_MIP_ADDCUTOBJTOL     =       5333;
       public const int    LS_IPARAM_MIP_HEUMINTIMLIM     =       5334;
       public const int    LS_IPARAM_MIP_BRANCH_PRIO      =       5335;
       public const int    LS_IPARAM_MIP_SCALING_BOUND    =       5336;
       public const int    LS_DPARAM_MIP_PSEUDOCOST_WEIGT =       5337;
       public const int    LS_DPARAM_MIP_LBIGM            =       5338;
       public const int    LS_DPARAM_MIP_DELTA            =       5339;
       public const int    LS_IPARAM_MIP_DUAL_SOLUTION    =       5340;
       public const int    LS_IPARAM_MIP_BRANCH_LIMIT     =       5341;
       public const int    LS_DPARAM_MIP_ITRLIM           =       5342;
       public const int    LS_IPARAM_MIP_AGGCUTLIM_TOP    =       5343;
       public const int    LS_IPARAM_MIP_AGGCUTLIM_TREE   =       5344;
       public const int    LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_ITER =       5345;
       public const int    LS_IPARAM_MIP_ANODES_SWITCH_DF =       5346;
       public const int    LS_DPARAM_MIP_ABSOPTTOL        =       5347;
       public const int    LS_DPARAM_MIP_MINABSOBJSTEP    =       5348;
       public const int    LS_IPARAM_MIP_PSEUDOCOST_RULE  =       5349;
       public const int    LS_IPARAM_MIP_ENUM_HEUMODE     =       5350;
       public const int    LS_IPARAM_MIP_PRELEVEL_TREE    =       5351;
       public const int    LS_DPARAM_MIP_REDCOSTFIX_CUTOFF_TREE =       5352;
       public const int    LS_IPARAM_MIP_USE_INT_ZERO_TOL =       5353;
       public const int    LS_IPARAM_MIP_USE_CUTS_HEU     =       5354;
       public const int    LS_DPARAM_MIP_BIGM_FOR_INTTOL  =       5355;
       public const int    LS_IPARAM_MIP_STRONGBRANCHDONUM =       5366;
       public const int    LS_IPARAM_MIP_MAKECUT_INACTIVE_COUNT =       5367;
       public const int    LS_IPARAM_MIP_PRE_ELIM_FILL    =       5368;
       public const int    LS_IPARAM_MIP_HEU_MODE         =       5369;
       public const int    LS_DPARAM_MIP_TIMLIM           =       5370;
       public const int    LS_DPARAM_MIP_AOPTTIMLIM       =       5371;
       public const int    LS_DPARAM_MIP_LSOLTIMLIM       =       5372;
       public const int    LS_DPARAM_MIP_CUTTIMLIM        =       5373;
       public const int    LS_DPARAM_MIP_HEUMINTIMLIM     =       5374;
       public const int    LS_IPARAM_MIP_FP_MODE          =       5375;
       public const int    LS_DPARAM_MIP_FP_WEIGHT        =       5376;
       public const int    LS_IPARAM_MIP_FP_OPT_METHOD    =       5377;
       public const int    LS_DPARAM_MIP_FP_TIMLIM        =       5378;
       public const int    LS_IPARAM_MIP_FP_ITRLIM        =       5379;
       public const int    LS_IPARAM_MIP_FP_HEU_MODE      =       5380;
       public const int    LS_DPARAM_MIP_OBJ_THRESHOLD    =       5381;
       public const int    LS_IPARAM_MIP_LOCALBRANCHNUM   =       5382;
       public const int    LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_TIME =       5383;
       public const int    LS_DPARAM_MIP_ITRLIM_SIM       =       5384;
       public const int    LS_DPARAM_MIP_ITRLIM_NLP       =       5385;
       public const int    LS_DPARAM_MIP_ITRLIM_IPM       =       5386;
       public const int    LS_IPARAM_MIP_MAXNUM_MIP_SOL_STORAGE =       5387;
       public const int    LS_IPARAM_MIP_CONCURRENT_TOPOPTMODE =       5388;
       public const int    LS_IPARAM_MIP_CONCURRENT_REOPTMODE =       5389;
       public const int    LS_IPARAM_MIP_PREHEU_LEVEL     =       5390;
       public const int    LS_IPARAM_MIP_PREHEU_PRE_LEVEL =       5391;
       public const int    LS_IPARAM_MIP_PREHEU_PRINT_LEVEL =       5392;
       public const int    LS_IPARAM_MIP_PREHEU_TC_ITERLIM =       5393;
       public const int    LS_IPARAM_MIP_PREHEU_DFE_VSTLIM =       5394;
       public const int    LS_IPARAM_MIP_PREHEU_VAR_SEQ   =       5395;
       public const int    LS_IPARAM_MIP_USE_PARTIALSOL_LEVEL =       5396;
       public const int    LS_IPARAM_MIP_GENERAL_MODE     =       5397;
       public const int    LS_IPARAM_MIP_NUM_THREADS      =       5398;
       public const int    LS_IPARAM_MIP_POLISH_NUM_BRANCH_NEXT =       5399;
       public const int    LS_IPARAM_MIP_POLISH_MAX_BRANCH_COUNT =       5400;
       public const int    LS_DPARAM_MIP_POLISH_ALPHA_TARGET =       5401;
       public const int    LS_IPARAM_MIP_CONCURRENT_STRATEGY =       5402;
       public const int    LS_DPARAM_MIP_BRANCH_TOP_VAL_DIFF_WEIGHT =       5403;
       public const int    LS_IPARAM_MIP_BASCUTS_DONUM    =       5404;
       public const int    LS_IPARAM_MIP_PARA_SUB         =       5405;
       public const int    LS_DPARAM_MIP_PARA_RND_ITRLMT  =       5406;
       public const int    LS_DPARAM_MIP_PARA_INIT_NODE   =       5407;
       public const int    LS_IPARAM_MIP_PARA_ITR_MODE    =       5408;
       public const int    LS_IPARAM_MIP_PARA_FP          =       5409;
       public const int    LS_IPARAM_MIP_PARA_FP_MODE     =       5410;
       public const int    LS_IPARAM_MIP_HEU_DROP_OBJ     =       5411;
       public const int    LS_DPARAM_MIP_ABSCUTTOL        =       5412;
       public const int    LS_IPARAM_MIP_PERSPECTIVE_REFORM =       5413;
       public const int    LS_IPARAM_MIP_TREEREORDERMODE  =       5414;
       public const int    LS_IPARAM_MIP_XSOLVER          =       5415;
       public const int    LS_IPARAM_MIP_BNB_TRY_BNP      =       5416;
       public const int    LS_IPARAM_MIP_KBEST_USE_GOP    =       5417;
       public const int    LS_IPARAM_MIP_SYMMETRY_MODE    =       5418;
       public const int    LS_IPARAM_MIP_ALLDIFF_METHOD   =       5419;
       public const int    LS_IPARAM_MIP_SOLLIM           =       5420;

    /* Global optimization (GOP) parameters (6000 - 6+++) */
       public const int    LS_DPARAM_GOP_RELOPTTOL        =       6400;
       public const int    LS_DPARAM_GOP_FLTTOL           =       6401;
       public const int    LS_DPARAM_GOP_BOXTOL           =       6402;
       public const int    LS_DPARAM_GOP_WIDTOL           =       6403;
       public const int    LS_DPARAM_GOP_DELTATOL         =       6404;
       public const int    LS_DPARAM_GOP_BNDLIM           =       6405;
       public const int    LS_IPARAM_GOP_TIMLIM           =       6406;
       public const int    LS_IPARAM_GOP_OPTCHKMD         =       6407;
       public const int    LS_IPARAM_GOP_BRANCHMD         =       6408;
       public const int    LS_IPARAM_GOP_MAXWIDMD         =       6409;
       public const int    LS_IPARAM_GOP_PRELEVEL         =       6410;
       public const int    LS_IPARAM_GOP_POSTLEVEL        =       6411;
       public const int    LS_IPARAM_GOP_BBSRCHMD         =       6412;
       public const int    LS_IPARAM_GOP_DECOMPPTMD       =       6413;
       public const int    LS_IPARAM_GOP_ALGREFORMMD      =       6414;
       public const int    LS_IPARAM_GOP_RELBRNDMD        =       6415;
       public const int    LS_IPARAM_GOP_PRINTLEVEL       =       6416;
       public const int    LS_IPARAM_GOP_BNDLIM_MODE      =       6417;
       public const int    LS_IPARAM_GOP_BRANCH_LIMIT     =       6418;
       public const int    LS_IPARAM_GOP_CORELEVEL        =       6419;
       public const int    LS_IPARAM_GOP_OPT_MODE         =       6420;
       public const int    LS_IPARAM_GOP_HEU_MODE         =       6421;
       public const int    LS_IPARAM_GOP_SUBOUT_MODE      =       6422;
       public const int    LS_IPARAM_GOP_USE_NLPSOLVE     =       6423;
       public const int    LS_IPARAM_GOP_LSOLBRANLIM      =       6424;
       public const int    LS_IPARAM_GOP_LPSOPT           =       6425;
       public const int    LS_DPARAM_GOP_TIMLIM           =       6426;
       public const int    LS_DPARAM_GOP_BRANCH_LIMIT     =       6427;
       public const int    LS_IPARAM_GOP_QUADMD           =       6428;
       public const int    LS_IPARAM_GOP_LIM_MODE         =       6429;
       public const int    LS_DPARAM_GOP_ITRLIM           =       6430;
       public const int    LS_DPARAM_GOP_ITRLIM_SIM       =       6431;
       public const int    LS_DPARAM_GOP_ITRLIM_IPM       =       6432;
       public const int    LS_DPARAM_GOP_ITRLIM_NLP       =       6433;
       public const int    LS_DPARAM_GOP_ABSOPTTOL        =       6434;
       public const int    LS_DPARAM_GOP_PEROPTTOL        =       6435;
       public const int    LS_DPARAM_GOP_AOPTTIMLIM       =       6436;
       public const int    LS_IPARAM_GOP_LINEARZ          =       6437;
       public const int    LS_IPARAM_GOP_NUM_THREADS      =       6438;
       public const int    LS_IPARAM_GOP_MULTILINEAR      =       6439;
       public const int    LS_DPARAM_GOP_OBJ_THRESHOLD    =       6440;
       public const int    LS_IPARAM_GOP_QUAD_METHOD      =       6441;
       public const int    LS_IPARAM_GOP_SOLLIM           =       6442;
       public const int    LS_IPARAM_GOP_CMINLP           =       6443;
       public const int    LS_IPARAM_GOP_CONIC_REFORM	  =       6444;

    /* License information parameters */
       public const int    LS_IPARAM_LIC_CONSTRAINTS      =        500;
       public const int    LS_IPARAM_LIC_VARIABLES        =        501;
       public const int    LS_IPARAM_LIC_INTEGERS         =        502;
       public const int    LS_IPARAM_LIC_NONLINEARVARS    =        503;
       public const int    LS_IPARAM_LIC_GOP_INTEGERS     =        504;
       public const int    LS_IPARAM_LIC_GOP_NONLINEARVARS =        505;
       public const int    LS_IPARAM_LIC_DAYSTOEXP        =        506;
       public const int    LS_IPARAM_LIC_DAYSTOTRIALEXP   =        507;
       public const int    LS_IPARAM_LIC_NONLINEAR        =        508;
       public const int    LS_IPARAM_LIC_EDUCATIONAL      =        509;
       public const int    LS_IPARAM_LIC_RUNTIME          =        510;
       public const int    LS_IPARAM_LIC_NUMUSERS         =        511;
       public const int    LS_IPARAM_LIC_BARRIER          =        512;
       public const int    LS_IPARAM_LIC_GLOBAL           =        513;
       public const int    LS_IPARAM_LIC_PLATFORM         =        514;
       public const int    LS_IPARAM_LIC_MIP              =        515;
       public const int    LS_IPARAM_LIC_SP               =        516;
       public const int    LS_IPARAM_LIC_CONIC            =        517;
       public const int    LS_IPARAM_LIC_RESERVED1        =        519;

    /* Model analysis parameters (1500 - 15++) */
       public const int    LS_IPARAM_IIS_ANALYZE_LEVEL    =       1550;
       public const int    LS_IPARAM_IUS_ANALYZE_LEVEL    =       1551;
       public const int    LS_IPARAM_IIS_TOPOPT           =       1552;
       public const int    LS_IPARAM_IIS_REOPT            =       1553;
       public const int    LS_IPARAM_IIS_USE_SFILTER      =       1554;
       public const int    LS_IPARAM_IIS_PRINT_LEVEL      =       1555;
       public const int    LS_IPARAM_IIS_INFEAS_NORM      =       1556;
       public const int    LS_IPARAM_IIS_ITER_LIMIT       =       1557;
       public const int    LS_DPARAM_IIS_ITER_LIMIT       =       1558;
       public const int    LS_IPARAM_IIS_TIME_LIMIT       =       1559;
       public const int    LS_IPARAM_IIS_METHOD           =       1560;
       public const int    LS_IPARAM_IIS_USE_EFILTER      =       1561;
       public const int    LS_IPARAM_IIS_USE_GOP          =       1562;
       public const int    LS_IPARAM_IIS_NUM_THREADS      =       1563;
       public const int    LS_IPARAM_IIS_GETMODE          =       1564;

    /* Output log format parameter */
       public const int    LS_IPARAM_FMT_ISSQL            =       1590;

    /*! Common sample size per stochastic parameter. */
       public const int    LS_IPARAM_STOC_NSAMPLE_SPAR    =       6600;

    /*! Common sample size per stage.  */
       public const int    LS_IPARAM_STOC_NSAMPLE_STAGE   =       6601;

    /*! Seed to initialize the random number generator. */
       public const int    LS_IPARAM_STOC_RG_SEED         =       6602;

    /*! Stochastic optimization method to solve the model. */
       public const int    LS_IPARAM_STOC_METHOD          =       6603;

    /*! Reoptimization method to solve the node-models. */
       public const int    LS_IPARAM_STOC_REOPT           =       6604;

    /*! Optimization method to solve the root problem. */
       public const int    LS_IPARAM_STOC_TOPOPT          =       6605;

    /*! Iteration limit for stochastic solver. */
       public const int    LS_IPARAM_STOC_ITER_LIM        =       6606;

    /*! Print level to display progress information during optimization */
       public const int    LS_IPARAM_STOC_PRINT_LEVEL     =       6607;

    /*! Type of deterministic equivalent */
       public const int    LS_IPARAM_STOC_DETEQ_TYPE      =       6608;

    /*! Flag to enable/disable calculation of EVPI. */
       public const int    LS_IPARAM_STOC_CALC_EVPI       =       6609;

    /*! Flag to restrict sampling to continuous stochastic parameters only or not.*/
       public const int    LS_IPARAM_STOC_SAMP_CONT_ONLY  =       6611;

    /*! Bucket size in Benders decomposition */
       public const int    LS_IPARAM_STOC_BUCKET_SIZE     =       6612;

    /*! Maximum number of scenarios before forcing automatic sampling */
       public const int    LS_IPARAM_STOC_MAX_NUMSCENS    =       6613;

    /*! Stage beyond which node-models are shared */
       public const int    LS_IPARAM_STOC_SHARE_BEGSTAGE  =       6614;

    /*! Print level to display progress information during optimization of node models */
       public const int    LS_IPARAM_STOC_NODELP_PRELEVEL =       6615;

    /*! Time limit for stochastic solver.*/
       public const int    LS_DPARAM_STOC_TIME_LIM        =       6616;

    /*! Relative optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. */
       public const int    LS_DPARAM_STOC_RELOPTTOL       =       6617;

    /*! Absolute optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. */
       public const int    LS_DPARAM_STOC_ABSOPTTOL       =       6618;

    /*! Internal mask */
       public const int    LS_IPARAM_STOC_DEBUG_MASK      =       6619;

    /*! Sampling method for variance reduction. */
       public const int    LS_IPARAM_STOC_VARCONTROL_METHOD =       6620;

    /*! Correlation type associated with correlation matrix.  */
       public const int    LS_IPARAM_STOC_CORRELATION_TYPE =       6621;

    /*! Warm start basis for wait-see model  .  */
       public const int    LS_IPARAM_STOC_WSBAS           =       6622;

    /*! Outer loop iteration limit for ALD  .  */
       public const int    LS_IPARAM_STOC_ALD_OUTER_ITER_LIM =       6623;

    /*! Inner loop iteration limit for ALD  .  */
       public const int    LS_IPARAM_STOC_ALD_INNER_ITER_LIM =       6624;

    /*! Dual feasibility tolerance for ALD  .  */
       public const int    LS_DPARAM_STOC_ALD_DUAL_FEASTOL =       6625;

    /*! Primal feasibility tolerance for ALD  .  */
       public const int    LS_DPARAM_STOC_ALD_PRIMAL_FEASTOL =       6626;

    /*! Dual step length for ALD  .  */
       public const int    LS_DPARAM_STOC_ALD_DUAL_STEPLEN =       6627;

    /*! Primal step length for ALD  .  */
       public const int    LS_DPARAM_STOC_ALD_PRIMAL_STEPLEN =       6628;

    /*! Order nontemporal models or not.  */
       public const int    LS_IPARAM_CORE_ORDER_BY_STAGE  =       6629;

    /*! Node name format.  */
       public const int    LS_SPARAM_STOC_FMT_NODE_NAME   =       6630;

    /*! Scenario name format.  */
       public const int    LS_SPARAM_STOC_FMT_SCENARIO_NAME =       6631;

    /*! Flag to specify whether stochastic parameters in MPI will be mapped as LP matrix elements.  */
       public const int    LS_IPARAM_STOC_MAP_MPI2LP      =       6632;

    /*! Flag to enable or disable autoaggregation */
       public const int    LS_IPARAM_STOC_AUTOAGGR        =       6633;

    /*! Benchmark scenario to compare EVPI and EVMU against*/
       public const int    LS_IPARAM_STOC_BENCHMARK_SCEN  =       6634;

    /*! Value to truncate infinite bounds at non-leaf nodes */
       public const int    LS_DPARAM_STOC_INFBND          =       6635;

    /*! Flag to use add-instructions mode when building deteq */
       public const int    LS_IPARAM_STOC_ADD_MPI         =       6636;

    /* Flag to enable elimination of fixed variables from deteq MPI */
       public const int    LS_IPARAM_STOC_ELIM_FXVAR      =       6637;

    /*! RHS value of objective cut in SBD master problem.  */
       public const int    LS_DPARAM_STOC_SBD_OBJCUTVAL   =       6638;

    /*! Flag to enable objective cut in SBD master problem.  */
       public const int    LS_IPARAM_STOC_SBD_OBJCUTFLAG  =       6639;

    /*! Maximum number of candidate solutions to generate at SBD root */
       public const int    LS_IPARAM_STOC_SBD_NUMCANDID   =       6640;

    /*! Big-M value for linearization and penalty functions */
       public const int    LS_DPARAM_STOC_BIGM            =       6641;

    /*! Name data level */
       public const int    LS_IPARAM_STOC_NAMEDATA_LEVEL  =       6642;

    /*! Max cuts to generate for master problem */
       public const int    LS_IPARAM_STOC_SBD_MAXCUTS     =       6643;

    /*! Optimization method to solve the deteq problem. */
       public const int    LS_IPARAM_STOC_DEQOPT          =       6644;

    /*! Subproblem formulation to use in DirectSearch. */
       public const int    LS_IPARAM_STOC_DS_SUBFORM      =       6645;

    /*! Primal-step tolerance */
       public const int    LS_DPARAM_STOC_REL_PSTEPTOL    =       6646;

    /*! Dual-step tolerance */
       public const int    LS_DPARAM_STOC_REL_DSTEPTOL    =       6647;

    /*! Number of parallel threads */
       public const int    LS_IPARAM_STOC_NUM_THREADS     =       6648;

    /*! Number of deteq blocks */
       public const int    LS_IPARAM_STOC_DETEQ_NBLOCKS   =       6649;

    /*! Bitmask to enable methods for solving the nearest correlation matrix (NCM) subproblem */
       public const int    LS_IPARAM_SAMP_NCM_METHOD      =       7701;

    /*! Objective cutoff (target) value to stop the nearest correlation matrix (NCM) subproblem */
       public const int    LS_DPARAM_SAMP_NCM_CUTOBJ      =       7702;

    /*! Flag to enable/disable sparse mode in NCM computations */
       public const int    LS_IPARAM_SAMP_NCM_DSTORAGE    =       7703;

    /*! Correlation matrix diagonal shift increment */
       public const int    LS_DPARAM_SAMP_CDSINC          =       7704;

    /*! Flag to enable scaling of raw sample data */
       public const int    LS_IPARAM_SAMP_SCALE           =       7705;

    /*! Iteration limit for NCM method */
       public const int    LS_IPARAM_SAMP_NCM_ITERLIM     =       7706;

    /*! Optimality tolerance for NCM method */
       public const int    LS_DPARAM_SAMP_NCM_OPTTOL      =       7707;

    /*! Number of parallel threads */
       public const int    LS_IPARAM_SAMP_NUM_THREADS     =       7708;

    /*! Buffer size for random number generators */
       public const int    LS_IPARAM_SAMP_RG_BUFFER_SIZE  =       7709;

    /*bound size when subproblem is unbounded*/
       public const int    LS_DPARAM_BNP_INFBND           =       8010;

    /*branch and bound type*/
       public const int    LS_IPARAM_BNP_LEVEL            =       8011;

    /*print level*/
       public const int    LS_IPARAM_BNP_PRINT_LEVEL      =       8012;

    /*box size for BOXSTEP method*/
       public const int    LS_DPARAM_BNP_BOX_SIZE         =       8013;

    /*number of threads in bnp*/
       public const int    LS_IPARAM_BNP_NUM_THREADS      =       8014;

    /*relative optimality tolerance for subproblems*/
       public const int    LS_DPARAM_BNP_SUB_ITRLMT       =       8015;

    /*method for finding block structure*/
       public const int    LS_IPARAM_BNP_FIND_BLK         =       8016;

    /*pre level*/
       public const int    LS_IPARAM_BNP_PRELEVEL         =       8017;

    /*column limit*/
       public const int    LS_DPARAM_BNP_COL_LMT          =       8018;

    /*time limit for bnp*/
       public const int    LS_DPARAM_BNP_TIMLIM           =       8019;

    /*simplex limit for bnp*/
       public const int    LS_DPARAM_BNP_ITRLIM_SIM       =       8020;

    /*ipm limit for bnp*/
       public const int    LS_DPARAM_BNP_ITRLIM_IPM       =       8021;

    /*branch limit for bnp*/
       public const int    LS_IPARAM_BNP_BRANCH_LIMIT     =       8022;

    /*iteration limit for bnp*/
       public const int    LS_DPARAM_BNP_ITRLIM           =       8023;

    /*  Probability of crossover for continuous variables */
       public const int    LS_DPARAM_GA_CXOVER_PROB       =       8501;

    /*  Spreading factor for crossover */
       public const int    LS_DPARAM_GA_XOVER_SPREAD      =       8502;

    /*  Probability of crossover for integer variables */
       public const int    LS_DPARAM_GA_IXOVER_PROB       =       8503;

    /*  Probability of mutation for continuous variables */
       public const int    LS_DPARAM_GA_CMUTAT_PROB       =       8504;

    /*  Spreading factor for mutation */
       public const int    LS_DPARAM_GA_MUTAT_SPREAD      =       8505;

    /*  Probability of mutation for integer variables */
       public const int    LS_DPARAM_GA_IMUTAT_PROB       =       8506;

    /*  Numeric zero tolerance in GA */
       public const int    LS_DPARAM_GA_TOL_ZERO          =       8507;

    /*  Primal feasibility tolerance in GA */
       public const int    LS_DPARAM_GA_TOL_PFEAS         =       8508;

    /*  Numeric infinity in GA */
       public const int    LS_DPARAM_GA_INF               =       8509;

    /*  Infinity threshold for finite bounds in GA */
       public const int    LS_DPARAM_GA_INFBND            =       8510;

    /*  Alpha parameter in Blending Alpha Crossover method */
       public const int    LS_DPARAM_GA_BLXA              =       8511;

    /*  Beta parameter in Blending Alpha-Beta Crossover method */
       public const int    LS_DPARAM_GA_BLXB              =       8512;

    /*  Method of crossover for continuous variables */
       public const int    LS_IPARAM_GA_CXOVER_METHOD     =       8513;

    /*  Method of crossover for integer variables */
       public const int    LS_IPARAM_GA_IXOVER_METHOD     =       8514;

    /*  Method of mutation for continuous variables */
       public const int    LS_IPARAM_GA_CMUTAT_METHOD     =       8515;

    /*  Method of mutation for integer variables */
       public const int    LS_IPARAM_GA_IMUTAT_METHOD     =       8516;

    /*  RNG seed for GA */
       public const int    LS_IPARAM_GA_SEED              =       8517;

    /*  Number of generations in GA */
       public const int    LS_IPARAM_GA_NGEN              =       8518;

    /*  Population size in GA */
       public const int    LS_IPARAM_GA_POPSIZE           =       8519;

    /*  Print level to log files */
       public const int    LS_IPARAM_GA_FILEOUT           =       8520;

    /*  Print level for GA */
       public const int    LS_IPARAM_GA_PRINTLEVEL        =       8521;

    /*  Flag to specify whether an optimum individual will be injected */
       public const int    LS_IPARAM_GA_INJECT_OPT        =       8522;

    /*  Number of threads in GA */
       public const int    LS_IPARAM_GA_NUM_THREADS       =       8523;

    /*  Objective function sense */
       public const int    LS_IPARAM_GA_OBJDIR            =       8524;

    /*  Target objective function value */
       public const int    LS_DPARAM_GA_OBJSTOP           =       8525;

    /*  Migration probability  */
       public const int    LS_DPARAM_GA_MIGRATE_PROB      =       8526;

    /*  Search space or search mode  */
       public const int    LS_IPARAM_GA_SSPACE            =       8527;

    /* Version info */
       public const int    LS_IPARAM_VER_MAJOR            =        990;
       public const int    LS_IPARAM_VER_MINOR            =        991;
       public const int    LS_IPARAM_VER_BUILD            =        992;
       public const int    LS_IPARAM_VER_REVISION         =        993;

    /* Last card for parameters */
       public const int    LS_IPARAM_VER_NUMBER           =        999;

 /* Math operator codes (1000-1500) */
       public const int    EP_NO_OP                       =       0000;
       public const int    EP_PLUS                        =       1001;
       public const int    EP_MINUS                       =       1002;
       public const int    EP_MULTIPLY                    =       1003;
       public const int    EP_DIVIDE                      =       1004;
       public const int    EP_POWER                       =       1005;
       public const int    EP_EQUAL                       =       1006;
       public const int    EP_NOT_EQUAL                   =       1007;
       public const int    EP_LTOREQ                      =       1008;
       public const int    EP_GTOREQ                      =       1009;
       public const int    EP_LTHAN                       =       1010;
       public const int    EP_GTHAN                       =       1011;
       public const int    EP_AND                         =       1012;
       public const int    EP_OR                          =       1013;
       public const int    EP_NOT                         =       1014;
       public const int    EP_PERCENT                     =       1015;
       public const int    EP_POSATE                      =       1016;
       public const int    EP_NEGATE                      =       1017;
       public const int    EP_ABS                         =       1018;
       public const int    EP_SQRT                        =       1019;
       public const int    EP_LOG                         =       1020;
       public const int    EP_LN                          =       1021;
       public const int    EP_PI                          =       1022;
       public const int    EP_SIN                         =       1023;
       public const int    EP_COS                         =       1024;
       public const int    EP_TAN                         =       1025;
       public const int    EP_ATAN2                       =       1026;
       public const int    EP_ATAN                        =       1027;
       public const int    EP_ASIN                        =       1028;
       public const int    EP_ACOS                        =       1029;
       public const int    EP_EXP                         =       1030;
       public const int    EP_MOD                         =       1031;
       public const int    EP_FALSE                       =       1032;
       public const int    EP_TRUE                        =       1033;
       public const int    EP_IF                          =       1034;
       public const int    EP_PSN                         =       1035;
       public const int    EP_PSL                         =       1036;
       public const int    EP_LGM                         =       1037;
       public const int    EP_SIGN                        =       1038;
       public const int    EP_FLOOR                       =       1039;
       public const int    EP_FPA                         =       1040;
       public const int    EP_FPL                         =       1041;
       public const int    EP_PEL                         =       1042;
       public const int    EP_PEB                         =       1043;
       public const int    EP_PPS                         =       1044;
       public const int    EP_PPL                         =       1045;
       public const int    EP_PTD                         =       1046;
       public const int    EP_PCX                         =       1047;
       public const int    EP_WRAP                        =       1048;
       public const int    EP_PBNO                        =       1049;
       public const int    EP_PFS                         =       1050;
       public const int    EP_PFD                         =       1051;
       public const int    EP_PHG                         =       1052;
       public const int    EP_RAND                        =       1053;
       public const int    EP_USER                        =       1054;
       public const int    EP_SUM                         =       1055;
       public const int    EP_AVG                         =       1056;
       public const int    EP_MIN                         =       1057;
       public const int    EP_MAX                         =       1058;
       public const int    EP_NPV                         =       1059;
       public const int    EP_VAND                        =       1060;
       public const int    EP_VOR                         =       1061;
       public const int    EP_PUSH_NUM                    =       1062;
       public const int    EP_PUSH_VAR                    =       1063;
       public const int    EP_NORMDENS                    =       1064;
       public const int    EP_NORMINV                     =       1065;
       public const int    EP_TRIAINV                     =       1066;
       public const int    EP_EXPOINV                     =       1067;
       public const int    EP_UNIFINV                     =       1068;
       public const int    EP_MULTINV                     =       1069;
       public const int    EP_USRCOD                      =       1070;
       public const int    EP_SUMPROD                     =       1071;
       public const int    EP_SUMIF                       =       1072;
       public const int    EP_VLOOKUP                     =       1073;
       public const int    EP_VPUSH_NUM                   =       1074;
       public const int    EP_VPUSH_VAR                   =       1075;
       public const int    EP_VMULT                       =       1076;
       public const int    EP_SQR                         =       1077;
       public const int    EP_SINH                        =       1078;
       public const int    EP_COSH                        =       1079;
       public const int    EP_TANH                        =       1080;
       public const int    EP_ASINH                       =       1081;
       public const int    EP_ACOSH                       =       1082;
       public const int    EP_ATANH                       =       1083;
       public const int    EP_LOGB                        =       1084;
       public const int    EP_LOGX                        =       1085;
       public const int    EP_LNX                         =       1086;
       public const int    EP_TRUNC                       =       1087;
       public const int    EP_NORMSINV                    =       1088;
       public const int    EP_INT                         =       1089;
       public const int    EP_PUSH_STR                    =       1090;
       public const int    EP_VPUSH_STR                   =       1091;
       public const int    EP_PUSH_SPAR                   =       1092;
       public const int    EP_NORMPDF                     =       1093;
       public const int    EP_NORMCDF                     =       1094;
       public const int    EP_LSQ                         =       1095;
       public const int    EP_LNPSNX                      =       1096;
       public const int    EP_LNCPSN                      =       1097;
       public const int    EP_XEXPNAX                     =       1098;
       public const int    EP_XNEXPMX                     =       1099;

    /* Probability density functions */
       public const int    EP_PBT                         =       1100;
       public const int    EP_PBTINV                      =       1101;
       public const int    EP_PBNINV                      =       1102;
       public const int    EP_PCC                         =       1103;
       public const int    EP_PCCINV                      =       1104;
       public const int    EP_PCXINV                      =       1105;
       public const int    EP_EXPN                        =       1106;
       public const int    EP_PFDINV                      =       1107;
       public const int    EP_PGA                         =       1108;
       public const int    EP_PGAINV                      =       1109;
       public const int    EP_PGE                         =       1110;
       public const int    EP_PGEINV                      =       1111;
       public const int    EP_PGU                         =       1112;
       public const int    EP_PGUINV                      =       1113;
       public const int    EP_PHGINV                      =       1114;
       public const int    EP_PLA                         =       1115;
       public const int    EP_PLAINV                      =       1116;
       public const int    EP_PLG                         =       1117;
       public const int    EP_PLGINV                      =       1118;
       public const int    EP_LGT                         =       1119;
       public const int    EP_LGTINV                      =       1120;
       public const int    EP_LGNM                        =       1121;
       public const int    EP_LGNMINV                     =       1122;
       public const int    EP_NGBN                        =       1123;
       public const int    EP_NGBNINV                     =       1124;
       public const int    EP_NRM                         =       1125;
       public const int    EP_PPT                         =       1126;
       public const int    EP_PPTINV                      =       1127;
       public const int    EP_PPSINV                      =       1128;
       public const int    EP_PTDINV                      =       1129;
       public const int    EP_TRIAN                       =       1130;
       public const int    EP_UNIFM                       =       1131;
       public const int    EP_PWB                         =       1132;
       public const int    EP_PWBINV                      =       1133;
       public const int    EP_NRMINV                      =       1134;
       public const int    EP_TRIANINV                    =       1135;
       public const int    EP_EXPNINV                     =       1136;
       public const int    EP_UNIFMINV                    =       1137;
       public const int    EP_MLTNMINV                    =       1138;
       public const int    EP_BTDENS                      =       1139;
       public const int    EP_BNDENS                      =       1140;
       public const int    EP_CCDENS                      =       1141;
       public const int    EP_CXDENS                      =       1142;
       public const int    EP_EXPDENS                     =       1143;
       public const int    EP_FDENS                       =       1144;
       public const int    EP_GADENS                      =       1145;
       public const int    EP_GEDENS                      =       1146;
       public const int    EP_GUDENS                      =       1147;
       public const int    EP_HGDENS                      =       1148;
       public const int    EP_LADENS                      =       1149;
       public const int    EP_LGDENS                      =       1150;
       public const int    EP_LGTDENS                     =       1151;
       public const int    EP_LGNMDENS                    =       1152;
       public const int    EP_NGBNDENS                    =       1153;
       public const int    EP_NRMDENS                     =       1154;
       public const int    EP_PTDENS                      =       1155;
       public const int    EP_PSDENS                      =       1156;
       public const int    EP_TDENS                       =       1157;
       public const int    EP_TRIADENS                    =       1158;
       public const int    EP_UNIFDENS                    =       1159;
       public const int    EP_WBDENS                      =       1160;
       public const int    EP_RADIANS                     =       1161;
       public const int    EP_DEGREES                     =       1162;
       public const int    EP_ROUND                       =       1163;
       public const int    EP_ROUNDUP                     =       1164;
       public const int    EP_ROUNDDOWN                   =       1165;
       public const int    EP_ERF                         =       1166;
       public const int    EP_PBN                         =       1167;
       public const int    EP_PBB                         =       1168;
       public const int    EP_PBBINV                      =       1169;
       public const int    EP_BBDENS                      =       1170;
       public const int    EP_PSS                         =       1171;
       public const int    EP_SSDENS                      =       1172;
       public const int    EP_SSINV                       =       1173;
       public const int    EP_POSD                        =       1174;
       public const int    EP_SETS                        =       1175;
       public const int    EP_CARD                        =       1176;
       public const int    EP_STDEV                       =       1177;
       public const int    EP_LMTD                        =       1178;
       public const int    EP_RLMTD                       =       1179;
       public const int    EP_LOGIT                       =       1180;
       public const int    EP_ALLDIFF                     =       1181;
       public const int    EP_SIGNPOWER                   =       1182;
       public const int    EP_QUADPROD                    =       1183;
       public const int    EP_ATAN2R                      =       1184;
       public const int    EP_XPOWDIVAB                   =       1185;
       public const int    	EP_LOGABEXPX									         =       1186;

 /* Model statistics (11001-11199)*/
       public const int    LS_IINFO_NUM_NONZ_OBJ          =      11001;
       public const int    LS_IINFO_NUM_SEMICONT          =      11002;
       public const int    LS_IINFO_NUM_SETS              =      11003;
       public const int    LS_IINFO_NUM_SETS_NNZ          =      11004;
       public const int    LS_IINFO_NUM_QCP_CONS          =      11005;
       public const int    LS_IINFO_NUM_CONT_CONS         =      11006;
       public const int    LS_IINFO_NUM_INT_CONS          =      11007;
       public const int    LS_IINFO_NUM_BIN_CONS          =      11008;
       public const int    LS_IINFO_NUM_QCP_VARS          =      11009;
       public const int    LS_IINFO_NUM_CONS              =      11010;
       public const int    LS_IINFO_NUM_VARS              =      11011;
       public const int    LS_IINFO_NUM_NONZ              =      11012;
       public const int    LS_IINFO_NUM_BIN               =      11013;
       public const int    LS_IINFO_NUM_INT               =      11014;
       public const int    LS_IINFO_NUM_CONT              =      11015;
       public const int    LS_IINFO_NUM_QC_NONZ           =      11016;
       public const int    LS_IINFO_NUM_NLP_NONZ          =      11017;
       public const int    LS_IINFO_NUM_NLPOBJ_NONZ       =      11018;
       public const int    LS_IINFO_NUM_RDCONS            =      11019;
       public const int    LS_IINFO_NUM_RDVARS            =      11020;
       public const int    LS_IINFO_NUM_RDNONZ            =      11021;
       public const int    LS_IINFO_NUM_RDINT             =      11022;
       public const int    LS_IINFO_LEN_VARNAMES          =      11023;
       public const int    LS_IINFO_LEN_CONNAMES          =      11024;
       public const int    LS_IINFO_NUM_NLP_CONS          =      11025;
       public const int    LS_IINFO_NUM_NLP_VARS          =      11026;
       public const int    LS_IINFO_NUM_SUF_ROWS          =      11027;
       public const int    LS_IINFO_NUM_IIS_ROWS          =      11028;
       public const int    LS_IINFO_NUM_SUF_BNDS          =      11029;
       public const int    LS_IINFO_NUM_IIS_BNDS          =      11030;
       public const int    LS_IINFO_NUM_SUF_COLS          =      11031;
       public const int    LS_IINFO_NUM_IUS_COLS          =      11032;
       public const int    LS_IINFO_NUM_CONES             =      11033;
       public const int    LS_IINFO_NUM_CONE_NONZ         =      11034;
       public const int    LS_IINFO_LEN_CONENAMES         =      11035;
       public const int    LS_DINFO_INST_VAL_MIN_COEF     =      11036;
       public const int    LS_IINFO_INST_VARNDX_MIN_COEF  =      11037;
       public const int    LS_IINFO_INST_CONNDX_MIN_COEF  =      11038;
       public const int    LS_DINFO_INST_VAL_MAX_COEF     =      11039;
       public const int    LS_IINFO_INST_VARNDX_MAX_COEF  =      11040;
       public const int    LS_IINFO_INST_CONNDX_MAX_COEF  =      11041;
       public const int    LS_IINFO_NUM_VARS_CARD         =      11042;
       public const int    LS_IINFO_NUM_VARS_SOS1         =      11043;
       public const int    LS_IINFO_NUM_VARS_SOS2         =      11044;
       public const int    LS_IINFO_NUM_VARS_SOS3         =      11045;
       public const int    LS_IINFO_NUM_VARS_SCONT        =      11046;
       public const int    LS_IINFO_NUM_CONS_L            =      11047;
       public const int    LS_IINFO_NUM_CONS_E            =      11048;
       public const int    LS_IINFO_NUM_CONS_G            =      11049;
       public const int    LS_IINFO_NUM_CONS_R            =      11050;
       public const int    LS_IINFO_NUM_CONS_N            =      11051;
       public const int    LS_IINFO_NUM_VARS_LB           =      11052;
       public const int    LS_IINFO_NUM_VARS_UB           =      11053;
       public const int    LS_IINFO_NUM_VARS_LUB          =      11054;
       public const int    LS_IINFO_NUM_VARS_FR           =      11055;
       public const int    LS_IINFO_NUM_VARS_FX           =      11056;
       public const int    LS_IINFO_NUM_INST_CODES        =      11057;
       public const int    LS_IINFO_NUM_INST_REAL_NUM     =      11058;
       public const int    LS_IINFO_NUM_SPARS             =      11059;
       public const int    LS_IINFO_NUM_PROCS             =      11060;
       public const int    LS_IINFO_NUM_POSDS             =      11061;
       public const int    LS_IINFO_NUM_SUF_INTS          =      11062;
       public const int    LS_IINFO_NUM_IIS_INTS          =      11063;
       public const int    LS_IINFO_NUM_OBJPOOL           =      11064;
       public const int    LS_IINFO_NUM_SOLPOOL           =      11065;
       public const int    LS_IINFO_NUM_ALLDIFF           =      11066;
       public const int    LS_IINFO_MAX_RNONZ             =      11067;
       public const int    LS_IINFO_MAX_CNONZ             =      11068;
       public const int    LS_DINFO_AVG_RNONZ             =      11069;
       public const int    LS_DINFO_AVG_CNONZ             =      11070;

 /* LP and NLP related info (11200-11299)*/
       public const int    LS_IINFO_METHOD                =      11200;
       public const int    LS_DINFO_POBJ                  =      11201;
       public const int    LS_DINFO_DOBJ                  =      11202;
       public const int    LS_DINFO_PINFEAS               =      11203;
       public const int    LS_DINFO_DINFEAS               =      11204;
       public const int    LS_IINFO_MODEL_STATUS          =      11205;
       public const int    LS_IINFO_PRIMAL_STATUS         =      11206;
       public const int    LS_IINFO_DUAL_STATUS           =      11207;
       public const int    LS_IINFO_BASIC_STATUS          =      11208;
       public const int    LS_IINFO_BAR_ITER              =      11209;
       public const int    LS_IINFO_SIM_ITER              =      11210;
       public const int    LS_IINFO_NLP_ITER              =      11211;
       public const int    LS_IINFO_ELAPSED_TIME          =      11212;
       public const int    LS_DINFO_MSW_POBJ              =      11213;
       public const int    LS_IINFO_MSW_PASS              =      11214;
       public const int    LS_IINFO_MSW_NSOL              =      11215;
       public const int    LS_IINFO_IPM_STATUS            =      11216;
       public const int    LS_DINFO_IPM_POBJ              =      11217;
       public const int    LS_DINFO_IPM_DOBJ              =      11218;
       public const int    LS_DINFO_IPM_PINFEAS           =      11219;
       public const int    LS_DINFO_IPM_DINFEAS           =      11220;
       public const int    LS_IINFO_NLP_CALL_FUN          =      11221;
       public const int    LS_IINFO_NLP_CALL_DEV          =      11222;
       public const int    LS_IINFO_NLP_CALL_HES          =      11223;
       public const int    LS_IINFO_CONCURRENT_OPTIMIZER  =      11224;
       public const int    LS_IINFO_LEN_STAGENAMES        =      11225;
       public const int    LS_DINFO_BAR_ITER              =      11226;
       public const int    LS_DINFO_SIM_ITER              =      11227;
       public const int    LS_DINFO_NLP_ITER              =      11228;
       public const int    LS_IINFO_BAR_THREADS           =      11229;
       public const int    LS_IINFO_NLP_THREADS           =      11230;
       public const int    LS_IINFO_SIM_THREADS           =      11231;
       public const int    LS_DINFO_NLP_THRIMBL           =      11232;
       public const int    LS_SINFO_NLP_THREAD_LOAD       =      11233;
       public const int    LS_SINFO_BAR_THREAD_LOAD       =      11234;
       public const int    LS_SINFO_SIM_THREAD_LOAD       =      11235;
       public const int    LS_SINFO_ARCH                  =      11236;
       public const int    LS_IINFO_ARCH_ID               =      11237;
       public const int    LS_IINFO_MSW_BESTRUNIDX        =      11238;
       public const int    LS_DINFO_ACONDEST              =      11239;
       public const int    LS_DINFO_BCONDEST              =      11240;
       public const int    LS_IINFO_LPTOOL                =      11241;
       public const int    LS_SINFO_MODEL_TYPE            =      11242;
       public const int    LS_IINFO_NLP_LINEARITY         =      11243;

 /* MIP and MINLP related info (11300-11400) */
       public const int    LS_IINFO_MIP_STATUS            =      11300;
       public const int    LS_DINFO_MIP_OBJ               =      11301;
       public const int    LS_DINFO_MIP_BESTBOUND         =      11302;
       public const int    LS_IINFO_MIP_SIM_ITER          =      11303;
       public const int    LS_IINFO_MIP_BAR_ITER          =      11304;
       public const int    LS_IINFO_MIP_NLP_ITER          =      11305;
       public const int    LS_IINFO_MIP_BRANCHCOUNT       =      11306;
       public const int    LS_IINFO_MIP_NEWIPSOL          =      11307;
       public const int    LS_IINFO_MIP_LPCOUNT           =      11308;
       public const int    LS_IINFO_MIP_ACTIVENODES       =      11309;
       public const int    LS_IINFO_MIP_LTYPE             =      11310;
       public const int    LS_IINFO_MIP_AOPTTIMETOSTOP    =      11311;
       public const int    LS_IINFO_MIP_NUM_TOTAL_CUTS    =      11312;
       public const int    LS_IINFO_MIP_GUB_COVER_CUTS    =      11313;
       public const int    LS_IINFO_MIP_FLOW_COVER_CUTS   =      11314;
       public const int    LS_IINFO_MIP_LIFT_CUTS         =      11315;
       public const int    LS_IINFO_MIP_PLAN_LOC_CUTS     =      11316;
       public const int    LS_IINFO_MIP_DISAGG_CUTS       =      11317;
       public const int    LS_IINFO_MIP_KNAPSUR_COVER_CUTS =      11318;
       public const int    LS_IINFO_MIP_LATTICE_CUTS      =      11319;
       public const int    LS_IINFO_MIP_GOMORY_CUTS       =      11320;
       public const int    LS_IINFO_MIP_COEF_REDC_CUTS    =      11321;
       public const int    LS_IINFO_MIP_GCD_CUTS          =      11322;
       public const int    LS_IINFO_MIP_OBJ_CUT           =      11323;
       public const int    LS_IINFO_MIP_BASIS_CUTS        =      11324;
       public const int    LS_IINFO_MIP_CARDGUB_CUTS      =      11325;
       public const int    LS_IINFO_MIP_CLIQUE_CUTS       =      11326;
       public const int    LS_IINFO_MIP_CONTRA_CUTS       =      11327;
       public const int    LS_IINFO_MIP_GUB_CONS          =      11328;
       public const int    LS_IINFO_MIP_GLB_CONS          =      11329;
       public const int    LS_IINFO_MIP_PLANTLOC_CONS     =      11330;
       public const int    LS_IINFO_MIP_DISAGG_CONS       =      11331;
       public const int    LS_IINFO_MIP_SB_CONS           =      11332;
       public const int    LS_IINFO_MIP_IKNAP_CONS        =      11333;
       public const int    LS_IINFO_MIP_KNAP_CONS         =      11334;
       public const int    LS_IINFO_MIP_NLP_CONS          =      11335;
       public const int    LS_IINFO_MIP_CONT_CONS         =      11336;
       public const int    LS_DINFO_MIP_TOT_TIME          =      11347;
       public const int    LS_DINFO_MIP_OPT_TIME          =      11348;
       public const int    LS_DINFO_MIP_HEU_TIME          =      11349;
       public const int    LS_IINFO_MIP_SOLSTATUS_LAST_BRANCH =      11350;
       public const int    LS_DINFO_MIP_SOLOBJVAL_LAST_BRANCH =      11351;
       public const int    LS_IINFO_MIP_HEU_LEVEL         =      11352;
       public const int    LS_DINFO_MIP_PFEAS             =      11353;
       public const int    LS_DINFO_MIP_INTPFEAS          =      11354;
       public const int    LS_IINFO_MIP_WHERE_IN_CODE     =      11355;
       public const int    LS_IINFO_MIP_FP_ITER           =      11356;
       public const int    LS_DINFO_MIP_FP_SUMFEAS        =      11357;
       public const int    LS_DINFO_MIP_RELMIPGAP         =      11358;
       public const int    LS_DINFO_MIP_ROOT_OPT_TIME     =      11359;
       public const int    LS_DINFO_MIP_ROOT_PRE_TIME     =      11360;
       public const int    LS_IINFO_MIP_ROOT_METHOD       =      11361;
       public const int    LS_DINFO_MIP_SIM_ITER          =      11362;
       public const int    LS_DINFO_MIP_BAR_ITER          =      11363;
       public const int    LS_DINFO_MIP_NLP_ITER          =      11364;
       public const int    LS_IINFO_MIP_TOP_RELAX_IS_NON_CONVEX =      11365;
       public const int    LS_DINFO_MIP_FP_TIME           =      11366;
       public const int    LS_IINFO_MIP_THREADS           =      11367;
       public const int    LS_SINFO_MIP_THREAD_LOAD       =      11368;
       public const int    LS_DINFO_MIP_ABSGAP            =      11369;
       public const int    LS_DINFO_MIP_RELGAP            =      11370;
       public const int    LS_IINFO_MIP_SOFTKNAP_CUTS     =      11371;
       public const int    LS_IINFO_MIP_LP_ROUND_CUTS     =      11372;
       public const int    LS_IINFO_MIP_PERSPECTIVE_CUTS  =      11373;
       public const int    LS_IINFO_MIP_STRATEGY_MASK     =      11374;

 /* GOP related info (11601-11699) */
       public const int    LS_DINFO_GOP_OBJ               =      11600;
       public const int    LS_IINFO_GOP_SIM_ITER          =      11601;
       public const int    LS_IINFO_GOP_BAR_ITER          =      11602;
       public const int    LS_IINFO_GOP_NLP_ITER          =      11603;
       public const int    LS_DINFO_GOP_BESTBOUND         =      11604;
       public const int    LS_IINFO_GOP_STATUS            =      11605;
       public const int    LS_IINFO_GOP_LPCOUNT           =      11606;
       public const int    LS_IINFO_GOP_NLPCOUNT          =      11607;
       public const int    LS_IINFO_GOP_MIPCOUNT          =      11608;
       public const int    LS_IINFO_GOP_NEWSOL            =      11609;
       public const int    LS_IINFO_GOP_BOX               =      11610;
       public const int    LS_IINFO_GOP_BBITER            =      11611;
       public const int    LS_IINFO_GOP_SUBITER           =      11612;
       public const int    LS_IINFO_GOP_MIPBRANCH         =      11613;
       public const int    LS_IINFO_GOP_ACTIVEBOXES       =      11614;
       public const int    LS_IINFO_GOP_TOT_TIME          =      11615;
       public const int    LS_IINFO_GOP_MAXDEPTH          =      11616;
       public const int    LS_DINFO_GOP_PFEAS             =      11617;
       public const int    LS_DINFO_GOP_INTPFEAS          =      11618;
       public const int    LS_DINFO_GOP_SIM_ITER          =      11619;
       public const int    LS_DINFO_GOP_BAR_ITER          =      11620;
       public const int    LS_DINFO_GOP_NLP_ITER          =      11621;
       public const int    LS_DINFO_GOP_LPCOUNT           =      11622;
       public const int    LS_DINFO_GOP_NLPCOUNT          =      11623;
       public const int    LS_DINFO_GOP_MIPCOUNT          =      11624;
       public const int    LS_DINFO_GOP_BBITER            =      11625;
       public const int    LS_DINFO_GOP_SUBITER           =      11626;
       public const int    LS_DINFO_GOP_MIPBRANCH         =      11627;
       public const int    LS_DINFO_GOP_FIRST_TIME        =      11628;
       public const int    LS_DINFO_GOP_BEST_TIME         =      11629;
       public const int    LS_DINFO_GOP_TOT_TIME          =      11630;
       public const int    LS_IINFO_GOP_THREADS           =      11631;
       public const int    LS_SINFO_GOP_THREAD_LOAD       =      11632;
       public const int    LS_DINFO_GOP_ABSGAP            =      11633;
       public const int    LS_DINFO_GOP_RELGAP            =      11634;

    /* Progress info during callbacks */
       public const int    LS_DINFO_SUB_OBJ               =      11700;
       public const int    LS_DINFO_SUB_PINF              =      11701;
       public const int    LS_DINFO_CUR_OBJ               =      11702;
       public const int    LS_IINFO_CUR_ITER              =      11703;
       public const int    LS_DINFO_CUR_BEST_BOUND        =      11704;
       public const int    LS_IINFO_CUR_STATUS            =      11705;
       public const int    LS_IINFO_CUR_LP_COUNT          =      11706;
       public const int    LS_IINFO_CUR_BRANCH_COUNT      =      11707;
       public const int    LS_IINFO_CUR_ACTIVE_COUNT      =      11708;
       public const int    LS_IINFO_CUR_NLP_COUNT         =      11709;
       public const int    LS_IINFO_CUR_MIP_COUNT         =      11710;
       public const int    LS_IINFO_CUR_CUT_COUNT         =      11711;
       public const int    LS_DINFO_CUR_ITER              =      11712;

 /* Model generation progress info (1800+)*/
       public const int    LS_DINFO_GEN_PERCENT           =      11800;
       public const int    LS_IINFO_GEN_NONZ_TTL          =      11801;
       public const int    LS_IINFO_GEN_NONZ_NL           =      11802;
       public const int    LS_IINFO_GEN_ROW_NL            =      11803;
       public const int    LS_IINFO_GEN_VAR_NL            =      11804;

 /* IIS-IUS info */
       public const int    LS_IINFO_IIS_BAR_ITER          =      11850;
       public const int    LS_IINFO_IIS_SIM_ITER          =      11851;
       public const int    LS_IINFO_IIS_NLP_ITER          =      11852;
       public const int    LS_DINFO_IIS_BAR_ITER          =      11853;
       public const int    LS_DINFO_IIS_SIM_ITER          =      11854;
       public const int    LS_DINFO_IIS_NLP_ITER          =      11855;
       public const int    LS_IINFO_IIS_TOT_TIME          =      11856;
       public const int    LS_IINFO_IIS_ACT_NODE          =      11857;
       public const int    LS_IINFO_IIS_LPCOUNT           =      11858;
       public const int    LS_IINFO_IIS_NLPCOUNT          =      11859;
       public const int    LS_IINFO_IIS_MIPCOUNT          =      11860;
       public const int    LS_IINFO_IIS_THREADS           =      11861;
       public const int    LS_SINFO_IIS_THREAD_LOAD       =      11862;
       public const int    LS_IINFO_IIS_STATUS            =      11863;
       public const int    LS_IINFO_IUS_BAR_ITER          =      11875;
       public const int    LS_IINFO_IUS_SIM_ITER          =      11876;
       public const int    LS_IINFO_IUS_NLP_ITER          =      11877;
       public const int    LS_DINFO_IUS_BAR_ITER          =      11878;
       public const int    LS_DINFO_IUS_SIM_ITER          =      11879;
       public const int    LS_DINFO_IUS_NLP_ITER          =      11880;
       public const int    LS_IINFO_IUS_TOT_TIME          =      11881;
       public const int    LS_IINFO_IUS_ACT_NODE          =      11882;
       public const int    LS_IINFO_IUS_LPCOUNT           =      11883;
       public const int    LS_IINFO_IUS_NLPCOUNT          =      11884;
       public const int    LS_IINFO_IUS_MIPCOUNT          =      11885;
       public const int    LS_IINFO_IUS_THREADS           =      11886;
       public const int    LS_SINFO_IUS_THREAD_LOAD       =      11887;
       public const int    LS_IINFO_IUS_STATUS            =      11888;

 /* Presolve info    */
       public const int    LS_IINFO_PRE_NUM_RED           =      11900;
       public const int    LS_IINFO_PRE_TYPE_RED          =      11901;
       public const int    LS_IINFO_PRE_NUM_RDCONS        =      11902;
       public const int    LS_IINFO_PRE_NUM_RDVARS        =      11903;
       public const int    LS_IINFO_PRE_NUM_RDNONZ        =      11904;
       public const int    LS_IINFO_PRE_NUM_RDINT         =      11905;

 /* Error info */
       public const int    LS_IINFO_ERR_OPTIM             =      11999;

    /* IIS Profiler */
       public const int    LS_DINFO_PROFILE_BASE          =      12000;
       public const int    LS_DINFO_PROFILE_IIS_FIND_NEC_ROWS =      12050;
       public const int    LS_DINFO_PROFILE_IIS_FIND_NEC_COLS =      12051;
       public const int    LS_DINFO_PROFILE_IIS_FIND_SUF_ROWS =      12052;
       public const int    LS_DINFO_PROFILE_IIS_FIND_SUF_COLS =      12053;

    /* MIP Profiler */
       public const int    LS_DINFO_PROFILE_MIP_ROOT_LP   =      12101;
       public const int    LS_DINFO_PROFILE_MIP_TOTAL_LP  =      12102;
       public const int    LS_DINFO_PROFILE_MIP_LP_SIM_PRIMAL =      12103;
       public const int    LS_DINFO_PROFILE_MIP_LP_SIM_DUAL =      12104;
       public const int    LS_DINFO_PROFILE_MIP_LP_SIM_BARRIER =      12105;
       public const int    LS_DINFO_PROFILE_MIP_PRE_PROCESS =      12106;
       public const int    LS_DINFO_PROFILE_MIP_FEA_PUMP  =      12107;
       public const int    LS_DINFO_PROFILE_MIP_TOP_HEURISTIC =      12108;
       public const int    LS_DINFO_PROFILE_MIP_BNB_HEURISTIC =      12109;
       public const int    LS_DINFO_PROFILE_MIP_BNB_MAIN_LOOP =      12110;
       public const int    LS_DINFO_PROFILE_MIP_BNB_SUB_LOOP =      12111;
       public const int    LS_DINFO_PROFILE_MIP_BNB_BEFORE_BEST =      12112;
       public const int    LS_DINFO_PROFILE_MIP_BNB_AFTER_BEST =      12113;
       public const int    LS_DINFO_PROFILE_MIP_TOP_CUT   =      12114;
       public const int    LS_DINFO_PROFILE_MIP_BNB_CUT   =      12115;
       public const int    LS_DINFO_PROFILE_MIP_LP_NON_BNB_LOOP =      12116;
       public const int    LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_MAIN =      12117;
       public const int    LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_SUB =      12118;
       public const int    LS_DINFO_PROFILE_MIP_NODE_PRESOLVE =      12119;
       public const int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING =      12120;
       public const int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING_MAIN =      12121;
       public const int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING_SUB =      12122;

    /* GOP Profiler */
       public const int    LS_DINFO_PROFILE_GOP_SUB_LP_SOLVER =      12251;
       public const int    LS_DINFO_PROFILE_GOP_SUB_NLP_SOLVER =      12252;
       public const int    LS_DINFO_PROFILE_GOP_SUB_MIP_SOLVER =      12253;
       public const int    LS_DINFO_PROFILE_GOP_SUB_GOP_SOLVER =      12254;
       public const int    LS_DINFO_PROFILE_GOP_CONS_PROP_LP =      12255;
       public const int    LS_DINFO_PROFILE_GOP_CONS_PROP_NLP =      12256;
       public const int    LS_DINFO_PROFILE_GOP_VAR_MIN_MAX =      12257;

 /* Misc info */
       public const int    LS_SINFO_MODEL_FILENAME        =      12950;
       public const int    LS_SINFO_MODEL_SOURCE          =      12951;
       public const int    LS_IINFO_MODEL_TYPE            =      12952;
       public const int    LS_SINFO_CORE_FILENAME         =      12953;
       public const int    LS_SINFO_STOC_FILENAME         =      12954;
       public const int    LS_SINFO_TIME_FILENAME         =      12955;
       public const int    LS_IINFO_ASSIGNED_MODEL_TYPE   =      12956;
       public const int    LS_IINFO_NZCINDEX              =      12957;
       public const int    LS_IINFO_NZRINDEX              =      12958;
       public const int    LS_IINFO_NZCRANK               =      12959;
       public const int    LS_IINFO_NZRRANK               =      12960;

   /*! Expected value of the objective function.  */
       public const int    LS_DINFO_STOC_EVOBJ            =      13201;

    /*! Expected value of perfect information.  */
       public const int    LS_DINFO_STOC_EVPI             =      13202;

    /*! Primal infeasibility of the first stage solution.  */
       public const int    LS_DINFO_STOC_PINFEAS          =      13203;

    /*! Dual infeasibility of the first stage solution.  */
       public const int    LS_DINFO_STOC_DINFEAS          =      13204;

    /*! Relative optimality gap at current solution.  */
       public const int    LS_DINFO_STOC_RELOPT_GAP       =      13205;

    /*! Absolute optimality gap at current solution.  */
       public const int    LS_DINFO_STOC_ABSOPT_GAP       =      13206;

    /*! Number of simplex iterations performed.  */
       public const int    LS_IINFO_STOC_SIM_ITER         =      13207;

    /*! Number of barrier iterations performed.  */
       public const int    LS_IINFO_STOC_BAR_ITER         =      13208;

    /*! Number of nonlinear iterations performed.  */
       public const int    LS_IINFO_STOC_NLP_ITER         =      13209;

    /*! Number of stochastic parameters in the RHS.  */
       public const int    LS_IINFO_NUM_STOCPAR_RHS       =      13210;

    /*! Number of stochastic parameters in the objective function.  */
       public const int    LS_IINFO_NUM_STOCPAR_OBJ       =      13211;

    /*! Number of stochastic parameters in the lower bound.  */
       public const int    LS_IINFO_NUM_STOCPAR_LB        =      13212;

    /*! Number of stochastic parameters in the upper bound.  */
       public const int    LS_IINFO_NUM_STOCPAR_UB        =      13213;

    /*! Number of stochastic parameters in the instructions constituting the objective.  */
       public const int    LS_IINFO_NUM_STOCPAR_INSTR_OBJS =      13214;

    /*! Number of stochastic parameters in the instructions constituting the constraints.  */
       public const int    LS_IINFO_NUM_STOCPAR_INSTR_CONS =      13215;

    /*! Number of stochastic parameters in the LP matrix.  */
       public const int    LS_IINFO_NUM_STOCPAR_AIJ       =      13216;

    /*! Total time elapsed in seconds to solve the model  */
       public const int    LS_DINFO_STOC_TOTAL_TIME       =      13217;

    /*! Status of the SP model.  */
       public const int    LS_IINFO_STOC_STATUS           =      13218;

    /*! Stage of the specified node.  */
       public const int    LS_IINFO_STOC_STAGE_BY_NODE    =      13219;

    /*! Number of scenarios (integer) in the scenario tree. */
       public const int    LS_IINFO_STOC_NUM_SCENARIOS    =      13220;

    /*! Number of scenarios (double) in the scenario tree. */
       public const int    LS_DINFO_STOC_NUM_SCENARIOS    =      13221;

    /*! Number of stages in the model. */
       public const int    LS_IINFO_STOC_NUM_STAGES       =      13222;

    /*! Number of nodes in the scenario tree (integer). */
       public const int    LS_IINFO_STOC_NUM_NODES        =      13223;

    /*! Number of nodes in the scenario tree (double). */
       public const int    LS_DINFO_STOC_NUM_NODES        =      13224;

    /*! Number of nodes that belong to specified stage in the scenario tree (integer). */
       public const int    LS_IINFO_STOC_NUM_NODES_STAGE  =      13225;

    /*! Number of nodes that belong to specified stage in the scenario tree (double). */
       public const int    LS_DINFO_STOC_NUM_NODES_STAGE  =      13226;

    /*! Number of node-models created or to be created. */
       public const int    LS_IINFO_STOC_NUM_NODE_MODELS  =      13227;

    /*! Column offset in DEQ of the first variable associated with the specified node.  */
       public const int    LS_IINFO_STOC_NUM_COLS_BEFORE_NODE =      13228;

    /*! Row offset in DEQ of the first variable associated with the specified node. */
       public const int    LS_IINFO_STOC_NUM_ROWS_BEFORE_NODE =      13229;

    /*! Total number of columns in the implicit DEQ (integer). */
       public const int    LS_IINFO_STOC_NUM_COLS_DETEQI  =      13230;

    /*! Total number of columns in the implicit DEQ (double). */
       public const int    LS_DINFO_STOC_NUM_COLS_DETEQI  =      13231;

    /*! Total number of rows in the implicit DEQ (integer). */
       public const int    LS_IINFO_STOC_NUM_ROWS_DETEQI  =      13232;

    /*! Total number of rows in the implicit DEQ (double). */
       public const int    LS_DINFO_STOC_NUM_ROWS_DETEQI  =      13233;

    /*! Total number of columns in the explict DEQ (integer). */
       public const int    LS_IINFO_STOC_NUM_COLS_DETEQE  =      13234;

    /*! Total number of columns in the explict DEQ (double). */
       public const int    LS_DINFO_STOC_NUM_COLS_DETEQE  =      13235;

    /*! Total number of rows in the explict DEQ (integer). */
       public const int    LS_IINFO_STOC_NUM_ROWS_DETEQE  =      13236;

    /*! Total number of rows in the explict DEQ (double). */
       public const int    LS_DINFO_STOC_NUM_ROWS_DETEQE  =      13237;

    /*! Total number of columns in non-anticipativity block. */
       public const int    LS_IINFO_STOC_NUM_COLS_NAC     =      13238;

    /*! Total number of rows in non-anticipativity block. */
       public const int    LS_IINFO_STOC_NUM_ROWS_NAC     =      13239;

    /*! Total number of columns in core model. */
       public const int    LS_IINFO_STOC_NUM_COLS_CORE    =      13240;

    /*! Total number of rows in core model. */
       public const int    LS_IINFO_STOC_NUM_ROWS_CORE    =      13241;

    /*! Total number of columns in core model in the specified stage. */
       public const int    LS_IINFO_STOC_NUM_COLS_STAGE   =      13242;

    /*! Total number of rows in core model in the specified stage. */
       public const int    LS_IINFO_STOC_NUM_ROWS_STAGE   =      13243;

    /*! Total number of feasibility cuts generated during NBD iterations. */
       public const int    LS_IINFO_STOC_NUM_NBF_CUTS     =      13244;

    /*! Total number of optimality cuts generated during NBD iterations. */
       public const int    LS_IINFO_STOC_NUM_NBO_CUTS     =      13245;

    /*! Distribution type of the sample */
       public const int    LS_IINFO_DIST_TYPE             =      13246;

    /*! Sample size. */
       public const int    LS_IINFO_SAMP_SIZE             =      13247;

    /*! Sample mean. */
       public const int    LS_DINFO_SAMP_MEAN             =      13248;

    /*! Sample standard deviation. */
       public const int    LS_DINFO_SAMP_STD              =      13249;

    /*! Sample skewness. */
       public const int    LS_DINFO_SAMP_SKEWNESS         =      13250;

    /*! Sample kurtosis. */
       public const int    LS_DINFO_SAMP_KURTOSIS         =      13251;

    /*! Total number of quadratic constraints in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQE =      13252;

    /*! Total number of continuous constraints in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQE =      13253;

    /*! Total number of constraints with general integer variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_CONS_DETEQE =      13254;

    /*! Total number of constraints with binary variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQE =      13255;

    /*! Total number of quadratic variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQE =      13256;

    /*! Total number of nonzeros in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NONZ_DETEQE  =      13259;

    /*! Total number of binaries in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_DETEQE   =      13260;

    /*! Total number of general integer variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_DETEQE   =      13261;

    /*! Total number of continuous variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_DETEQE  =      13262;

    /*! Total number of quadratic nonzeros in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQE =      13263;

    /*! Total number of nonlinear nonzeros in the constraints of explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQE =      13264;

    /*! Total number of nonlinear nonzeros in the objective function of explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQE =      13265;

    /*! Total number of quadratic constraints in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQI =      13266;

    /*! Total number of continuous constraints in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQI =      13267;

    /*! Total number of constraints with general integer variables in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_CONS_DETEQI =      13268;

    /*! Total number of constraints with binary variables in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQI =      13269;

    /*! Total number of quadratic variables in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQI =      13270;

    /*! Total number of nonzeros in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NONZ_DETEQI  =      13271;

    /*! Total number of binaries in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_DETEQI   =      13272;

    /*! Total number of general integer variables in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_DETEQI   =      13273;

    /*! Total number of continuous variables in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_DETEQI  =      13274;

    /*! Total number of quadratic nonzeros in the implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQI =      13275;

    /*! Total number of nonlinear nonzeros in the constraints of implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQI =      13276;

    /*! Total number of nonlinear nonzeros in the objective function of implicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQI =      13277;

    /*! Total number of block events. */
       public const int    LS_IINFO_STOC_NUM_EVENTS_BLOCK =      13278;

    /*! Total number of independent events with a discrete distribution. */
       public const int    LS_IINFO_STOC_NUM_EVENTS_DISCRETE =      13279;

    /*! Total number of independent events with a parametric distribution. */
       public const int    LS_IINFO_STOC_NUM_EVENTS_PARAMETRIC =      13280;

    /*! Total number of events loaded explictly as a scenario */
       public const int    LS_IINFO_STOC_NUM_EXPLICIT_SCENARIOS =      13281;

    /*! Index of a node's parent*/
       public const int    LS_IINFO_STOC_PARENT_NODE      =      13282;

    /*! Index of a node's eldest child*/
       public const int    LS_IINFO_STOC_ELDEST_CHILD_NODE =      13283;

    /*! Total number of childs a node has */
       public const int    LS_IINFO_STOC_NUM_CHILD_NODES  =      13284;

    /*! Number of stochastic parameters in the instruction list.  */
       public const int    LS_IINFO_NUM_STOCPAR_INSTR     =      13285;

    /*! The index of the scenario which is infeasible or unbounded.  */
       public const int    LS_IINFO_INFORUNB_SCEN_IDX     =      13286;

    /*! Expected value of modeling uncertainity.  */
       public const int    LS_DINFO_STOC_EVMU             =      13287;

    /*! Expected value of wait-and-see model's objective.  */
       public const int    LS_DINFO_STOC_EVWS             =      13288;

    /*! Expected value of the objective based on average model's first-stage optimal decisions.  */
       public const int    LS_DINFO_STOC_EVAVR            =      13289;

    /*! Number of arguments of a distribution sample.  */
       public const int    LS_IINFO_DIST_NARG             =      13290;

    /*! Variance reduction/control method used in generating the sample.  */
       public const int    LS_IINFO_SAMP_VARCONTROL_METHOD =      13291;

    /*! Total number of nonlinear variables in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_VARS_DETEQE =      13292;

    /*! Total number of nonlinear constraints in the explicit deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_CONS_DETEQE =      13293;

    /*! Best lower bound on expected value of the objective function.  */
       public const int    LS_DINFO_STOC_EVOBJ_LB         =      13294;

    /*! Best upper bound on expected value of the objective function.  */
       public const int    LS_DINFO_STOC_EVOBJ_UB         =      13295;

    /*! Expected value of average model's objective.  */
       public const int    LS_DINFO_STOC_AVROBJ           =      13296;

    /*! Sample median. */
       public const int    LS_DINFO_SAMP_MEDIAN           =      13297;

    /*! Distribution (population) median. */
       public const int    LS_DINFO_DIST_MEDIAN           =      13298;

    /*! Number of chance-constraints. */
       public const int    LS_IINFO_STOC_NUM_CC           =      13299;

    /*! Number of rows in chance-constraints. */
       public const int    LS_IINFO_STOC_NUM_ROWS_CC      =      13300;

    /*! Internal. */
       public const int    LS_IINFO_STOC_ISCBACK          =      13301;

    /*! Total number of LPs solved. */
       public const int    LS_IINFO_STOC_LP_COUNT         =      13302;

    /*! Total number of NLPs solved. */
       public const int    LS_IINFO_STOC_NLP_COUNT        =      13303;

    /*! Total number of MILPs solved. */
       public const int    LS_IINFO_STOC_MIP_COUNT        =      13304;

    /*! Time elapsed in seconds in the optimizer (excluding setup)  */
       public const int    LS_DINFO_STOC_OPT_TIME         =      13305;

    /*! Difference between underlying sample's correlation (S) and target correlation (T) loaded.  */
       public const int    LS_DINFO_SAMP_CORRDIFF_ST      =      13306;

    /*! Difference between underlying sample's induced correlation (C) and target correlation (T) loaded.  */
       public const int    LS_DINFO_SAMP_CORRDIFF_CT      =      13307;

    /*! Difference between underlying sample's correlation (S) and induced correlation (C).  */
       public const int    LS_DINFO_SAMP_CORRDIFF_SC      =      13308;

    /*! Number of rows with equality type in chance-constraints. */
       public const int    LS_IINFO_STOC_NUM_EQROWS_CC    =      13309;

    /*! Number of stochastic rows*/
       public const int    LS_IINFO_STOC_NUM_ROWS         =      13310;

    /*! Number of chance sets violated over all scenarios */
       public const int    LS_IINFO_STOC_NUM_CC_VIOLATED  =      13311;

    /*! Total number of columns in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_COLS_DETEQC  =      13312;

    /*! Total number of rows in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_ROWS_DETEQC  =      13313;

    /*! Total number of quadratic constraints in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQC =      13314;

    /*! Total number of continuous constraints in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQC =      13315;

    /*! Total number of constraints with general integer variables in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_CONS_DETEQC =      13316;

    /*! Total number of constraints with binary variables in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQC =      13317;

    /*! Total number of quadratic variables in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQC =      13318;

    /*! Total number of nonzeros in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NONZ_DETEQC  =      13319;

    /*! Total number of binaries in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_BIN_DETEQC   =      13320;

    /*! Total number of general integer variables in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_INT_DETEQC   =      13321;

    /*! Total number of continuous variables in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_CONT_DETEQC  =      13322;

    /*! Total number of quadratic nonzeros in the chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQC =      13323;

    /*! Total number of nonlinear nonzeros in the constraints of chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQC =      13324;

    /*! Total number of nonlinear nonzeros in the objective function of chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQC =      13325;

    /*! Total number of nonlinear constraints in the constraints of chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_CONS_DETEQC =      13326;

    /*! Total number of nonlinear variables in the constraints of chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NLP_VARS_DETEQC =      13327;

    /*! Total number of nonzeros in the objective of chance deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQC =      13328;

    /*! Total number of nonzeros in the objective of explict deterministic equivalent. */
       public const int    LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQE =      13329;

    /*! p-level for chance constraint */
       public const int    LS_DINFO_STOC_CC_PLEVEL        =      13340;

    /*! Number of parallel threads used */
       public const int    LS_IINFO_STOC_THREADS          =      13341;

    /*! Work imbalance across threads */
       public const int    LS_DINFO_STOC_THRIMBL          =      13342;

    /*! Number of EQ type stochastic rows*/
       public const int    LS_IINFO_STOC_NUM_EQROWS       =      13343;

    /*! Thread workloads */
       public const int    LS_SINFO_STOC_THREAD_LOAD      =      13344;

    /*! Number of buckets */
       public const int    LS_IINFO_STOC_NUM_BUCKETS      =      13345;

    /*BNP information*/
       public const int    LS_IINFO_BNP_SIM_ITER          =      14000;
       public const int    LS_IINFO_BNP_LPCOUNT           =      14001;
       public const int    LS_IINFO_BNP_NUMCOL            =      14002;
       public const int    LS_DINFO_BNP_BESTBOUND         =      14003;
       public const int    LS_DINFO_BNP_BESTOBJ           =      14004;

 /* Error codes (2001-2299) */
       public const int    LSERR_NO_ERROR                 =       0000;
       public const int    LSERR_OUT_OF_MEMORY            =       2001;
       public const int    LSERR_CANNOT_OPEN_FILE         =       2002;
       public const int    LSERR_BAD_MPS_FILE             =       2003;
       public const int    LSERR_BAD_CONSTRAINT_TYPE      =       2004;
       public const int    LSERR_BAD_MODEL                =       2005;
       public const int    LSERR_BAD_SOLVER_TYPE          =       2006;
       public const int    LSERR_BAD_OBJECTIVE_SENSE      =       2007;
       public const int    LSERR_BAD_MPI_FILE             =       2008;
       public const int    LSERR_INFO_NOT_AVAILABLE       =       2009;
       public const int    LSERR_ILLEGAL_NULL_POINTER     =       2010;
       public const int    LSERR_UNABLE_TO_SET_PARAM      =       2011;
       public const int    LSERR_INDEX_OUT_OF_RANGE       =       2012;
       public const int    LSERR_ERRMSG_FILE_NOT_FOUND    =       2013;
       public const int    LSERR_VARIABLE_NOT_FOUND       =       2014;
       public const int    LSERR_INTERNAL_ERROR           =       2015;
       public const int    LSERR_ITER_LIMIT               =       2016;
       public const int    LSERR_TIME_LIMIT               =       2017;
       public const int    LSERR_NOT_CONVEX               =       2018;
       public const int    LSERR_NUMERIC_INSTABILITY      =       2019;
       public const int    LSERR_STEP_TOO_SMALL           =       2021;
       public const int    LSERR_USER_INTERRUPT           =       2023;
       public const int    LSERR_PARAMETER_OUT_OF_RANGE   =       2024;
       public const int    LSERR_ERROR_IN_INPUT           =       2025;
       public const int    LSERR_TOO_SMALL_LICENSE        =       2026;
       public const int    LSERR_NO_VALID_LICENSE         =       2027;
       public const int    LSERR_NO_METHOD_LICENSE        =       2028;
       public const int    LSERR_NOT_SUPPORTED            =       2029;
       public const int    LSERR_MODEL_ALREADY_LOADED     =       2030;
       public const int    LSERR_MODEL_NOT_LOADED         =       2031;
       public const int    LSERR_INDEX_DUPLICATE          =       2032;
       public const int    LSERR_INSTRUCT_NOT_LOADED      =       2033;
       public const int    LSERR_OLD_LICENSE              =       2034;
       public const int    LSERR_NO_LICENSE_FILE          =       2035;
       public const int    LSERR_BAD_LICENSE_FILE         =       2036;
       public const int    LSERR_MIP_BRANCH_LIMIT         =       2037;
       public const int    LSERR_GOP_FUNC_NOT_SUPPORTED   =       2038;
       public const int    LSERR_GOP_BRANCH_LIMIT         =       2039;
       public const int    LSERR_BAD_DECOMPOSITION_TYPE   =       2040;
       public const int    LSERR_BAD_VARIABLE_TYPE        =       2041;
       public const int    LSERR_BASIS_BOUND_MISMATCH     =       2042;
       public const int    LSERR_BASIS_COL_STATUS         =       2043;
       public const int    LSERR_BASIS_INVALID            =       2044;
       public const int    LSERR_BASIS_ROW_STATUS         =       2045;
       public const int    LSERR_BLOCK_OF_BLOCK           =       2046;
       public const int    LSERR_BOUND_OUT_OF_RANGE       =       2047;
       public const int    LSERR_COL_BEGIN_INDEX          =       2048;
       public const int    LSERR_COL_INDEX_OUT_OF_RANGE   =       2049;
       public const int    LSERR_COL_NONZCOUNT            =       2050;
       public const int    LSERR_INVALID_ERRORCODE        =       2051;
       public const int    LSERR_ROW_INDEX_OUT_OF_RANGE   =       2052;
       public const int    LSERR_TOTAL_NONZCOUNT          =       2053;
       public const int    LSERR_MODEL_NOT_LINEAR         =       2054;
       public const int    LSERR_CHECKSUM                 =       2055;
       public const int    LSERR_USER_FUNCTION_NOT_FOUND  =       2056;
       public const int    LSERR_TRUNCATED_NAME_DATA      =       2057;
       public const int    LSERR_ILLEGAL_STRING_OPERATION =       2058;
       public const int    LSERR_STRING_ALREADY_LOADED    =       2059;
       public const int    LSERR_STRING_NOT_LOADED        =       2060;
       public const int    LSERR_STRING_LENGTH_LIMIT      =       2061;
       public const int    LSERR_DATA_TERM_EXIST          =       2062;
       public const int    LSERR_NOT_SORTED_ORDER         =       2063;
       public const int    LSERR_INST_MISS_ELEMENTS       =       2064;
       public const int    LSERR_INST_TOO_SHORT           =       2065;
       public const int    LSERR_INST_INVALID_BOUND       =       2066;
       public const int    LSERR_INST_SYNTAX_ERROR        =       2067;
       public const int    LSERR_COL_TOKEN_NOT_FOUND      =       2068;
       public const int    LSERR_ROW_TOKEN_NOT_FOUND      =       2069;
       public const int    LSERR_NAME_TOKEN_NOT_FOUND     =       2070;
       public const int    LSERR_NOT_LSQ_MODEL            =       2071;
       public const int    LSERR_INCOMPATBLE_DECOMPOSITION =       2072;
       public const int    LSERR_NO_MULTITHREAD_SUPPORT   =       2073;
       public const int    LSERR_INVALID_PARAMID          =       2074;
       public const int    LSERR_INVALID_NTHREADS         =       2075;
       public const int    LSERR_COL_LIMIT                =       2076;
       public const int    LSERR_QCDATA_NOT_LOADED        =       2077;
       public const int    LSERR_NO_QCDATA_IN_ROW         =       2078;
       public const int    LSERR_CLOCK_SETBACK            =       2079;
       public const int    LSERR_XSOLVER_LOAD             =       2080;
       public const int    LSERR_XSOLVER_NO_FILENAME      =       2081;
       public const int    LSERR_XSOLVER_ALREADY_LOADED   =       2082;
       public const int    LSERR_XSOLVER_FUNC_NOT_INSTALLED =       2083;
       public const int    LSERR_XSOLVER_LIB_NOT_INSTALLED =       2084;
       public const int    LSERR_ZLIB_LOAD                =       2085;
       public const int    LSERR_XSOLVER_ENV_NOT_CREATED  =       2086;
       public const int    LSERR_SOLPOOL_EMPTY            =       2087;
       public const int    LSERR_SOLPOOL_FULL             =       2088;
       public const int    LSERR_SOL_LIMIT                =       2089;
       public const int    LSERR_TUNER_NOT_SETUP          =       2090;

    /* Error in LDLt factorization */
       public const int    LSERR_LDL_FACTORIZATION        =       2201;

    /* Empty column detected in LDLt factorization */
       public const int    LSERR_LDL_EMPTY_COL            =       2202;

    /* Matrix data is invalid or has bad input in LDLt factorization */
       public const int    LSERR_LDL_BAD_MATRIX_DATA      =       2203;

    /* Invalid matrix or vector dimension */
       public const int    LSERR_LDL_INVALID_DIM          =       2204;

    /* Matrix or vector is empty */
       public const int    LSERR_LDL_EMPTY_MATRIX         =       2205;

    /* Matrix is not symmetric */
       public const int    LSERR_LDL_MATRIX_NOTSYM        =       2206;

    /* Matrix has zero diagonal */
       public const int    LSERR_LDL_ZERO_DIAG            =       2207;

    /* Invalid permutation */
       public const int    LSERR_LDL_INVALID_PERM         =       2208;

    /* Duplicate elements detected in LDLt factorization */
       public const int    LSERR_LDL_DUPELEM              =       2209;

    /* Detected rank deficiency in LDLt factorization */
       public const int    LSERR_LDL_RANK                 =       2210;

   /*! Core MPS file/model has an error */
       public const int    LSERR_BAD_SMPS_CORE_FILE       =       2301;

    /*! Time file/model has an error */
       public const int    LSERR_BAD_SMPS_TIME_FILE       =       2302;

    /*! Stoc file/model has an error */
       public const int    LSERR_BAD_SMPS_STOC_FILE       =       2303;

    /*! Core MPI file/model has an error */
       public const int    LSERR_BAD_SMPI_CORE_FILE       =       2304;

    /*! Stoc file associated with Core MPI file has an error */
       public const int    LSERR_BAD_SMPI_STOC_FILE       =       2305;

    /*! Unable to open Core file */
       public const int    LSERR_CANNOT_OPEN_CORE_FILE    =       2306;

    /*! Unable to open Time file */
       public const int    LSERR_CANNOT_OPEN_TIME_FILE    =       2307;

    /*! Unable to open Stoc file */
       public const int    LSERR_CANNOT_OPEN_STOC_FILE    =       2308;

    /*! Stochastic model/data has not been loaded yet. */
       public const int    LSERR_STOC_MODEL_NOT_LOADED    =       2309;

    /*! Stochastic parameter specified in Stoc file has not been found . */
       public const int    LSERR_STOC_SPAR_NOT_FOUND      =       2310;

    /*! Stochastic parameter specified in Time file has not been found . */
       public const int    LSERR_TIME_SPAR_NOT_FOUND      =       2311;

    /*! Specified scenario index is out of sequence */
       public const int    LSERR_SCEN_INDEX_OUT_OF_SEQUENCE =       2312;

    /*! Stochastic model/data has already been loaded. */
       public const int    LSERR_STOC_MODEL_ALREADY_PARSED =       2313;

    /*! Specified scenario CDF is invalid, e.g. scenario probabilities don't sum to 1.0*/
       public const int    LSERR_STOC_INVALID_SCENARIO_CDF =       2314;

    /*! No stochastic parameters was found in the Core file */
       public const int    LSERR_CORE_SPAR_NOT_FOUND      =       2315;

    /*! Number of stochastic parameters found in Core file don't match to that of Time file */
       public const int    LSERR_CORE_SPAR_COUNT_MISMATCH =       2316;

    /*! Specified stochastic parameter index is invalid */
       public const int    LSERR_CORE_INVALID_SPAR_INDEX  =       2317;

    /*! A stochastic parameter was not expected in Time file. */
       public const int    LSERR_TIME_SPAR_NOT_EXPECTED   =       2318;

    /*! Number of stochastic parameters found in Time file don't match to that of Stoc file */
       public const int    LSERR_TIME_SPAR_COUNT_MISMATCH =       2319;

    /*! Specified stochastic parameter doesn't have a valid outcome value */
       public const int    LSERR_CORE_SPAR_VALUE_NOT_FOUND =       2320;

    /*! Requested information is unavailable */
       public const int    LSERR_INFO_UNAVAILABLE         =       2321;

    /*! Core file doesn't have a valid bound name tag */
       public const int    LSERR_STOC_MISSING_BNDNAME     =       2322;

    /*! Core file doesn't have a valid objective name tag */
       public const int    LSERR_STOC_MISSING_OBJNAME     =       2323;

    /*! Core file doesn't have a valid right-hand-side name tag */
       public const int    LSERR_STOC_MISSING_RHSNAME     =       2324;

    /*! Core file doesn't have a valid range name tag */
       public const int    LSERR_STOC_MISSING_RNGNAME     =       2325;

    /*! Stoc file doesn't have an expected token name. */
       public const int    LSERR_MISSING_TOKEN_NAME       =       2326;

    /*! Stoc file doesn't have a 'ROOT' token to specify a root scenario */
       public const int    LSERR_MISSING_TOKEN_ROOT       =       2327;

    /*! Node model is unbounded */
       public const int    LSERR_STOC_NODE_UNBOUNDED      =       2328;

    /*! Node model is infeasible */
       public const int    LSERR_STOC_NODE_INFEASIBLE     =       2329;

    /*! Stochastic model has too many scenarios to solve with specified solver */
       public const int    LSERR_STOC_TOO_MANY_SCENARIOS  =       2330;

    /*! One or more node-models have irrecoverable numerical problems */
       public const int    LSERR_STOC_BAD_PRECISION       =       2331;

    /*! Specified aggregation structure is not compatible with model's stage structure */
       public const int    LSERR_CORE_BAD_AGGREGATION     =       2332;

    /*! Event tree is either not initialized yet or was too big to create */
       public const int    LSERR_STOC_NULL_EVENT_TREE     =       2333;

    /*! Specified stage index is invalid */
       public const int    LSERR_CORE_BAD_STAGE_INDEX     =       2334;

    /*! Specified algorithm/method is invalid or not supported */
       public const int    LSERR_STOC_BAD_ALGORITHM       =       2335;

    /*! Specified number of stages in Core model is invalid */
       public const int    LSERR_CORE_BAD_NUMSTAGES       =       2336;

    /*! Underlying model has an invalid temporal order */
       public const int    LSERR_TIME_BAD_TEMPORAL_ORDER  =       2337;

    /*! Number of stages specified in Time structure is invalid */
       public const int    LSERR_TIME_BAD_NUMSTAGES       =       2338;

    /*! Core and Time data are inconsistent */
       public const int    LSERR_CORE_TIME_MISMATCH       =       2339;

    /*! Specified stochastic structure has an invalid CDF */
       public const int    LSERR_STOC_INVALID_CDF         =       2340;

    /*! Specified distribution type is invalid or not supported. */
       public const int    LSERR_BAD_DISTRIBUTION_TYPE    =       2341;

    /*! Scale parameter for specified distribution is out of range. */
       public const int    LSERR_DIST_SCALE_OUT_OF_RANGE  =       2342;

    /*! Shape parameter for specified distribution is out of range. */
       public const int    LSERR_DIST_SHAPE_OUT_OF_RANGE  =       2343;

    /*! Specified probabability value is invalid */
       public const int    LSERR_DIST_INVALID_PROBABILITY =       2344;

    /*! Derivative information is unavailable */
       public const int    LSERR_DIST_NO_DERIVATIVE       =       2345;

    /*! Specified standard deviation is invalid */
       public const int    LSERR_DIST_INVALID_SD          =       2346;

    /*! Specified value is invalid */
       public const int    LSERR_DIST_INVALID_X           =       2347;

    /*! Specified parameters are invalid for the given distribution. */
       public const int    LSERR_DIST_INVALID_PARAMS      =       2348;

    /*! Iteration limit has been reached during a root finding operation */
       public const int    LSERR_DIST_ROOTER_ITERLIM      =       2349;

    /*! Given array is out of bounds */
       public const int    LSERR_ARRAY_OUT_OF_BOUNDS      =       2350;

    /*! Limiting PDF does not exist */
       public const int    LSERR_DIST_NO_PDF_LIMIT        =       2351;

    /*! A random number generator is not set. */
       public const int    LSERR_RG_NOT_SET               =       2352;

    /*! Distribution function value was truncated during calculations */
       public const int    LSERR_DIST_TRUNCATED           =       2353;

    /*! Stoc file has a parameter value missing */
       public const int    LSERR_STOC_MISSING_PARAM_TOKEN =       2354;

    /*! Distribution has invalid number of parameters */
       public const int    LSERR_DIST_INVALID_NUMPARAM    =       2355;

    /*! Core file/model is not in temporal order */
       public const int    LSERR_CORE_NOT_IN_TEMPORAL_ORDER =       2357;

    /*! Specified sample size is invalid */
       public const int    LSERR_STOC_INVALID_SAMPLE_SIZE =       2358;

    /*! Node probability cannot be computed due to presence of continuous stochastic parameters */
       public const int    LSERR_STOC_NOT_DISCRETE        =       2359;

    /*! Event tree exceeds the maximum number of scenarios allowed to attempt an exact solution.*/
       public const int    LSERR_STOC_SCENARIO_LIMIT      =       2360;

    /*! Specified correlation type is invalid */
       public const int    LSERR_DIST_BAD_CORRELATION_TYPE =       2361;

    /*! Number of stages in the model is not set yet. */
       public const int    LSERR_TIME_NUMSTAGES_NOT_SET   =       2362;

    /*! Model already contains a sampled tree */
       public const int    LSERR_STOC_SAMPLE_ALREADY_LOADED =       2363;

    /*! Stochastic events are not loaded yet */
       public const int    LSERR_STOC_EVENTS_NOT_LOADED   =       2364;

    /*! Stochastic tree already initialized */
       public const int    LSERR_STOC_TREE_ALREADY_INIT   =       2365;

    /*! Random number generator seed not initialized */
       public const int    LSERR_RG_SEED_NOT_SET          =       2366;

    /*! All sample points in the sample has been used. Resampling may be required. */
       public const int    LSERR_STOC_OUT_OF_SAMPLE_POINTS =       2367;

    /*! Sampling is not supported for models with explicit scenarios. */
       public const int    LSERR_STOC_SCENARIO_SAMPLING_NOT_SUPPORTED =       2368;

    /*! Sample points are not yet generated for a stochastic parameter. */
       public const int    LSERR_STOC_SAMPLE_NOT_GENERATED =       2369;

    /*! Sample points are already generated for a stochastic parameter. */
       public const int    LSERR_STOC_SAMPLE_ALREADY_GENERATED =       2370;

    /*! Sample sizes selected are too small. */
       public const int    LSERR_STOC_SAMPLE_SIZE_TOO_SMALL =       2371;

    /*! A random number generator is already set. */
       public const int    LSERR_RG_ALREADY_SET           =       2372;

    /*! Sampling is not allowed for block/joint distributions. */
       public const int    LSERR_STOC_BLOCK_SAMPLING_NOT_SUPPORTED =       2373;

    /*! No stochastic parameters were assigned to one of the stages. */
       public const int    LSERR_EMPTY_SPAR_STAGE         =       2374;

    /*! No rows were assigned to one of the stages. */
       public const int    LSERR_EMPTY_ROW_STAGE          =       2375;

    /*! No columns were assigned to one of the stages. */
       public const int    LSERR_EMPTY_COL_STAGE          =       2376;

    /*! Default sample sizes per stoc.pars and stage are in conflict. */
       public const int    LSERR_STOC_CONFLICTING_SAMP_SIZES =       2377;

    /*! Empty scenario data */
       public const int    LSERR_STOC_EMPTY_SCENARIO_DATA =       2378;

    /*! A correlation structure has not been induced yet */
       public const int    LSERR_STOC_CORRELATION_NOT_INDUCED =       2379;

    /*! A discrete PDF table has not been loaded */
       public const int    LSERR_STOC_PDF_TABLE_NOT_LOADED =       2380;

    /*! No continously distributed random parameters are found */
       public const int    LSERR_STOC_NO_CONTINUOUS_SPAR_FOUND =       2381;

    /*! One or more rows already belong to another chance constraint */
       public const int    LSERR_STOC_ROW_ALREADY_IN_CC   =       2382;

    /*! No chance-constraints were loaded */
       public const int    LSERR_STOC_CC_NOT_LOADED       =       2383;

    /*! Cut limit has been reached */
       public const int    LSERR_STOC_CUT_LIMIT           =       2384;

    /*! GA object has not been initialized yet */
       public const int    LSERR_STOC_GA_NOT_INIT         =       2385;

    /*! There exists stochastic rows not loaded to any chance constraints yet.*/
       public const int    LSERR_STOC_ROWS_NOT_LOADED_IN_CC =       2386;

    /*! Specified sample is already assigned as the source for the target sample. */
       public const int    LSERR_SAMP_ALREADY_SOURCE      =       2387;

    /*! No user-defined distribution function has been set for the specified sample. */
       public const int    LSERR_SAMP_USERFUNC_NOT_SET    =       2388;

    /*! Specified sample does not support the function call or it is incompatible with the argument list. */
       public const int    LSERR_SAMP_INVALID_CALL        =       2389;

    /*! Mapping stochastic instructions leads to multiple occurrences in matrix model. */
       public const int    LSERR_STOC_MAP_MULTI_SPAR      =       2390;

    /*! Two or more stochastic instructions maps to the same position in matrix model. */
       public const int    LSERR_STOC_MAP_SAME_SPAR       =       2391;

    /*! A stochastic parameter was not expected in the objective function. */
       public const int    LSERR_STOC_SPAR_NOT_EXPECTED_OBJ =       2392;

    /*! One of the distribution parameters of the specified sample was not set. */
       public const int    LSERR_DIST_PARAM_NOT_SET       =       2393;

    /*! Specified stochastic input is invalid. */
       public const int    LSERR_STOC_INVALID_INPUT       =       2394;

    /* Error codes for the sprint method. */
       public const int    LSERR_SPRINT_MISSING_TAG_ROWS  =       2577;
       public const int    LSERR_SPRINT_MISSING_TAG_COLS  =       2578;
       public const int    LSERR_SPRINT_MISSING_TAG_RHS   =       2579;
       public const int    LSERR_SPRINT_MISSING_TAG_ENDATA =       2580;
       public const int    LSERR_SPRINT_MISSING_VALUE_ROW =       2581;
       public const int    LSERR_SPRINT_EXTRA_VALUE_ROW   =       2582;
       public const int    LSERR_SPRINT_MISSING_VALUE_COL =       2583;
       public const int    LSERR_SPRINT_EXTRA_VALUE_COL   =       2584;
       public const int    LSERR_SPRINT_MISSING_VALUE_RHS =       2585;
       public const int    LSERR_SPRINT_EXTRA_VALUE_RHS   =       2586;
       public const int    LSERR_SPRINT_MISSING_VALUE_BOUND =       2587;
       public const int    LSERR_SPRINT_EXTRA_VALUE_BOUND =       2588;
       public const int    LSERR_SPRINT_INTEGER_VARS_IN_MPS =       2589;
       public const int    LSERR_SPRINT_BINARY_VARS_IN_MPS =       2590;
       public const int    LSERR_SPRINT_SEMI_CONT_VARS_IN_MPS =       2591;
       public const int    LSERR_SPRINT_UNKNOWN_TAG_BOUNDS =       2592;
       public const int    LSERR_SPRINT_MULTIPLE_OBJ_ROWS =       2593;
       public const int    LSERR_SPRINT_COULD_NOT_SOLVE_SUBPROBLEM =       2594;
       public const int    LSERR_COULD_NOT_WRITE_TO_FILE  =       2595;
       public const int    LSERR_COULD_NOT_READ_FROM_FILE =       2596;
       public const int    LSERR_READING_PAST_EOF         =       2597;

    /* Error codes associated with scripting 2700-2750 */
       public const int    LSERR_SCRIPT                   =       2700;

    /*! @} */
       public const int    LSERR_LAST_ERROR               =       2598;

 /* Callback locations */
       public const int    LSLOC_PRIMAL                   =          0;
       public const int    LSLOC_DUAL                     =          1;
       public const int    LSLOC_BARRIER                  =          2;
       public const int    LSLOC_CROSSOVER                =          3;
       public const int    LSLOC_CONOPT                   =          4;
       public const int    LSLOC_MIP                      =          5;
       public const int    LSLOC_LOCAL_OPT                =          6;
       public const int    LSLOC_GEN_START                =          7;
       public const int    LSLOC_GEN_PROCESSING           =          8;
       public const int    LSLOC_GEN_END                  =          9;
       public const int    LSLOC_GOP                      =         10;
       public const int    LSLOC_EXIT_SOLVER              =         11;
       public const int    LSLOC_PRESOLVE                 =         12;
       public const int    LSLOC_MSW                      =         13;
       public const int    LSLOC_FUNC_CALC                =         14;
       public const int    LSLOC_IISIUS                   =         15;
       public const int    LSLOC_SP                       =         16;
       public const int    LSLOC_GEN_SP_START             =         17;
       public const int    LSLOC_GEN_SP                   =         18;
       public const int    LSLOC_GEN_SP_END               =         19;
       public const int    LSLOC_SP_WS                    =         20;
       public const int    LSLOC_LSQ                      =         21;
       public const int    LSLOC_SP_WS_START              =         22;
       public const int    LSLOC_SP_WS_END                =         23;
       public const int    LSLOC_SP_BENCH_START           =         24;
       public const int    LSLOC_SP_BENCH_END             =         25;
       public const int    LSLOC_BNP                      =         26;
       public const int    LS_METHOD_FREE                 =          0;
       public const int    LS_METHOD_PSIMPLEX             =          1;
       public const int    LS_METHOD_DSIMPLEX             =          2;
       public const int    LS_METHOD_BARRIER              =          3;
       public const int    LS_METHOD_NLP                  =          4;
       public const int    LS_METHOD_MIP                  =          5;
       public const int    LS_METHOD_MULTIS               =          6;
       public const int    LS_METHOD_GOP                  =          7;
       public const int    LS_METHOD_IIS                  =          8;
       public const int    LS_METHOD_IUS                  =          9;
       public const int    LS_METHOD_SBD                  =         10;
       public const int    LS_METHOD_SPRINT               =         11;
       public const int    LS_METHOD_GA                   =         12;
       public const int    LS_STRATEGY_USER               =          0;
       public const int    LS_STRATEGY_PRIMIP             =          1;
       public const int    LS_STRATEGY_NODEMIP            =          2;
       public const int    LS_STRATEGY_HEUMIP             =          3;
       public const int    LS_NMETHOD_FREE                =          4;
       public const int    LS_NMETHOD_LSQ                 =          5;
       public const int    LS_NMETHOD_QP                  =          6;
       public const int    LS_NMETHOD_CONOPT              =          7;
       public const int    LS_NMETHOD_SLP                 =          8;
       public const int    LS_NMETHOD_MSW_GRG             =          9;
       public const int    LS_NMETHOD_IPOPT               =         10;
       public const int    LS_PROB_SOLVE_FREE             =          0;
       public const int    LS_PROB_SOLVE_PRIMAL           =          1;
       public const int    LS_PROB_SOLVE_DUAL             =          2;
       public const int    LS_BAR_METHOD_FREE             =          4;
       public const int    LS_BAR_METHOD_INTPNT           =          5;
       public const int    LS_BAR_METHOD_CONIC            =          6;
       public const int    LS_BAR_METHOD_QCONE            =          7;
       public const int    LSSOL_BASIC_PRIMAL             =         11;
       public const int    LSSOL_BASIC_DUAL               =         12;
       public const int    LSSOL_BASIC_SLACK              =         13;
       public const int    LSSOL_BASIC_REDCOST            =         14;
       public const int    LSSOL_INTERIOR_PRIMAL          =         15;
       public const int    LSSOL_INTERIOR_DUAL            =         16;
       public const int    LSSOL_INTERIOR_SLACK           =         17;
       public const int    LSSOL_INTERIOR_REDCOST         =         18;

    /* linear programs                          */
       public const int    LS_LP                          =         10;

    /* quadratic programs                       */
       public const int    LS_QP                          =         11;

    /* conic programs                           */
       public const int    LS_SOCP                        =         12;

    /* semidefinite programs                    */
       public const int    LS_SDP                         =         13;

    /* nonlinear programs                       */
       public const int    LS_NLP                         =         14;

    /* mixed-integer linear programs            */
       public const int    LS_MILP                        =         15;

    /* mixed-integer quadratic programs         */
       public const int    LS_MIQP                        =         16;

    /* mixed-integer conic programs             */
       public const int    LS_MISOCP                      =         17;

    /* mixed-integer semidefinite programs      */
       public const int    LS_MISDP                       =         18;

    /* mixed-integer nonlinear programs         */
       public const int    LS_MINLP                       =         19;

    /* convex QP */
       public const int    LS_CONVEX_QP                   =         20;

    /*convex NLP */
       public const int    LS_CONVEX_NLP                  =         21;

    /*convex MIQP */
       public const int    LS_CONVEX_MIQP                 =         22;

    /*convex MINLP */
       public const int    LS_CONVEX_MINLP                =         23;

    /* undetermined   */
       public const int    LS_UNDETERMINED                =         -1;
       public const int    LS_LINK_BLOCKS_FREE            =          0;
       public const int    LS_LINK_BLOCKS_SELF            =          1;
       public const int    LS_LINK_BLOCKS_NONE            =          2;
       public const int    LS_LINK_BLOCKS_COLS            =          3;
       public const int    LS_LINK_BLOCKS_ROWS            =          4;
       public const int    LS_LINK_BLOCKS_BOTH            =          5;
       public const int    LS_LINK_BLOCKS_MATRIX          =          6;

 /* Controls the way objective function and
 * objective sense are printed when writing
 * LS_MAX type problems in MPS format.
 */
       public const int    LS_MPS_USE_MAX_NOTE            =          0;
       public const int    LS_MPS_USE_MAX_CARD            =          1;
       public const int    LS_MPS_USE_MAX_FLIP            =          2;

 /* Finite differences methods */
       public const int    LS_DERIV_FREE                  =          0;
       public const int    LS_DERIV_FORWARD_DIFFERENCE    =          1;
       public const int    LS_DERIV_BACKWARD_DIFFERENCE   =          2;
       public const int    LS_DERIV_CENTER_DIFFERENCE     =          3;

 /* MIP Sets
 *  SOS1: S={x_1,...,x_p}  only one x_j can be different from zero
 *  SOS2: S={x_1,...,x_p}  at most two x_j can be different from zero
 *                         and  when they are they have to be adjacent
 *  SOS3: S={x_1,...,x_p}  @sum(j: x_j      )  = 1;  x_j >=0,
 *  CARD: S={x_1,...,x_p}  @sum(j: sign(x_j)) <= k;  x_j >=0
 */
       public const int    LS_MIP_SET_CARD                =          4;
       public const int    LS_MIP_SET_SOS1                =          1;
       public const int    LS_MIP_SET_SOS2                =          2;
       public const int    LS_MIP_SET_SOS3                =          3;
       public const int    LS_QTERM_NONE                  =          0;
       public const int    LS_QTERM_INDEF                 =          1;
       public const int    LS_QTERM_POSDEF                =          2;
       public const int    LS_QTERM_NEGDEF                =          3;
       public const int    LS_QTERM_POS_SEMIDEF           =          4;
       public const int    LS_QTERM_NEG_SEMIDEF           =          5;

 /* Bit masks for general MIP mode. Use sums
 * to enable a collection of available levels.
 */
       public const int    LS_MIP_MODE_NO_TIME_EVENTS     =          2;
       public const int    LS_MIP_MODE_FAST_FEASIBILITY   =          4;
       public const int    LS_MIP_MODE_FAST_OPTIMALITY    =          8;
       public const int    LS_MIP_MODE_NO_BRANCH_CUTS     =         16;
       public const int    LS_MIP_MODE_NO_LP_BARRIER      =         32;

 /* Bit mask for cut generation levels. Use sums to
 * enable a collection of available cuts.
 */
       public const int    LS_MIP_GUB_COVER_CUTS          =          2;
       public const int    LS_MIP_FLOW_COVER_CUTS         =          4;
       public const int    LS_MIP_LIFT_CUTS               =          8;
       public const int    LS_MIP_PLAN_LOC_CUTS           =         16;
       public const int    LS_MIP_DISAGG_CUTS             =         32;
       public const int    LS_MIP_KNAPSUR_COVER_CUTS      =         64;
       public const int    LS_MIP_LATTICE_CUTS            =        128;
       public const int    LS_MIP_GOMORY_CUTS             =        256;
       public const int    LS_MIP_COEF_REDC_CUTS          =        512;
       public const int    LS_MIP_GCD_CUTS                =       1024;
       public const int    LS_MIP_OBJ_CUT                 =       2048;
       public const int    LS_MIP_BASIS_CUTS              =       4096;
       public const int    LS_MIP_CARDGUB_CUTS            =       8192;
       public const int    LS_MIP_DISJUN_CUTS             =      16384;

 /* Bit masks for MIP preprocessing levels. Use sums
 * to enable a collection of available levels.
 */
       public const int    LS_MIP_PREP_SPRE               =          2;
       public const int    LS_MIP_PREP_PROB               =          4;
       public const int    LS_MIP_PREP_COEF               =          8;
       public const int    LS_MIP_PREP_ELIM               =         16;
       public const int    LS_MIP_PREP_DUAL               =         32;
       public const int    LS_MIP_PREP_DBACK              =         64;
       public const int    LS_MIP_PREP_BINROWS            =        128;
       public const int    LS_MIP_PREP_AGGROWS            =        256;
       public const int    LS_MIP_PREP_COEF_LIFTING       =        512;
       public const int    LS_MIP_PREP_MAXPASS            =       1024;
       public const int    LS_MIP_PREP_SIMROW             =       2048;

 /* Bit masks for solver preprocessing levels. Use sums
 * to enable a collection of available levels.
 */
       public const int    LS_SOLVER_PREP_SPRE            =          2;
       public const int    LS_SOLVER_PREP_PFOR            =          4;
       public const int    LS_SOLVER_PREP_DFOR            =          8;
       public const int    LS_SOLVER_PREP_ELIM            =         16;
       public const int    LS_SOLVER_PREP_DCOL            =         32;
       public const int    LS_SOLVER_PREP_DROW            =         64;
       public const int    LS_SOLVER_PREP_CONE            =        128;
       public const int    LS_SOLVER_PREP_MAXPASS         =       1024;
       public const int    LS_SOLVER_PREP_DECOMP          =       4096;
       public const int    LS_SOLVER_PREP_LOWMEM          =       8192;
       public const int    LS_SOLVER_PREP_EXTERNAL        =      16384;

 /* Bit masks for IIS & IUS analysis levels. Use sums to
 * enable a collection of available levels.
 */
       public const int    LS_NECESSARY_ROWS              =          1;
       public const int    LS_NECESSARY_COLS              =          2;
       public const int    LS_SUFFICIENT_ROWS             =          4;
       public const int    LS_SUFFICIENT_COLS             =          8;
       public const int    LS_IIS_INTS                    =         16;
       public const int    LS_IISRANK_LTF                 =         32;
       public const int    LS_IISRANK_DECOMP              =         64;
       public const int    LS_IISRANK_NNZ                 =        128;
       public const int    LS_IISLIMIT_MIS                =        256;
       public const int    LS_IIS_MASK_IISCOLS            =        512;
       public const int    LS_IIS_SETS                    =       1024;

 /* Infeasibility norms for IIS finder */
       public const int    LS_IIS_NORM_FREE               =          0;
       public const int    LS_IIS_NORM_ONE                =          1;
       public const int    LS_IIS_NORM_INFINITY           =          2;

 /* IIS methods */
       public const int    LS_IIS_DEFAULT                 =          0;
       public const int    LS_IIS_DEL_FILTER              =          1;
       public const int    LS_IIS_ADD_FILTER              =          2;
       public const int    LS_IIS_GBS_FILTER              =          3;
       public const int    LS_IIS_DFBS_FILTER             =          4;
       public const int    LS_IIS_FSC_FILTER              =          5;
       public const int    LS_IIS_ELS_FILTER              =          6;

 /*codes for IINFO_MIP_WHERE_IN_CODE*/
       public const int    LS_MIP_IN_PRESOLVE             =          0;
       public const int    LS_MIP_IN_FP_MODE              =          1;
       public const int    LS_MIP_IN_HEU_MODE             =          2;
       public const int    LS_MIP_IN_ENUM                 =          3;
       public const int    LS_MIP_IN_CUT_ADD_TOP          =          4;
       public const int    LS_MIP_IN_BANDB                =          6;

/**
 * @ingroup LSstocOptDataTypes
 */
       /*! Stochastic parameter is an instruction code  */
       public const int    LS_JCOL_INST                   = -8;
       /*! Stochastic parameter is a RHS upper bound (reserved for future use)*/
       public const int    LS_JCOL_RUB                    = -7;
       /*! Stochastic parameter is a RHS lower bound (reserved for future use)*/
       public const int    LS_JCOL_RLB                    = -6;
       /*! Stochastic parameter is a RHS value (belongs to RHS column)      */
       public const int    LS_JCOL_RHS                    = -5;
       /*! Stochastic parameter is an objective coefficient (belongs to OBJ row)   */
       public const int    LS_IROW_OBJ                    = -4;
       /*! Stochastic parameter is a variable lower bound (belongs to LO row) */
       public const int    LS_IROW_VUB                    = -3;
       /*! Stochastic parameter is a variable upper bound (belongs to UP row) */
       public const int    LS_IROW_VLB                    = -2;
       /*! Stochastic parameter is a variable fixed bound (belongs to FX row) */
       public const int    LS_IROW_VFX                    = -1;
       /*! Stochastic parameter is an LP matrix entry. */
       public const int    LS_IMAT_AIJ                    =  0;

   /* discrete distributions */
       public const int    LSDIST_TYPE_BINOMIAL           =        701;
       public const int    LSDIST_TYPE_DISCRETE           =        702;
       public const int    LSDIST_TYPE_DISCRETE_BLOCK     =        703;
       public const int    LSDIST_TYPE_GEOMETRIC          =        705;
       public const int    LSDIST_TYPE_POISSON            =        706;
       public const int    LSDIST_TYPE_LOGARITHMIC        =        707;
       public const int    LSDIST_TYPE_HYPER_GEOMETRIC    =        708;
       public const int    LSDIST_TYPE_LINTRAN_BLOCK      =        709;
       public const int    LSDIST_TYPE_SUB_BLOCK          =        710;
       public const int    LSDIST_TYPE_SUB                =        711;
       public const int    LSDIST_TYPE_USER               =        712;

    /* continuous distributions */
       public const int    LSDIST_TYPE_BETA               =        801;
       public const int    LSDIST_TYPE_CAUCHY             =        802;
       public const int    LSDIST_TYPE_CHI_SQUARE         =        803;
       public const int    LSDIST_TYPE_EXPONENTIAL        =        804;
       public const int    LSDIST_TYPE_F_DISTRIBUTION     =        805;
       public const int    LSDIST_TYPE_GAMMA              =        806;
       public const int    LSDIST_TYPE_GUMBEL             =        807;
       public const int    LSDIST_TYPE_LAPLACE            =        808;
       public const int    LSDIST_TYPE_LOGNORMAL          =        809;
       public const int    LSDIST_TYPE_LOGISTIC           =        810;
       public const int    LSDIST_TYPE_NORMAL             =        811;
       public const int    LSDIST_TYPE_PARETO             =        812;
       public const int    LSDIST_TYPE_STABLE_PARETIAN    =        813;
       public const int    LSDIST_TYPE_STUDENTS_T         =        814;
       public const int    LSDIST_TYPE_TRIANGULAR         =        815;
       public const int    LSDIST_TYPE_UNIFORM            =        816;
       public const int    LSDIST_TYPE_WEIBULL            =        817;
       public const int    LSDIST_TYPE_WILCOXON           =        818;
       public const int    LSDIST_TYPE_BETABINOMIAL       =        819;
       public const int    LSDIST_TYPE_SYMMETRICSTABLE    =        820;

 /* supported operations modifying the core. */
       public const int    LS_REPLACE                     =          0;
       public const int    LS_ADD                         =          1;
       public const int    LS_SUB                         =          2;
       public const int    LS_MULTIPLY                    =          3;
       public const int    LS_DIVIDE                      =          4;

 /* scenario indices for special cases */
       public const int    LS_SCEN_ROOT                   =         -1;
       public const int    LS_SCEN_AVRG                   =         -2;
       public const int    LS_SCEN_MEDIAN                 =         -3;
       public const int    LS_SCEN_USER                   =         -4;
       public const int    LS_SCEN_NONE                   =         -5;

 /* warmstart rule in optimizing wait-see model */
       public const int    LS_WSBAS_FREE                  =         -1;
       public const int    LS_WSBAS_NONE                  =          0;
       public const int    LS_WSBAS_AVRG                  =          1;
       public const int    LS_WSBAS_LAST                  =          2;

   /*! Solve with the method chosen by the solver. */
       public const int    LS_METHOD_STOC_FREE            =         -1;

    /*! Solve the deterministic equivalent (DETEQ).  */
       public const int    LS_METHOD_STOC_DETEQ           =          0;

    /*! Solve with the Nested Benders Decomposition (NBD) method. */
       public const int    LS_METHOD_STOC_NBD             =          1;

    /*! Solve with the Augmented Lagrangian Decomposition (ALD) method. */
       public const int    LS_METHOD_STOC_ALD             =          2;

    /*! Solve with the Heuristic-Search (HS) method. */
       public const int    LS_METHOD_STOC_HS              =          4;

 /*
 * @ingroup LSstocOptDeteqType
 */
       public const int    LS_DETEQ_FREE                  =         -1;
       public const int    LS_DETEQ_IMPLICIT              =          0;
       public const int    LS_DETEQ_EXPLICIT              =          1;
       public const int    LS_DETEQ_CHANCE                =          2;

 /* Distribution functions */
       public const int    LS_USER                        =          0;
       public const int    LS_PDF                         =          1;
       public const int    LS_CDF                         =          2;
       public const int    LS_CDFINV                      =          3;
       public const int    LS_PDFDIFF                     =          4;

 /* Correlation types */
       public const int    LS_CORR_TARGET                 =         -1;
       public const int    LS_CORR_LINEAR                 =          0;
       public const int    LS_CORR_PEARSON                =          0;
       public const int    LS_CORR_KENDALL                =          1;
       public const int    LS_CORR_SPEARMAN               =          2;

 /* Sampling types */
       public const int    LS_MONTECARLO                  =          0;
       public const int    LS_LATINSQUARE                 =          1;
       public const int    LS_ANTITHETIC                  =          2;

 /* Random number generator algorithms */
       public const int    LS_RANDGEN_FREE                =         -1;
       public const int    LS_RANDGEN_SYSTEM              =          0;
       public const int    LS_RANDGEN_LINDO1              =          1;
       public const int    LS_RANDGEN_LINDO2              =          2;
       public const int    LS_RANDGEN_LIN1                =          3;
       public const int    LS_RANDGEN_MULT1               =          4;
       public const int    LS_RANDGEN_MULT2               =          5;
       public const int    LS_RANDGEN_MERSENNE            =          6;

 /* NCM methods */
       public const int    LS_NCM_GA                      =          2;
       public const int    LS_NCM_ALTP                    =          4;
       public const int    LS_NCM_L2NORM_CONE             =          8;
       public const int    LS_NCM_L2NORM_NLP              =         16;

 /* pointer types used */
       public const int    LS_PTR_ENV                     =          0;
       public const int    LS_PTR_MODEL                   =          1;
       public const int    LS_PTR_SAMPLE                  =          2;
       public const int    LS_PTR_RG                      =          3;

 /* multithreading mode */
       public const int    LS_MTMODE_FREE                 =         -1;
       public const int    LS_MTMODE_EXPLCT               =          0;
       public const int    LS_MTMODE_PPCC                 =          1;
       public const int    LS_MTMODE_PP                   =          2;
       public const int    LS_MTMODE_CCPP                 =          3;
       public const int    LS_MTMODE_CC                   =          4;

 /* Output file types created by the Sprint code*/
       public const int    LS_SPRINT_OUTPUT_FILE_FREE     =          0;
       public const int    LS_SPRINT_OUTPUT_FILE_BIN      =          1;
       public const int    LS_SPRINT_OUTPUT_FILE_TXT      =          2;

   /*! scan for basic solutions for pool */
       public const int    LS_SOLVER_MODE_POOLBAS         =          1;

   /*! scan for edge solutions for pool */
       public const int    LS_SOLVER_MODE_POOLEDGE        =          2;

   /*! scan for integer basic solutions */
       public const int    LS_SOLVER_MODE_INTBAS          =          4;

 /* Equivalences */
       public const int    LS_IINFO_OBJSENSE              = LS_IPARAM_OBJSENSE;
       public const int    LS_IINFO_VER_MAJOR             = LS_IPARAM_VER_MAJOR;
       public const int    LS_IINFO_VER_MINOR             = LS_IPARAM_VER_MINOR;
       public const int    LS_IINFO_VER_BUILD             = LS_IPARAM_VER_BUILD;
       public const int    LS_IINFO_VER_REVISION          = LS_IPARAM_VER_REVISION;

 /* Conic vs Second-Order-Cone equivalence*/
       public const int    LS_CONIC                       =    LS_SOCP;
       public const int    LS_MICONIC                     =  LS_MISOCP;

 /*********************************************************************
 *                   Conversion to version 1.x                       *
 *********************************************************************/
 /* old parameter names, changed in 12.x */
       public const int    LS_INT_PARAMETER_TYPE          = LS_INT_TYPE;
       public const int    LS_DOUBLE_PARAMETER_TYPE       = LS_DOUBLE_TYPE;

 /* old parameter names, changed in 8.x */
       public const int    LS_IPARAM_NLP_MSW_MAXREF       = LS_IPARAM_NLP_MSW_POPSIZE;
       public const int    LS_IPARAM_STOC_DEBUG_LEVEL     = LS_IPARAM_STOC_DEBUG_MASK;

 /* old parameter names changed in 6.x */
       public const int    LS_SPRINT_OUTPUT_FILE_DEFAULT  = LS_SPRINT_OUTPUT_FILE_FREE;

 /* old parameter names changed in 5.x */
       public const int    LS_IPARAM_SPLEX_SCALE          = LS_IPARAM_LP_SCALE;
       public const int    LS_IPARAM_SPLEX_ITRLMT         = LS_IPARAM_LP_ITRLMT;
       public const int    LS_IPARAM_MIP_USE_ENUM_HEU     = LS_IPARAM_MIP_ENUM_HEUMODE;
       public const int    LS_IPARAM_SOLVER_USE_CONCURRENT_OPT = LS_IPARAM_SOLVER_CONCURRENT_OPTMODE;
       public const int    LS_IPARAM_GOP_USEBNDLIM        = LS_IPARAM_GOP_BNDLIM_MODE;

 /* old parameter names changed in 4.x or older*/
       public const int    LSLOC_BANDB                    =  LSLOC_MIP;
       public const int    LS_IPARAM_ITRLMT               = LS_IPARAM_SPLEX_ITRLMT;
       public const int    LS_IPARAM_PRICING              = LS_IPARAM_SPLEX_PPRICING;
       public const int    LS_IPARAM_SCALE                = LS_IPARAM_SPLEX_SCALE;
       public const int    LS_IPARAM_TIMLMT               = LS_IPARAM_SOLVER_TIMLMT;
       public const int    LS_DPARAM_CUTOFFVAL            = LS_DPARAM_SOLVER_CUTOFFVAL;
       public const int    LS_IPARAM_RESTART              = LS_IPARAM_SOLVER_RESTART;
       public const int    LS_DPARAM_FEASTOL              = LS_DPARAM_SOLVER_FEASTOL;
       public const int    LS_IPARAM_IUSOL                = LS_IPARAM_SOLVER_IUSOL;
       public const int    LS_IPARAM_MIPTIMLIM            = LS_IPARAM_MIP_TIMLIM;
       public const int    LS_IPARAM_MIPAOPTTIMLIM        = LS_IPARAM_MIP_AOPTTIMLIM;
       public const int    LS_IPARAM_MIPPRELEVEL          = LS_IPARAM_MIP_PRELEVEL;
       public const int    LS_IPARAM_MIPNODESELRULE       = LS_IPARAM_MIP_NODESELRULE;
       public const int    LS_DPARAM_MIPINTTOL            = LS_DPARAM_MIP_INTTOL;
       public const int    LS_DPARAM_MIPRELINTTOL         = LS_DPARAM_MIP_RELINTTOL;
       public const int    LS_DPARAM_MIP_OPTTOL           = LS_DPARAM_MIP_RELOPTTOL;
       public const int    LS_DPARAM_MIPOPTTOL            = LS_DPARAM_MIP_OPTTOL;
       public const int    LS_DPARAM_MIPPEROPTTOL         = LS_DPARAM_MIP_PEROPTTOL;
       public const int    LS_IPARAM_MIPMAXCUTPASS        = LS_IPARAM_MIP_MAXCUTPASS_TOP;
       public const int    LS_DPARAM_MIPADDCUTPER         = LS_DPARAM_MIP_ADDCUTPER;
       public const int    LS_IPARAM_MIPCUTLEVEL          = LS_IPARAM_MIP_CUTLEVEL_TOP;
       public const int    LS_IPARAM_MIPHEULEVEL          = LS_IPARAM_MIP_HEULEVEL;
       public const int    LS_IPARAM_MIPPRINTLEVEL        = LS_IPARAM_MIP_PRINTLEVEL;
       public const int    LS_IPARAM_MIPPREPRINTLEVEL     = LS_IPARAM_MIP_PREPRINTLEVEL;
       public const int    LS_DPARAM_MIPCUTOFFOBJ         = LS_DPARAM_MIP_CUTOFFOBJ;
       public const int    LS_IPARAM_MIPSTRONGBRANCHLEVEL = LS_IPARAM_MIP_STRONGBRANCHLEVEL;
       public const int    LS_IPARAM_MIPBRANCHDIR         = LS_IPARAM_MIP_BRANCHDIR;
       public const int    LS_IPARAM_MIPTOPOPT            = LS_IPARAM_MIP_TOPOPT;
       public const int    LS_IPARAM_MIPREOPT             = LS_IPARAM_MIP_REOPT;
       public const int    LS_IPARAM_MIPSOLVERTYPE        = LS_IPARAM_MIP_SOLVERTYPE;
       public const int    LS_IPARAM_MIPKEEPINMEM         = LS_IPARAM_MIP_KEEPINMEM;
       public const int    LS_DPARAM_MIP_REDCOSTFIXING_CUTOFF = LS_DPARAM_MIP_REDCOSTFIX_CUTOFF;
       public const int    LS_IPARAM_NLPPRINTLEVEL        = LS_IPARAM_NLP_PRINTLEVEL;
       public const int    LS_IPARAM_LPPRINTLEVEL         = LS_IPARAM_LP_PRINTLEVEL;
       public const int    LS_IPARAM_NLPSOLVER            = LS_IPARAM_NLP_SOLVER;
       public const int    LS_IPARAM_MODEL_CONVEX_FLAG    = LS_IPARAM_NLP_CONVEX;
       public const int    LS_IPARAM_NLP_SOLVEASLP        = LS_IPARAM_NLP_SOLVE_AS_LP;
       public const int    LS_DINFO_MIPBESTBOUND          = LS_DINFO_MIP_BESTBOUND;
       public const int    LS_IINFO_MIPBRANCHCOUNT        = LS_IINFO_MIP_BRANCHCOUNT;
       public const int    LS_IINFO_MIPSTATUS             = LS_IINFO_MIP_STATUS;
       public const int    LS_IINFO_MIPNEWIPSOL           = LS_IINFO_MIP_NEWIPSOL;
       public const int    LS_IINFO_MIPLPCOUNT            = LS_IINFO_MIP_LPCOUNT;
       public const int    LS_IINFO_MIPACTIVENODES        = LS_IINFO_MIP_ACTIVENODES;
       public const int    LS_IINFO_MIPLTYPE              = LS_IINFO_MIP_LTYPE;
       public const int    LS_IINFO_MIPAOPTTIMETOSTOP     = LS_IINFO_MIP_AOPTTIMETOSTOP;
       public const int    LS_DINFO_MIPOBJ                = LS_DINFO_MIP_OBJ;
       public const int    LS_IPARAM_BARRIER_PROB_TO_SOLVE = LS_IPARAM_PROB_TO_SOLVE;
       public const int    LS_IINFO_STATUS                = LS_IINFO_PRIMAL_STATUS;
       public const int    LS_GOPSOLSTAT_GLOBAL_OPTIMAL   = LS_STATUS_OPTIMAL;
       public const int    LS_GOPSOLSTAT_LOCAL_OPTIMAL    = LS_STATUS_LOCAL_OPTIMAL;
       public const int    LS_GOPSOLSTAT_INFEASIBLE       = LS_STATUS_INFEASIBLE;
       public const int    LS_GOPSOLSTAT_TOPUNBOUNDED     = LS_STATUS_UNBOUNDED;
       public const int    LS_GOPSOLSTAT_FEASIBLE         = LS_STATUS_FEASIBLE;
       public const int    LS_GOPSOLSTAT_UNKNOWN          = LS_STATUS_UNKNOWN;
       public const int    LS_GOPSOLSTAT_NUMERICAL_ERROR  = LS_STATUS_NUMERICAL_ERROR;
       public const int    LS_IIS_NORM_NONE               = LS_IIS_NORM_FREE;
       public const int    LS_IPARAM_STOC_SAMPLING_METHOD = LS_IPARAM_STOC_VARCONTROL_METHOD;
       public const int    LS_DPARAM_GOP_OPTTOL           = LS_DPARAM_GOP_RELOPTTOL;

 /* old operator names */
       public const int    EP_EXT_AND                     =    EP_VAND;
       public const int    EP_EXT_OR                      =     EP_VOR;
       public const int    EP_MULTMULT                    =   EP_VMULT;
       public const int    EP_PUSH_SVAR                   = EP_PUSH_SPAR;

 /*********************************************************************
 * Structure Creation and Deletion Routines (4)                      *
 *********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LScreateEnv")]
      public static extern IntPtr LScreateEnv 
      (                                     ref          int        pnErrorcode         ,
                                                      string        pszPassword         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScreateModel")]
      public static extern IntPtr LScreateModel 
      (                                               IntPtr               pEnv         ,
                                            ref          int        pnErrorcode         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteEnv")]
      public static extern int LSdeleteEnv 
      (                                     ref       IntPtr               pEnv         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteModel")]
      public static extern int LSdeleteModel 
      (                                     ref       IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadLicenseString")]
      public static extern int LSloadLicenseString 
      (                                               string           pszFname         ,
                                                StringBuilder        pachLicense         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVersionInfo")]
      public static extern void LSgetVersionInfo 
      (                                         StringBuilder         pachVernum         ,
                                                StringBuilder       pachBuildDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScopyParam")]
      public static extern int LScopyParam 
      (                                               IntPtr        sourceModel         ,
                                                      IntPtr        targetModel         ,
                                                         int        mSolverType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetXSolverLibrary")]
      public static extern int LSsetXSolverLibrary 
      (                                               IntPtr               pEnv         ,
                                                         int          mVendorId         ,
                                                      string          szLibrary         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetXSolverLibrary")]
      public static extern int LSgetXSolverLibrary 
      (                                               IntPtr               pEnv         ,
                                                         int          mVendorId         ,
                                                  StringBuilder          szLibrary         );

 /**********************************************************************
 * Model I-O Routines (13)                                            *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadMPSFile")]
      public static extern int LSreadMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteMPSFile")]
      public static extern int LSwriteMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadLINDOFile")]
      public static extern int LSreadLINDOFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteLINDOFile")]
      public static extern int LSwriteLINDOFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadLINDOStream")]
      public static extern int LSreadLINDOStream 
      (                                               IntPtr             pModel         ,
                                                      string          pszStream         ,
                                                         int         nStreamLen         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteLINGOFile")]
      public static extern int LSwriteLINGOFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteDualMPSFile")]
      public static extern int LSwriteDualMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         ,
                                                         int          nObjSense         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteDualLINDOFile")]
      public static extern int LSwriteDualLINDOFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int          nObjSense         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteSolution")]
      public static extern int LSwriteSolution 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteNLSolution")]
      public static extern int LSwriteNLSolution 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteSolutionOfType")]
      public static extern int LSwriteSolutionOfType 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteIIS")]
      public static extern int LSwriteIIS 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteIUS")]
      public static extern int LSwriteIUS 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadMPIFile")]
      public static extern int LSreadMPIFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteMPIFile")]
      public static extern int LSwriteMPIFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteMPXFile")]
      public static extern int LSwriteMPXFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int              mMask         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteWithSetsAndSC")]
      public static extern int LSwriteWithSetsAndSC 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadBasis")]
      public static extern int LSreadBasis 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteBasis")]
      public static extern int LSwriteBasis 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteVarPriorities")]
      public static extern int LSwriteVarPriorities 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                         int              nMode         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadLPFile")]
      public static extern int LSreadLPFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadLPStream")]
      public static extern int LSreadLPStream 
      (                                               IntPtr             pModel         ,
                                                      string          pszStream         ,
                                                         int         nStreamLen         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadSDPAFile")]
      public static extern int LSreadSDPAFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadCBFFile")]
      public static extern int LSreadCBFFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadMPXFile")]
      public static extern int LSreadMPXFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadMPXStream")]
      public static extern int LSreadMPXStream 
      (                                               IntPtr             pModel         ,
                                                      string          pszStream         ,
                                                         int         nStreamLen         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadNLFile")]
      public static extern int LSreadNLFile 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );

 /**********************************************************************
 * Error Handling Routines (3)                                        *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetErrorMessage")]
      public static extern int LSgetErrorMessage 
      (                                               IntPtr               pEnv         ,
                                                         int         nErrorcode         ,
                                                StringBuilder        pachMessage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetFileError")]
      public static extern int LSgetFileError 
      (                                               IntPtr             pModel         ,
                                            ref          int          pnLinenum         ,
                                                StringBuilder        pachLinetxt         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetErrorRowIndex")]
      public static extern int LSgetErrorRowIndex 
      (                                               IntPtr             pModel         ,
                                            ref          int              piRow         );

 /**********************************************************************
 * Routines for Setting and Retrieving Parameter Values (14)          *
 **********************************************************************/


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetModelParameter")]
    public static extern int LSsetModelParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                    ref int            nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetModelParameter")]
    public static extern int LSgetModelParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                    ref int            nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvParameter")]
    public static extern int LSsetEnvParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                    ref int            nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetEnvParameter")]
    public static extern int LSgetEnvParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                    ref int            nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetModelParameter")]
    public static extern int LSsetModelParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                    ref double         nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetModelParameter")]
    public static extern int LSgetModelParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                    ref double         nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvParameter")]
    public static extern int LSsetEnvParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                    ref double         nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetEnvParameter")]
    public static extern int LSgetEnvParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                    ref double         nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetModelDouParameter")]
    public static extern int LSsetModelDouParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                        double         dVal);


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetModelDouParameter")]
    public static extern int LSgetModelDouParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                            ref       double              pdVal         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetModelIntParameter")]
    public static extern int LSsetModelIntParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                        int            nVal);


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetModelIntParameter")]
    public static extern int LSgetModelIntParameter
    (                                   IntPtr         pModel,
                                        int            nParameter,
                                            ref          int              pnVal         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvDouParameter")]
    public static extern int LSsetEnvDouParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                        double         dVal);


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetEnvDouParameter")]
    public static extern int LSgetEnvDouParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                            ref       double              pdVal         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvIntParameter")]
    public static extern int LSsetEnvIntParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                        int            nVal);


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetEnvIntParameter")]
    public static extern int LSgetEnvIntParameter
    (                                   IntPtr         pEnv,
                                        int            nParameter,
                                            ref          int              pnVal         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSreadModelParameter")]
    public static extern int LSreadModelParameter
    (                                   IntPtr         pModel,
                                        string       pszFname         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSreadEnvParameter")]
    public static extern int LSreadEnvParameter
    (                                   IntPtr         pEnv,
                                                      string           pszFname         );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetFormattedInfo")]
    public static extern int LSgetFormattedInfo   
    (                                   IntPtr         pModel,    
                                        string         pszFname,
                                        StringBuilder  achStrHead,
                                        StringBuilder  achStrInfo,
                                        int            nFormatId);

      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteModelParameter")]
      public static extern int LSwriteModelParameter 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteEnvParameter")]
      public static extern int LSwriteEnvParameter 
      (                                               IntPtr               pEnv         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteParameterAsciiDoc")]
      public static extern int LSwriteParameterAsciiDoc 
      (                                               IntPtr               pEnv         ,
                                                      string        pszFileName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetIntParameterRange")]
      public static extern int LSgetIntParameterRange 
      (                                               IntPtr             pModel         ,
                                                         int         nParameter         ,
                                            ref          int           pnValMIN         ,
                                            ref          int           pnValMAX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDouParameterRange")]
      public static extern int LSgetDouParameterRange 
      (                                               IntPtr             pModel         ,
                                                         int         nParameter         ,
                                            ref       double           pdValMIN         ,
                                            ref       double           pdValMAX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetParamShortDesc")]
      public static extern int LSgetParamShortDesc 
      (                                               IntPtr               pEnv         ,
                                                         int             nParam         ,
                                                StringBuilder       pachDescription         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetParamLongDesc")]
      public static extern int LSgetParamLongDesc 
      (                                               IntPtr               pEnv         ,
                                                         int             nParam         ,
                                                StringBuilder       pachDescription         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetParamMacroName")]
      public static extern int LSgetParamMacroName 
      (                                               IntPtr               pEnv         ,
                                                         int             nParam         ,
                                                StringBuilder          pachParam         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetParamMacroID")]
      public static extern int LSgetParamMacroID 
      (                                               IntPtr               pEnv         ,
                                                      string            szParam         ,
                                            ref          int        pnParamType         ,
                                            ref          int            pnParam         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetQCEigs")]
      public static extern int LSgetQCEigs 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                                StringBuilder          pachWhich        ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padEigval         ,
                                            ref       double          padEigvec         ,
                                                         int            nEigval         ,
                                                         int                ncv         ,
                                                      double               dTol         ,
                                                         int           nMaxIter         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetEigs")]
      public static extern int  LSgetEigs 
      (                                                  int               nDim         ,
                                                      string               chUL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padD         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padV         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetEigg")]
      public static extern int  LSgetEigg 
      (                                                  int               nDim         ,
                                                      string             chJOBV         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padWR         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padWI         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padVRR         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padVRI         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padVLR         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padVLI         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixTranspose")]
      public static extern int LSgetMatrixTranspose 
      (                                                  int              nRows         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padAT         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixInverse")]
      public static extern int LSgetMatrixInverse 
      (                                                  int              nRows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padAinv         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixInverseSY")]
      public static extern int LSgetMatrixInverseSY 
      (                                                  int              nRows         ,
                                                      string             chUpLo         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padAinv         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixLUFactor")]
      public static extern int LSgetMatrixLUFactor 
      (                                                  int              nRows         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []         panP         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixQRFactor")]
      public static extern int LSgetMatrixQRFactor 
      (                                                  int              nRows         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padQ         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padR         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixDeterminant")]
      public static extern int LSgetMatrixDeterminant 
      (                                                  int              nRows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padDet         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixCholFactor")]
      public static extern int LSgetMatrixCholFactor 
      (                                                  int              nRows         ,
                                                      string             chUpLo         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padUL         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixSVDFactor")]
      public static extern int LSgetMatrixSVDFactor 
      (                                                  int              nRows         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padS         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padVT         ,
                                            ref          int             pnInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixFSolve")]
      public static extern int LSgetMatrixFSolve 
      (                                               string             szuplo         ,
                                                      string            sztrans         ,
                                                      string             szdiag         ,
                                                         int              nRows         ,
                                                      double             dAlpha         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixBSolve")]
      public static extern int LSgetMatrixBSolve 
      (                                               string             szuplo         ,
                                                      string            sztrans         ,
                                                      string             szdiag         ,
                                                         int              nRows         ,
                                                      double             dAlpha         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMatrixSolve")]
      public static extern int LSgetMatrixSolve 
      (                                               string             szside         ,
                                                      string             szuplo         ,
                                                      string            sztrans         ,
                                                      string             szdiag         ,
                                                         int              nRows         ,
                                                         int               nRHS         ,
                                                      double             dAlpha         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padA         ,
                                                         int               nLDA         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
                                                         int               nLDB         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );

  /*********************************************************************
  * Model Loading Routines (9)                                        *
  *********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadLPData")]
      public static extern int LSloadLPData 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
                                                         int              nVars         ,
                                                         int          dObjSense         ,
                                                      double          dObjConst         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padC         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
                                                      string        pszConTypes         ,
                                                         int              nAnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padAcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadQCData")]
      public static extern int LSloadQCData 
      (                                               IntPtr             pModel         ,
                                                         int             nQCnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiQCrows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadConeData")]
      public static extern int LSloadConeData 
      (                                               IntPtr             pModel         ,
                                                         int              nCone         ,
                                                      string       pszConeTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padConeAlpha         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiConebegcone         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiConecols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadPOSDData")]
      public static extern int LSloadPOSDData 
      (                                               IntPtr             pModel         ,
                                                         int              nPOSD         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiPOSDdim         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiPOSDbeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDrowndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDcolndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDvarndx         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadALLDIFFData")]
      public static extern int LSloadALLDIFFData 
      (                                               IntPtr             pModel         ,
                                                         int           nALLDIFF         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffDim         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiAlldiffL         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiAlldiffU         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffVar         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadSETSData")]
      public static extern int LSloadSETSData 
      (                                               IntPtr             pModel         ,
                                                         int              nSETS         ,
                                                      string        pszSETStype         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiCARDnum         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiSETSbegcol         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiSETScols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadSemiContData")]
      public static extern int LSloadSemiContData 
      (                                               IntPtr             pModel         ,
                                                         int            nSCVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadVarType")]
      public static extern int LSloadVarType 
      (                                               IntPtr             pModel         ,
                                                      string        pszVarTypes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadNameData")]
      public static extern int LSloadNameData 
      (                                               IntPtr             pModel         ,
                                                      string           pszTitle         ,
                                                      string         pszObjName         ,
                                                      string         pszRhsName         ,
                                                      string         pszRngName         ,
                                                      string         pszBndname         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszConNames         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszVarNames         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszConeNames         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadNLPData")]
      public static extern int LSloadNLPData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiNLPcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panNLPcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padNLPcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiNLProws         ,
                                                         int            nNLPobj         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiNLPobj         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNLPobj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadNLPDense")]
      public static extern int LSloadNLPDense 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
                                                         int              nVars         ,
                                                         int          dObjSense         ,
                                                      string        pszConTypes         ,
                                                      string        pszVarTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padX0         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadInstruct")]
      public static extern int LSloadInstruct 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
                                                         int              nObjs         ,
                                                         int              nVars         ,
                                                         int           nNumbers         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panObjSense         ,
                                                      string         pszConType         ,
                                                      string         pszVarType         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panInstruct         ,
                                                         int          nInstruct         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNumVal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padVarVal         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiObjBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panObjLen         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiConBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panConLen         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padLB         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padUB         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddInstruct")]
      public static extern int LSaddInstruct 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
                                                         int              nObjs         ,
                                                         int              nVars         ,
                                                         int           nNumbers         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panObjSense         ,
                                                      string         pszConType         ,
                                                      string         pszVarType         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panInstruct         ,
                                                         int          nInstruct         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNumVal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padVarVal         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiObjBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panObjLen         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiConBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panConLen         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padLB         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padUB         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadStringData")]
      public static extern int LSloadStringData 
      (                                               IntPtr             pModel         ,
                                                         int           nStrings         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszStringData         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadString")]
      public static extern int LSloadString 
      (                                               IntPtr             pModel         ,
                                                      string          pszString         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSbuildStringData")]
      public static extern int LSbuildStringData 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteStringData")]
      public static extern int LSdeleteStringData 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteString")]
      public static extern int LSdeleteString 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStringValue")]
      public static extern int LSgetStringValue 
      (                                               IntPtr             pModel         ,
                                                         int            iString         ,
                                            ref       double            pdValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintProperty")]
      public static extern int LSgetConstraintProperty 
      (                                               IntPtr             pModel         ,
                                                         int            ndxCons         ,
                                            ref          int         pnConptype         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetConstraintProperty")]
      public static extern int LSsetConstraintProperty 
      (                                               IntPtr             pModel         ,
                                                         int            ndxCons         ,
                                                         int          nConptype         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetGOPVariablePriority")]
      public static extern int LSgetGOPVariablePriority 
      (                                               IntPtr             pModel         ,
                                                         int             ndxVar         ,
                                            ref          int         pnPriority         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetGOPVariablePriority")]
      public static extern int LSsetGOPVariablePriority 
      (                                               IntPtr             pModel         ,
                                                         int             ndxVar         ,
                                                         int          nPriority         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadMultiStartSolution")]
      public static extern int LSloadMultiStartSolution 
      (                                               IntPtr             pModel         ,
                                                         int             nIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadGASolution")]
      public static extern int LSloadGASolution 
      (                                               IntPtr             pModel         ,
                                                         int             nIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddQCShift")]
      public static extern int LSaddQCShift 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                                      double             dShift         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetQCShift")]
      public static extern int LSgetQCShift 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                            ref       double            pdShift         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSresetQCShift")]
      public static extern int LSresetQCShift 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddObjPool")]
      public static extern int LSaddObjPool 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padC         ,
                                                         int          mObjSense         ,
                                                         int              mRank         ,
                                                      double         dRelOptTol         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSremObjPool")]
      public static extern int LSremObjPool 
      (                                               IntPtr             pModel         ,
                                                         int          nObjIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeObjPool")]
      public static extern int LSfreeObjPool 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetObjPoolInfo")]
      public static extern int LSsetObjPoolInfo 
      (                                               IntPtr             pModel         ,
                                                         int          nObjIndex         ,
                                                         int              mInfo         ,
                                                      double             dValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetObjPoolNumSol")]
      public static extern int LSgetObjPoolNumSol 
      (                                               IntPtr             pModel         ,
                                                         int          nObjIndex         ,
                                            ref          int            pNumSol         );

 /**********************************************************************
 * Solver Initialization Routines (6)                                 *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadBasis")]
      public static extern int LSloadBasis 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panCstatus         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panRstatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadVarPriorities")]
      public static extern int LSloadVarPriorities 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panCprior         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadVarPriorities")]
      public static extern int LSreadVarPriorities 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadVarStartPoint")]
      public static extern int LSloadVarStartPoint 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadVarStartPointPartial")]
      public static extern int LSloadVarStartPointPartial 
      (                                               IntPtr             pModel         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadMIPVarStartPoint")]
      public static extern int LSloadMIPVarStartPoint 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadMIPVarStartPointPartial")]
      public static extern int LSloadMIPVarStartPointPartial 
      (                                               IntPtr             pModel         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadVarStartPoint")]
      public static extern int LSreadVarStartPoint 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadBlockStructure")]
      public static extern int LSloadBlockStructure 
      (                                               IntPtr             pModel         ,
                                                         int             nBlock         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panRblock         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panCblock         ,
                                                         int              nType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadIISPriorities")]
      public static extern int LSloadIISPriorities 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panRprior         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panCprior         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadSolutionAt")]
      public static extern int LSloadSolutionAt 
      (                                               IntPtr             pModel         ,
                                                         int          nObjIndex         ,
                                                         int          nSolIndex         );

 /**********************************************************************
 * Optimization Routines (3)                                          *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSoptimize")]
      public static extern int LSoptimize 
      (                                               IntPtr             pModel         ,
                                                         int            nMethod         ,
                                            ref          int        pnSolStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveMIP")]
      public static extern int LSsolveMIP 
      (                                               IntPtr             pModel         ,
                                            ref          int       pnMIPSolStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveGOP")]
      public static extern int LSsolveGOP 
      (                                               IntPtr             pModel         ,
                                            ref          int       pnGOPSolStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSoptimizeQP")]
      public static extern int LSoptimizeQP 
      (                                               IntPtr             pModel         ,
                                            ref          int       pnQPSolStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScheckConvexity")]
      public static extern int LScheckConvexity 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveSBD")]
      public static extern int LSsolveSBD 
      (                                               IntPtr             pModel         ,
                                                         int            nStages         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panRowStage         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panColStage         ,
                                            ref          int           pnStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveMipBnp")]
      public static extern int LSsolveMipBnp 
      (                                               IntPtr             pModel         ,
                                                         int             nBlock         ,
                                                      string           pszFname         ,
                                            ref          int           pnStatus         );

  /* query general model and solver information */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetInfo")]
      public static extern int  LSgetInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref          int           pvResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetInfo")]
      public static extern int  LSgetInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref       double           pvResult         );
      [DllImport(LINDO_DLL,
      EntryPoint="LSgetProfilerInfo")]
      public static extern int LSgetProfilerInfo 
      (                                               IntPtr             pModel         ,
                                                         int           mContext         ,
                                            ref          int            pnCalls         ,
                                            ref       double       pdElapsedTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetProfilerContext")]
      public static extern string LSgetProfilerContext 
      (                                               IntPtr             pModel         ,
                                                         int           mContext         );

  /* query continous models */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetPrimalSolution")]
      public static extern int LSgetPrimalSolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDualSolution")]
      public static extern int LSgetDualSolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padDual         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetReducedCosts")]
      public static extern int LSgetReducedCosts 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padRedcosts         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetReducedCostsCone")]
      public static extern int LSgetReducedCostsCone 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padRedcosts         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSlacks")]
      public static extern int LSgetSlacks 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padSlacks         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetBasis")]
      public static extern int LSgetBasis 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panCstatus         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panRstatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSolution")]
      public static extern int LSgetSolution 
      (                                               IntPtr             pModel         ,
                                                         int             nWhich         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padVal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNextBestSol")]
      public static extern int LSgetNextBestSol 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnModStatus         );

  /* query integer models */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPPrimalSolution")]
      public static extern int LSgetMIPPrimalSolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPDualSolution")]
      public static extern int LSgetMIPDualSolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padDual         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPReducedCosts")]
      public static extern int LSgetMIPReducedCosts 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padRedcosts         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPSlacks")]
      public static extern int LSgetMIPSlacks 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padSlacks         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPBasis")]
      public static extern int LSgetMIPBasis 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panCstatus         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panRstatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNextBestMIPSol")]
      public static extern int LSgetNextBestMIPSol 
      (                                               IntPtr             pModel         ,
                                            ref          int       pnIntModStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetKBestMIPSols")]
      public static extern int LSgetKBestMIPSols 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         ,
                                                      typMIPCallback        pfMIPCallback         ,
                                                      object            pvCbData         ,
                                                         int           nMaxSols         );

  /*********************************************************************
  * Model Query Routines (13)                                         *
  *********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetLPData")]
      public static extern int LSgetLPData 
      (                                               IntPtr             pModel         ,
                                            ref          int         pdObjSense         ,
                                            ref       double         pdObjConst         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padC         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
                                                StringBuilder       pachConTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padAcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetQCData")]
      public static extern int LSgetQCData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiQCrows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetQCDatai")]
      public static extern int LSgetQCDatai 
      (                                               IntPtr             pModel         ,
                                                         int               iCon         ,
                                            ref          int            pnQCnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVarType")]
      public static extern int LSgetVarType 
      (                                               IntPtr             pModel         ,
                                                StringBuilder       pachVarTypes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVarStartPoint")]
      public static extern int LSgetVarStartPoint 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVarStartPointPartial")]
      public static extern int LSgetVarStartPointPartial 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      panCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPVarStartPointPartial")]
      public static extern int LSgetMIPVarStartPointPartial 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      panCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPVarStartPoint")]
      public static extern int LSgetMIPVarStartPoint 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSETSData")]
      public static extern int LSgetSETSData 
      (                                               IntPtr             pModel         ,
                                            ref          int            piNsets         ,
                                            ref          int             piNtnz         ,
                                                StringBuilder        pachSETtype         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiCardnum         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []       paiNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiBegset         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiVarndx         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSETSDatai")]
      public static extern int LSgetSETSDatai 
      (                                               IntPtr             pModel         ,
                                                         int               iSet         ,
                                                StringBuilder        pachSETType         ,
                                            ref          int          piCardnum         ,
                                            ref          int              piNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiVarndx         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetSETSStatei")]
      public static extern int LSsetSETSStatei 
      (                                               IntPtr             pModel         ,
                                                         int               iSet         ,
                                                         int             mState         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSemiContData")]
      public static extern int LSgetSemiContData 
      (                                               IntPtr             pModel         ,
                                            ref          int            piNvars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiVarndx         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetALLDIFFData")]
      public static extern int LSgetALLDIFFData 
      (                                               IntPtr             pModel         ,
                                            ref          int         pinALLDIFF         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffDim         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiAlldiffL         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiAlldiffU         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffVar         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetALLDIFFDatai")]
      public static extern int LSgetALLDIFFDatai 
      (                                               IntPtr             pModel         ,
                                                         int           iALLDIFF         ,
                                            ref          int       piAlldiffDim         ,
                                            ref          int         piAlldiffL         ,
                                            ref          int         piAlldiffU         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiAlldiffVar         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetPOSDData")]
      public static extern int LSgetPOSDData 
      (                                               IntPtr             pModel         ,
                                            ref          int            pinPOSD         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiPOSDdim         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiPOSDnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiPOSDbeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDrowndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDcolndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDvarndx         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetPOSDDatai")]
      public static extern int LSgetPOSDDatai 
      (                                               IntPtr             pModel         ,
                                                         int              iPOSD         ,
                                            ref          int          piPOSDdim         ,
                                            ref          int          piPOSDnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDrowndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDcolndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiPOSDvarndx         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNameData")]
      public static extern int LSgetNameData 
      (                                               IntPtr             pModel         ,
                                                StringBuilder          pachTitle         ,
                                                StringBuilder        pachObjName         ,
                                                StringBuilder        pachRhsName         ,
                                                StringBuilder        pachRngName         ,
                                                StringBuilder        pachBndname         ,
                                                StringBuilder       pachConNames         ,
                                                StringBuilder       pachConNameData      ,
                                                StringBuilder       pachVarNames         ,
                                                StringBuilder       pachVarNameData         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetLPVariableDataj")]
      public static extern int LSgetLPVariableDataj 
      (                                               IntPtr             pModel         ,
                                                         int               iVar         ,
                                                StringBuilder        pachVartype         ,
                                            ref       double                pdC         ,
                                            ref       double                pdL         ,
                                            ref       double                pdU         ,
                                            ref          int             pnAnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padAcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVariableNamej")]
      public static extern int LSgetVariableNamej 
      (                                               IntPtr             pModel         ,
                                                         int               iVar         ,
                                                StringBuilder        pachVarName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVariableIndex")]
      public static extern int LSgetVariableIndex 
      (                                               IntPtr             pModel         ,
                                                      string         pszVarName         ,
                                            ref          int              piVar         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintNamei")]
      public static extern int LSgetConstraintNamei 
      (                                               IntPtr             pModel         ,
                                                         int               iCon         ,
                                                StringBuilder        pachConName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintIndex")]
      public static extern int LSgetConstraintIndex 
      (                                               IntPtr             pModel         ,
                                                      string         pszConName         ,
                                            ref          int              piCon         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintDatai")]
      public static extern int LSgetConstraintDatai 
      (                                               IntPtr             pModel         ,
                                                         int               iCon         ,
                                                StringBuilder        pachConType         ,
                                                StringBuilder          pachIsNlp         ,
                                            ref       double                pdB         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetLPConstraintDatai")]
      public static extern int LSgetLPConstraintDatai 
      (                                               IntPtr             pModel         ,
                                                         int               iCon         ,
                                                StringBuilder        pachConType         ,
                                            ref       double                pdB         ,
                                            ref          int              pnNnz         ,
                                            ref          int              piVar         ,
                                            ref       double            pdAcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConeNamei")]
      public static extern int LSgetConeNamei 
      (                                               IntPtr             pModel         ,
                                                         int              iCone         ,
                                                StringBuilder       pachConeName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConeIndex")]
      public static extern int LSgetConeIndex 
      (                                               IntPtr             pModel         ,
                                                      string        pszConeName         ,
                                            ref          int             piCone         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConeDatai")]
      public static extern int LSgetConeDatai 
      (                                               IntPtr             pModel         ,
                                                         int              iCone         ,
                                                StringBuilder       pachConeType         ,
                                            ref       double        pdConeAlpha         ,
                                            ref          int              piNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNLPData")]
      public static extern int LSgetNLPData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiNLPcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panNLPcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padNLPcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiNLProws         ,
                                            ref          int           pnNLPobj         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiNLPobj         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNLPobj         ,
                                                StringBuilder       pachNLPConTypes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNLPConstraintDatai")]
      public static extern int LSgetNLPConstraintDatai 
      (                                               IntPtr             pModel         ,
                                                         int               iCon         ,
                                            ref          int              pnNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiNLPcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padNLPcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNLPVariableDataj")]
      public static extern int LSgetNLPVariableDataj 
      (                                               IntPtr             pModel         ,
                                                         int               iVar         ,
                                            ref          int              pnNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panNLProws         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padNLPcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNLPObjectiveData")]
      public static extern int LSgetNLPObjectiveData 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnNLPobjnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiNLPobj         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNLPobj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDualModel")]
      public static extern int LSgetDualModel 
      (                                               IntPtr             pModel         ,
                                                         int         pDualModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetInstruct")]
      public static extern int LSgetInstruct 
      (                                               IntPtr             pModel         ,
                                            ref          int         pnObjSense         ,
                                                StringBuilder        pachConType         ,
                                                StringBuilder        pachVarType         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      panCode         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padNumVal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padVarVal         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panObjBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panObjLength         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panConBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panConLength         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padLwrBnd         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padUprBnd         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScalinfeasMIPsolution")]
      public static extern int LScalinfeasMIPsolution 
      (                                               IntPtr             pModel         ,
                                            ref       double         pdIntPfeas         ,
                                            ref       double        pbConsPfeas         ,
                                            ref       double       pdPrimalMipsol         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetRoundMIPsolution")]
      public static extern int LSgetRoundMIPsolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padPrimalRound         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padObjRound         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padPfeasRound         ,
                                            ref          int           pnstatus         ,
                                                         int           iUseOpti         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDuplicateColumns")]
      public static extern int LSgetDuplicateColumns 
      (                                               IntPtr             pModel         ,
                                                         int         nCheckVals         ,
                                            ref          int             pnSets         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiSetsBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetRangeData")]
      public static extern int LSgetRangeData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padR         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetJac")]
      public static extern int   LSgetJac 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnJnonzeros         ,
                                            ref          int          pnJobjnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiJrows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiJcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padJcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetHess")]
      public static extern int  LSgetHess 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnHnonzeros         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiHrows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiHcol1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiHcol2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padHcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );

 /**********************************************************************
 *  Model Modification Routines (22)                                  *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddConstraints")]
      public static extern int LSaddConstraints 
      (                                               IntPtr             pModel         ,
                                                         int        nNumaddcons         ,
                                                      string        pszConTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszConNames         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padAcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddVariables")]
      public static extern int LSaddVariables 
      (                                               IntPtr             pModel         ,
                                                         int        nNumaddvars         ,
                                                      string        pszVarTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszVarNames         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padAcoef         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padC         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddCones")]
      public static extern int LSaddCones 
      (                                               IntPtr             pModel         ,
                                                         int              nCone         ,
                                                      string       pszConeTypes         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padConeAlpha         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszConenames         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiConebegcol         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiConecols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddSETS")]
      public static extern int  LSaddSETS 
      (                                               IntPtr             pModel         ,
                                                         int              nSETS         ,
                                                      string        pszSETStype         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiCARDnum         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiSETSbegcol         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiSETScols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddQCterms")]
      public static extern int LSaddQCterms 
      (                                               IntPtr             pModel         ,
                                                         int        nQCnonzeros         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiQCconndx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteConstraints")]
      public static extern int LSdeleteConstraints 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteCones")]
      public static extern int LSdeleteCones 
      (                                               IntPtr             pModel         ,
                                                         int             nCones         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiCones         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteSETS")]
      public static extern int LSdeleteSETS 
      (                                               IntPtr             pModel         ,
                                                         int              nSETS         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiSETS         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteSemiContVars")]
      public static extern int LSdeleteSemiContVars 
      (                                               IntPtr             pModel         ,
                                                         int            nSCVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiSCVars         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteVariables")]
      public static extern int LSdeleteVariables 
      (                                               IntPtr             pModel         ,
                                                         int              nVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteQCterms")]
      public static extern int LSdeleteQCterms 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteAj")]
      public static extern int LSdeleteAj 
      (                                               IntPtr             pModel         ,
                                                         int              iVar1         ,
                                                         int              nRows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiRows         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyLowerBounds")]
      public static extern int LSmodifyLowerBounds 
      (                                               IntPtr             pModel         ,
                                                         int              nVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyUpperBounds")]
      public static extern int LSmodifyUpperBounds 
      (                                               IntPtr             pModel         ,
                                                         int              nVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyRHS")]
      public static extern int LSmodifyRHS 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyObjective")]
      public static extern int LSmodifyObjective 
      (                                               IntPtr             pModel         ,
                                                         int              nVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padC         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyObjConstant")]
      public static extern int LSmodifyObjConstant 
      (                                               IntPtr             pModel         ,
                                                      double          dObjConst         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyAj")]
      public static extern int LSmodifyAj 
      (                                               IntPtr             pModel         ,
                                                         int              iVar1         ,
                                                         int              nRows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiRows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padAj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyCone")]
      public static extern int LSmodifyCone 
      (                                               IntPtr             pModel         ,
                                                      string          cConeType         ,
                                                         int           iConeNum         ,
                                                         int           iConeNnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  paiConeCols         ,
                                                      double         dConeAlpha         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifySET")]
      public static extern int LSmodifySET 
      (                                               IntPtr             pModel         ,
                                                      string           cSETtype         ,
                                                         int            iSETnum         ,
                                                         int            iSETnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiSETcols         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifySemiContVars")]
      public static extern int LSmodifySemiContVars 
      (                                               IntPtr             pModel         ,
                                                         int            nSCVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiSCVars         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyConstraintType")]
      public static extern int LSmodifyConstraintType 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
                                                      string        pszConTypes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSmodifyVariableType")]
      public static extern int LSmodifyVariableType 
      (                                               IntPtr             pModel         ,
                                                         int              nVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
                                                      string        pszVarTypes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddNLPAj")]
      public static extern int LSaddNLPAj 
      (                                               IntPtr             pModel         ,
                                                         int              iVar1         ,
                                                         int              nRows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiRows         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []        padAj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddNLPobj")]
      public static extern int LSaddNLPobj 
      (                                               IntPtr             pModel         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padColj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteNLPobj")]
      public static extern int LSdeleteNLPobj 
      (                                               IntPtr             pModel         ,
                                                         int              nCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         );

 /*********************************************************************
 *   Model & Solution Analysis Routines (10)                         *
 *********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintRanges")]
      public static extern int LSgetConstraintRanges 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padDec         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padInc         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetObjectiveRanges")]
      public static extern int LSgetObjectiveRanges 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padDec         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padInc         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetBoundRanges")]
      public static extern int LSgetBoundRanges 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padDec         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []       padInc         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetBestBounds")]
      public static extern int LSgetBestBounds 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padBestL         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padBestU         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfindIIS")]
      public static extern int  LSfindIIS 
      (                                               IntPtr             pModel         ,
                                                         int             nLevel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfindIUS")]
      public static extern int  LSfindIUS 
      (                                               IntPtr             pModel         ,
                                                         int             nLevel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfindBlockStructure")]
      public static extern int LSfindBlockStructure 
      (                                               IntPtr             pModel         ,
                                                         int             nBlock         ,
                                                         int              nType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdisplayBlockStructure")]
      public static extern int LSdisplayBlockStructure 
      (                                               IntPtr             pModel         ,
                                            ref          int            pnBlock         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColPos         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowPos         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetIIS")]
      public static extern int   LSgetIIS 
      (                                               IntPtr             pModel         ,
                                            ref          int            pnSuf_r         ,
                                            ref          int            pnIIS_r         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
                                            ref          int            pnSuf_c         ,
                                            ref          int            pnIIS_c         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      panBnds         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetIISInts")]
      public static extern int LSgetIISInts 
      (                                               IntPtr             pModel         ,
                                            ref          int          pnSuf_int         ,
                                            ref          int          pnIIS_int         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetIISSETs")]
      public static extern int LSgetIISSETs 
      (                                               IntPtr             pModel         ,
                                            ref          int          pnSuf_set         ,
                                            ref          int          pnIIS_set         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiSets         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetIUS")]
      public static extern int   LSgetIUS 
      (                                               IntPtr             pModel         ,
                                            ref          int              pnSuf         ,
                                            ref          int              pnIUS         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiVars         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetBlockStructure")]
      public static extern int LSgetBlockStructure 
      (                                               IntPtr             pModel         ,
                                            ref          int            pnBlock         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panRblock         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panCblock         ,
                                            ref          int             pnType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfindSymmetry")]
      public static extern IntPtr LSfindSymmetry 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnerrorcode         );
      [DllImport(LINDO_DLL,
      EntryPoint="LSdeleteSymmetry")]
      public static extern int LSdeleteSymmetry 
      (                                     ref         IntPtr          pSymInfo         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetOrbitInfo")]
      public static extern int LSgetOrbitInfo 
      (                                               IntPtr           pSymInfo         ,
                                            ref          int       pnNumGenerators         ,
                                            ref          int       pnNumOfOrbits         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []  panOrbitBeg         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panOrbits         );

 /**********************************************************************
 * Advanced Routines (6)                                              *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSdoBTRAN")]
      public static extern int  LSdoBTRAN 
      (                                               IntPtr             pModel         ,
                                            ref          int              pcYnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []         paiY         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padY         ,
                                            ref          int              pcXnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []         paiX         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdoFTRAN")]
      public static extern int  LSdoFTRAN 
      (                                               IntPtr             pModel         ,
                                            ref          int              pcYnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []         paiY         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padY         ,
                                            ref          int              pcXnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []         paiX         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );

  /* function and gradient evaluations */


      [DllImport(LINDO_DLL,
      EntryPoint="LScalcObjFunc")]
      public static extern int LScalcObjFunc 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
                                            ref       double           pdObjval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScalcConFunc")]
      public static extern int LScalcConFunc 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padSlacks         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScalcObjGrad")]
      public static extern int LScalcObjGrad 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
                                                         int           nParList         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiParList         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padParGrad         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScalcConGrad")]
      public static extern int LScalcConGrad 
      (                                               IntPtr             pModel         ,
                                                         int               irow         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
                                                         int           nParList         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiParList         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padParGrad         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScheckQterms")]
      public static extern int LScheckQterms 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSrepairQterms")]
      public static extern int LSrepairQterms 
      (                                               IntPtr             pModel         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScomputeFunction")]
      public static extern int LScomputeFunction 
      (                                                  int               inst         ,
                                            ref       double            pdInput         ,
                                            ref       double           pdOutput         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfindLtf")]
      public static extern int  LSfindLtf 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColPos         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowPos         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSapplyLtf")]
      public static extern int LSapplyLtf 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowIdx         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewColPos         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panNewRowPos         ,
                                                         int              nMode         );

 /**********************************************************************
 * Callback Management Routines (9)                                   *
 **********************************************************************/


    /* Delegate declarations */
    
    public delegate int typCallback
    (                                   IntPtr            nModel,
                                        int            nLoc,
                                        IntPtr         nvCbData );
                                        
    public delegate int typMIPCallback
    (                                   IntPtr            nModel,
                                        IntPtr         nvCbData,
                                        double         dObj,
                                        IntPtr         adPrimal);   

    public delegate int typGOPCallback
    (                                   IntPtr            nModel,
                                        IntPtr         nvCbData,
                                        double         dObj,
                                        IntPtr         adPrimal);  

    public delegate int typStrategy
    (                                   IntPtr            nModel,
                                        int            nRunId,
                                        IntPtr         nvCbData ); 
                                        
    public delegate int typFuncalc 
    (
                                        IntPtr            nModel, 
                                        IntPtr         nvCbData,
                                        int            nRow,
                                        IntPtr         adX,
                                        int            nJDiff,
                                        double         dXJBase,
                                   ref  double         dFuncVal,
                                        IntPtr         pReserved);
                                        
    public delegate int typUsercalc 
    (
                                        IntPtr            nModel, 
                                        int            nArgs,
                                        IntPtr         pdValues,                                        
                                        IntPtr         nvCbData,
                                   ref  double         dFuncVal);                                        
                                        
    public delegate int typGradcalc 
    (                                   IntPtr            nModel,
                                        IntPtr         nvCbData,
                                        int            nRow,
                                        IntPtr         adX,
                                        IntPtr         adLB,
                                        IntPtr         adUB,
                                        int            nNewPnt,
                                        int            nNPar,
                                        IntPtr         aiPartial,
                                        IntPtr         adPartial);
                                        
    public delegate int typModelLOG
    (                                   IntPtr            nModel,
                                       string        achStr,
                                       IntPtr        nvCbData);

    public delegate int typEnvLOG
    (
                                        IntPtr            nEnv,
                                       string       achStr,
                                       IntPtr       nvCbData);                                        

    public delegate int typExitFunc
    (
                                        IntPtr      pvUserData,
                                       string       achFile,
                                        int         nLine,
                                       string       achMsg);   
                                       
    public delegate int typUserDistr
    (
                                        IntPtr            pSamp,
                                       int          nFuncType,
                                       int          dInput,                   
                    ref                int          pdOutput,
                                       IntPtr       nvCbData);                                         
    // General Callback Declaration 
    
    [DllImport(LINDO_DLL,
    EntryPoint="LSsetCallback")]
    public static extern int LSsetCallback
    (                                   IntPtr            nModel,
                                        typCallback    nfCallback,
                                        IntPtr         nvCbData );

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetCallback")]
    public static extern int LSsetCallback
    (                                   IntPtr            nModel,
                                        typCallback    nfCallback,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );


 
 
    // MIP Callback Declaration 

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetMIPCallback")]
    public static extern int LSsetMIPCallback
    (                                   IntPtr            nModel,
                                        typMIPCallback nfMIPCallback,
                                        IntPtr         nvCbData );

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetMIPCallback")]
    public static extern int LSsetMIPCallback
    (                                   IntPtr            nModel,
                                        typMIPCallback nfMIPCallback,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetMIPCCStrategy")]
    public static extern int LSsetMIPCCStrategy
    (                                   IntPtr            nModel,
                                        typStrategy    nfCallback,
                                        int            nRunId,
                                        string         szParamFile,
                                        IntPtr         nvCbData );

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetMIPCCStrategy")]
    public static extern int LSsetMIPCCStrategy
    (                                   IntPtr            nModel,
                                        typStrategy    nfCallback,
                                        int            nRunId,
                                        string         szParamFile,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );

    // GOP Callback Declaration 

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetGOPCallback")]
    public static extern int LSsetGOPCallback
    (                                   IntPtr            nModel,
                                        typGOPCallback nfGOPCallback,
                                        IntPtr         nvCbData );

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetGOPCallback")]
    public static extern int LSsetGOPCallback
    (                                   IntPtr            nModel,
                                        typGOPCallback nfGOPCallback,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );

    [DllImport(LINDO_DLL,
    EntryPoint = "LSsetModelLogfunc")]
    public static extern int LSsetModelLogfunc   
    (                                   IntPtr            nModel,
                                        typModelLOG nfCallback,
     [MarshalAs(UnmanagedType.AsAny)]   object nvCbData);


    [DllImport(LINDO_DLL,
    EntryPoint = "LSsetEnvLogfunc")]
    public static extern int LSsetEnvLogfunc
    (                                   IntPtr            nEnv,
                                        typEnvLOG nfCallback,
     [MarshalAs(UnmanagedType.AsAny)]   object nvCbData);

    [DllImport(LINDO_DLL,
    EntryPoint="LSgetCallbackInfo")]
    public static extern int LSgetCallbackInfo
    (                                   IntPtr            nModel,
                                        int            nLocation,
                                        int            nQuery,
                                    ref int            nvValue );

    [DllImport(LINDO_DLL,
    EntryPoint="LSgetCallbackInfo")]
    public static extern int LSgetCallbackInfo
    (                                   IntPtr            nModel,
                                        int            nLocation,
                                        int            nQuery,
                                    ref double         nvValue );

    [DllImport(LINDO_DLL,
    EntryPoint="LSgetQuickbackInfo")]
    public static extern int LSgetQuickbackInfo
    (                                   IntPtr            nModel,
                                        int            nLocation,
                                        int            nQuery,
                                    ref int            nvValue );

    [DllImport(LINDO_DLL,
    EntryPoint="LSgetQuickbackInfo")]
    public static extern int LSgetQuickbackInfo
    (                                   IntPtr            nModel,
                                        int            nLocation,
                                        int            nQuery,
                                    ref double         nvValue );


    [DllImport(LINDO_DLL,
    EntryPoint="LSgetMIPCallbackInfo")]
    public static extern int LSgetMIPCallbackInfo
    (                                   IntPtr            nModel,
                                        int            nQuery,
                                    ref int            nvValue );

    [DllImport(LINDO_DLL,
    EntryPoint="LSgetMIPCallbackInfo")]
    public static extern int LSgetMIPCallbackInfo
    (                                   IntPtr            nModel,
                                        int            nQuery,
                                    ref double         nvValue );


 /* function evaluation routines for NLP solvers */

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetUsercalc")]
    public static extern int LSsetUsercalc
    (                                   IntPtr            nModel,
                                        typUsercalc    nfFunc,
                                        IntPtr         nvCbData );
                                        
    [DllImport(LINDO_DLL,
    EntryPoint="LSsetUsercalc")]
    public static extern int LSsetUsercalc
    (                                   IntPtr            nModel,
                                        typUsercalc    nfFunc,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );
 

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetFuncalc")]
    public static extern int LSsetFuncalc
    (                                   IntPtr            nModel,
                                        typFuncalc     nfFunc,
                                        IntPtr         nvCbData );
                                        
    [DllImport(LINDO_DLL,
    EntryPoint="LSsetFuncalc")]
    public static extern int LSsetFuncalc
    (                                   IntPtr            nModel,
                                        typFuncalc     nfFunc,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );
 


    [DllImport(LINDO_DLL,
    EntryPoint="LSsetGradcalc")]
    public static extern int LSsetGradcalc
    (                                   IntPtr            nModel,
                                        typGradcalc    nfGrad_func,
                                        IntPtr         nvCbData ,
                                        int            nLenUseGrad,
                                    ref int            nUseGrad );                                    
                                    
    [DllImport(LINDO_DLL,
    EntryPoint="LSsetGradcalc")]
    public static extern int LSsetGradcalc
    (                                   IntPtr            nModel,
                                        typGradcalc    nfGrad_func,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData,
                                        int            nLenUseGrad,
                                    ref int            nUseGrad ); 
                                    

    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvExitFunc")]
    public static extern int LSsetEnvExitFunc 
    (                                   IntPtr               pEnv ,
                                        typExitFunc     pfExitFunc ,
                                        IntPtr         nvCbData );
                                    
    [DllImport(LINDO_DLL,
    EntryPoint="LSsetEnvExitFunc")]
    public static extern int LSsetEnvExitFunc 
    (                                   IntPtr               pEnv ,
                                        typExitFunc     pfExitFunc ,
     [MarshalAs(UnmanagedType.AsAny)]   object         nvCbData );

 /**********************************************************************
 *  Memory Related Routines (7)                                       *
 **********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeSolverMemory")]
      public static extern void LSfreeSolverMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeHashMemory")]
      public static extern void LSfreeHashMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeSolutionMemory")]
      public static extern void LSfreeSolutionMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeMIPSolutionMemory")]
      public static extern void LSfreeMIPSolutionMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeGOPSolutionMemory")]
      public static extern void LSfreeGOPSolutionMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetProbAllocSizes")]
      public static extern int LSsetProbAllocSizes 
      (                                               IntPtr             pModel         ,
                                                         int       n_vars_alloc         ,
                                                         int       n_cons_alloc         ,
                                                         int         n_QC_alloc         ,
                                                         int       n_Annz_alloc         ,
                                                         int       n_Qnnz_alloc         ,
                                                         int       n_NLPnnz_alloc         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetProbNameAllocSizes")]
      public static extern int LSsetProbNameAllocSizes 
      (                                               IntPtr             pModel         ,
                                                         int       n_varname_alloc         ,
                                                         int       n_rowname_alloc         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddEmptySpacesAcolumns")]
      public static extern int LSaddEmptySpacesAcolumns 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiColnnz         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddEmptySpacesNLPAcolumns")]
      public static extern int LSaddEmptySpacesNLPAcolumns 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiColnnz         );

/*********************************************************************
 **
 **    Stochastic Programming Interface
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2017
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: lindo.cs 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/

 
 /* basic I/O routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteDeteqMPSFile")]
      public static extern int LSwriteDeteqMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszFilename         ,
                                                         int            nFormat         ,
                                                         int              iType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteDeteqLINDOFile")]
      public static extern int LSwriteDeteqLINDOFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszFilename         ,
                                                         int              iType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteSMPSFile")]
      public static extern int LSwriteSMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszCorefile         ,
                                                      string        pszTimefile         ,
                                                      string        pszStocfile         ,
                                                         int          nCoretype         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadSMPSFile")]
      public static extern int LSreadSMPSFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszCorefile         ,
                                                      string        pszTimefile         ,
                                                      string        pszStocfile         ,
                                                         int          nCoretype         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteSMPIFile")]
      public static extern int LSwriteSMPIFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszCorefile         ,
                                                      string        pszTimefile         ,
                                                      string        pszStocfile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadSMPIFile")]
      public static extern int LSreadSMPIFile 
      (                                               IntPtr             pModel         ,
                                                      string        pszCorefile         ,
                                                      string        pszTimefile         ,
                                                      string        pszStocfile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteScenarioSolutionFile")]
      public static extern int LSwriteScenarioSolutionFile 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteNodeSolutionFile")]
      public static extern int LSwriteNodeSolutionFile 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int             iStage         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteScenarioMPIFile")]
      public static extern int LSwriteScenarioMPIFile 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                      string           pszFname         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteScenarioMPSFile")]
      public static extern int LSwriteScenarioMPSFile 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                      string           pszFname         ,
                                                         int            nFormat         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteScenarioLINDOFile")]
      public static extern int LSwriteScenarioLINDOFile 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                      string           pszFname         );

 /* parameter routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetModelStocDouParameter")]
      public static extern int LSsetModelStocDouParameter 
      (                                               IntPtr             pModel         ,
                                                         int               iPar         ,
                                                      double               dVal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetModelStocDouParameter")]
      public static extern int LSgetModelStocDouParameter 
      (                                               IntPtr             pModel         ,
                                                         int               iPar         ,
                                            ref       double              pdVal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetModelStocIntParameter")]
      public static extern int LSsetModelStocIntParameter 
      (                                               IntPtr             pModel         ,
                                                         int               iPar         ,
                                                         int               iVal         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetModelStocIntParameter")]
      public static extern int LSgetModelStocIntParameter 
      (                                               IntPtr             pModel         ,
                                                         int               iPar         ,
                                            ref          int              piVal         );

 /* general query routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioIndex")]
      public static extern int LSgetScenarioIndex 
      (                                               IntPtr             pModel         ,
                                                      string            pszName         ,
                                            ref          int            pnIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStageIndex")]
      public static extern int LSgetStageIndex 
      (                                               IntPtr             pModel         ,
                                                      string            pszName         ,
                                            ref          int            pnIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocParIndex")]
      public static extern int LSgetStocParIndex 
      (                                               IntPtr             pModel         ,
                                                      string            pszName         ,
                                            ref          int            pnIndex         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocParName")]
      public static extern int LSgetStocParName 
      (                                               IntPtr             pModel         ,
                                                         int             nIndex         ,
                                                StringBuilder           pachName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioName")]
      public static extern int LSgetScenarioName 
      (                                               IntPtr             pModel         ,
                                                         int             nIndex         ,
                                                StringBuilder           pachName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStageName")]
      public static extern int LSgetStageName 
      (                                               IntPtr             pModel         ,
                                                         int             nIndex         ,
                                                StringBuilder           pachName         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocInfo")]
      public static extern int LSgetStocInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                                         int             nParam         ,
                                            ref          int            pResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocInfo")]
      public static extern int LSgetStocInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                                         int             nParam         ,
                                            ref       double            pResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocCCPInfo")]
      public static extern int LSgetStocCCPInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                                         int       nScenarioIndex         ,
                                                         int          nCPPIndex         ,
                                            ref          int            pResult         );
      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocCCPInfo")]
      public static extern int LSgetStocCCPInfo 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                                         int       nScenarioIndex         ,
                                                         int          nCPPIndex         ,
                                            ref       double            pResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadSampleSizes")]
      public static extern int LSloadSampleSizes 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panSampleSize         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadConstraintStages")]
      public static extern int LSloadConstraintStages 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panStage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadVariableStages")]
      public static extern int LSloadVariableStages 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panStage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadStageData")]
      public static extern int LSloadStageData 
      (                                               IntPtr             pModel         ,
                                                         int          numStages         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panRstage         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panCstage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadStocParData")]
      public static extern int LSloadStocParData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panSparStage         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padSparValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadStocParNames")]
      public static extern int LSloadStocParNames 
      (                                               IntPtr             pModel         ,
                                                         int             nSvars         ,
             [MarshalAs(UnmanagedType.LPArray)]       string    [] paszSVarNames         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDeteqModel")]
      public static extern IntPtr LSgetDeteqModel 
      (                                               IntPtr             pModel         ,
                                                         int           iDeqType         ,
                                            ref          int        pnErrorCode         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaggregateStages")]
      public static extern int LSaggregateStages 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panScheme         ,
                                                         int            nLength         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStageAggScheme")]
      public static extern int LSgetStageAggScheme 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panScheme         ,
                                            ref          int           pnLength         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdeduceStages")]
      public static extern int LSdeduceStages 
      (                                               IntPtr             pModel         ,
                                                         int          nMaxStage         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panRowStagse         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panColStages         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panSparStage         );

 /* optimization routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveSP")]
      public static extern int  LSsolveSP 
      (                                               IntPtr             pModel         ,
                                            ref          int           pnStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveHS")]
      public static extern int  LSsolveHS 
      (                                               IntPtr             pModel         ,
                                                         int       nSearchMethod         ,
                                            ref          int           pnStatus         );

 /* solution access routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioObjective")]
      public static extern int LSgetScenarioObjective 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                            ref       double               pObj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNodePrimalSolution")]
      public static extern int LSgetNodePrimalSolution 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int             iStage         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNodeDualSolution")]
      public static extern int LSgetNodeDualSolution 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int             iStage         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padY         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNodeReducedCost")]
      public static extern int LSgetNodeReducedCost 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int             iStage         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNodeSlacks")]
      public static extern int LSgetNodeSlacks 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int             iStage         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padY         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioPrimalSolution")]
      public static extern int LSgetScenarioPrimalSolution 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         ,
                                            ref       double               pObj         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioReducedCost")]
      public static extern int LSgetScenarioReducedCost 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padD         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioDualSolution")]
      public static extern int LSgetScenarioDualSolution 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padY         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioSlacks")]
      public static extern int LSgetScenarioSlacks 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padS         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNodeListByScenario")]
      public static extern int LSgetNodeListByScenario 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiNodes         ,
                                            ref          int            pnNodes         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetProbabilityByScenario")]
      public static extern int LSgetProbabilityByScenario 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                            ref       double             pdProb         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetProbabilityByNode")]
      public static extern int LSgetProbabilityByNode 
      (                                               IntPtr             pModel         ,
                                                         int              iNode         ,
                                            ref       double             pdProb         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocParData")]
      public static extern int LSgetStocParData 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    paiStages         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         );

 /* load stochastic data */


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddDiscreteBlocks")]
      public static extern int LSaddDiscreteBlocks 
      (                                               IntPtr             pModel         ,
                                                         int             iStage         ,
                                                         int        nRealzBlock         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padProb         ,
                                            ref          int           pakStart         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiRows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiStvs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
                                                         int        nModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddScenario")]
      public static extern int LSaddScenario 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                                         int        iParentScen         ,
                                                         int             iStage         ,
                                                      double              dProb         ,
                                                         int             nElems         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiRows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiStvs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
                                                         int        nModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddDiscreteIndep")]
      public static extern int LSaddDiscreteIndep 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                                         int               jCol         ,
                                                         int               iStv         ,
                                                         int       nRealizations         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padProbs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
                                                         int        nModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddParamDistIndep")]
      public static extern int LSaddParamDistIndep 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                                         int               jCol         ,
                                                         int               iStv         ,
                                                         int          nDistType         ,
                                                         int            nParams         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padParams         ,
                                                         int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddUserDist")]
      public static extern int LSaddUserDist 
      (                                               IntPtr             pModel         ,
                                                         int               iRow         ,
                                                         int               jCol         ,
                                                         int               iStv         ,
                                                typUserDistr         pfUserFunc         ,
                                                         int           nSamples         ,
                                                      IntPtr  []      paSamples         ,
                                                      IntPtr         pvUserData         ,
                                                         int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddChanceConstraint")]
      public static extern int LSaddChanceConstraint 
      (                                               IntPtr             pModel         ,
                                                         int             iSense         ,
                                                         int              nCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
                                                      double           dPrLevel         ,
                                                      double         dObjWeight         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetNumStages")]
      public static extern int LSsetNumStages 
      (                                               IntPtr             pModel         ,
                                                         int          numStages         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocParOutcomes")]
      public static extern int LSgetStocParOutcomes 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padProbs         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadCorrelationMatrix")]
      public static extern int LSloadCorrelationMatrix 
      (                                               IntPtr             pModel         ,
                                                         int               nDim         ,
                                                         int          nCorrType         ,
                                                         int             nQCnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetCorrelationMatrix")]
      public static extern int LSgetCorrelationMatrix 
      (                                               IntPtr             pModel         ,
                                                         int              iFlag         ,
                                                         int          nCorrType         ,
                                            ref          int            pnQCnnz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   paiQCcols2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocParSample")]
      public static extern IntPtr LSgetStocParSample 
      (                                               IntPtr             pModel         ,
                                                         int               iStv         ,
                                                         int               iRow         ,
                                                         int               jCol         ,
                                            ref          int        pnErrorCode         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDiscreteBlocks")]
      public static extern int LSgetDiscreteBlocks 
      (                                               IntPtr             pModel         ,
                                                         int             iEvent         ,
                                            ref          int          nDistType         ,
                                            ref          int             iStage         ,
                                            ref          int        nRealzBlock         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padProbs         ,
                                            ref          int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDiscreteBlockOutcomes")]
      public static extern int LSgetDiscreteBlockOutcomes 
      (                                               IntPtr             pModel         ,
                                                         int             iEvent         ,
                                                         int             iRealz         ,
                                            ref          int             nRealz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiStvs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDiscreteIndep")]
      public static extern int LSgetDiscreteIndep 
      (                                               IntPtr             pModel         ,
                                                         int             iEvent         ,
                                            ref          int          nDistType         ,
                                            ref          int             iStage         ,
                                            ref          int               iRow         ,
                                            ref          int               jCol         ,
                                            ref          int               iStv         ,
                                            ref          int       nRealizations         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padProbs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
                                            ref          int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetParamDistIndep")]
      public static extern int LSgetParamDistIndep 
      (                                               IntPtr             pModel         ,
                                                         int             iEvent         ,
                                            ref          int          nDistType         ,
                                            ref          int             iStage         ,
                                            ref          int               iRow         ,
                                            ref          int               jCol         ,
                                            ref          int               iStv         ,
                                            ref          int            nParams         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padParams         ,
                                            ref          int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenario")]
      public static extern int LSgetScenario 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                            ref          int        iParentScen         ,
                                            ref          int       iBranchStage         ,
                                            ref       double             pdProb         ,
                                            ref          int             nRealz         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiArows         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiAcols         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiStvs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
                                            ref          int        iModifyRule         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetChanceConstraint")]
      public static extern int LSgetChanceConstraint 
      (                                               IntPtr             pModel         ,
                                                         int            iChance         ,
                                            ref          int            piSense         ,
                                            ref          int             pnCons         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []      paiCons         ,
                                            ref       double             pdProb         ,
                                            ref       double        pdObjWeight         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSampleSizes")]
      public static extern int LSgetSampleSizes 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panSampleSize         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetConstraintStages")]
      public static extern int LSgetConstraintStages 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panStage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetVariableStages")]
      public static extern int LSgetVariableStages 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     panStage         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetStocRowIndices")]
      public static extern int LSgetStocRowIndices 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []     paiSrows         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetStocParRG")]
      public static extern int LSsetStocParRG 
      (                                               IntPtr             pModel         ,
                                                         int               iStv         ,
                                                         int               iRow         ,
                                                         int               jCol         ,
                                                      IntPtr                pRG         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetScenarioModel")]
      public static extern IntPtr LSgetScenarioModel 
      (                                               IntPtr             pModel         ,
                                                         int          jScenario         ,
                                            ref          int        pnErrorcode         );

 /* memory routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeStocMemory")]
      public static extern void LSfreeStocMemory 
      (                                               IntPtr             pModel         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfreeStocHashMemory")]
      public static extern void LSfreeStocHashMemory 
      (                                               IntPtr             pModel         );

 /* stochastic parameter routines */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetModelStocParameter")]
      public static extern int LSgetModelStocParameter 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref          int           pvResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetModelStocParameter")]
      public static extern int LSsetModelStocParameter 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref          int           pvResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetModelStocParameter")]
      public static extern int LSgetModelStocParameter 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref       double           pvResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetModelStocParameter")]
      public static extern int LSsetModelStocParameter 
      (                                               IntPtr             pModel         ,
                                                         int             nQuery         ,
                                            ref       double           pvResult         );


 /* Public Functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampCreate")]
      public static extern IntPtr LSsampCreate 
      (                                               IntPtr               pEnv         ,
                                                         int          nDistType         ,
                                            ref          int        pnErrorCode         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampDelete")]
      public static extern int LSsampDelete 
      (                                     ref          IntPtr            pSample         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampSetUserDistr")]
      public static extern int LSsampSetUserDistr 
      (                                               IntPtr            pSample         ,
                                                         typUserDistr      pfUserFunc         ,
                                                         IntPtr            pvUserData );

      [DllImport(LINDO_DLL,
      EntryPoint="LSsampSetUserDistr")]
      public static extern int LSsampSetUserDistr 
      (                                               IntPtr            pSample         ,
                                                         typUserDistr      pfUserFunc         ,
        [MarshalAs(UnmanagedType.AsAny)]                 object            pvUserData );

      [DllImport(LINDO_DLL,
      EntryPoint="LSsampSetDistrParam")]
      public static extern int LSsampSetDistrParam 
      (                                               IntPtr            pSample         ,
                                                         int             nIndex         ,
                                                      double             dValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetDistrParam")]
      public static extern int LSsampGetDistrParam 
      (                                               IntPtr            pSample         ,
                                                         int             nIndex         ,
                                            ref       double            pdValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampEvalDistr")]
      public static extern int LSsampEvalDistr 
      (                                               IntPtr            pSample         ,
                                                         int          nFuncType         ,
                                                      double              dXval         ,
                                            ref       double           pdResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampEvalDistrLI")]
      public static extern int LSsampEvalDistrLI 
      (                                               IntPtr            pSample         ,
                                                         int          nFuncType         ,
                                                      double              dXval         ,
                                            ref       double           pdResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampEvalUserDistr")]
      public static extern int LSsampEvalUserDistr 
      (                                               IntPtr            pSample         ,
                                                         int          nFuncType         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padXval         ,
                                                         int               nDim         ,
                                            ref       double           pdResult         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampSetRG")]
      public static extern int LSsampSetRG 
      (                                               IntPtr            pSample         ,
                                                      IntPtr                pRG         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGenerate")]
      public static extern int LSsampGenerate 
      (                                               IntPtr            pSample         ,
                                                         int            nMethod         ,
                                                         int              nSize         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetPointsPtr")]
      public static extern int LSsampGetPointsPtr 
      (                                               IntPtr            pSample         ,
                                            ref          int         pnSampSize         ,
                                                      IntPtr            pdXval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetPoints")]
      public static extern int LSsampGetPoints 
      (                                               IntPtr            pSample         ,
                                            ref          int         pnSampSize         ,
             [MarshalAs(UnmanagedType.LPArray)]          double    []    pdXval         );
      [DllImport(LINDO_DLL,
      EntryPoint="LSsampLoadPoints")]
      public static extern int LSsampLoadPoints 
      (                                               IntPtr            pSample         ,
                                                         int          nSampSize         ,
                                            ref       double             pdXval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetCIPointsPtr")]
      public static extern int LSsampGetCIPointsPtr 
      (                                               IntPtr            pSample         ,
                                            ref          int         pnSampSize         ,
                                                      IntPtr           pdXval         );
      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetCIPoints")]
      public static extern int LSsampGetCIPoints 
      (                                               IntPtr            pSample         ,
                                            ref          int         pnSampSize         ,
             [MarshalAs(UnmanagedType.LPArray)]          double    []    pdXval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampInduceCorrelation")]
      public static extern int LSsampInduceCorrelation 
      (                                                  IntPtr []     paSample         ,
                                                         int               nDim         ,
                                                         int          nCorrType         ,
                                                         int        nQCnonzeros         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetCorrelationMatrix")]
      public static extern int LSsampGetCorrelationMatrix 
      (                                                  IntPtr []     paSample         ,
                                                         int               nDim         ,
                                                         int              iFlag         ,
                                                         int          nCorrType         ,
                                            ref          int        nQCnonzeros         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx1         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] paiQCvarndx2         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padQCcoef         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampLoadDiscretePdfTable")]
      public static extern int LSsampLoadDiscretePdfTable 
      (                                               IntPtr            pSample         ,
                                                         int               nLen         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padProb         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetDiscretePdfTable")]
      public static extern int LSsampGetDiscretePdfTable 
      (                                               IntPtr            pSample         ,
                                            ref          int               nLen         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padProb         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetInfo")]
      public static extern int LSsampGetInfo 
      (                                               IntPtr            pSample         ,
                                                         int             nQuery         ,
                                            ref          int            pvResult        );

      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetInfo")]
      public static extern int LSsampGetInfo 
      (                                               IntPtr            pSample         ,
                                                         int             nQuery         ,
                                            ref          double          dResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSsampAddUserFuncArg")]
      public static extern int LSsampAddUserFuncArg 
      (                                               IntPtr            pSample         ,
                                                      IntPtr       pSampleSource         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSregress")]
      public static extern int  LSregress 
      (                                                  int              nNdim         ,
                                                         int              nPdim         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padU         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padX         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padB         ,
                                            ref       double               pdB0         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []         padR         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []     padstats         );

 /* Public functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LScreateRG")]
      public static extern IntPtr LScreateRG 
      (                                               IntPtr               pEnv         ,
                                                         int            nMethod         );


      [DllImport(LINDO_DLL,
      EntryPoint="LScreateRGMT")]
      public static extern IntPtr LScreateRGMT 
      (                                               IntPtr               pEnv         ,
                                                         int            nMethod         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDoubleRV")]
      public static extern double LSgetDoubleRV 
      (                                               IntPtr                pRG         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetInt32RV")]
      public static extern int LSgetInt32RV 
      (                                               IntPtr                pRG         ,
                                                         int               iLow         ,
                                                         int              iHigh         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetRGSeed")]
      public static extern void LSsetRGSeed 
      (                                               IntPtr                pRG         ,
                                                         int              nSeed         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdisposeRG")]
      public static extern void LSdisposeRG 
      (                                     ref       IntPtr               ppRG         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetDistrParamRG")]
      public static extern int LSsetDistrParamRG 
      (                                               IntPtr                pRG         ,
                                                         int             iParam         ,
                                                      double             dParam         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetDistrRG")]
      public static extern int LSsetDistrRG 
      (                                               IntPtr                pRG         ,
                                                         int          nDistType         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDistrRV")]
      public static extern int LSgetDistrRV 
      (                                               IntPtr                pRG         ,
                                            ref          double           pvResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDistrRV")]
      public static extern int LSgetDistrRV 
      (                                               IntPtr                pRG         ,
                                            ref          int               pResult         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetInitSeed")]
      public static extern int LSgetInitSeed 
      (                                               IntPtr                pRG         );

      [DllImport(LINDO_DLL,
      EntryPoint="LSgetRGNumThreads")]
      public static extern int LSgetRGNumThreads 
      (                                               IntPtr                pRG         ,
                                            ref          int          pnThreads         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSfillRGBuffer")]
      public static extern int LSfillRGBuffer 
      (                                               IntPtr                pRG         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetRGBufferPtr")]
      public static extern double[] LSgetRGBufferPtr 
      (                                               IntPtr                pRG         ,
                                            ref          int        pnBufferLen         );
 /* Auxiliary functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetHistogram")]
      public static extern int LSgetHistogram 
      (                                               IntPtr             pModel         ,
                                                         int          nSampSize         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padVals         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padWeights         ,
                                                      double           dHistLow         ,
                                                      double          dHistHigh         ,
                                            ref          int             pnBins         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    [] panBinCounts         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padBinProbs         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padBinLow         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []   padBinHigh         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padBinLeftEdge         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padBinRightEdge         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsampGetCorrDiff")]
      public static extern int LSsampGetCorrDiff 
      (                                               IntPtr             pModel         ,
                                            ref          int           paSample         ,
                                                         int               nDim         ,
                                                         int          nDiffType         ,
                                            ref       double            pdNorm1         ,
                                            ref       double            pdNorm2         ,
                                            ref       double       pdVecNormInf         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetNnzData")]
      public static extern int LSgetNnzData 
      (                                               IntPtr             pModel         ,
                                                         int              mStat         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []    panOutput         );

 /* Public functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveFileLP")]
      public static extern int LSsolveFileLP 
      (                                               IntPtr             pModel         ,
                                                      string       szFileNameMPS         ,
                                                      string       szFileNameSol         ,
                                                         int       nNoOfColsEvaluatedPerSet         ,
                                                         int       nNoOfColsSelectedPerSet         ,
                                                         int       nTimeLimitSec         ,
                                            ref          int       pnSolStatusParam         ,
                                            ref          int       pnNoOfConsMps         ,
                                            ref          int       pnNoOfColsMps         ,
                                            ref          int        pnErrorLine         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSreadSolutionFileLP")]
      public static extern int LSreadSolutionFileLP 
      (                                               string       szFileNameSol         ,
                                                         int        nFileFormat         ,
                                                         int       nBeginIndexPrimalSol         ,
                                                         int       nEndIndexPrimalSol         ,
                                            ref          int        pnSolStatus         ,
                                            ref       double         pdObjValue         ,
                                            ref          int         pnNoOfCons         ,
                                            ref          int         plNoOfCols         ,
                                            ref          int       pnNoOfColsEvaluated         ,
                                            ref          int       pnNoOfIterations         ,
                                            ref       double       pdTimeTakenInSeconds         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padPrimalValues         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    [] padDualValues         );

 /* Documented Public functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDiffSecs")]
      public static extern int LSdateDiffSecs 
      (                                                  int               nYr1         ,
                                                         int              nMon1         ,
                                                         int              nDay1         ,
                                                         int               nHr1         ,
                                                         int              nMin1         ,
                                                      double              dSec1         ,
                                                         int               nYr2         ,
                                                         int              nMon2         ,
                                                         int              nDay2         ,
                                                         int               nHr2         ,
                                                         int              nMin2         ,
                                                      double              dSec2         ,
                                            ref       double          pdSecdiff         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateYmdhms")]
      public static extern int LSdateYmdhms 
      (                                               double           dSecdiff         ,
                                                         int               nYr1         ,
                                                         int              nMon1         ,
                                                         int              nDay1         ,
                                                         int               nHr1         ,
                                                         int              nMin1         ,
                                                      double              dSec1         ,
                                            ref          int              pnYr2         ,
                                            ref          int             pnMon2         ,
                                            ref          int             pnDay2         ,
                                            ref          int              pnHr2         ,
                                            ref          int             pnMin2         ,
                                            ref       double             pdSec2         ,
                                            ref          int              pnDow         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateToday")]
      public static extern int LSdateToday 
      (                                     ref          int              pnYr1         ,
                                            ref          int             pnMon1         ,
                                            ref          int             pnDay1         ,
                                            ref          int              pnHr1         ,
                                            ref          int             pnMin1         ,
                                            ref       double             pdSec1         ,
                                            ref          int              pnDow         );

 /* Undocumented Public functions */


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateMakeDate")]
      public static extern int LSdateMakeDate 
      (                                                  int              nYYYY         ,
                                                         int                nMM         ,
                                                         int                nDD         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateMakeTime")]
      public static extern double LSdateMakeTime 
      (                                                  int                nHH         ,
                                                         int                nMM         ,
                                                      double                dSS         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateSetBaseDate")]
      public static extern void LSdateSetBaseDate 
      (                                                  int              nYYYY         ,
                                                         int                nMM         ,
                                                         int                nDD         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateScalarSec")]
      public static extern double LSdateScalarSec 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateScalarSecInverse")]
      public static extern void LSdateScalarSecInverse 
      (                                               double              dSSEC         ,
                                            ref          int             pnDate         ,
                                            ref       double             pdTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateScalarHour")]
      public static extern double LSdateScalarHour 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateScalarHourInverse")]
      public static extern void LSdateScalarHourInverse 
      (                                               double             dSHOUR         ,
                                            ref          int             pnDate         ,
                                            ref       double             pdTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateJulianSec")]
      public static extern double LSdateJulianSec 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateJulianSecInverse")]
      public static extern void LSdateJulianSecInverse 
      (                                               double              dJSEC         ,
                                            ref          int             pnDate         ,
                                            ref       double             pdTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateJulianHour")]
      public static extern double LSdateJulianHour 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateJulianHourInverse")]
      public static extern void LSdateJulianHourInverse 
      (                                               double             dJHOUR         ,
                                            ref          int             pnDate         ,
                                            ref       double             pdTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDiff")]
      public static extern void LSdateDiff 
      (                                                  int             nDate1         ,
                                                      double             dTime1         ,
                                                         int             nDate2         ,
                                                      double             dTime2         ,
                                            ref          int             pnDays         ,
                                            ref       double             pdSecs         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateNow")]
      public static extern void  LSdateNow 
      (                                     ref          int             pnDate         ,
                                            ref       double             pdTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateIsLeapYear")]
      public static extern int LSdateIsLeapYear 
      (                                                  int              nYear         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateJulianDay")]
      public static extern int LSdateJulianDay 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDayOfWeek")]
      public static extern int LSdateDayOfWeek 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateWeekOfYear")]
      public static extern int LSdateWeekOfYear 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateQuarterOfYear")]
      public static extern int LSdateQuarterOfYear 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDayOfYear")]
      public static extern int LSdateDayOfYear 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateNextWeekDay")]
      public static extern int LSdateNextWeekDay 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdatePrevWeekDay")]
      public static extern int LSdatePrevWeekDay 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateNextMonth")]
      public static extern int LSdateNextMonth 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDateToDays")]
      public static extern int LSdateDateToDays 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDaysToDate")]
      public static extern int LSdateDaysToDate 
      (                                                  int              nDays         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateTimeToSecs")]
      public static extern double LSdateTimeToSecs 
      (                                               double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateSecsToTime")]
      public static extern double LSdateSecsToTime 
      (                                               double              dSecs         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateFutureDate")]
      public static extern void LSdateFutureDate 
      (                                     ref          int             pnDate         ,
                                            ref       double             pdTime         ,
                                                         int              nDays         ,
                                                         int              dSecs         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdatePastDate")]
      public static extern void LSdatePastDate 
      (                                     ref          int             pnDate         ,
                                            ref       double             pdTime         ,
                                                         int              nDays         ,
                                                      double              dSecs         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateIsValidDate")]
      public static extern int LSdateIsValidDate 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateIsValidTime")]
      public static extern int LSdateIsValidTime 
      (                                               double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateIsDateFuture")]
      public static extern int LSdateIsDateFuture 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateIsDatePast")]
      public static extern int LSdateIsDatePast 
      (                                                  int              nDate         ,
                                                      double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateYear")]
      public static extern int LSdateYear 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateMonth")]
      public static extern int LSdateMonth 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDay")]
      public static extern int  LSdateDay 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateHour")]
      public static extern int LSdateHour 
      (                                               double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateMinute")]
      public static extern int LSdateMinute 
      (                                               double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateSecond")]
      public static extern double LSdateSecond 
      (                                               double              dTime         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateWeekOfMonth")]
      public static extern int LSdateWeekOfMonth 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateLocalTimeStamp")]
      public static extern string LSdateLocalTimeStamp 
      (                                               string       szTimeBuffer         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateDateNum")]
      public static extern int LSdateDateNum 
      (                                                  int              nDate         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdateMakeDateNum")]
      public static extern int LSdateMakeDateNum 
      (                                                  int              nYYYY         ,
                                                         int                nMM         ,
                                                         int                nDD         );

 /*********************************************************************
 **
 **    Tuner Functions
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2019-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: lindo.cs 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/


      [DllImport(LINDO_DLL,
      EntryPoint="LSrunTuner")]
      public static extern int LSrunTuner 
      (                                               IntPtr               pEnv         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSrunTunerFile")]
      public static extern int LSrunTunerFile 
      (                                               IntPtr               pEnv         ,
                                                      string         szJsonFile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSrunTunerString")]
      public static extern int LSrunTunerString 
      (                                               IntPtr               pEnv         ,
                                                      string       szJsonString         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadTunerConfigString")]
      public static extern int LSloadTunerConfigString 
      (                                               IntPtr               pEnv         ,
                                                      string       szJsonString         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSloadTunerConfigFile")]
      public static extern int LSloadTunerConfigFile 
      (                                               IntPtr               pEnv         ,
                                                      string         szJsonFile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSclearTuner")]
      public static extern int LSclearTuner 
      (                                               IntPtr               pEnv         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSresetTuner")]
      public static extern int LSresetTuner 
      (                                               IntPtr               pEnv         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSprintTuner")]
      public static extern int LSprintTuner 
      (                                               IntPtr               pEnv         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsetTunerOption")]
      public static extern int LSsetTunerOption 
      (                                               IntPtr               pEnv         ,
                                                      string              szKey         ,
                                                      double               dval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetTunerOption")]
      public static extern int LSgetTunerOption 
      (                                               IntPtr               pEnv         ,
                                                      string              szkey         ,
                                            ref       double              pdval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetTunerResult")]
      public static extern int LSgetTunerResult 
      (                                               IntPtr               pEnv         ,
                                                      string              szkey         ,
                                                         int          jInstance         ,
                                                         int            kConfig         ,
                                            ref       double              pdval         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetTunerSpace")]
      public static extern int LSgetTunerSpace 
      (                                               IntPtr               pEnv         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panParamId         ,
                                            ref          int           numParam         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteTunerConfigString")]
      public static extern int LSwriteTunerConfigString 
      (                                               IntPtr               pEnv         ,
                                                      string       szJsonString         ,
                                                      string         szJsonFile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetTunerConfigString")]
      public static extern int LSgetTunerConfigString 
      (                                               IntPtr               pEnv         ,
                                                      string       szJsonString         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSwriteTunerParameters")]
      public static extern int LSwriteTunerParameters 
      (                                               IntPtr               pEnv         ,
                                                      string             szFile         ,
                                                         int          jInstance         ,
                                                         int         mCriterion         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddTunerInstance")]
      public static extern int LSaddTunerInstance 
      (                                               IntPtr               pEnv         ,
                                                      string             szFile         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddTunerZStatic")]
      public static extern int LSaddTunerZStatic 
      (                                               IntPtr               pEnv         ,
                                                         int           jGroupId         ,
                                                         int             iParam         ,
                                                      double             dValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddTunerZDynamic")]
      public static extern int LSaddTunerZDynamic 
      (                                               IntPtr               pEnv         ,
                                                         int             iParam         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSaddTunerOption")]
      public static extern int LSaddTunerOption 
      (                                               IntPtr               pEnv         ,
                                                      string              szKey         ,
                                                      double             dValue         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSdisplayTunerResults")]
      public static extern int LSdisplayTunerResults 
      (                                               IntPtr               pEnv         );

  /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetLicenseInfo")]
      public static extern int LSgetLicenseInfo 
      (                                               IntPtr             pModel         ,
                                            ref          int          pnMaxcons         ,
                                            ref          int          pnMaxvars         ,
                                            ref          int       pnMaxintvars         ,
                                            ref          int        pnReserved1         ,
                                            ref          int        pnDaystoexp         ,
                                            ref          int       pnDaystotrialexp         ,
                                            ref          int       pnNlpAllowed         ,
                                            ref          int            pnUsers         ,
                                            ref          int       pnBarAllowed         ,
                                            ref          int          pnRuntime         ,
                                            ref          int       pnEdulicense         ,
                                                StringBuilder           pachText         );

  /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDimensions")]
      public static extern int LSgetDimensions 
      (                                               IntPtr             pModel         ,
                                            ref          int             pnVars         ,
                                            ref          int             pnCons         ,
                                            ref          int            pnCones         ,
                                            ref          int             pnAnnz         ,
                                            ref          int            pnQCnnz         ,
                                            ref          int          pnConennz         ,
                                            ref          int           pnNLPnnz         ,
                                            ref          int        pnNLPobjnnz         ,
                                            ref          int       pnVarNamelen         ,
                                            ref          int       pnConNamelen         ,
                                            ref          int       pnConeNamelen         );

  /* Deprecated, use LSsolveMIP() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSbnbSolve")]
      public static extern int LSbnbSolve 
      (                                               IntPtr             pModel         ,
                                                      string           pszFname         );

  /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetDualMIPsolution")]
      public static extern int LSgetDualMIPsolution 
      (                                               IntPtr             pModel         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []      padDual         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []  padRedcosts         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panCstatus         ,
             [MarshalAs(UnmanagedType.LPArray)]          int    []   panRstatus         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPSolutionStatus")]
      public static extern int LSgetMIPSolutionStatus 
      (                                               IntPtr             pModel         ,
                                            ref          int           pnStatus         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSolutionStatus")]
      public static extern int LSgetSolutionStatus 
      (                                               IntPtr             pModel         ,
                                            ref          int            nStatus         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetObjective")]
      public static extern int LSgetObjective 
      (                                               IntPtr             pModel         ,
                                            ref       double           pdObjval         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetSolutionInfo")]
      public static extern int LSgetSolutionInfo 
      (                                               IntPtr             pModel         ,
                                            ref          int           pnMethod         ,
                                            ref          int          pnElapsed         ,
                                            ref          int          pnSpxiter         ,
                                            ref          int          pnBariter         ,
                                            ref          int          pnNlpiter         ,
                                            ref          int       pnPrimStatus         ,
                                            ref          int       pnDualStatus         ,
                                            ref          int        pnBasStatus         ,
                                            ref       double          pdPobjval         ,
                                            ref       double          pdDobjval         ,
                                            ref       double          pdPinfeas         ,
                                            ref       double          pdDinfeas         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMIPSolution")]
      public static extern int LSgetMIPSolution 
      (                                               IntPtr             pModel         ,
                                            ref       double          pdPobjval         ,
             [MarshalAs(UnmanagedType.LPArray)]       double    []    padPrimal         );

 /* Deprecated,  use LSgetInfo() */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetCurrentMIPSolutionInfo")]
      public static extern int LSgetCurrentMIPSolutionInfo 
      (                                               IntPtr             pModel         ,
                                            ref          int        pnMIPstatus         ,
                                            ref       double        pdMIPobjval         ,
                                            ref       double        pdBestbound         ,
                                            ref       double          pdSpxiter         ,
                                            ref       double          pdBariter         ,
                                            ref       double          pdNlpiter         ,
                                            ref          int            pnLPcnt         ,
                                            ref          int        pnBranchcnt         ,
                                            ref          int        pnActivecnt         ,
                                            ref          int        pnCons_prep         ,
                                            ref          int        pnVars_prep         ,
                                            ref          int        pnAnnz_prep         ,
                                            ref          int         pnInt_prep         ,
                                            ref          int       pnCut_contra         ,
                                            ref          int          pnCut_obj         ,
                                            ref          int          pnCut_gub         ,
                                            ref          int         pnCut_lift         ,
                                            ref          int         pnCut_flow         ,
                                            ref          int       pnCut_gomory         ,
                                            ref          int          pnCut_gcd         ,
                                            ref          int       pnCut_clique         ,
                                            ref          int       pnCut_disagg         ,
                                            ref          int       pnCut_planloc         ,
                                            ref          int       pnCut_latice         ,
                                            ref          int         pnCut_coef         );

 /* Command Line Parser */


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetCLOpt")]
      public static extern int LSgetCLOpt 
      (                                               IntPtr               pEnv         ,
                                                         int              nArgc         ,
                                                      string     []     pszArgv       ,
                                                      string             pszOpt         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetCLOptArg")]
      public static extern int LSgetCLOptArg 
      (                                               IntPtr               pEnv         ,
                                                      string     []   pszOptArg         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetCLOptInd")]
      public static extern int LSgetCLOptInd 
      (                                               IntPtr               pEnv         ,
                                            ref          int           pnOptInd         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSsolveExternally")]
      public static extern int LSsolveExternally 
      (                                               IntPtr             pModel         ,
                                                         int            mSolver         ,
                                                         int            nMethod         ,
                                                         int        nFileFormat         ,
                                            ref          int           pnStatus         );


      [DllImport(LINDO_DLL,
      EntryPoint="LSgetMasterModel")]
      public static extern IntPtr LSgetMasterModel 
      (                                               IntPtr             pModel         );
}
