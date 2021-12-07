/*********************************************************************
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2000-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
package com.lindo;


/** @dll.import("LINDO13_0.DLL")
*/


public class Lindo
{

/*********************************************************************
 *                        Constant Definitions                       *
 *********************************************************************/

/* Version macros */
   public static final int    LS_MAJOR_VER_NUMBER               = 13;
   public static final int    LS_MINOR_VER_NUMBER               = 0;
   public static final int    LS_REV_VER_NUMBER                 = 107;
   public static final int    LS_VER_NUMBER                     = 1300;
   public static final int    LS_BUILD_VER_NUMBER               = 4099;

   public static final int    LS_MIN                            = +1;
   public static final int    LS_MAX                            = -1;

   public static final byte   LS_CONTYPE_GE                     = 71 ;//'G'
   public static final byte   LS_CONTYPE_LE                     = 76 ;//'L'
   public static final byte   LS_CONTYPE_EQ                     = 69 ;//'E'
   public static final byte   LS_CONTYPE_FR                     = 78 ;//'N'
   public static final byte   LS_CONETYPE_QUAD                  = 81 ;//'Q'
   public static final byte   LS_CONETYPE_RQUAD                 = 82 ;//'R'
   public static final byte   LS_CONETYPE_PEXP                  = 69 ;//'E'
   public static final byte   LS_CONETYPE_PPOW                  = 80 ;//'P'
   public static final byte   LS_VARTYPE_CONT                   = 67 ;//'C'
   public static final byte   LS_VARTYPE_BIN                    = 66 ;//'B'
   public static final byte   LS_VARTYPE_INT                    = 73 ;//'I'

   public static final double LS_INFINITY                       = 1.0E+30;

   public static final int    LS_BASTYPE_BAS                    = 0;
   public static final int    LS_BASTYPE_ATLO                   = -1;
   public static final int    LS_BASTYPE_ATUP                   = -2;
   public static final int    LS_BASTYPE_FNUL                   = -3;
   public static final int    LS_BASTYPE_SBAS                   = -4;

   public static final int    LS_UNFORMATTED_MPS                = 0;
   public static final int    LS_FORMATTED_MPS                  = 1;
   public static final int    LS_UNFORMATTED_MPS_COMP           = 2;
   public static final int    LS_FORMATTED_MPS_COMP             = 3;

   public static final int    LS_SOLUTION_OPT                   = 0;
   public static final int    LS_SOLUTION_MIP                   = 1;
   public static final int    LS_SOLUTION_OPT_IPM               = 2;
   public static final int    LS_SOLUTION_OPT_OLD               = 3;
   public static final int    LS_SOLUTION_MIP_OLD               = 4;

   public static final int    LS_BASFILE_BIN                    = 1;
   public static final int    LS_BASFILE_MPS                    = 2;
   public static final int    LS_BASFILE_TXT                    = 3;
   public static final int    LS_BASFILE_MTX                    = 4;

   public static final int    LS_INT_TYPE                       = 4;
   public static final int    LS_DOUBLE_TYPE                    = 8;

   public static final int    LS_MAX_ERROR_MESSAGE_LENGTH       = 1024;

   public static final int    LS_DEFAULT                        = -1;
   public static final int    LS_MAX_JOBJECTS                   = 100;

   public static final int    LS_PROPERTY_UNKNOWN               = 0;
   public static final int    LS_PROPERTY_CONST                 = 1;
   public static final int    LS_PROPERTY_LINEAR                = 2;
   public static final int    LS_PROPERTY_CONVEX                = 3;
   public static final int    LS_PROPERTY_CONCAVE               = 4;
   public static final int    LS_PROPERTY_QUASI_CONVEX          = 5;
   public static final int    LS_PROPERTY_QUASI_CONCAVE         = 6;
   public static final int    LS_PROPERTY_MAX                   = 7;
   public static final int    LS_PROPERTY_MONO_INCREASE         = 8;
   public static final int    LS_PROPERTY_MONO_DECREASE         = 9;

/* bitmasks for LScopyModel */
   public static final int    LS_RAW_COPY                       = 0;
   public static final int    LS_DEEP_COPY                      = 1;
   public static final int    LS_TIME_COPY                      = 2;
   public static final int    LS_STOC_COPY                      = 4;
   public static final int    LS_SNGSTG_COPY                    = 8;

/* Time frames in seconds */
   public static final int    LSSEC01                           = 1;
   public static final int    LSSEC02                           = 2;
   public static final int    LSSEC03                           = 3;
   public static final int    LSSEC04                           = 4;
   public static final int    LSSEC05                           = 5;
   public static final int    LSSEC06                           = 6;
   public static final int    LSSEC10                           = 10;
   public static final int    LSSEC15                           = 15;
   public static final int    LSSEC20                           = 20;
   public static final int    LSSEC30                           = 30;
   public static final int    LSMIN01                           = 60;
   public static final int    LSMIN02                           = 120;
   public static final int    LSMIN03                           = 180;
   public static final int    LSMIN05                           = 300;
   public static final int    LSMIN06                           = 600;
   public static final int    LSMIN10                           = 600;
   public static final int    LSMIN15                           = 900;
   public static final int    LSMIN20                           = 1200;
   public static final int    LSMIN30                           = 1800;
   public static final int    LSHOUR01                          = 3600;
   public static final int    LSHOUR02                          = 7200;
   public static final int    LSHOUR03                          = 10800;
   public static final int    LSHOUR05                          = 18000;
   public static final int    LSHOUR06                          = 21600;
   public static final int    LSHOUR08                          = 28800;
   public static final int    LSHOUR12                          = 43200;
   public static final int    LSDAY                             = 86400;
   public static final int    LSWEEK                            = 604800;
   public static final int    LSMONTH                           = 2592000;
   public static final int    LSQUARTER                         = 7776000;
   public static final int    LSYEAR                            = 31104000;

/* Days of week */
   public static final int    LSSUNDAY                          = 0;
   public static final int    LSMONDAY                          = 1;
   public static final int    LSTUESDAY                         = 2;
   public static final int    LSWEDNESDAY                       = 3;
   public static final int    LSTHURSDAY                        = 4;
   public static final int    LSFRIDAY                          = 5;
   public static final int    LSSATURDAY                        = 6;

/* bitmask for components */
   public static final int    LS_DATA_CORE                      = 1;
   public static final int    LS_DATA_TIME                      = 2;
   public static final int    LS_DATA_STOC                      = 4;
   public static final int    LS_DATA_FILE                      = 8;

/* external solvers */
   public static final int    LS_XSOLVER_MSKLP                  = 1;
   public static final int    LS_XSOLVER_GRBLP                  = 2;
   public static final int    LS_XSOLVER_GRBCL                  = 3;
   public static final int    LS_XSOLVER_GRBMIP                 = 4;
   public static final int    LS_XSOLVER_CPXLP                  = 5;
   public static final int    LS_XSOLVER_CPXMIP                 = 6;
   public static final int    LS_XSOLVER_OSI                    = 7;
   public static final int    LS_XSOLVER_CLP                    = 8;
   public static final int    LS_XSOLVER_MSK                    = 9;
   public static final int    LS_XSOLVER_COI                    = 10;
   public static final int    LS_XSOLVER_SOP                    = 11;

/*********************************************************************
 *                      Macro Type Definitions                       *
 *********************************************************************/

/* Solution or model status (1-20) */
   public static final int    LS_STATUS_OPTIMAL                 = 1;
   public static final int    LS_STATUS_BASIC_OPTIMAL           = 2;
   public static final int    LS_STATUS_INFEASIBLE              = 3;
   public static final int    LS_STATUS_UNBOUNDED               = 4;
   public static final int    LS_STATUS_FEASIBLE                = 5;
   public static final int    LS_STATUS_INFORUNB                = 6;
   public static final int    LS_STATUS_NEAR_OPTIMAL            = 7;
   public static final int    LS_STATUS_LOCAL_OPTIMAL           = 8;
   public static final int    LS_STATUS_LOCAL_INFEASIBLE        = 9;
   public static final int    LS_STATUS_CUTOFF                  = 10;
   public static final int    LS_STATUS_NUMERICAL_ERROR         = 11;
   public static final int    LS_STATUS_UNKNOWN                 = 12;
   public static final int    LS_STATUS_UNLOADED                = 13;
   public static final int    LS_STATUS_LOADED                  = 14;
   public static final int    LS_STATUS_BOUNDED                 = 15;


/* Parameter codes (21-999) */
   /* General parameters (1021 - 1099) */
   public static final int    LS_IPARAM_OBJSENSE                = 1022;
   public static final int    LS_DPARAM_CALLBACKFREQ            = 1023;
   public static final int    LS_DPARAM_OBJPRINTMUL             = 1024;
   public static final int    LS_IPARAM_CHECK_FOR_ERRORS        = 1025;
   public static final int    LS_IPARAM_ALLOW_CNTRLBREAK        = 1026;
   public static final int    LS_IPARAM_DECOMPOSITION_TYPE      = 1027;
   public static final int    LS_IPARAM_LP_SCALE                = 1029;
   public static final int    LS_IPARAM_LP_ITRLMT               = 1030;
   public static final int    LS_IPARAM_SPLEX_PPRICING          = 1031;
   public static final int    LS_IPARAM_SPLEX_REFACFRQ          = 1032;
   public static final int    LS_IPARAM_BARRIER_SOLVER          = 1033;
   public static final int    LS_IPARAM_PROB_TO_SOLVE           = 1034;
   public static final int    LS_IPARAM_LP_PRINTLEVEL           = 1035;
   public static final int    LS_IPARAM_MPS_OBJ_WRITESTYLE      = 1036;
   public static final int    LS_IPARAM_SPLEX_DPRICING          = 1037;
   public static final int    LS_IPARAM_SOL_REPORT_STYLE        = 1038;
   public static final int    LS_IPARAM_INSTRUCT_LOADTYPE       = 1039;
   public static final int    LS_IPARAM_SPLEX_DUAL_PHASE        = 1040;
   public static final int    LS_IPARAM_LP_PRELEVEL             = 1041;
   public static final int    LS_IPARAM_STRING_LENLMT           = 1042;
   public static final int    LS_IPARAM_USE_NAMEDATA            = 1043;
   public static final int    LS_IPARAM_SPLEX_USE_EXTERNAL      = 1044;
   public static final int    LS_DPARAM_LP_ITRLMT               = 1045;
   public static final int    LS_IPARAM_COPY_MODE               = 1046;
   public static final int    LS_IPARAM_SBD_NUM_THREADS         = 1047;
   public static final int    LS_IPARAM_NUM_THREADS             = 1048;
   public static final int    LS_IPARAM_MULTITHREAD_MODE        = 1049;
   public static final int    LS_IPARAM_FIND_BLOCK              = 1050;
   public static final int    LS_IPARAM_PROFILER_LEVEL          = 1051;
   public static final int    LS_IPARAM_INSTRUCT_READMODE       = 1052;
   public static final int    LS_IPARAM_INSTRUCT_SUBOUT         = 1053;
   public static final int    LS_IPARAM_SOLPOOL_LIM             = 1054;
   public static final int    LS_IPARAM_FIND_SYMMETRY_LEVEL     = 1055;
   public static final int    LS_IPARAM_FIND_SYMMETRY_PRINT_LEVEL = 1056;
   public static final int    LS_IPARAM_TUNER_PRINT_LEVEL       = 1057;

   /* Generic solver parameters (1251 - 1500) */
   public static final int    LS_IPARAM_SOLVER_IUSOL            = 1251;
   public static final int    LS_IPARAM_SOLVER_TIMLMT           = 1252;
   public static final int    LS_DPARAM_SOLVER_CUTOFFVAL        = 1253;
   public static final int    LS_DPARAM_SOLVER_FEASTOL          = 1254;
   public static final int    LS_IPARAM_SOLVER_RESTART          = 1255;
   public static final int    LS_IPARAM_SOLVER_IPMSOL           = 1256;
   public static final int    LS_DPARAM_SOLVER_OPTTOL           = 1257;
   public static final int    LS_IPARAM_SOLVER_USECUTOFFVAL     = 1258;
   public static final int    LS_IPARAM_SOLVER_PRE_ELIM_FILL    = 1259;
   public static final int    LS_DPARAM_SOLVER_TIMLMT           = 1260;
   public static final int    LS_IPARAM_SOLVER_CONCURRENT_OPTMODE = 1261;
   public static final int    LS_DPARAM_SOLVER_PERT_FEASTOL     = 1262;
   public static final int    LS_IPARAM_SOLVER_PARTIALSOL_LEVEL = 1263;
   public static final int    LS_IPARAM_SOLVER_MODE             = 1264;

   /* Advanced parameters for the simplex method (4000 - 41++) */
   public static final int    LS_DPARAM_LP_MIN_FEASTOL          = 4060;
   public static final int    LS_DPARAM_LP_MAX_FEASTOL          = 4061;
   public static final int    LS_DPARAM_LP_MIN_OPTTOL           = 4062;
   public static final int    LS_DPARAM_LP_MAX_OPTTOL           = 4063;
   public static final int    LS_DPARAM_LP_MIN_PIVTOL           = 4064;
   public static final int    LS_DPARAM_LP_MAX_PIVTOL           = 4065;
   public static final int    LS_DPARAM_LP_AIJ_ZEROTOL          = 4066;
   public static final int    LS_DPARAM_LP_PIV_ZEROTOL          = 4067;
   public static final int    LS_DPARAM_LP_PIV_BIGTOL           = 4068;
   public static final int    LS_DPARAM_LP_BIGM                 = 4069;
   public static final int    LS_DPARAM_LP_BNDINF               = 4070;
   public static final int    LS_DPARAM_LP_INFINITY             = 4071;
   public static final int    LS_IPARAM_LP_PPARTIAL             = 4072;
   public static final int    LS_IPARAM_LP_DPARTIAL             = 4073;
   public static final int    LS_IPARAM_LP_DRATIO               = 4074;
   public static final int    LS_IPARAM_LP_PRATIO               = 4075;
   public static final int    LS_IPARAM_LP_RATRANGE             = 4076;
   public static final int    LS_IPARAM_LP_DPSWITCH             = 4077;
   public static final int    LS_IPARAM_LP_PALLOC               = 4078;
   public static final int    LS_IPARAM_LP_PRTFG                = 4079;
   public static final int    LS_IPARAM_LP_OPRFREE              = 4080;
   public static final int    LS_IPARAM_LP_SPRINT_SUB           = 4081;
   public static final int    LS_IPARAM_LP_PERTMODE             = 4082;
   public static final int    LS_IPARAM_LP_PCOLAL_FACTOR        = 4083;
   public static final int    LS_IPARAM_LP_MAXMERGE             = 4084;
   public static final int    LS_DPARAM_LP_PERTFACT             = 4085;
   public static final int    LS_DPARAM_LP_DYNOBJFACT           = 4086;
   public static final int    LS_IPARAM_LP_DYNOBJMODE           = 4087;
   public static final int    LS_DPARAM_LP_MERGEFACT            = 4088;
   public static final int    LS_IPARAM_LP_UMODE                = 4089;
   public static final int    LS_IPARAM_LP_SPRINT_MAXPASS       = 4090;
   public static final int    LS_IPARAM_LP_SPRINT_COLFACT       = 4091;

   /* Advanced parameters for LU decomposition (4800 - 4+++) */
   public static final int    LS_IPARAM_LU_NUM_CANDITS          = 4800;
   public static final int    LS_IPARAM_LU_MAX_UPDATES          = 4801;
   public static final int    LS_IPARAM_LU_PRINT_LEVEL          = 4802;
   public static final int    LS_IPARAM_LU_UPDATE_TYPE          = 4803;
   public static final int    LS_IPARAM_LU_MODE                 = 4804;
   public static final int    LS_IPARAM_LU_PIVMOD               = 4806;
   public static final int    LS_DPARAM_LU_EPS_DIAG             = 4900;
   public static final int    LS_DPARAM_LU_EPS_NONZ             = 4901;
   public static final int    LS_DPARAM_LU_EPS_PIVABS           = 4902;
   public static final int    LS_DPARAM_LU_EPS_PIVREL           = 4903;
   public static final int    LS_DPARAM_LU_INI_RCOND            = 4904;
   public static final int    LS_DPARAM_LU_SPVTOL_UPDATE        = 4905;
   public static final int    LS_DPARAM_LU_SPVTOL_FTRAN         = 4906;
   public static final int    LS_DPARAM_LU_SPVTOL_BTRAN         = 4907;
   public static final int    LS_IPARAM_LU_CORE                 = 4908;

   /* Parameters for the IPM method (3000 - 3+++) */
   public static final int    LS_DPARAM_IPM_TOL_INFEAS          = 3150;
   public static final int    LS_DPARAM_IPM_TOL_PATH            = 3151;
   public static final int    LS_DPARAM_IPM_TOL_PFEAS           = 3152;
   public static final int    LS_DPARAM_IPM_TOL_REL_STEP        = 3153;
   public static final int    LS_DPARAM_IPM_TOL_PSAFE           = 3154;
   public static final int    LS_DPARAM_IPM_TOL_DFEAS           = 3155;
   public static final int    LS_DPARAM_IPM_TOL_DSAFE           = 3156;
   public static final int    LS_DPARAM_IPM_TOL_MU_RED          = 3157;
   public static final int    LS_DPARAM_IPM_BASIS_REL_TOL_S     = 3158;
   public static final int    LS_DPARAM_IPM_BASIS_TOL_S         = 3159;
   public static final int    LS_DPARAM_IPM_BASIS_TOL_X         = 3160;
   public static final int    LS_DPARAM_IPM_BI_LU_TOL_REL_PIV   = 3161;
   public static final int    LS_DPARAM_IPM_CO_TOL_INFEAS       = 3162;
   public static final int    LS_DPARAM_IPM_CO_TOL_PFEAS        = 3163;
   public static final int    LS_DPARAM_IPM_CO_TOL_DFEAS        = 3164;
   public static final int    LS_DPARAM_IPM_CO_TOL_MU_RED       = 3165;
   public static final int    LS_IPARAM_IPM_MAX_ITERATIONS      = 3166;
   public static final int    LS_IPARAM_IPM_OFF_COL_TRH         = 3167;
   public static final int    LS_IPARAM_IPM_NUM_THREADS         = 3168;
   public static final int    LS_IPARAM_IPM_CHECK_CONVEXITY     = 3169;

   /* Nonlinear programming (NLP) parameters (2500 - 25++) */
   public static final int    LS_IPARAM_NLP_SOLVE_AS_LP         = 2500;
   public static final int    LS_IPARAM_NLP_SOLVER              = 2501;
   public static final int    LS_IPARAM_NLP_SUBSOLVER           = 2502;
   public static final int    LS_IPARAM_NLP_PRINTLEVEL          = 2503;
   public static final int    LS_DPARAM_NLP_PSTEP_FINITEDIFF    = 2504;
   public static final int    LS_IPARAM_NLP_DERIV_DIFFTYPE      = 2505;
   public static final int    LS_DPARAM_NLP_FEASTOL             = 2506;
   public static final int    LS_DPARAM_NLP_REDGTOL             = 2507;
   public static final int    LS_IPARAM_NLP_USE_CRASH           = 2508;
   public static final int    LS_IPARAM_NLP_USE_STEEPEDGE       = 2509;
   public static final int    LS_IPARAM_NLP_USE_SLP             = 2510;
   public static final int    LS_IPARAM_NLP_USE_SELCONEVAL      = 2511;
   public static final int    LS_IPARAM_NLP_PRELEVEL            = 2512;
   public static final int    LS_IPARAM_NLP_ITRLMT              = 2513;
   public static final int    LS_IPARAM_NLP_LINEARZ             = 2514;
   public static final int    LS_IPARAM_NLP_LINEARITY           = 2515;
   public static final int    LS_IPARAM_NLP_STARTPOINT          = 2516;
   public static final int    LS_IPARAM_NLP_CONVEXRELAX         = 2517;
   public static final int    LS_IPARAM_NLP_CR_ALG_REFORM       = 2518;
   public static final int    LS_IPARAM_NLP_QUADCHK             = 2519;
   public static final int    LS_IPARAM_NLP_AUTODERIV           = 2520;
   public static final int    LS_IPARAM_NLP_MAXLOCALSEARCH      = 2521;
   public static final int    LS_IPARAM_NLP_CONVEX              = 2522;
   public static final int    LS_IPARAM_NLP_CONOPT_VER          = 2523;
   public static final int    LS_IPARAM_NLP_USE_LINDO_CRASH     = 2524;
   public static final int    LS_IPARAM_NLP_STALL_ITRLMT        = 2525;
   public static final int    LS_IPARAM_NLP_AUTOHESS            = 2526;
   public static final int    LS_IPARAM_NLP_FEASCHK             = 2527;
   public static final int    LS_DPARAM_NLP_ITRLMT              = 2528;
   public static final int    LS_IPARAM_NLP_MAXSUP              = 2529;
   public static final int    LS_IPARAM_NLP_MSW_SOLIDX          = 2530;
   public static final int    LS_IPARAM_NLP_ITERS_PER_LOGLINE   = 2531;
   public static final int    LS_IPARAM_NLP_MAX_RETRY           = 2532;
   public static final int    LS_IPARAM_NLP_MSW_NORM            = 2533;
   public static final int    LS_IPARAM_NLP_MSW_POPSIZE         = 2534;
   public static final int    LS_IPARAM_NLP_MSW_MAXPOP          = 2535;
   public static final int    LS_IPARAM_NLP_MSW_MAXNOIMP        = 2536;
   public static final int    LS_IPARAM_NLP_MSW_FILTMODE        = 2537;
   public static final int    LS_DPARAM_NLP_MSW_POXDIST_THRES   = 2538;
   public static final int    LS_DPARAM_NLP_MSW_EUCDIST_THRES   = 2539;
   public static final int    LS_DPARAM_NLP_MSW_XNULRAD_FACTOR  = 2540;
   public static final int    LS_DPARAM_NLP_MSW_XKKTRAD_FACTOR  = 2541;
   public static final int    LS_IPARAM_NLP_MAXLOCALSEARCH_TREE = 2542;
   public static final int    LS_IPARAM_NLP_MSW_NUM_THREADS     = 2543;
   public static final int    LS_IPARAM_NLP_MSW_RG_SEED         = 2544;
   public static final int    LS_IPARAM_NLP_MSW_PREPMODE        = 2545;
   public static final int    LS_IPARAM_NLP_MSW_RMAPMODE        = 2546;
   public static final int    LS_IPARAM_NLP_XSMODE              = 2547;
   public static final int    LS_DPARAM_NLP_MSW_OVERLAP_RATIO   = 2548;
   public static final int    LS_DPARAM_NLP_INF                 = 2549;
   public static final int    LS_IPARAM_NLP_IPM2GRG             = 2550;
   public static final int    LS_IPARAM_NLP_USE_SDP             = 2551;
   public static final int    LS_IPARAM_NLP_LINEARZ_WB_CONSISTENT = 2552;
   public static final int    LS_DPARAM_NLP_CUTOFFOBJ           = 2553;
   public static final int    LS_IPARAM_NLP_USECUTOFFOBJ        = 2554;

   /* Mixed integer programming (MIP) parameters (5000 - 5+++) */
   public static final int    LS_IPARAM_MIP_TIMLIM              = 5300;
   public static final int    LS_IPARAM_MIP_AOPTTIMLIM          = 5301;
   public static final int    LS_IPARAM_MIP_LSOLTIMLIM          = 5302;
   public static final int    LS_IPARAM_MIP_PRELEVEL            = 5303;
   public static final int    LS_IPARAM_MIP_NODESELRULE         = 5304;
   public static final int    LS_DPARAM_MIP_INTTOL              = 5305;
   public static final int    LS_DPARAM_MIP_RELINTTOL           = 5306;
   public static final int    LS_DPARAM_MIP_RELOPTTOL           = 5307;
   public static final int    LS_DPARAM_MIP_PEROPTTOL           = 5308;
   public static final int    LS_IPARAM_MIP_MAXCUTPASS_TOP      = 5309;
   public static final int    LS_IPARAM_MIP_MAXCUTPASS_TREE     = 5310;
   public static final int    LS_DPARAM_MIP_ADDCUTPER           = 5311;
   public static final int    LS_DPARAM_MIP_ADDCUTPER_TREE      = 5312;
   public static final int    LS_IPARAM_MIP_MAXNONIMP_CUTPASS   = 5313;
   public static final int    LS_IPARAM_MIP_CUTLEVEL_TOP        = 5314;
   public static final int    LS_IPARAM_MIP_CUTLEVEL_TREE       = 5315;
   public static final int    LS_IPARAM_MIP_CUTTIMLIM           = 5316;
   public static final int    LS_IPARAM_MIP_CUTDEPTH            = 5317;
   public static final int    LS_IPARAM_MIP_CUTFREQ             = 5318;
   public static final int    LS_IPARAM_MIP_HEULEVEL            = 5319;
   public static final int    LS_IPARAM_MIP_PRINTLEVEL          = 5320;
   public static final int    LS_IPARAM_MIP_PREPRINTLEVEL       = 5321;
   public static final int    LS_DPARAM_MIP_CUTOFFOBJ           = 5322;
   public static final int    LS_IPARAM_MIP_USECUTOFFOBJ        = 5323;
   public static final int    LS_IPARAM_MIP_STRONGBRANCHLEVEL   = 5324;
   public static final int    LS_IPARAM_MIP_TREEREORDERLEVEL    = 5325;
   public static final int    LS_IPARAM_MIP_BRANCHDIR           = 5326;
   public static final int    LS_IPARAM_MIP_TOPOPT              = 5327;
   public static final int    LS_IPARAM_MIP_REOPT               = 5328;
   public static final int    LS_IPARAM_MIP_SOLVERTYPE          = 5329;
   public static final int    LS_IPARAM_MIP_KEEPINMEM           = 5330;
   public static final int    LS_IPARAM_MIP_BRANCHRULE          = 5331;
   public static final int    LS_DPARAM_MIP_REDCOSTFIX_CUTOFF   = 5332;
   public static final int    LS_DPARAM_MIP_ADDCUTOBJTOL        = 5333;
   public static final int    LS_IPARAM_MIP_HEUMINTIMLIM        = 5334;
   public static final int    LS_IPARAM_MIP_BRANCH_PRIO         = 5335;
   public static final int    LS_IPARAM_MIP_SCALING_BOUND       = 5336;
   public static final int    LS_DPARAM_MIP_PSEUDOCOST_WEIGT    = 5337;
   public static final int    LS_DPARAM_MIP_LBIGM               = 5338;
   public static final int    LS_DPARAM_MIP_DELTA               = 5339;
   public static final int    LS_IPARAM_MIP_DUAL_SOLUTION       = 5340;
   public static final int    LS_IPARAM_MIP_BRANCH_LIMIT        = 5341;
   public static final int    LS_DPARAM_MIP_ITRLIM              = 5342;
   public static final int    LS_IPARAM_MIP_AGGCUTLIM_TOP       = 5343;
   public static final int    LS_IPARAM_MIP_AGGCUTLIM_TREE      = 5344;
   public static final int    LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_ITER = 5345;
   public static final int    LS_IPARAM_MIP_ANODES_SWITCH_DF    = 5346;
   public static final int    LS_DPARAM_MIP_ABSOPTTOL           = 5347;
   public static final int    LS_DPARAM_MIP_MINABSOBJSTEP       = 5348;
   public static final int    LS_IPARAM_MIP_PSEUDOCOST_RULE     = 5349;
   public static final int    LS_IPARAM_MIP_ENUM_HEUMODE        = 5350;
   public static final int    LS_IPARAM_MIP_PRELEVEL_TREE       = 5351;
   public static final int    LS_DPARAM_MIP_REDCOSTFIX_CUTOFF_TREE = 5352;
   public static final int    LS_IPARAM_MIP_USE_INT_ZERO_TOL    = 5353;
   public static final int    LS_IPARAM_MIP_USE_CUTS_HEU        = 5354;
   public static final int    LS_DPARAM_MIP_BIGM_FOR_INTTOL     = 5355;
   public static final int    LS_IPARAM_MIP_STRONGBRANCHDONUM   = 5366;
   public static final int    LS_IPARAM_MIP_MAKECUT_INACTIVE_COUNT = 5367;
   public static final int    LS_IPARAM_MIP_PRE_ELIM_FILL       = 5368;
   public static final int    LS_IPARAM_MIP_HEU_MODE            = 5369;
   public static final int    LS_DPARAM_MIP_TIMLIM              = 5370;
   public static final int    LS_DPARAM_MIP_AOPTTIMLIM          = 5371;
   public static final int    LS_DPARAM_MIP_LSOLTIMLIM          = 5372;
   public static final int    LS_DPARAM_MIP_CUTTIMLIM           = 5373;
   public static final int    LS_DPARAM_MIP_HEUMINTIMLIM        = 5374;
   public static final int    LS_IPARAM_MIP_FP_MODE             = 5375;
   public static final int    LS_DPARAM_MIP_FP_WEIGHT           = 5376;
   public static final int    LS_IPARAM_MIP_FP_OPT_METHOD       = 5377;
   public static final int    LS_DPARAM_MIP_FP_TIMLIM           = 5378;
   public static final int    LS_IPARAM_MIP_FP_ITRLIM           = 5379;
   public static final int    LS_IPARAM_MIP_FP_HEU_MODE         = 5380;
   public static final int    LS_DPARAM_MIP_OBJ_THRESHOLD       = 5381;
   public static final int    LS_IPARAM_MIP_LOCALBRANCHNUM      = 5382;
   public static final int    LS_DPARAM_MIP_SWITCHFAC_SIM_IPM_TIME = 5383;
   public static final int    LS_DPARAM_MIP_ITRLIM_SIM          = 5384;
   public static final int    LS_DPARAM_MIP_ITRLIM_NLP          = 5385;
   public static final int    LS_DPARAM_MIP_ITRLIM_IPM          = 5386;
   public static final int    LS_IPARAM_MIP_MAXNUM_MIP_SOL_STORAGE = 5387;
   public static final int    LS_IPARAM_MIP_CONCURRENT_TOPOPTMODE = 5388;
   public static final int    LS_IPARAM_MIP_CONCURRENT_REOPTMODE = 5389;
   public static final int    LS_IPARAM_MIP_PREHEU_LEVEL        = 5390;
   public static final int    LS_IPARAM_MIP_PREHEU_PRE_LEVEL    = 5391;
   public static final int    LS_IPARAM_MIP_PREHEU_PRINT_LEVEL  = 5392;
   public static final int    LS_IPARAM_MIP_PREHEU_TC_ITERLIM   = 5393;
   public static final int    LS_IPARAM_MIP_PREHEU_DFE_VSTLIM   = 5394;
   public static final int    LS_IPARAM_MIP_PREHEU_VAR_SEQ      = 5395;
   public static final int    LS_IPARAM_MIP_USE_PARTIALSOL_LEVEL = 5396;
   public static final int    LS_IPARAM_MIP_GENERAL_MODE        = 5397;
   public static final int    LS_IPARAM_MIP_NUM_THREADS         = 5398;
   public static final int    LS_IPARAM_MIP_POLISH_NUM_BRANCH_NEXT = 5399;
   public static final int    LS_IPARAM_MIP_POLISH_MAX_BRANCH_COUNT = 5400;
   public static final int    LS_DPARAM_MIP_POLISH_ALPHA_TARGET = 5401;
   public static final int    LS_IPARAM_MIP_CONCURRENT_STRATEGY = 5402;
   public static final int    LS_DPARAM_MIP_BRANCH_TOP_VAL_DIFF_WEIGHT = 5403;
   public static final int    LS_IPARAM_MIP_BASCUTS_DONUM       = 5404;
   public static final int    LS_IPARAM_MIP_PARA_SUB            = 5405;
   public static final int    LS_DPARAM_MIP_PARA_RND_ITRLMT     = 5406;
   public static final int    LS_DPARAM_MIP_PARA_INIT_NODE      = 5407;
   public static final int    LS_IPARAM_MIP_PARA_ITR_MODE       = 5408;
   public static final int    LS_IPARAM_MIP_PARA_FP             = 5409;
   public static final int    LS_IPARAM_MIP_PARA_FP_MODE        = 5410;
   public static final int    LS_IPARAM_MIP_HEU_DROP_OBJ        = 5411;
   public static final int    LS_DPARAM_MIP_ABSCUTTOL           = 5412;
   public static final int    LS_IPARAM_MIP_PERSPECTIVE_REFORM  = 5413;
   public static final int    LS_IPARAM_MIP_TREEREORDERMODE     = 5414;
   public static final int    LS_IPARAM_MIP_XSOLVER             = 5415;
   public static final int    LS_IPARAM_MIP_BNB_TRY_BNP         = 5416;
   public static final int    LS_IPARAM_MIP_KBEST_USE_GOP       = 5417;
   public static final int    LS_IPARAM_MIP_SYMMETRY_MODE       = 5418;
   public static final int    LS_IPARAM_MIP_ALLDIFF_METHOD      = 5419;
   public static final int    LS_IPARAM_MIP_SOLLIM              = 5420;

   /* Global optimization (GOP) parameters (6000 - 6+++) */
   public static final int    LS_DPARAM_GOP_RELOPTTOL           = 6400;
   public static final int    LS_DPARAM_GOP_FLTTOL              = 6401;
   public static final int    LS_DPARAM_GOP_BOXTOL              = 6402;
   public static final int    LS_DPARAM_GOP_WIDTOL              = 6403;
   public static final int    LS_DPARAM_GOP_DELTATOL            = 6404;
   public static final int    LS_DPARAM_GOP_BNDLIM              = 6405;
   public static final int    LS_IPARAM_GOP_TIMLIM              = 6406;
   public static final int    LS_IPARAM_GOP_OPTCHKMD            = 6407;
   public static final int    LS_IPARAM_GOP_BRANCHMD            = 6408;
   public static final int    LS_IPARAM_GOP_MAXWIDMD            = 6409;
   public static final int    LS_IPARAM_GOP_PRELEVEL            = 6410;
   public static final int    LS_IPARAM_GOP_POSTLEVEL           = 6411;
   public static final int    LS_IPARAM_GOP_BBSRCHMD            = 6412;
   public static final int    LS_IPARAM_GOP_DECOMPPTMD          = 6413;
   public static final int    LS_IPARAM_GOP_ALGREFORMMD         = 6414;
   public static final int    LS_IPARAM_GOP_RELBRNDMD           = 6415;
   public static final int    LS_IPARAM_GOP_PRINTLEVEL          = 6416;
   public static final int    LS_IPARAM_GOP_BNDLIM_MODE         = 6417;
   public static final int    LS_IPARAM_GOP_BRANCH_LIMIT        = 6418;
   public static final int    LS_IPARAM_GOP_CORELEVEL           = 6419;
   public static final int    LS_IPARAM_GOP_OPT_MODE            = 6420;
   public static final int    LS_IPARAM_GOP_HEU_MODE            = 6421;
   public static final int    LS_IPARAM_GOP_SUBOUT_MODE         = 6422;
   public static final int    LS_IPARAM_GOP_USE_NLPSOLVE        = 6423;
   public static final int    LS_IPARAM_GOP_LSOLBRANLIM         = 6424;
   public static final int    LS_IPARAM_GOP_LPSOPT              = 6425;
   public static final int    LS_DPARAM_GOP_TIMLIM              = 6426;
   public static final int    LS_DPARAM_GOP_BRANCH_LIMIT        = 6427;
   public static final int    LS_IPARAM_GOP_QUADMD              = 6428;
   public static final int    LS_IPARAM_GOP_LIM_MODE            = 6429;
   public static final int    LS_DPARAM_GOP_ITRLIM              = 6430;
   public static final int    LS_DPARAM_GOP_ITRLIM_SIM          = 6431;
   public static final int    LS_DPARAM_GOP_ITRLIM_IPM          = 6432;
   public static final int    LS_DPARAM_GOP_ITRLIM_NLP          = 6433;
   public static final int    LS_DPARAM_GOP_ABSOPTTOL           = 6434;
   public static final int    LS_DPARAM_GOP_PEROPTTOL           = 6435;
   public static final int    LS_DPARAM_GOP_AOPTTIMLIM          = 6436;
   public static final int    LS_IPARAM_GOP_LINEARZ             = 6437;
   public static final int    LS_IPARAM_GOP_NUM_THREADS         = 6438;
   public static final int    LS_IPARAM_GOP_MULTILINEAR         = 6439;
   public static final int    LS_DPARAM_GOP_OBJ_THRESHOLD       = 6440;
   public static final int    LS_IPARAM_GOP_QUAD_METHOD         = 6441;
   public static final int    LS_IPARAM_GOP_SOLLIM              = 6442;
   public static final int    LS_IPARAM_GOP_CMINLP              = 6443;
   public static final int    LS_IPARAM_GOP_CONIC_REFORM        = 6444;

   /* License information parameters */
   public static final int    LS_IPARAM_LIC_CONSTRAINTS         = 500;
   public static final int    LS_IPARAM_LIC_VARIABLES           = 501;
   public static final int    LS_IPARAM_LIC_INTEGERS            = 502;
   public static final int    LS_IPARAM_LIC_NONLINEARVARS       = 503;
   public static final int    LS_IPARAM_LIC_GOP_INTEGERS        = 504;
   public static final int    LS_IPARAM_LIC_GOP_NONLINEARVARS   = 505;
   public static final int    LS_IPARAM_LIC_DAYSTOEXP           = 506;
   public static final int    LS_IPARAM_LIC_DAYSTOTRIALEXP      = 507;
   public static final int    LS_IPARAM_LIC_NONLINEAR           = 508;
   public static final int    LS_IPARAM_LIC_EDUCATIONAL         = 509;
   public static final int    LS_IPARAM_LIC_RUNTIME             = 510;
   public static final int    LS_IPARAM_LIC_NUMUSERS            = 511;
   public static final int    LS_IPARAM_LIC_BARRIER             = 512;
   public static final int    LS_IPARAM_LIC_GLOBAL              = 513;
   public static final int    LS_IPARAM_LIC_PLATFORM            = 514;
   public static final int    LS_IPARAM_LIC_MIP                 = 515;
   public static final int    LS_IPARAM_LIC_SP                  = 516;
   public static final int    LS_IPARAM_LIC_CONIC               = 517;
   public static final int    LS_IPARAM_LIC_RESERVED1           = 519;

   /* Model analysis parameters (1500 - 15++) */
   public static final int    LS_IPARAM_IIS_ANALYZE_LEVEL       = 1550;
   public static final int    LS_IPARAM_IUS_ANALYZE_LEVEL       = 1551;
   public static final int    LS_IPARAM_IIS_TOPOPT              = 1552;
   public static final int    LS_IPARAM_IIS_REOPT               = 1553;
   public static final int    LS_IPARAM_IIS_USE_SFILTER         = 1554;
   public static final int    LS_IPARAM_IIS_PRINT_LEVEL         = 1555;
   public static final int    LS_IPARAM_IIS_INFEAS_NORM         = 1556;
   public static final int    LS_IPARAM_IIS_ITER_LIMIT          = 1557;
   public static final int    LS_DPARAM_IIS_ITER_LIMIT          = 1558;
   public static final int    LS_IPARAM_IIS_TIME_LIMIT          = 1559;
   public static final int    LS_IPARAM_IIS_METHOD              = 1560;
   public static final int    LS_IPARAM_IIS_USE_EFILTER         = 1561;
   public static final int    LS_IPARAM_IIS_USE_GOP             = 1562;
   public static final int    LS_IPARAM_IIS_NUM_THREADS         = 1563;
   public static final int    LS_IPARAM_IIS_GETMODE             = 1564;

   /* Output log format parameter */
   public static final int    LS_IPARAM_FMT_ISSQL               = 1590;

   /* Stochastic Parameters (6000 - 6+++) */

   /*! @ingroup LSstocParam @{ */
   /*! Common sample size per stochastic parameter. */
   public static final int    LS_IPARAM_STOC_NSAMPLE_SPAR       = 6600;
   /*! Common sample size per stage.  */
   public static final int    LS_IPARAM_STOC_NSAMPLE_STAGE      = 6601;
   /*! Seed to initialize the random number generator. */
   public static final int    LS_IPARAM_STOC_RG_SEED            = 6602;
   /*! Stochastic optimization method to solve the model. */
   public static final int    LS_IPARAM_STOC_METHOD             = 6603;
   /*! Reoptimization method to solve the node-models. */
   public static final int    LS_IPARAM_STOC_REOPT              = 6604;
   /*! Optimization method to solve the root problem. */
   public static final int    LS_IPARAM_STOC_TOPOPT             = 6605;
   /*! Iteration limit for stochastic solver. */
   public static final int    LS_IPARAM_STOC_ITER_LIM           = 6606;
   /*! Print level to display progress information during optimization */
   public static final int    LS_IPARAM_STOC_PRINT_LEVEL        = 6607;
   /*! Type of deterministic equivalent */
   public static final int    LS_IPARAM_STOC_DETEQ_TYPE         = 6608;
   /*! Flag to enable/disable calculation of EVPI. */
   public static final int    LS_IPARAM_STOC_CALC_EVPI          = 6609;

   /*! Flag to restrict sampling to continuous stochastic parameters only or not.*/
   public static final int    LS_IPARAM_STOC_SAMP_CONT_ONLY     = 6611;

   /*! Bucket size in Benders decomposition */
   public static final int    LS_IPARAM_STOC_BUCKET_SIZE        = 6612;
   /*! Maximum number of scenarios before forcing automatic sampling */
   public static final int    LS_IPARAM_STOC_MAX_NUMSCENS       = 6613;
   /*! Stage beyond which node-models are shared */
   public static final int    LS_IPARAM_STOC_SHARE_BEGSTAGE     = 6614;
   /*! Print level to display progress information during optimization of node models */
   public static final int    LS_IPARAM_STOC_NODELP_PRELEVEL    = 6615;

   /*! Time limit for stochastic solver.*/
   public static final int    LS_DPARAM_STOC_TIME_LIM           = 6616;
   /*! Relative optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. */
   public static final int    LS_DPARAM_STOC_RELOPTTOL          = 6617;
   /*! Absolute optimality tolerance (w.r.t lower and upper bounds on the true objective) to stop the solver. */
   public static final int    LS_DPARAM_STOC_ABSOPTTOL          = 6618;
   /*! Internal mask */
   public static final int    LS_IPARAM_STOC_DEBUG_MASK         = 6619;
   /*! Sampling method for variance reduction. */
   public static final int    LS_IPARAM_STOC_VARCONTROL_METHOD  = 6620;
   /*! Correlation type associated with correlation matrix.  */
   public static final int    LS_IPARAM_STOC_CORRELATION_TYPE   = 6621;
   /*! Warm start basis for wait-see model  .  */
   public static final int    LS_IPARAM_STOC_WSBAS              = 6622;

   /*! Outer loop iteration limit for ALD  .  */
   public static final int    LS_IPARAM_STOC_ALD_OUTER_ITER_LIM = 6623;
   /*! Inner loop iteration limit for ALD  .  */
   public static final int    LS_IPARAM_STOC_ALD_INNER_ITER_LIM = 6624;
   /*! Dual feasibility tolerance for ALD  .  */
   public static final int    LS_DPARAM_STOC_ALD_DUAL_FEASTOL   = 6625;
   /*! Primal feasibility tolerance for ALD  .  */
   public static final int    LS_DPARAM_STOC_ALD_PRIMAL_FEASTOL = 6626;
   /*! Dual step length for ALD  .  */
   public static final int    LS_DPARAM_STOC_ALD_DUAL_STEPLEN   = 6627;
   /*! Primal step length for ALD  .  */
   public static final int    LS_DPARAM_STOC_ALD_PRIMAL_STEPLEN = 6628;
   /*! Order nontemporal models or not.  */
   public static final int    LS_IPARAM_CORE_ORDER_BY_STAGE     = 6629;

   /*! Node name format.  */
   public static final int    LS_SPARAM_STOC_FMT_NODE_NAME      = 6630;
   /*! Scenario name format.  */
   public static final int    LS_SPARAM_STOC_FMT_SCENARIO_NAME  = 6631;
   /*! Flag to specify whether stochastic parameters in MPI will be mapped as LP matrix elements.  */
   public static final int    LS_IPARAM_STOC_MAP_MPI2LP         = 6632;
   /*! Flag to enable or disable autoaggregation */
   public static final int    LS_IPARAM_STOC_AUTOAGGR           = 6633;
   /*! Benchmark scenario to compare EVPI and EVMU against*/
   public static final int    LS_IPARAM_STOC_BENCHMARK_SCEN     = 6634;
   /*! Value to truncate infinite bounds at non-leaf nodes */
   public static final int    LS_DPARAM_STOC_INFBND             = 6635;
   /*! Flag to use add-instructions mode when building deteq */
   public static final int    LS_IPARAM_STOC_ADD_MPI            = 6636;
   /* Flag to enable elimination of fixed variables from deteq MPI */
   public static final int    LS_IPARAM_STOC_ELIM_FXVAR         = 6637;
   /*! RHS value of objective cut in SBD master problem.  */
   public static final int    LS_DPARAM_STOC_SBD_OBJCUTVAL      = 6638;
   /*! Flag to enable objective cut in SBD master problem.  */
   public static final int    LS_IPARAM_STOC_SBD_OBJCUTFLAG     = 6639;
   /*! Maximum number of candidate solutions to generate at SBD root */
   public static final int    LS_IPARAM_STOC_SBD_NUMCANDID      = 6640;
   /*! Big-M value for linearization and penalty functions */
   public static final int    LS_DPARAM_STOC_BIGM               = 6641;
   /*! Name data level */
   public static final int    LS_IPARAM_STOC_NAMEDATA_LEVEL     = 6642;
   /*! Max cuts to generate for master problem */
   public static final int    LS_IPARAM_STOC_SBD_MAXCUTS        = 6643;
   /*! Optimization method to solve the deteq problem. */
   public static final int    LS_IPARAM_STOC_DEQOPT             = 6644;
   /*! Subproblem formulation to use in DirectSearch. */
   public static final int    LS_IPARAM_STOC_DS_SUBFORM         = 6645;
   /*! Primal-step tolerance */
   public static final int    LS_DPARAM_STOC_REL_PSTEPTOL       = 6646;
   /*! Dual-step tolerance */
   public static final int    LS_DPARAM_STOC_REL_DSTEPTOL       = 6647;
   /*! Number of parallel threads */
   public static final int    LS_IPARAM_STOC_NUM_THREADS        = 6648;
   /*! Number of deteq blocks */
   public static final int    LS_IPARAM_STOC_DETEQ_NBLOCKS      = 6649;

   /* Sampling parameters (7000 - 7+++) */

   /*! Bitmask to enable methods for solving the nearest correlation matrix (NCM) subproblem */
   public static final int    LS_IPARAM_SAMP_NCM_METHOD         = 7701;
   /*! Objective cutoff (target) value to stop the nearest correlation matrix (NCM) subproblem */
   public static final int    LS_DPARAM_SAMP_NCM_CUTOBJ         = 7702;
   /*! Flag to enable/disable sparse mode in NCM computations */
   public static final int    LS_IPARAM_SAMP_NCM_DSTORAGE       = 7703;
   /*! Correlation matrix diagonal shift increment */
   public static final int    LS_DPARAM_SAMP_CDSINC             = 7704;
   /*! Flag to enable scaling of raw sample data */
   public static final int    LS_IPARAM_SAMP_SCALE              = 7705;
   /*! Iteration limit for NCM method */
   public static final int    LS_IPARAM_SAMP_NCM_ITERLIM        = 7706;
   /*! Optimality tolerance for NCM method */
   public static final int    LS_DPARAM_SAMP_NCM_OPTTOL         = 7707;
   /*! Number of parallel threads */
   public static final int    LS_IPARAM_SAMP_NUM_THREADS        = 7708;
   /*! Buffer size for random number generators */
   public static final int    LS_IPARAM_SAMP_RG_BUFFER_SIZE     = 7709;

   /*Branch And Price parameters (8000 - 8499) */

   /*bound size when subproblem is unbounded*/
   public static final int    LS_DPARAM_BNP_INFBND              = 8010;
   /*branch and bound type*/
   public static final int    LS_IPARAM_BNP_LEVEL               = 8011;
   /*print level*/
   public static final int    LS_IPARAM_BNP_PRINT_LEVEL         = 8012;
   /*box size for BOXSTEP method*/
   public static final int    LS_DPARAM_BNP_BOX_SIZE            = 8013;
   /*number of threads in bnp*/
   public static final int    LS_IPARAM_BNP_NUM_THREADS         = 8014;
   /*relative optimality tolerance for subproblems*/
   public static final int    LS_DPARAM_BNP_SUB_ITRLMT          = 8015;
   /*method for finding block structure*/
   public static final int    LS_IPARAM_BNP_FIND_BLK            = 8016;
   /*pre level*/
   public static final int    LS_IPARAM_BNP_PRELEVEL            = 8017;
   /*column limit*/
   public static final int    LS_DPARAM_BNP_COL_LMT             = 8018;
   /*time limit for bnp*/
   public static final int    LS_DPARAM_BNP_TIMLIM              = 8019;
   /*simplex limit for bnp*/
   public static final int    LS_DPARAM_BNP_ITRLIM_SIM          = 8020;
   /*ipm limit for bnp*/
   public static final int    LS_DPARAM_BNP_ITRLIM_IPM          = 8021;
   /*branch limit for bnp*/
   public static final int    LS_IPARAM_BNP_BRANCH_LIMIT        = 8022;
   /*iteration limit for bnp*/
   public static final int    LS_DPARAM_BNP_ITRLIM              = 8023;

   /* Genetic Algorithm Parameters (8500-8+++) */

   /*  Probability of crossover for continuous variables */
   public static final int    LS_DPARAM_GA_CXOVER_PROB          = 8501;
   /*  Spreading factor for crossover */
   public static final int    LS_DPARAM_GA_XOVER_SPREAD         = 8502;
   /*  Probability of crossover for integer variables */
   public static final int    LS_DPARAM_GA_IXOVER_PROB          = 8503;
   /*  Probability of mutation for continuous variables */
   public static final int    LS_DPARAM_GA_CMUTAT_PROB          = 8504;
   /*  Spreading factor for mutation */
   public static final int    LS_DPARAM_GA_MUTAT_SPREAD         = 8505;
   /*  Probability of mutation for integer variables */
   public static final int    LS_DPARAM_GA_IMUTAT_PROB          = 8506;
   /*  Numeric zero tolerance in GA */
   public static final int    LS_DPARAM_GA_TOL_ZERO             = 8507;
   /*  Primal feasibility tolerance in GA */
   public static final int    LS_DPARAM_GA_TOL_PFEAS            = 8508;
   /*  Numeric infinity in GA */
   public static final int    LS_DPARAM_GA_INF                  = 8509;
   /*  Infinity threshold for finite bounds in GA */
   public static final int    LS_DPARAM_GA_INFBND               = 8510;
   /*  Alpha parameter in Blending Alpha Crossover method */
   public static final int    LS_DPARAM_GA_BLXA                 = 8511;
   /*  Beta parameter in Blending Alpha-Beta Crossover method */
   public static final int    LS_DPARAM_GA_BLXB                 = 8512;
   /*  Method of crossover for continuous variables */
   public static final int    LS_IPARAM_GA_CXOVER_METHOD        = 8513;
   /*  Method of crossover for integer variables */
   public static final int    LS_IPARAM_GA_IXOVER_METHOD        = 8514;
   /*  Method of mutation for continuous variables */
   public static final int    LS_IPARAM_GA_CMUTAT_METHOD        = 8515;
   /*  Method of mutation for integer variables */
   public static final int    LS_IPARAM_GA_IMUTAT_METHOD        = 8516;
   /*  RNG seed for GA */
   public static final int    LS_IPARAM_GA_SEED                 = 8517;
   /*  Number of generations in GA */
   public static final int    LS_IPARAM_GA_NGEN                 = 8518;
   /*  Population size in GA */
   public static final int    LS_IPARAM_GA_POPSIZE              = 8519;
   /*  Print level to log files */
   public static final int    LS_IPARAM_GA_FILEOUT              = 8520;
   /*  Print level for GA */
   public static final int    LS_IPARAM_GA_PRINTLEVEL           = 8521;
   /*  Flag to specify whether an optimum individual will be injected */
   public static final int    LS_IPARAM_GA_INJECT_OPT           = 8522;
   /*  Number of threads in GA */
   public static final int    LS_IPARAM_GA_NUM_THREADS          = 8523;
   /*  Objective function sense */
   public static final int    LS_IPARAM_GA_OBJDIR               = 8524;
   /*  Target objective function value */
   public static final int    LS_DPARAM_GA_OBJSTOP              = 8525;
   /*  Migration probability  */
   public static final int    LS_DPARAM_GA_MIGRATE_PROB         = 8526;
   /*  Search space or search mode  */
   public static final int    LS_IPARAM_GA_SSPACE               = 8527;
   /*! @} */

   /* Version info */
   public static final int    LS_IPARAM_VER_MAJOR               = 990;
   public static final int    LS_IPARAM_VER_MINOR               = 991;
   public static final int    LS_IPARAM_VER_BUILD               = 992;
   public static final int    LS_IPARAM_VER_REVISION            = 993;

   /* Last card for parameters */
   public static final int    LS_IPARAM_VER_NUMBER              = 999;


/* Math operator codes (1000-1500) */
   public static final int    EP_NO_OP                          = 0000;
   public static final int    EP_PLUS                           = 1001;
   public static final int    EP_MINUS                          = 1002;
   public static final int    EP_MULTIPLY                       = 1003;
   public static final int    EP_DIVIDE                         = 1004;
   public static final int    EP_POWER                          = 1005;
   public static final int    EP_EQUAL                          = 1006;
   public static final int    EP_NOT_EQUAL                      = 1007;
   public static final int    EP_LTOREQ                         = 1008;
   public static final int    EP_GTOREQ                         = 1009;
   public static final int    EP_LTHAN                          = 1010;
   public static final int    EP_GTHAN                          = 1011;
   public static final int    EP_AND                            = 1012;
   public static final int    EP_OR                             = 1013;
   public static final int    EP_NOT                            = 1014;
   public static final int    EP_PERCENT                        = 1015;
   public static final int    EP_POSATE                         = 1016;
   public static final int    EP_NEGATE                         = 1017;
   public static final int    EP_ABS                            = 1018;
   public static final int    EP_SQRT                           = 1019;
   public static final int    EP_LOG                            = 1020;
   public static final int    EP_LN                             = 1021;
   public static final int    EP_PI                             = 1022;
   public static final int    EP_SIN                            = 1023;
   public static final int    EP_COS                            = 1024;
   public static final int    EP_TAN                            = 1025;
   public static final int    EP_ATAN2                          = 1026;
   public static final int    EP_ATAN                           = 1027;
   public static final int    EP_ASIN                           = 1028;
   public static final int    EP_ACOS                           = 1029;
   public static final int    EP_EXP                            = 1030;
   public static final int    EP_MOD                            = 1031;
   public static final int    EP_FALSE                          = 1032;
   public static final int    EP_TRUE                           = 1033;
   public static final int    EP_IF                             = 1034;
   public static final int    EP_PSN                            = 1035;
   public static final int    EP_PSL                            = 1036;
   public static final int    EP_LGM                            = 1037;
   public static final int    EP_SIGN                           = 1038;
   public static final int    EP_FLOOR                          = 1039;
   public static final int    EP_FPA                            = 1040;
   public static final int    EP_FPL                            = 1041;
   public static final int    EP_PEL                            = 1042;
   public static final int    EP_PEB                            = 1043;
   public static final int    EP_PPS                            = 1044;
   public static final int    EP_PPL                            = 1045;
   public static final int    EP_PTD                            = 1046;
   public static final int    EP_PCX                            = 1047;
   public static final int    EP_WRAP                           = 1048;
   public static final int    EP_PBNO                           = 1049;
   public static final int    EP_PFS                            = 1050;
   public static final int    EP_PFD                            = 1051;
   public static final int    EP_PHG                            = 1052;
   public static final int    EP_RAND                           = 1053;
   public static final int    EP_USER                           = 1054;
   public static final int    EP_SUM                            = 1055;
   public static final int    EP_AVG                            = 1056;
   public static final int    EP_MIN                            = 1057;
   public static final int    EP_MAX                            = 1058;
   public static final int    EP_NPV                            = 1059;
   public static final int    EP_VAND                           = 1060;
   public static final int    EP_VOR                            = 1061;
   public static final int    EP_PUSH_NUM                       = 1062;
   public static final int    EP_PUSH_VAR                       = 1063;
   public static final int    EP_NORMDENS                       = 1064;
   public static final int    EP_NORMINV                        = 1065;
   public static final int    EP_TRIAINV                        = 1066;
   public static final int    EP_EXPOINV                        = 1067;
   public static final int    EP_UNIFINV                        = 1068;
   public static final int    EP_MULTINV                        = 1069;
   public static final int    EP_USRCOD                         = 1070;
   public static final int    EP_SUMPROD                        = 1071;
   public static final int    EP_SUMIF                          = 1072;
   public static final int    EP_VLOOKUP                        = 1073;
   public static final int    EP_VPUSH_NUM                      = 1074;
   public static final int    EP_VPUSH_VAR                      = 1075;
   public static final int    EP_VMULT                          = 1076;
   public static final int    EP_SQR                            = 1077;
   public static final int    EP_SINH                           = 1078;
   public static final int    EP_COSH                           = 1079;
   public static final int    EP_TANH                           = 1080;
   public static final int    EP_ASINH                          = 1081;
   public static final int    EP_ACOSH                          = 1082;
   public static final int    EP_ATANH                          = 1083;
   public static final int    EP_LOGB                           = 1084;
   public static final int    EP_LOGX                           = 1085;
   public static final int    EP_LNX                            = 1086;
   public static final int    EP_TRUNC                          = 1087;
   public static final int    EP_NORMSINV                       = 1088;
   public static final int    EP_INT                            = 1089;
   public static final int    EP_PUSH_STR                       = 1090;
   public static final int    EP_VPUSH_STR                      = 1091;
   public static final int    EP_PUSH_SPAR                      = 1092;
   public static final int    EP_NORMPDF                        = 1093;
   public static final int    EP_NORMCDF                        = 1094;
   public static final int    EP_LSQ                            = 1095;
   public static final int    EP_LNPSNX                         = 1096;
   public static final int    EP_LNCPSN                         = 1097;
   public static final int    EP_XEXPNAX                        = 1098;
   public static final int    EP_XNEXPMX                        = 1099;
   /* Probability density functions */
   public static final int    EP_PBT                            = 1100;
   public static final int    EP_PBTINV                         = 1101;
   public static final int    EP_PBNINV                         = 1102;
   public static final int    EP_PCC                            = 1103;
   public static final int    EP_PCCINV                         = 1104;
   public static final int    EP_PCXINV                         = 1105;
   public static final int    EP_EXPN                           = 1106;
   public static final int    EP_PFDINV                         = 1107;
   public static final int    EP_PGA                            = 1108;
   public static final int    EP_PGAINV                         = 1109;
   public static final int    EP_PGE                            = 1110;
   public static final int    EP_PGEINV                         = 1111;
   public static final int    EP_PGU                            = 1112;
   public static final int    EP_PGUINV                         = 1113;
   public static final int    EP_PHGINV                         = 1114;
   public static final int    EP_PLA                            = 1115;
   public static final int    EP_PLAINV                         = 1116;
   public static final int    EP_PLG                            = 1117;
   public static final int    EP_PLGINV                         = 1118;
   public static final int    EP_LGT                            = 1119;
   public static final int    EP_LGTINV                         = 1120;
   public static final int    EP_LGNM                           = 1121;
   public static final int    EP_LGNMINV                        = 1122;
   public static final int    EP_NGBN                           = 1123;
   public static final int    EP_NGBNINV                        = 1124;
   public static final int    EP_NRM                            = 1125;
   public static final int    EP_PPT                            = 1126;
   public static final int    EP_PPTINV                         = 1127;
   public static final int    EP_PPSINV                         = 1128;
   public static final int    EP_PTDINV                         = 1129;
   public static final int    EP_TRIAN                          = 1130;
   public static final int    EP_UNIFM                          = 1131;
   public static final int    EP_PWB                            = 1132;
   public static final int    EP_PWBINV                         = 1133;
   public static final int    EP_NRMINV                         = 1134;
   public static final int    EP_TRIANINV                       = 1135;
   public static final int    EP_EXPNINV                        = 1136;
   public static final int    EP_UNIFMINV                       = 1137;
   public static final int    EP_MLTNMINV                       = 1138;
   public static final int    EP_BTDENS                         = 1139;
   public static final int    EP_BNDENS                         = 1140;
   public static final int    EP_CCDENS                         = 1141;
   public static final int    EP_CXDENS                         = 1142;
   public static final int    EP_EXPDENS                        = 1143;
   public static final int    EP_FDENS                          = 1144;
   public static final int    EP_GADENS                         = 1145;
   public static final int    EP_GEDENS                         = 1146;
   public static final int    EP_GUDENS                         = 1147;
   public static final int    EP_HGDENS                         = 1148;
   public static final int    EP_LADENS                         = 1149;
   public static final int    EP_LGDENS                         = 1150;
   public static final int    EP_LGTDENS                        = 1151;
   public static final int    EP_LGNMDENS                       = 1152;
   public static final int    EP_NGBNDENS                       = 1153;
   public static final int    EP_NRMDENS                        = 1154;
   public static final int    EP_PTDENS                         = 1155;
   public static final int    EP_PSDENS                         = 1156;
   public static final int    EP_TDENS                          = 1157;
   public static final int    EP_TRIADENS                       = 1158;
   public static final int    EP_UNIFDENS                       = 1159;
   public static final int    EP_WBDENS                         = 1160;
   public static final int    EP_RADIANS                        = 1161;
   public static final int    EP_DEGREES                        = 1162;
   public static final int    EP_ROUND                          = 1163;
   public static final int    EP_ROUNDUP                        = 1164;
   public static final int    EP_ROUNDDOWN                      = 1165;
   public static final int    EP_ERF                            = 1166;
   public static final int    EP_PBN                            = 1167;
   public static final int    EP_PBB                            = 1168;
   public static final int    EP_PBBINV                         = 1169;
   public static final int    EP_BBDENS                         = 1170;
   public static final int    EP_PSS                            = 1171;
   public static final int    EP_SSDENS                         = 1172;
   public static final int    EP_SSINV                          = 1173;
   public static final int    EP_POSD                           = 1174;
   public static final int    EP_SETS                           = 1175;
   public static final int    EP_CARD                           = 1176;
   public static final int    EP_STDEV                          = 1177;
   public static final int    EP_LMTD                           = 1178;
   public static final int    EP_RLMTD                          = 1179;
   public static final int    EP_LOGIT                          = 1180;
   public static final int    EP_ALLDIFF                        = 1181;
   public static final int    EP_SIGNPOWER                      = 1182;
   public static final int    EP_QUADPROD                       = 1183;
   public static final int    EP_ATAN2R                         = 1184;
   public static final int    EP_XPOWDIVAB                      = 1185;


/* Model and solution information codes ( 110xx-140xx) */
/* Model statistics (11001-11199)*/
   public static final int    LS_IINFO_NUM_NONZ_OBJ             = 11001;
   public static final int    LS_IINFO_NUM_SEMICONT             = 11002;
   public static final int    LS_IINFO_NUM_SETS                 = 11003;
   public static final int    LS_IINFO_NUM_SETS_NNZ             = 11004;
   public static final int    LS_IINFO_NUM_QCP_CONS             = 11005;
   public static final int    LS_IINFO_NUM_CONT_CONS            = 11006;
   public static final int    LS_IINFO_NUM_INT_CONS             = 11007;
   public static final int    LS_IINFO_NUM_BIN_CONS             = 11008;
   public static final int    LS_IINFO_NUM_QCP_VARS             = 11009;
   public static final int    LS_IINFO_NUM_CONS                 = 11010;
   public static final int    LS_IINFO_NUM_VARS                 = 11011;
   public static final int    LS_IINFO_NUM_NONZ                 = 11012;
   public static final int    LS_IINFO_NUM_BIN                  = 11013;
   public static final int    LS_IINFO_NUM_INT                  = 11014;
   public static final int    LS_IINFO_NUM_CONT                 = 11015;
   public static final int    LS_IINFO_NUM_QC_NONZ              = 11016;
   public static final int    LS_IINFO_NUM_NLP_NONZ             = 11017;
   public static final int    LS_IINFO_NUM_NLPOBJ_NONZ          = 11018;
   public static final int    LS_IINFO_NUM_RDCONS               = 11019;
   public static final int    LS_IINFO_NUM_RDVARS               = 11020;
   public static final int    LS_IINFO_NUM_RDNONZ               = 11021;
   public static final int    LS_IINFO_NUM_RDINT                = 11022;
   public static final int    LS_IINFO_LEN_VARNAMES             = 11023;
   public static final int    LS_IINFO_LEN_CONNAMES             = 11024;
   public static final int    LS_IINFO_NUM_NLP_CONS             = 11025;
   public static final int    LS_IINFO_NUM_NLP_VARS             = 11026;
   public static final int    LS_IINFO_NUM_SUF_ROWS             = 11027;
   public static final int    LS_IINFO_NUM_IIS_ROWS             = 11028;
   public static final int    LS_IINFO_NUM_SUF_BNDS             = 11029;
   public static final int    LS_IINFO_NUM_IIS_BNDS             = 11030;
   public static final int    LS_IINFO_NUM_SUF_COLS             = 11031;
   public static final int    LS_IINFO_NUM_IUS_COLS             = 11032;
   public static final int    LS_IINFO_NUM_CONES                = 11033;
   public static final int    LS_IINFO_NUM_CONE_NONZ            = 11034;
   public static final int    LS_IINFO_LEN_CONENAMES            = 11035;
   public static final int    LS_DINFO_INST_VAL_MIN_COEF        = 11036;
   public static final int    LS_IINFO_INST_VARNDX_MIN_COEF     = 11037;
   public static final int    LS_IINFO_INST_CONNDX_MIN_COEF     = 11038;
   public static final int    LS_DINFO_INST_VAL_MAX_COEF        = 11039;
   public static final int    LS_IINFO_INST_VARNDX_MAX_COEF     = 11040;
   public static final int    LS_IINFO_INST_CONNDX_MAX_COEF     = 11041;
   public static final int    LS_IINFO_NUM_VARS_CARD            = 11042;
   public static final int    LS_IINFO_NUM_VARS_SOS1            = 11043;
   public static final int    LS_IINFO_NUM_VARS_SOS2            = 11044;
   public static final int    LS_IINFO_NUM_VARS_SOS3            = 11045;
   public static final int    LS_IINFO_NUM_VARS_SCONT           = 11046;
   public static final int    LS_IINFO_NUM_CONS_L               = 11047;
   public static final int    LS_IINFO_NUM_CONS_E               = 11048;
   public static final int    LS_IINFO_NUM_CONS_G               = 11049;
   public static final int    LS_IINFO_NUM_CONS_R               = 11050;
   public static final int    LS_IINFO_NUM_CONS_N               = 11051;
   public static final int    LS_IINFO_NUM_VARS_LB              = 11052;
   public static final int    LS_IINFO_NUM_VARS_UB              = 11053;
   public static final int    LS_IINFO_NUM_VARS_LUB             = 11054;
   public static final int    LS_IINFO_NUM_VARS_FR              = 11055;
   public static final int    LS_IINFO_NUM_VARS_FX              = 11056;
   public static final int    LS_IINFO_NUM_INST_CODES           = 11057;
   public static final int    LS_IINFO_NUM_INST_REAL_NUM        = 11058;
   public static final int    LS_IINFO_NUM_SPARS                = 11059;
   public static final int    LS_IINFO_NUM_PROCS                = 11060;
   public static final int    LS_IINFO_NUM_POSDS                = 11061;
   public static final int    LS_IINFO_NUM_SUF_INTS             = 11062;
   public static final int    LS_IINFO_NUM_IIS_INTS             = 11063;
   public static final int    LS_IINFO_NUM_OBJPOOL              = 11064;
   public static final int    LS_IINFO_NUM_SOLPOOL              = 11065;
   public static final int    LS_IINFO_NUM_ALLDIFF              = 11066;
   public static final int    LS_IINFO_MAX_RNONZ                = 11067;
   public static final int    LS_IINFO_MAX_CNONZ                = 11068;
   public static final int    LS_DINFO_AVG_RNONZ                = 11069;
   public static final int    LS_DINFO_AVG_CNONZ                = 11070;

/* LP and NLP related info (11200-11299)*/
   public static final int    LS_IINFO_METHOD                   = 11200;
   public static final int    LS_DINFO_POBJ                     = 11201;
   public static final int    LS_DINFO_DOBJ                     = 11202;
   public static final int    LS_DINFO_PINFEAS                  = 11203;
   public static final int    LS_DINFO_DINFEAS                  = 11204;
   public static final int    LS_IINFO_MODEL_STATUS             = 11205;
   public static final int    LS_IINFO_PRIMAL_STATUS            = 11206;
   public static final int    LS_IINFO_DUAL_STATUS              = 11207;
   public static final int    LS_IINFO_BASIC_STATUS             = 11208;
   public static final int    LS_IINFO_BAR_ITER                 = 11209;
   public static final int    LS_IINFO_SIM_ITER                 = 11210;
   public static final int    LS_IINFO_NLP_ITER                 = 11211;
   public static final int    LS_IINFO_ELAPSED_TIME             = 11212;
   public static final int    LS_DINFO_MSW_POBJ                 = 11213;
   public static final int    LS_IINFO_MSW_PASS                 = 11214;
   public static final int    LS_IINFO_MSW_NSOL                 = 11215;
   public static final int    LS_IINFO_IPM_STATUS               = 11216;
   public static final int    LS_DINFO_IPM_POBJ                 = 11217;
   public static final int    LS_DINFO_IPM_DOBJ                 = 11218;
   public static final int    LS_DINFO_IPM_PINFEAS              = 11219;
   public static final int    LS_DINFO_IPM_DINFEAS              = 11220;
   public static final int    LS_IINFO_NLP_CALL_FUN             = 11221;
   public static final int    LS_IINFO_NLP_CALL_DEV             = 11222;
   public static final int    LS_IINFO_NLP_CALL_HES             = 11223;
   public static final int    LS_IINFO_CONCURRENT_OPTIMIZER     = 11224;
   public static final int    LS_IINFO_LEN_STAGENAMES           = 11225;
   public static final int    LS_DINFO_BAR_ITER                 = 11226;
   public static final int    LS_DINFO_SIM_ITER                 = 11227;
   public static final int    LS_DINFO_NLP_ITER                 = 11228;
   public static final int    LS_IINFO_BAR_THREADS              = 11229;
   public static final int    LS_IINFO_NLP_THREADS              = 11230;
   public static final int    LS_IINFO_SIM_THREADS              = 11231;
   public static final int    LS_DINFO_NLP_THRIMBL              = 11232;
   public static final int    LS_SINFO_NLP_THREAD_LOAD          = 11233;
   public static final int    LS_SINFO_BAR_THREAD_LOAD          = 11234;
   public static final int    LS_SINFO_SIM_THREAD_LOAD          = 11235;
   public static final int    LS_SINFO_ARCH                     = 11236;
   public static final int    LS_IINFO_ARCH_ID                  = 11237;
   public static final int    LS_IINFO_MSW_BESTRUNIDX           = 11238;
   public static final int    LS_DINFO_ACONDEST                 = 11239;
   public static final int    LS_DINFO_BCONDEST                 = 11240;
   public static final int    LS_IINFO_LPTOOL                   = 11241;
   public static final int    LS_SINFO_MODEL_TYPE               = 11242;
   public static final int    LS_IINFO_NLP_LINEARITY            = 11243;

/* MIP and MINLP related info (11300-11400) */
   public static final int    LS_IINFO_MIP_STATUS               = 11300;
   public static final int    LS_DINFO_MIP_OBJ                  = 11301;
   public static final int    LS_DINFO_MIP_BESTBOUND            = 11302;
   public static final int    LS_IINFO_MIP_SIM_ITER             = 11303;
   public static final int    LS_IINFO_MIP_BAR_ITER             = 11304;
   public static final int    LS_IINFO_MIP_NLP_ITER             = 11305;
   public static final int    LS_IINFO_MIP_BRANCHCOUNT          = 11306;
   public static final int    LS_IINFO_MIP_NEWIPSOL             = 11307;
   public static final int    LS_IINFO_MIP_LPCOUNT              = 11308;
   public static final int    LS_IINFO_MIP_ACTIVENODES          = 11309;
   public static final int    LS_IINFO_MIP_LTYPE                = 11310;
   public static final int    LS_IINFO_MIP_AOPTTIMETOSTOP       = 11311;
   public static final int    LS_IINFO_MIP_NUM_TOTAL_CUTS       = 11312;
   public static final int    LS_IINFO_MIP_GUB_COVER_CUTS       = 11313;
   public static final int    LS_IINFO_MIP_FLOW_COVER_CUTS      = 11314;
   public static final int    LS_IINFO_MIP_LIFT_CUTS            = 11315;
   public static final int    LS_IINFO_MIP_PLAN_LOC_CUTS        = 11316;
   public static final int    LS_IINFO_MIP_DISAGG_CUTS          = 11317;
   public static final int    LS_IINFO_MIP_KNAPSUR_COVER_CUTS   = 11318;
   public static final int    LS_IINFO_MIP_LATTICE_CUTS         = 11319;
   public static final int    LS_IINFO_MIP_GOMORY_CUTS          = 11320;
   public static final int    LS_IINFO_MIP_COEF_REDC_CUTS       = 11321;
   public static final int    LS_IINFO_MIP_GCD_CUTS             = 11322;
   public static final int    LS_IINFO_MIP_OBJ_CUT              = 11323;
   public static final int    LS_IINFO_MIP_BASIS_CUTS           = 11324;
   public static final int    LS_IINFO_MIP_CARDGUB_CUTS         = 11325;
   public static final int    LS_IINFO_MIP_CLIQUE_CUTS          = 11326;
   public static final int    LS_IINFO_MIP_CONTRA_CUTS          = 11327;
   public static final int    LS_IINFO_MIP_GUB_CONS             = 11328;
   public static final int    LS_IINFO_MIP_GLB_CONS             = 11329;
   public static final int    LS_IINFO_MIP_PLANTLOC_CONS        = 11330;
   public static final int    LS_IINFO_MIP_DISAGG_CONS          = 11331;
   public static final int    LS_IINFO_MIP_SB_CONS              = 11332;
   public static final int    LS_IINFO_MIP_IKNAP_CONS           = 11333;
   public static final int    LS_IINFO_MIP_KNAP_CONS            = 11334;
   public static final int    LS_IINFO_MIP_NLP_CONS             = 11335;
   public static final int    LS_IINFO_MIP_CONT_CONS            = 11336;
   public static final int    LS_DINFO_MIP_TOT_TIME             = 11347;
   public static final int    LS_DINFO_MIP_OPT_TIME             = 11348;
   public static final int    LS_DINFO_MIP_HEU_TIME             = 11349;
   public static final int    LS_IINFO_MIP_SOLSTATUS_LAST_BRANCH = 11350;
   public static final int    LS_DINFO_MIP_SOLOBJVAL_LAST_BRANCH = 11351;
   public static final int    LS_IINFO_MIP_HEU_LEVEL            = 11352;
   public static final int    LS_DINFO_MIP_PFEAS                = 11353;
   public static final int    LS_DINFO_MIP_INTPFEAS             = 11354;
   public static final int    LS_IINFO_MIP_WHERE_IN_CODE        = 11355;
   public static final int    LS_IINFO_MIP_FP_ITER              = 11356;
   public static final int    LS_DINFO_MIP_FP_SUMFEAS           = 11357;
   public static final int    LS_DINFO_MIP_RELMIPGAP            = 11358;
   public static final int    LS_DINFO_MIP_ROOT_OPT_TIME        = 11359;
   public static final int    LS_DINFO_MIP_ROOT_PRE_TIME        = 11360;
   public static final int    LS_IINFO_MIP_ROOT_METHOD          = 11361;
   public static final int    LS_DINFO_MIP_SIM_ITER             = 11362;
   public static final int    LS_DINFO_MIP_BAR_ITER             = 11363;
   public static final int    LS_DINFO_MIP_NLP_ITER             = 11364;
   public static final int    LS_IINFO_MIP_TOP_RELAX_IS_NON_CONVEX = 11365;
   public static final int    LS_DINFO_MIP_FP_TIME              = 11366;
   public static final int    LS_IINFO_MIP_THREADS              = 11367;
   public static final int    LS_SINFO_MIP_THREAD_LOAD          = 11368;
   public static final int    LS_DINFO_MIP_ABSGAP               = 11369;
   public static final int    LS_DINFO_MIP_RELGAP               = 11370;
   public static final int    LS_IINFO_MIP_SOFTKNAP_CUTS        = 11371;
   public static final int    LS_IINFO_MIP_LP_ROUND_CUTS        = 11372;
   public static final int    LS_IINFO_MIP_PERSPECTIVE_CUTS     = 11373;
   public static final int    LS_IINFO_MIP_STRATEGY_MASK        = 11374;

/* GOP related info (11601-11699) */
   public static final int    LS_DINFO_GOP_OBJ                  = 11600;
   public static final int    LS_IINFO_GOP_SIM_ITER             = 11601;
   public static final int    LS_IINFO_GOP_BAR_ITER             = 11602;
   public static final int    LS_IINFO_GOP_NLP_ITER             = 11603;
   public static final int    LS_DINFO_GOP_BESTBOUND            = 11604;
   public static final int    LS_IINFO_GOP_STATUS               = 11605;
   public static final int    LS_IINFO_GOP_LPCOUNT              = 11606;
   public static final int    LS_IINFO_GOP_NLPCOUNT             = 11607;
   public static final int    LS_IINFO_GOP_MIPCOUNT             = 11608;
   public static final int    LS_IINFO_GOP_NEWSOL               = 11609;
   public static final int    LS_IINFO_GOP_BOX                  = 11610;
   public static final int    LS_IINFO_GOP_BBITER               = 11611;
   public static final int    LS_IINFO_GOP_SUBITER              = 11612;
   public static final int    LS_IINFO_GOP_MIPBRANCH            = 11613;
   public static final int    LS_IINFO_GOP_ACTIVEBOXES          = 11614;
   public static final int    LS_IINFO_GOP_TOT_TIME             = 11615;
   public static final int    LS_IINFO_GOP_MAXDEPTH             = 11616;
   public static final int    LS_DINFO_GOP_PFEAS                = 11617;
   public static final int    LS_DINFO_GOP_INTPFEAS             = 11618;
   public static final int    LS_DINFO_GOP_SIM_ITER             = 11619;
   public static final int    LS_DINFO_GOP_BAR_ITER             = 11620;
   public static final int    LS_DINFO_GOP_NLP_ITER             = 11621;
   public static final int    LS_DINFO_GOP_LPCOUNT              = 11622;
   public static final int    LS_DINFO_GOP_NLPCOUNT             = 11623;
   public static final int    LS_DINFO_GOP_MIPCOUNT             = 11624;
   public static final int    LS_DINFO_GOP_BBITER               = 11625;
   public static final int    LS_DINFO_GOP_SUBITER              = 11626;
   public static final int    LS_DINFO_GOP_MIPBRANCH            = 11627;
   public static final int    LS_DINFO_GOP_FIRST_TIME           = 11628;
   public static final int    LS_DINFO_GOP_BEST_TIME            = 11629;
   public static final int    LS_DINFO_GOP_TOT_TIME             = 11630;
   public static final int    LS_IINFO_GOP_THREADS              = 11631;
   public static final int    LS_SINFO_GOP_THREAD_LOAD          = 11632;
   public static final int    LS_DINFO_GOP_ABSGAP               = 11633;
   public static final int    LS_DINFO_GOP_RELGAP               = 11634;

   /* Progress info during callbacks */
   public static final int    LS_DINFO_SUB_OBJ                  = 11700;
   public static final int    LS_DINFO_SUB_PINF                 = 11701;
   public static final int    LS_DINFO_CUR_OBJ                  = 11702;
   public static final int    LS_IINFO_CUR_ITER                 = 11703;
   public static final int    LS_DINFO_CUR_BEST_BOUND           = 11704;
   public static final int    LS_IINFO_CUR_STATUS               = 11705;
   public static final int    LS_IINFO_CUR_LP_COUNT             = 11706;
   public static final int    LS_IINFO_CUR_BRANCH_COUNT         = 11707;
   public static final int    LS_IINFO_CUR_ACTIVE_COUNT         = 11708;
   public static final int    LS_IINFO_CUR_NLP_COUNT            = 11709;
   public static final int    LS_IINFO_CUR_MIP_COUNT            = 11710;
   public static final int    LS_IINFO_CUR_CUT_COUNT            = 11711;
   public static final int    LS_DINFO_CUR_ITER                 = 11712;

/* Model generation progress info (1800+)*/
   public static final int    LS_DINFO_GEN_PERCENT              = 11800;
   public static final int    LS_IINFO_GEN_NONZ_TTL             = 11801;
   public static final int    LS_IINFO_GEN_NONZ_NL              = 11802;
   public static final int    LS_IINFO_GEN_ROW_NL               = 11803;
   public static final int    LS_IINFO_GEN_VAR_NL               = 11804;

/* IIS-IUS info */
   public static final int    LS_IINFO_IIS_BAR_ITER             = 11850;
   public static final int    LS_IINFO_IIS_SIM_ITER             = 11851;
   public static final int    LS_IINFO_IIS_NLP_ITER             = 11852;
   public static final int    LS_DINFO_IIS_BAR_ITER             = 11853;
   public static final int    LS_DINFO_IIS_SIM_ITER             = 11854;
   public static final int    LS_DINFO_IIS_NLP_ITER             = 11855;
   public static final int    LS_IINFO_IIS_TOT_TIME             = 11856;
   public static final int    LS_IINFO_IIS_ACT_NODE             = 11857;
   public static final int    LS_IINFO_IIS_LPCOUNT              = 11858;
   public static final int    LS_IINFO_IIS_NLPCOUNT             = 11859;
   public static final int    LS_IINFO_IIS_MIPCOUNT             = 11860;
   public static final int    LS_IINFO_IIS_THREADS              = 11861;
   public static final int    LS_SINFO_IIS_THREAD_LOAD          = 11862;
   public static final int    LS_IINFO_IIS_STATUS               = 11863;

   public static final int    LS_IINFO_IUS_BAR_ITER             = 11875;
   public static final int    LS_IINFO_IUS_SIM_ITER             = 11876;
   public static final int    LS_IINFO_IUS_NLP_ITER             = 11877;
   public static final int    LS_DINFO_IUS_BAR_ITER             = 11878;
   public static final int    LS_DINFO_IUS_SIM_ITER             = 11879;
   public static final int    LS_DINFO_IUS_NLP_ITER             = 11880;
   public static final int    LS_IINFO_IUS_TOT_TIME             = 11881;
   public static final int    LS_IINFO_IUS_ACT_NODE             = 11882;
   public static final int    LS_IINFO_IUS_LPCOUNT              = 11883;
   public static final int    LS_IINFO_IUS_NLPCOUNT             = 11884;
   public static final int    LS_IINFO_IUS_MIPCOUNT             = 11885;
   public static final int    LS_IINFO_IUS_THREADS              = 11886;
   public static final int    LS_SINFO_IUS_THREAD_LOAD          = 11887;
   public static final int    LS_IINFO_IUS_STATUS               = 11888;

/* Presolve info    */
   public static final int    LS_IINFO_PRE_NUM_RED              = 11900;
   public static final int    LS_IINFO_PRE_TYPE_RED             = 11901;
   public static final int    LS_IINFO_PRE_NUM_RDCONS           = 11902;
   public static final int    LS_IINFO_PRE_NUM_RDVARS           = 11903;
   public static final int    LS_IINFO_PRE_NUM_RDNONZ           = 11904;
   public static final int    LS_IINFO_PRE_NUM_RDINT            = 11905;

/* Error info */
   public static final int    LS_IINFO_ERR_OPTIM                = 11999;

/* Profiler contexts */
   /* IIS Profiler */
   public static final int    LS_DINFO_PROFILE_BASE             = 12000;
   public static final int    LS_DINFO_PROFILE_IIS_FIND_NEC_ROWS = 12050;
   public static final int    LS_DINFO_PROFILE_IIS_FIND_NEC_COLS = 12051;
   public static final int    LS_DINFO_PROFILE_IIS_FIND_SUF_ROWS = 12052;
   public static final int    LS_DINFO_PROFILE_IIS_FIND_SUF_COLS = 12053;
   /* MIP Profiler */
   public static final int    LS_DINFO_PROFILE_MIP_ROOT_LP      = 12101;
   public static final int    LS_DINFO_PROFILE_MIP_TOTAL_LP     = 12102;
   public static final int    LS_DINFO_PROFILE_MIP_LP_SIM_PRIMAL = 12103;
   public static final int    LS_DINFO_PROFILE_MIP_LP_SIM_DUAL  = 12104;
   public static final int    LS_DINFO_PROFILE_MIP_LP_SIM_BARRIER = 12105;
   public static final int    LS_DINFO_PROFILE_MIP_PRE_PROCESS  = 12106;
   public static final int    LS_DINFO_PROFILE_MIP_FEA_PUMP     = 12107;
   public static final int    LS_DINFO_PROFILE_MIP_TOP_HEURISTIC = 12108;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_HEURISTIC = 12109;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_MAIN_LOOP = 12110;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_SUB_LOOP = 12111;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_BEFORE_BEST = 12112;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_AFTER_BEST = 12113;
   public static final int    LS_DINFO_PROFILE_MIP_TOP_CUT      = 12114;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_CUT      = 12115;
   public static final int    LS_DINFO_PROFILE_MIP_LP_NON_BNB_LOOP = 12116;
   public static final int    LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_MAIN = 12117;
   public static final int    LS_DINFO_PROFILE_MIP_LP_BNB_LOOP_SUB = 12118;
   public static final int    LS_DINFO_PROFILE_MIP_NODE_PRESOLVE = 12119;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING = 12120;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING_MAIN = 12121;
   public static final int    LS_DINFO_PROFILE_MIP_BNB_BRANCHING_SUB = 12122;
   /* GOP Profiler */
   public static final int    LS_DINFO_PROFILE_GOP_SUB_LP_SOLVER = 12251;
   public static final int    LS_DINFO_PROFILE_GOP_SUB_NLP_SOLVER = 12252;
   public static final int    LS_DINFO_PROFILE_GOP_SUB_MIP_SOLVER = 12253;
   public static final int    LS_DINFO_PROFILE_GOP_SUB_GOP_SOLVER = 12254;
   public static final int    LS_DINFO_PROFILE_GOP_CONS_PROP_LP = 12255;
   public static final int    LS_DINFO_PROFILE_GOP_CONS_PROP_NLP = 12256;
   public static final int    LS_DINFO_PROFILE_GOP_VAR_MIN_MAX  = 12257;

/* Misc info */
   public static final int    LS_SINFO_MODEL_FILENAME           = 12950;
   public static final int    LS_SINFO_MODEL_SOURCE             = 12951;
   public static final int    LS_IINFO_MODEL_TYPE               = 12952;
   public static final int    LS_SINFO_CORE_FILENAME            = 12953;
   public static final int    LS_SINFO_STOC_FILENAME            = 12954;
   public static final int    LS_SINFO_TIME_FILENAME            = 12955;
   public static final int    LS_IINFO_ASSIGNED_MODEL_TYPE      = 12956;
   public static final int    LS_IINFO_NZCINDEX                 = 12957;
   public static final int    LS_IINFO_NZRINDEX                 = 12958;
   public static final int    LS_IINFO_NZCRANK                  = 12959;
   public static final int    LS_IINFO_NZRRANK                  = 12960;

/* Stochastic Information */

  /*! @ingroup LSstocInfo @{ */
  /*! Expected value of the objective function.  */
   public static final int    LS_DINFO_STOC_EVOBJ               = 13201;
   /*! Expected value of perfect information.  */
   public static final int    LS_DINFO_STOC_EVPI                = 13202;
   /*! Primal infeasibility of the first stage solution.  */
   public static final int    LS_DINFO_STOC_PINFEAS             = 13203;
   /*! Dual infeasibility of the first stage solution.  */
   public static final int    LS_DINFO_STOC_DINFEAS             = 13204;
   /*! Relative optimality gap at current solution.  */
   public static final int    LS_DINFO_STOC_RELOPT_GAP          = 13205;
   /*! Absolute optimality gap at current solution.  */
   public static final int    LS_DINFO_STOC_ABSOPT_GAP          = 13206;
   /*! Number of simplex iterations performed.  */
   public static final int    LS_IINFO_STOC_SIM_ITER            = 13207;
   /*! Number of barrier iterations performed.  */
   public static final int    LS_IINFO_STOC_BAR_ITER            = 13208;
   /*! Number of nonlinear iterations performed.  */
   public static final int    LS_IINFO_STOC_NLP_ITER            = 13209;
   /*! Number of stochastic parameters in the RHS.  */
   public static final int    LS_IINFO_NUM_STOCPAR_RHS          = 13210;
   /*! Number of stochastic parameters in the objective function.  */
   public static final int    LS_IINFO_NUM_STOCPAR_OBJ          = 13211;
   /*! Number of stochastic parameters in the lower bound.  */
   public static final int    LS_IINFO_NUM_STOCPAR_LB           = 13212;
   /*! Number of stochastic parameters in the upper bound.  */
   public static final int    LS_IINFO_NUM_STOCPAR_UB           = 13213;
   /*! Number of stochastic parameters in the instructions constituting the objective.  */
   public static final int    LS_IINFO_NUM_STOCPAR_INSTR_OBJS   = 13214;
   /*! Number of stochastic parameters in the instructions constituting the constraints.  */
   public static final int    LS_IINFO_NUM_STOCPAR_INSTR_CONS   = 13215;
   /*! Number of stochastic parameters in the LP matrix.  */
   public static final int    LS_IINFO_NUM_STOCPAR_AIJ          = 13216;
   /*! Total time elapsed in seconds to solve the model  */
   public static final int    LS_DINFO_STOC_TOTAL_TIME          = 13217;
   /*! Status of the SP model.  */
   public static final int    LS_IINFO_STOC_STATUS              = 13218;
   /*! Stage of the specified node.  */
   public static final int    LS_IINFO_STOC_STAGE_BY_NODE       = 13219;
   /*! Number of scenarios (integer) in the scenario tree. */
   public static final int    LS_IINFO_STOC_NUM_SCENARIOS       = 13220;
   /*! Number of scenarios (double) in the scenario tree. */
   public static final int    LS_DINFO_STOC_NUM_SCENARIOS       = 13221;
   /*! Number of stages in the model. */
   public static final int    LS_IINFO_STOC_NUM_STAGES          = 13222;
   /*! Number of nodes in the scenario tree (integer). */
   public static final int    LS_IINFO_STOC_NUM_NODES           = 13223;
   /*! Number of nodes in the scenario tree (double). */
   public static final int    LS_DINFO_STOC_NUM_NODES           = 13224;
   /*! Number of nodes that belong to specified stage in the scenario tree (integer). */
   public static final int    LS_IINFO_STOC_NUM_NODES_STAGE     = 13225;
   /*! Number of nodes that belong to specified stage in the scenario tree (double). */
   public static final int    LS_DINFO_STOC_NUM_NODES_STAGE     = 13226;
   /*! Number of node-models created or to be created. */
   public static final int    LS_IINFO_STOC_NUM_NODE_MODELS     = 13227;
   /*! Column offset in DEQ of the first variable associated with the specified node.  */
   public static final int    LS_IINFO_STOC_NUM_COLS_BEFORE_NODE = 13228;
   /*! Row offset in DEQ of the first variable associated with the specified node. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_BEFORE_NODE = 13229;
   /*! Total number of columns in the implicit DEQ (integer). */
   public static final int    LS_IINFO_STOC_NUM_COLS_DETEQI     = 13230;
   /*! Total number of columns in the implicit DEQ (double). */
   public static final int    LS_DINFO_STOC_NUM_COLS_DETEQI     = 13231;
   /*! Total number of rows in the implicit DEQ (integer). */
   public static final int    LS_IINFO_STOC_NUM_ROWS_DETEQI     = 13232;
   /*! Total number of rows in the implicit DEQ (double). */
   public static final int    LS_DINFO_STOC_NUM_ROWS_DETEQI     = 13233;
   /*! Total number of columns in the explict DEQ (integer). */
   public static final int    LS_IINFO_STOC_NUM_COLS_DETEQE     = 13234;
   /*! Total number of columns in the explict DEQ (double). */
   public static final int    LS_DINFO_STOC_NUM_COLS_DETEQE     = 13235;
   /*! Total number of rows in the explict DEQ (integer). */
   public static final int    LS_IINFO_STOC_NUM_ROWS_DETEQE     = 13236;
   /*! Total number of rows in the explict DEQ (double). */
   public static final int    LS_DINFO_STOC_NUM_ROWS_DETEQE     = 13237;
   /*! Total number of columns in non-anticipativity block. */
   public static final int    LS_IINFO_STOC_NUM_COLS_NAC        = 13238;
   /*! Total number of rows in non-anticipativity block. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_NAC        = 13239;
   /*! Total number of columns in core model. */
   public static final int    LS_IINFO_STOC_NUM_COLS_CORE       = 13240;
   /*! Total number of rows in core model. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_CORE       = 13241;
   /*! Total number of columns in core model in the specified stage. */
   public static final int    LS_IINFO_STOC_NUM_COLS_STAGE      = 13242;
   /*! Total number of rows in core model in the specified stage. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_STAGE      = 13243;
   /*! Total number of feasibility cuts generated during NBD iterations. */
   public static final int    LS_IINFO_STOC_NUM_NBF_CUTS        = 13244;
   /*! Total number of optimality cuts generated during NBD iterations. */
   public static final int    LS_IINFO_STOC_NUM_NBO_CUTS        = 13245;
   /*! Distribution type of the sample */
   public static final int    LS_IINFO_DIST_TYPE                = 13246;
   /*! Sample size. */
   public static final int    LS_IINFO_SAMP_SIZE                = 13247;
   /*! Sample mean. */
   public static final int    LS_DINFO_SAMP_MEAN                = 13248;
   /*! Sample standard deviation. */
   public static final int    LS_DINFO_SAMP_STD                 = 13249;
   /*! Sample skewness. */
   public static final int    LS_DINFO_SAMP_SKEWNESS            = 13250;
   /*! Sample kurtosis. */
   public static final int    LS_DINFO_SAMP_KURTOSIS            = 13251;
   /*! Total number of quadratic constraints in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQE = 13252;
   /*! Total number of continuous constraints in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQE = 13253;
   /*! Total number of constraints with general integer variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_CONS_DETEQE = 13254;
   /*! Total number of constraints with binary variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQE = 13255;
   /*! Total number of quadratic variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQE = 13256;
   /*! Total number of nonzeros in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NONZ_DETEQE     = 13259;
   /*! Total number of binaries in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_DETEQE      = 13260;
   /*! Total number of general integer variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_DETEQE      = 13261;
   /*! Total number of continuous variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_DETEQE     = 13262;
   /*! Total number of quadratic nonzeros in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQE  = 13263;
   /*! Total number of nonlinear nonzeros in the constraints of explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQE = 13264;
   /*! Total number of nonlinear nonzeros in the objective function of explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQE = 13265;
   /*! Total number of quadratic constraints in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQI = 13266;
   /*! Total number of continuous constraints in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQI = 13267;
   /*! Total number of constraints with general integer variables in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_CONS_DETEQI = 13268;
   /*! Total number of constraints with binary variables in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQI = 13269;
   /*! Total number of quadratic variables in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQI = 13270;
   /*! Total number of nonzeros in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NONZ_DETEQI     = 13271;
   /*! Total number of binaries in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_DETEQI      = 13272;
   /*! Total number of general integer variables in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_DETEQI      = 13273;
   /*! Total number of continuous variables in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_DETEQI     = 13274;
   /*! Total number of quadratic nonzeros in the implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQI  = 13275;
   /*! Total number of nonlinear nonzeros in the constraints of implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQI = 13276;
   /*! Total number of nonlinear nonzeros in the objective function of implicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQI = 13277;
   /*! Total number of block events. */
   public static final int    LS_IINFO_STOC_NUM_EVENTS_BLOCK    = 13278;
   /*! Total number of independent events with a discrete distribution. */
   public static final int    LS_IINFO_STOC_NUM_EVENTS_DISCRETE = 13279;
   /*! Total number of independent events with a parametric distribution. */
   public static final int    LS_IINFO_STOC_NUM_EVENTS_PARAMETRIC = 13280;
   /*! Total number of events loaded explictly as a scenario */
   public static final int    LS_IINFO_STOC_NUM_EXPLICIT_SCENARIOS = 13281;
   /*! Index of a node's parent*/
   public static final int    LS_IINFO_STOC_PARENT_NODE         = 13282;
   /*! Index of a node's eldest child*/
   public static final int    LS_IINFO_STOC_ELDEST_CHILD_NODE   = 13283;
   /*! Total number of childs a node has */
   public static final int    LS_IINFO_STOC_NUM_CHILD_NODES     = 13284;
   /*! Number of stochastic parameters in the instruction list.  */
   public static final int    LS_IINFO_NUM_STOCPAR_INSTR        = 13285;
   /*! The index of the scenario which is infeasible or unbounded.  */
   public static final int    LS_IINFO_INFORUNB_SCEN_IDX        = 13286;
   /*! Expected value of modeling uncertainity.  */
   public static final int    LS_DINFO_STOC_EVMU                = 13287;
   /*! Expected value of wait-and-see model's objective.  */
   public static final int    LS_DINFO_STOC_EVWS                = 13288;
   /*! Expected value of the objective based on average model's first-stage optimal decisions.  */
   public static final int    LS_DINFO_STOC_EVAVR               = 13289;
   /*! Number of arguments of a distribution sample.  */
   public static final int    LS_IINFO_DIST_NARG                = 13290;
   /*! Variance reduction/control method used in generating the sample.  */
   public static final int    LS_IINFO_SAMP_VARCONTROL_METHOD   = 13291;
   /*! Total number of nonlinear variables in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_VARS_DETEQE = 13292;
   /*! Total number of nonlinear constraints in the explicit deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_CONS_DETEQE = 13293;
   /*! Best lower bound on expected value of the objective function.  */
   public static final int    LS_DINFO_STOC_EVOBJ_LB            = 13294;
   /*! Best upper bound on expected value of the objective function.  */
   public static final int    LS_DINFO_STOC_EVOBJ_UB            = 13295;
   /*! Expected value of average model's objective.  */
   public static final int    LS_DINFO_STOC_AVROBJ              = 13296;
   /*! Sample median. */
   public static final int    LS_DINFO_SAMP_MEDIAN              = 13297;
   /*! Distribution (population) median. */
   public static final int    LS_DINFO_DIST_MEDIAN              = 13298;
   /*! Number of chance-constraints. */
   public static final int    LS_IINFO_STOC_NUM_CC              = 13299;
   /*! Number of rows in chance-constraints. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_CC         = 13300;
   /*! Internal. */
   public static final int    LS_IINFO_STOC_ISCBACK             = 13301;
   /*! Total number of LPs solved. */
   public static final int    LS_IINFO_STOC_LP_COUNT            = 13302;
   /*! Total number of NLPs solved. */
   public static final int    LS_IINFO_STOC_NLP_COUNT           = 13303;
   /*! Total number of MILPs solved. */
   public static final int    LS_IINFO_STOC_MIP_COUNT           = 13304;
   /*! Time elapsed in seconds in the optimizer (excluding setup)  */
   public static final int    LS_DINFO_STOC_OPT_TIME            = 13305;
   /*! Difference between underlying sample's correlation (S) and target correlation (T) loaded.  */
   public static final int    LS_DINFO_SAMP_CORRDIFF_ST         = 13306;
   /*! Difference between underlying sample's induced correlation (C) and target correlation (T) loaded.  */
   public static final int    LS_DINFO_SAMP_CORRDIFF_CT         = 13307;
   /*! Difference between underlying sample's correlation (S) and induced correlation (C).  */
   public static final int    LS_DINFO_SAMP_CORRDIFF_SC         = 13308;
   /*! Number of rows with equality type in chance-constraints. */
   public static final int    LS_IINFO_STOC_NUM_EQROWS_CC       = 13309;
   /*! Number of stochastic rows*/
   public static final int    LS_IINFO_STOC_NUM_ROWS            = 13310;
   /*! Number of chance sets violated over all scenarios */
   public static final int    LS_IINFO_STOC_NUM_CC_VIOLATED     = 13311;
   /*! Total number of columns in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_COLS_DETEQC     = 13312;
   /*! Total number of rows in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_ROWS_DETEQC     = 13313;
   /*! Total number of quadratic constraints in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_CONS_DETEQC = 13314;
   /*! Total number of continuous constraints in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_CONS_DETEQC = 13315;
   /*! Total number of constraints with general integer variables in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_CONS_DETEQC = 13316;
   /*! Total number of constraints with binary variables in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_CONS_DETEQC = 13317;
   /*! Total number of quadratic variables in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QCP_VARS_DETEQC = 13318;
   /*! Total number of nonzeros in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NONZ_DETEQC     = 13319;
   /*! Total number of binaries in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_BIN_DETEQC      = 13320;
   /*! Total number of general integer variables in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_INT_DETEQC      = 13321;
   /*! Total number of continuous variables in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_CONT_DETEQC     = 13322;
   /*! Total number of quadratic nonzeros in the chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_QC_NONZ_DETEQC  = 13323;
   /*! Total number of nonlinear nonzeros in the constraints of chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_NONZ_DETEQC = 13324;
   /*! Total number of nonlinear nonzeros in the objective function of chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLPOBJ_NONZ_DETEQC = 13325;
   /*! Total number of nonlinear constraints in the constraints of chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_CONS_DETEQC = 13326;
   /*! Total number of nonlinear variables in the constraints of chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NLP_VARS_DETEQC = 13327;
   /*! Total number of nonzeros in the objective of chance deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQC = 13328;
   /*! Total number of nonzeros in the objective of explict deterministic equivalent. */
   public static final int    LS_IINFO_STOC_NUM_NONZ_OBJ_DETEQE = 13329;
   /*! p-level for chance constraint */
   public static final int    LS_DINFO_STOC_CC_PLEVEL           = 13340;
   /*! Number of parallel threads used */
   public static final int    LS_IINFO_STOC_THREADS             = 13341;
   /*! Work imbalance across threads */
   public static final int    LS_DINFO_STOC_THRIMBL             = 13342;
   /*! Number of EQ type stochastic rows*/
   public static final int    LS_IINFO_STOC_NUM_EQROWS          = 13343;
   /*! Thread workloads */
   public static final int    LS_SINFO_STOC_THREAD_LOAD         = 13344;
   /*! Number of buckets */
   public static final int    LS_IINFO_STOC_NUM_BUCKETS         = 13345;

   /*BNP information*/
   public static final int    LS_IINFO_BNP_SIM_ITER             = 14000;
   public static final int    LS_IINFO_BNP_LPCOUNT              = 14001;
   public static final int    LS_IINFO_BNP_NUMCOL               = 14002;
   public static final int    LS_DINFO_BNP_BESTBOUND            = 14003;
   public static final int    LS_DINFO_BNP_BESTOBJ              = 14004;

   /*! @} */


/* Error codes (2001-2299) */
   public static final int    LSERR_NO_ERROR                    = 0000;
   public static final int    LSERR_OUT_OF_MEMORY               = 2001;
   public static final int    LSERR_CANNOT_OPEN_FILE            = 2002;
   public static final int    LSERR_BAD_MPS_FILE                = 2003;
   public static final int    LSERR_BAD_CONSTRAINT_TYPE         = 2004;
   public static final int    LSERR_BAD_MODEL                   = 2005;
   public static final int    LSERR_BAD_SOLVER_TYPE             = 2006;
   public static final int    LSERR_BAD_OBJECTIVE_SENSE         = 2007;
   public static final int    LSERR_BAD_MPI_FILE                = 2008;
   public static final int    LSERR_INFO_NOT_AVAILABLE          = 2009;
   public static final int    LSERR_ILLEGAL_NULL_POINTER        = 2010;
   public static final int    LSERR_UNABLE_TO_SET_PARAM         = 2011;
   public static final int    LSERR_INDEX_OUT_OF_RANGE          = 2012;
   public static final int    LSERR_ERRMSG_FILE_NOT_FOUND       = 2013;
   public static final int    LSERR_VARIABLE_NOT_FOUND          = 2014;
   public static final int    LSERR_INTERNAL_ERROR              = 2015;
   public static final int    LSERR_ITER_LIMIT                  = 2016;
   public static final int    LSERR_TIME_LIMIT                  = 2017;
   public static final int    LSERR_NOT_CONVEX                  = 2018;
   public static final int    LSERR_NUMERIC_INSTABILITY         = 2019;
   public static final int    LSERR_STEP_TOO_SMALL              = 2021;
   public static final int    LSERR_USER_INTERRUPT              = 2023;
   public static final int    LSERR_PARAMETER_OUT_OF_RANGE      = 2024;
   public static final int    LSERR_ERROR_IN_INPUT              = 2025;
   public static final int    LSERR_TOO_SMALL_LICENSE           = 2026;
   public static final int    LSERR_NO_VALID_LICENSE            = 2027;
   public static final int    LSERR_NO_METHOD_LICENSE           = 2028;
   public static final int    LSERR_NOT_SUPPORTED               = 2029;
   public static final int    LSERR_MODEL_ALREADY_LOADED        = 2030;
   public static final int    LSERR_MODEL_NOT_LOADED            = 2031;
   public static final int    LSERR_INDEX_DUPLICATE             = 2032;
   public static final int    LSERR_INSTRUCT_NOT_LOADED         = 2033;
   public static final int    LSERR_OLD_LICENSE                 = 2034;
   public static final int    LSERR_NO_LICENSE_FILE             = 2035;
   public static final int    LSERR_BAD_LICENSE_FILE            = 2036;
   public static final int    LSERR_MIP_BRANCH_LIMIT            = 2037;
   public static final int    LSERR_GOP_FUNC_NOT_SUPPORTED      = 2038;
   public static final int    LSERR_GOP_BRANCH_LIMIT            = 2039;
   public static final int    LSERR_BAD_DECOMPOSITION_TYPE      = 2040;
   public static final int    LSERR_BAD_VARIABLE_TYPE           = 2041;
   public static final int    LSERR_BASIS_BOUND_MISMATCH        = 2042;
   public static final int    LSERR_BASIS_COL_STATUS            = 2043;
   public static final int    LSERR_BASIS_INVALID               = 2044;
   public static final int    LSERR_BASIS_ROW_STATUS            = 2045;
   public static final int    LSERR_BLOCK_OF_BLOCK              = 2046;
   public static final int    LSERR_BOUND_OUT_OF_RANGE          = 2047;
   public static final int    LSERR_COL_BEGIN_INDEX             = 2048;
   public static final int    LSERR_COL_INDEX_OUT_OF_RANGE      = 2049;
   public static final int    LSERR_COL_NONZCOUNT               = 2050;
   public static final int    LSERR_INVALID_ERRORCODE           = 2051;
   public static final int    LSERR_ROW_INDEX_OUT_OF_RANGE      = 2052;
   public static final int    LSERR_TOTAL_NONZCOUNT             = 2053;
   public static final int    LSERR_MODEL_NOT_LINEAR            = 2054;
   public static final int    LSERR_CHECKSUM                    = 2055;
   public static final int    LSERR_USER_FUNCTION_NOT_FOUND     = 2056;
   public static final int    LSERR_TRUNCATED_NAME_DATA         = 2057;
   public static final int    LSERR_ILLEGAL_STRING_OPERATION    = 2058;
   public static final int    LSERR_STRING_ALREADY_LOADED       = 2059;
   public static final int    LSERR_STRING_NOT_LOADED           = 2060;
   public static final int    LSERR_STRING_LENGTH_LIMIT         = 2061;
   public static final int    LSERR_DATA_TERM_EXIST             = 2062;
   public static final int    LSERR_NOT_SORTED_ORDER            = 2063;
   public static final int    LSERR_INST_MISS_ELEMENTS          = 2064;
   public static final int    LSERR_INST_TOO_SHORT              = 2065;
   public static final int    LSERR_INST_INVALID_BOUND          = 2066;
   public static final int    LSERR_INST_SYNTAX_ERROR           = 2067;
   public static final int    LSERR_COL_TOKEN_NOT_FOUND         = 2068;
   public static final int    LSERR_ROW_TOKEN_NOT_FOUND         = 2069;
   public static final int    LSERR_NAME_TOKEN_NOT_FOUND        = 2070;
   public static final int    LSERR_NOT_LSQ_MODEL               = 2071;
   public static final int    LSERR_INCOMPATBLE_DECOMPOSITION   = 2072;
   public static final int    LSERR_NO_MULTITHREAD_SUPPORT      = 2073;
   public static final int    LSERR_INVALID_PARAMID             = 2074;
   public static final int    LSERR_INVALID_NTHREADS            = 2075;
   public static final int    LSERR_COL_LIMIT                   = 2076;
   public static final int    LSERR_QCDATA_NOT_LOADED           = 2077;
   public static final int    LSERR_NO_QCDATA_IN_ROW            = 2078;
   public static final int    LSERR_CLOCK_SETBACK               = 2079;
   public static final int    LSERR_XSOLVER_LOAD                = 2080;
   public static final int    LSERR_XSOLVER_NO_FILENAME         = 2081;
   public static final int    LSERR_XSOLVER_ALREADY_LOADED      = 2082;
   public static final int    LSERR_XSOLVER_FUNC_NOT_INSTALLED  = 2083;
   public static final int    LSERR_XSOLVER_LIB_NOT_INSTALLED   = 2084;
   public static final int    LSERR_ZLIB_LOAD                   = 2085;
   public static final int    LSERR_XSOLVER_ENV_NOT_CREATED     = 2086;
   public static final int    LSERR_SOLPOOL_EMPTY               = 2087;
   public static final int    LSERR_SOLPOOL_FULL                = 2088;

   public static final int    LSERR_SOL_LIMIT                   = 2089;
   public static final int    LSERR_TUNER_NOT_SETUP             = 2090;

   /*! @ingroup LSmatrixOps @{ */

   /* Error in LDLt factorization */
   public static final int    LSERR_LDL_FACTORIZATION           = 2201;
   /* Empty column detected in LDLt factorization */
   public static final int    LSERR_LDL_EMPTY_COL               = 2202;
   /* Matrix data is invalid or has bad input in LDLt factorization */
   public static final int    LSERR_LDL_BAD_MATRIX_DATA         = 2203;
   /* Invalid matrix or vector dimension */
   public static final int    LSERR_LDL_INVALID_DIM             = 2204;
   /* Matrix or vector is empty */
   public static final int    LSERR_LDL_EMPTY_MATRIX            = 2205;
   /* Matrix is not symmetric */
   public static final int    LSERR_LDL_MATRIX_NOTSYM           = 2206;
   /* Matrix has zero diagonal */
   public static final int    LSERR_LDL_ZERO_DIAG               = 2207;
   /* Invalid permutation */
   public static final int    LSERR_LDL_INVALID_PERM            = 2208;
   /* Duplicate elements detected in LDLt factorization */
   public static final int    LSERR_LDL_DUPELEM                 = 2209;
   /* Detected rank deficiency in LDLt factorization */
   public static final int    LSERR_LDL_RANK                    = 2210;
   /*! @} */

  /*! @ingroup LSstocError @{ */
  /*! Core MPS file/model has an error */
   public static final int    LSERR_BAD_SMPS_CORE_FILE          = 2301;
   /*! Time file/model has an error */
   public static final int    LSERR_BAD_SMPS_TIME_FILE          = 2302;
   /*! Stoc file/model has an error */
   public static final int    LSERR_BAD_SMPS_STOC_FILE          = 2303;
   /*! Core MPI file/model has an error */
   public static final int    LSERR_BAD_SMPI_CORE_FILE          = 2304;
   /*! Stoc file associated with Core MPI file has an error */
   public static final int    LSERR_BAD_SMPI_STOC_FILE          = 2305;
   /*! Unable to open Core file */
   public static final int    LSERR_CANNOT_OPEN_CORE_FILE       = 2306;
   /*! Unable to open Time file */
   public static final int    LSERR_CANNOT_OPEN_TIME_FILE       = 2307;
   /*! Unable to open Stoc file */
   public static final int    LSERR_CANNOT_OPEN_STOC_FILE       = 2308;
   /*! Stochastic model/data has not been loaded yet. */
   public static final int    LSERR_STOC_MODEL_NOT_LOADED       = 2309;
   /*! Stochastic parameter specified in Stoc file has not been found . */
   public static final int    LSERR_STOC_SPAR_NOT_FOUND         = 2310;
   /*! Stochastic parameter specified in Time file has not been found . */
   public static final int    LSERR_TIME_SPAR_NOT_FOUND         = 2311;
   /*! Specified scenario index is out of sequence */
   public static final int    LSERR_SCEN_INDEX_OUT_OF_SEQUENCE  = 2312;
   /*! Stochastic model/data has already been loaded. */
   public static final int    LSERR_STOC_MODEL_ALREADY_PARSED   = 2313;
   /*! Specified scenario CDF is invalid, e.g. scenario probabilities don't sum to 1.0*/
   public static final int    LSERR_STOC_INVALID_SCENARIO_CDF   = 2314;
   /*! No stochastic parameters was found in the Core file */
   public static final int    LSERR_CORE_SPAR_NOT_FOUND         = 2315;
   /*! Number of stochastic parameters found in Core file don't match to that of Time file */
   public static final int    LSERR_CORE_SPAR_COUNT_MISMATCH    = 2316;
   /*! Specified stochastic parameter index is invalid */
   public static final int    LSERR_CORE_INVALID_SPAR_INDEX     = 2317;
   /*! A stochastic parameter was not expected in Time file. */
   public static final int    LSERR_TIME_SPAR_NOT_EXPECTED      = 2318;
   /*! Number of stochastic parameters found in Time file don't match to that of Stoc file */
   public static final int    LSERR_TIME_SPAR_COUNT_MISMATCH    = 2319;
   /*! Specified stochastic parameter doesn't have a valid outcome value */
   public static final int    LSERR_CORE_SPAR_VALUE_NOT_FOUND   = 2320;
   /*! Requested information is unavailable */
   public static final int    LSERR_INFO_UNAVAILABLE            = 2321;
   /*! Core file doesn't have a valid bound name tag */
   public static final int    LSERR_STOC_MISSING_BNDNAME        = 2322;
   /*! Core file doesn't have a valid objective name tag */
   public static final int    LSERR_STOC_MISSING_OBJNAME        = 2323;
   /*! Core file doesn't have a valid right-hand-side name tag */
   public static final int    LSERR_STOC_MISSING_RHSNAME        = 2324;
   /*! Core file doesn't have a valid range name tag */
   public static final int    LSERR_STOC_MISSING_RNGNAME        = 2325;
   /*! Stoc file doesn't have an expected token name. */
   public static final int    LSERR_MISSING_TOKEN_NAME          = 2326;
   /*! Stoc file doesn't have a 'ROOT' token to specify a root scenario */
   public static final int    LSERR_MISSING_TOKEN_ROOT          = 2327;
   /*! Node model is unbounded */
   public static final int    LSERR_STOC_NODE_UNBOUNDED         = 2328;
   /*! Node model is infeasible */
   public static final int    LSERR_STOC_NODE_INFEASIBLE        = 2329;
   /*! Stochastic model has too many scenarios to solve with specified solver */
   public static final int    LSERR_STOC_TOO_MANY_SCENARIOS     = 2330;
   /*! One or more node-models have irrecoverable numerical problems */
   public static final int    LSERR_STOC_BAD_PRECISION          = 2331;
   /*! Specified aggregation structure is not compatible with model's stage structure */
   public static final int    LSERR_CORE_BAD_AGGREGATION        = 2332;
   /*! Event tree is either not initialized yet or was too big to create */
   public static final int    LSERR_STOC_NULL_EVENT_TREE        = 2333;
   /*! Specified stage index is invalid */
   public static final int    LSERR_CORE_BAD_STAGE_INDEX        = 2334;
   /*! Specified algorithm/method is invalid or not supported */
   public static final int    LSERR_STOC_BAD_ALGORITHM          = 2335;
   /*! Specified number of stages in Core model is invalid */
   public static final int    LSERR_CORE_BAD_NUMSTAGES          = 2336;
   /*! Underlying model has an invalid temporal order */
   public static final int    LSERR_TIME_BAD_TEMPORAL_ORDER     = 2337;
   /*! Number of stages specified in Time structure is invalid */
   public static final int    LSERR_TIME_BAD_NUMSTAGES          = 2338;
   /*! Core and Time data are inconsistent */
   public static final int    LSERR_CORE_TIME_MISMATCH          = 2339;
   /*! Specified stochastic structure has an invalid CDF */
   public static final int    LSERR_STOC_INVALID_CDF            = 2340;
   /*! Specified distribution type is invalid or not supported. */
   public static final int    LSERR_BAD_DISTRIBUTION_TYPE       = 2341;
   /*! Scale parameter for specified distribution is out of range. */
   public static final int    LSERR_DIST_SCALE_OUT_OF_RANGE     = 2342;
   /*! Shape parameter for specified distribution is out of range. */
   public static final int    LSERR_DIST_SHAPE_OUT_OF_RANGE     = 2343;
   /*! Specified probabability value is invalid */
   public static final int    LSERR_DIST_INVALID_PROBABILITY    = 2344;
   /*! Derivative information is unavailable */
   public static final int    LSERR_DIST_NO_DERIVATIVE          = 2345;
   /*! Specified standard deviation is invalid */
   public static final int    LSERR_DIST_INVALID_SD             = 2346;
   /*! Specified value is invalid */
   public static final int    LSERR_DIST_INVALID_X              = 2347;
   /*! Specified parameters are invalid for the given distribution. */
   public static final int    LSERR_DIST_INVALID_PARAMS         = 2348;
   /*! Iteration limit has been reached during a root finding operation */
   public static final int    LSERR_DIST_ROOTER_ITERLIM         = 2349;
   /*! Given array is out of bounds */
   public static final int    LSERR_ARRAY_OUT_OF_BOUNDS         = 2350;
   /*! Limiting PDF does not exist */
   public static final int    LSERR_DIST_NO_PDF_LIMIT           = 2351;

   /*! A random number generator is not set. */
   public static final int    LSERR_RG_NOT_SET                  = 2352;
   /*! Distribution function value was truncated during calculations */
   public static final int    LSERR_DIST_TRUNCATED              = 2353;
   /*! Stoc file has a parameter value missing */
   public static final int    LSERR_STOC_MISSING_PARAM_TOKEN    = 2354;
   /*! Distribution has invalid number of parameters */
   public static final int    LSERR_DIST_INVALID_NUMPARAM       = 2355;
   /*! Core file/model is not in temporal order */
   public static final int    LSERR_CORE_NOT_IN_TEMPORAL_ORDER  = 2357;
   /*! Specified sample size is invalid */
   public static final int    LSERR_STOC_INVALID_SAMPLE_SIZE    = 2358;
   /*! Node probability cannot be computed due to presence of continuous stochastic parameters */
   public static final int    LSERR_STOC_NOT_DISCRETE           = 2359;
   /*! Event tree exceeds the maximum number of scenarios allowed to attempt an exact solution.*/
   public static final int    LSERR_STOC_SCENARIO_LIMIT         = 2360;
   /*! Specified correlation type is invalid */
   public static final int    LSERR_DIST_BAD_CORRELATION_TYPE   = 2361;
   /*! Number of stages in the model is not set yet. */
   public static final int    LSERR_TIME_NUMSTAGES_NOT_SET      = 2362;
   /*! Model already contains a sampled tree */
   public static final int    LSERR_STOC_SAMPLE_ALREADY_LOADED  = 2363;
   /*! Stochastic events are not loaded yet */
   public static final int    LSERR_STOC_EVENTS_NOT_LOADED      = 2364;
   /*! Stochastic tree already initialized */
   public static final int    LSERR_STOC_TREE_ALREADY_INIT      = 2365;
   /*! Random number generator seed not initialized */
   public static final int    LSERR_RG_SEED_NOT_SET             = 2366;
   /*! All sample points in the sample has been used. Resampling may be required. */
   public static final int    LSERR_STOC_OUT_OF_SAMPLE_POINTS   = 2367;
   /*! Sampling is not supported for models with explicit scenarios. */
   public static final int    LSERR_STOC_SCENARIO_SAMPLING_NOT_SUPPORTED = 2368;
   /*! Sample points are not yet generated for a stochastic parameter. */
   public static final int    LSERR_STOC_SAMPLE_NOT_GENERATED   = 2369;
   /*! Sample points are already generated for a stochastic parameter. */
   public static final int    LSERR_STOC_SAMPLE_ALREADY_GENERATED = 2370;
   /*! Sample sizes selected are too small. */
   public static final int    LSERR_STOC_SAMPLE_SIZE_TOO_SMALL  = 2371;
   /*! A random number generator is already set. */
   public static final int    LSERR_RG_ALREADY_SET              = 2372;
   /*! Sampling is not allowed for block/joint distributions. */
   public static final int    LSERR_STOC_BLOCK_SAMPLING_NOT_SUPPORTED = 2373;
   /*! No stochastic parameters were assigned to one of the stages. */
   public static final int    LSERR_EMPTY_SPAR_STAGE            = 2374;
   /*! No rows were assigned to one of the stages. */
   public static final int    LSERR_EMPTY_ROW_STAGE             = 2375;
   /*! No columns were assigned to one of the stages. */
   public static final int    LSERR_EMPTY_COL_STAGE             = 2376;
   /*! Default sample sizes per stoc.pars and stage are in conflict. */
   public static final int    LSERR_STOC_CONFLICTING_SAMP_SIZES = 2377;
   /*! Empty scenario data */
   public static final int    LSERR_STOC_EMPTY_SCENARIO_DATA    = 2378;
   /*! A correlation structure has not been induced yet */
   public static final int    LSERR_STOC_CORRELATION_NOT_INDUCED = 2379;
   /*! A discrete PDF table has not been loaded */
   public static final int    LSERR_STOC_PDF_TABLE_NOT_LOADED   = 2380;
   /*! No continously distributed random parameters are found */
   public static final int    LSERR_STOC_NO_CONTINUOUS_SPAR_FOUND = 2381;
   /*! One or more rows already belong to another chance constraint */
   public static final int    LSERR_STOC_ROW_ALREADY_IN_CC      = 2382;
   /*! No chance-constraints were loaded */
   public static final int    LSERR_STOC_CC_NOT_LOADED          = 2383;
   /*! Cut limit has been reached */
   public static final int    LSERR_STOC_CUT_LIMIT              = 2384;
   /*! GA object has not been initialized yet */
   public static final int    LSERR_STOC_GA_NOT_INIT            = 2385;
   /*! There exists stochastic rows not loaded to any chance constraints yet.*/
   public static final int    LSERR_STOC_ROWS_NOT_LOADED_IN_CC  = 2386;
   /*! Specified sample is already assigned as the source for the target sample. */
   public static final int    LSERR_SAMP_ALREADY_SOURCE         = 2387;
   /*! No user-defined distribution function has been set for the specified sample. */
   public static final int    LSERR_SAMP_USERFUNC_NOT_SET       = 2388;
   /*! Specified sample does not support the function call or it is incompatible with the argument list. */
   public static final int    LSERR_SAMP_INVALID_CALL           = 2389;
   /*! Mapping stochastic instructions leads to multiple occurrences in matrix model. */
   public static final int    LSERR_STOC_MAP_MULTI_SPAR         = 2390;
   /*! Two or more stochastic instructions maps to the same position in matrix model. */
   public static final int    LSERR_STOC_MAP_SAME_SPAR          = 2391;
   /*! A stochastic parameter was not expected in the objective function. */
   public static final int    LSERR_STOC_SPAR_NOT_EXPECTED_OBJ  = 2392;
   /*! One of the distribution parameters of the specified sample was not set. */
   public static final int    LSERR_DIST_PARAM_NOT_SET          = 2393;
   /*! Specified stochastic input is invalid. */
   public static final int    LSERR_STOC_INVALID_INPUT          = 2394;

   /* Error codes for the sprint method. */

   public static final int    LSERR_SPRINT_MISSING_TAG_ROWS     = 2577;
   public static final int    LSERR_SPRINT_MISSING_TAG_COLS     = 2578;
   public static final int    LSERR_SPRINT_MISSING_TAG_RHS      = 2579;
   public static final int    LSERR_SPRINT_MISSING_TAG_ENDATA   = 2580;
   public static final int    LSERR_SPRINT_MISSING_VALUE_ROW    = 2581;
   public static final int    LSERR_SPRINT_EXTRA_VALUE_ROW      = 2582;
   public static final int    LSERR_SPRINT_MISSING_VALUE_COL    = 2583;
   public static final int    LSERR_SPRINT_EXTRA_VALUE_COL      = 2584;
   public static final int    LSERR_SPRINT_MISSING_VALUE_RHS    = 2585;
   public static final int    LSERR_SPRINT_EXTRA_VALUE_RHS      = 2586;
   public static final int    LSERR_SPRINT_MISSING_VALUE_BOUND  = 2587;
   public static final int    LSERR_SPRINT_EXTRA_VALUE_BOUND    = 2588;

   public static final int    LSERR_SPRINT_INTEGER_VARS_IN_MPS  = 2589;
   public static final int    LSERR_SPRINT_BINARY_VARS_IN_MPS   = 2590;
   public static final int    LSERR_SPRINT_SEMI_CONT_VARS_IN_MPS = 2591;
   public static final int    LSERR_SPRINT_UNKNOWN_TAG_BOUNDS   = 2592;
   public static final int    LSERR_SPRINT_MULTIPLE_OBJ_ROWS    = 2593;

   public static final int    LSERR_SPRINT_COULD_NOT_SOLVE_SUBPROBLEM = 2594;

   public static final int    LSERR_COULD_NOT_WRITE_TO_FILE     = 2595;
   public static final int    LSERR_COULD_NOT_READ_FROM_FILE    = 2596;
   public static final int    LSERR_READING_PAST_EOF            = 2597;

   /* Error codes associated with scripting 2700-2750 */
   public static final int    LSERR_SCRIPT                      = 2700;

   /*! @} */

   public static final int    LSERR_LAST_ERROR                  = 2598;


/* Callback locations */
   public static final int    LSLOC_PRIMAL                      = 0;
   public static final int    LSLOC_DUAL                        = 1;
   public static final int    LSLOC_BARRIER                     = 2;
   public static final int    LSLOC_CROSSOVER                   = 3;
   public static final int    LSLOC_CONOPT                      = 4;
   public static final int    LSLOC_MIP                         = 5;
   public static final int    LSLOC_LOCAL_OPT                   = 6;
   public static final int    LSLOC_GEN_START                   = 7;
   public static final int    LSLOC_GEN_PROCESSING              = 8;
   public static final int    LSLOC_GEN_END                     = 9;
   public static final int    LSLOC_GOP                         = 10;
   public static final int    LSLOC_EXIT_SOLVER                 = 11;
   public static final int    LSLOC_PRESOLVE                    = 12;
   public static final int    LSLOC_MSW                         = 13;
   public static final int    LSLOC_FUNC_CALC                   = 14;
   public static final int    LSLOC_IISIUS                      = 15;
   public static final int    LSLOC_SP                          = 16;
   public static final int    LSLOC_GEN_SP_START                = 17;
   public static final int    LSLOC_GEN_SP                      = 18;
   public static final int    LSLOC_GEN_SP_END                  = 19;
   public static final int    LSLOC_SP_WS                       = 20;
   public static final int    LSLOC_LSQ                         = 21;
   public static final int    LSLOC_SP_WS_START                 = 22;
   public static final int    LSLOC_SP_WS_END                   = 23;
   public static final int    LSLOC_SP_BENCH_START              = 24;
   public static final int    LSLOC_SP_BENCH_END                = 25;
   public static final int    LSLOC_BNP                         = 26;


   public static final int    LS_METHOD_FREE                    = 0;
   public static final int    LS_METHOD_PSIMPLEX                = 1;
   public static final int    LS_METHOD_DSIMPLEX                = 2;
   public static final int    LS_METHOD_BARRIER                 = 3;
   public static final int    LS_METHOD_NLP                     = 4;
   public static final int    LS_METHOD_MIP                     = 5;
   public static final int    LS_METHOD_MULTIS                  = 6;
   public static final int    LS_METHOD_GOP                     = 7;
   public static final int    LS_METHOD_IIS                     = 8;
   public static final int    LS_METHOD_IUS                     = 9;
   public static final int    LS_METHOD_SBD                     = 10;
   public static final int    LS_METHOD_SPRINT                  = 11;
   public static final int    LS_METHOD_GA                      = 12;


   public static final int    LS_STRATEGY_USER                  = 0;
   public static final int    LS_STRATEGY_PRIMIP                = 1;
   public static final int    LS_STRATEGY_NODEMIP               = 2;
   public static final int    LS_STRATEGY_HEUMIP                = 3;


   public static final int    LS_NMETHOD_FREE                   = 4;
   public static final int    LS_NMETHOD_LSQ                    = 5;
   public static final int    LS_NMETHOD_QP                     = 6;
   public static final int    LS_NMETHOD_CONOPT                 = 7;
   public static final int    LS_NMETHOD_SLP                    = 8;
   public static final int    LS_NMETHOD_MSW_GRG                = 9;
   public static final int    LS_NMETHOD_IPOPT                  = 10;


   public static final int    LS_PROB_SOLVE_FREE                = 0;
   public static final int    LS_PROB_SOLVE_PRIMAL              = 1;
   public static final int    LS_PROB_SOLVE_DUAL                = 2;
   public static final int    LS_BAR_METHOD_FREE                = 4;
   public static final int    LS_BAR_METHOD_INTPNT              = 5;
   public static final int    LS_BAR_METHOD_CONIC               = 6;
   public static final int    LS_BAR_METHOD_QCONE               = 7;

   public static final int    LSSOL_BASIC_PRIMAL                = 11;
   public static final int    LSSOL_BASIC_DUAL                  = 12;
   public static final int    LSSOL_BASIC_SLACK                 = 13;
   public static final int    LSSOL_BASIC_REDCOST               = 14;
   public static final int    LSSOL_INTERIOR_PRIMAL             = 15;
   public static final int    LSSOL_INTERIOR_DUAL               = 16;
   public static final int    LSSOL_INTERIOR_SLACK              = 17;
   public static final int    LSSOL_INTERIOR_REDCOST            = 18;


/* Model types */
   /* linear programs                          */
   public static final int    LS_LP                             = 10;

   /* quadratic programs                       */
   public static final int    LS_QP                             = 11;

   /* conic programs                           */
   public static final int    LS_SOCP                           = 12;

   /* semidefinite programs                    */
   public static final int    LS_SDP                            = 13;

   /* nonlinear programs                       */
   public static final int    LS_NLP                            = 14;

   /* mixed-integer linear programs            */
   public static final int    LS_MILP                           = 15;

   /* mixed-integer quadratic programs         */
   public static final int    LS_MIQP                           = 16;

   /* mixed-integer conic programs             */
   public static final int    LS_MISOCP                         = 17;

   /* mixed-integer semidefinite programs      */
   public static final int    LS_MISDP                          = 18;

   /* mixed-integer nonlinear programs         */
   public static final int    LS_MINLP                          = 19;

   /* convex QP */
   public static final int    LS_CONVEX_QP                      = 20;

   /*convex NLP */
   public static final int    LS_CONVEX_NLP                     = 21;

   /*convex MIQP */
   public static final int    LS_CONVEX_MIQP                    = 22;

   /*convex MINLP */
   public static final int    LS_CONVEX_MINLP                   = 23;

   /* undetermined   */
   public static final int    LS_UNDETERMINED                   = -1;


   public static final int    LS_LINK_BLOCKS_FREE               = 0;
   public static final int    LS_LINK_BLOCKS_SELF               = 1;
   public static final int    LS_LINK_BLOCKS_NONE               = 2;
   public static final int    LS_LINK_BLOCKS_COLS               = 3;
   public static final int    LS_LINK_BLOCKS_ROWS               = 4;
   public static final int    LS_LINK_BLOCKS_BOTH               = 5;
   public static final int    LS_LINK_BLOCKS_MATRIX             = 6;


/* Controls the way objective function and
 * objective sense are printed when writing
 * LS_MAX type problems in MPS format.
 */
   public static final int    LS_MPS_USE_MAX_NOTE               = 0;
   public static final int    LS_MPS_USE_MAX_CARD               = 1;
   public static final int    LS_MPS_USE_MAX_FLIP               = 2;


/* Finite differences methods */
   public static final int    LS_DERIV_FREE                     = 0;
   public static final int    LS_DERIV_FORWARD_DIFFERENCE       = 1;
   public static final int    LS_DERIV_BACKWARD_DIFFERENCE      = 2;
   public static final int    LS_DERIV_CENTER_DIFFERENCE        = 3;


/* MIP Sets
 *  SOS1: S={x_1,...,x_p}  only one x_j can be different from zero
 *  SOS2: S={x_1,...,x_p}  at most two x_j can be different from zero
 *                         and  when they are they have to be adjacent
 *  SOS3: S={x_1,...,x_p}  @sum(j: x_j      )  = 1;  x_j >=0,
 *  CARD: S={x_1,...,x_p}  @sum(j: sign(x_j)) <= k;  x_j >=0
 */
   public static final int    LS_MIP_SET_CARD                   = 4;
   public static final int    LS_MIP_SET_SOS1                   = 1;
   public static final int    LS_MIP_SET_SOS2                   = 2;
   public static final int    LS_MIP_SET_SOS3                   = 3;


   public static final int    LS_QTERM_NONE                     = 0;
   public static final int    LS_QTERM_INDEF                    = 1;
   public static final int    LS_QTERM_POSDEF                   = 2;
   public static final int    LS_QTERM_NEGDEF                   = 3;
   public static final int    LS_QTERM_POS_SEMIDEF              = 4;
   public static final int    LS_QTERM_NEG_SEMIDEF              = 5;


/* Bit masks for general MIP mode. Use sums
 * to enable a collection of available levels.
 */
   public static final int    LS_MIP_MODE_NO_TIME_EVENTS        = 2;
   public static final int    LS_MIP_MODE_FAST_FEASIBILITY      = 4;
   public static final int    LS_MIP_MODE_FAST_OPTIMALITY       = 8;
   public static final int    LS_MIP_MODE_NO_BRANCH_CUTS        = 16;
   public static final int    LS_MIP_MODE_NO_LP_BARRIER         = 32;


/* Bit mask for cut generation levels. Use sums to
 * enable a collection of available cuts.
 */
   public static final int    LS_MIP_GUB_COVER_CUTS             = 2;
   public static final int    LS_MIP_FLOW_COVER_CUTS            = 4;
   public static final int    LS_MIP_LIFT_CUTS                  = 8;
   public static final int    LS_MIP_PLAN_LOC_CUTS              = 16;
   public static final int    LS_MIP_DISAGG_CUTS                = 32;
   public static final int    LS_MIP_KNAPSUR_COVER_CUTS         = 64;
   public static final int    LS_MIP_LATTICE_CUTS               = 128;
   public static final int    LS_MIP_GOMORY_CUTS                = 256;
   public static final int    LS_MIP_COEF_REDC_CUTS             = 512;
   public static final int    LS_MIP_GCD_CUTS                   = 1024;
   public static final int    LS_MIP_OBJ_CUT                    = 2048;
   public static final int    LS_MIP_BASIS_CUTS                 = 4096;
   public static final int    LS_MIP_CARDGUB_CUTS               = 8192;
   public static final int    LS_MIP_DISJUN_CUTS                = 16384;


/* Bit masks for MIP preprocessing levels. Use sums
 * to enable a collection of available levels.
 */
   public static final int    LS_MIP_PREP_SPRE                  = 2;
   public static final int    LS_MIP_PREP_PROB                  = 4;
   public static final int    LS_MIP_PREP_COEF                  = 8;
   public static final int    LS_MIP_PREP_ELIM                  = 16;
   public static final int    LS_MIP_PREP_DUAL                  = 32;
   public static final int    LS_MIP_PREP_DBACK                 = 64;
   public static final int    LS_MIP_PREP_BINROWS               = 128;
   public static final int    LS_MIP_PREP_AGGROWS               = 256;
   public static final int    LS_MIP_PREP_COEF_LIFTING          = 512;
   public static final int    LS_MIP_PREP_MAXPASS               = 1024;
   public static final int    LS_MIP_PREP_SIMROW                = 2048;


/* Bit masks for solver preprocessing levels. Use sums
 * to enable a collection of available levels.
 */
   public static final int    LS_SOLVER_PREP_SPRE               = 2;
   public static final int    LS_SOLVER_PREP_PFOR               = 4;
   public static final int    LS_SOLVER_PREP_DFOR               = 8;
   public static final int    LS_SOLVER_PREP_ELIM               = 16;
   public static final int    LS_SOLVER_PREP_DCOL               = 32;
   public static final int    LS_SOLVER_PREP_DROW               = 64;
   public static final int    LS_SOLVER_PREP_CONE               = 128;
   public static final int    LS_SOLVER_PREP_MAXPASS            = 1024;
   public static final int    LS_SOLVER_PREP_DECOMP             = 4096;
   public static final int    LS_SOLVER_PREP_LOWMEM             = 8192;
   public static final int    LS_SOLVER_PREP_EXTERNAL           = 16384;


/* Bit masks for IIS & IUS analysis levels. Use sums to
 * enable a collection of available levels.
 */
   public static final int    LS_NECESSARY_ROWS                 = 1;
   public static final int    LS_NECESSARY_COLS                 = 2;
   public static final int    LS_SUFFICIENT_ROWS                = 4;
   public static final int    LS_SUFFICIENT_COLS                = 8;
   public static final int    LS_IIS_INTS                       = 16;
   public static final int    LS_IISRANK_LTF                    = 32;
   public static final int    LS_IISRANK_DECOMP                 = 64;
   public static final int    LS_IISRANK_NNZ                    = 128;
   public static final int    LS_IISLIMIT_MIS                   = 256;
   public static final int    LS_IIS_MASK_IISCOLS               = 512;
   public static final int    LS_IIS_SETS                       = 1024;


/* Infeasibility norms for IIS finder */
   public static final int    LS_IIS_NORM_FREE                  = 0;
   public static final int    LS_IIS_NORM_ONE                   = 1;
   public static final int    LS_IIS_NORM_INFINITY              = 2;


/* IIS methods */
   public static final int    LS_IIS_DEFAULT                    = 0;
   public static final int    LS_IIS_DEL_FILTER                 = 1;
   public static final int    LS_IIS_ADD_FILTER                 = 2;
   public static final int    LS_IIS_GBS_FILTER                 = 3;
   public static final int    LS_IIS_DFBS_FILTER                = 4;
   public static final int    LS_IIS_FSC_FILTER                 = 5;
   public static final int    LS_IIS_ELS_FILTER                 = 6;


/*codes for IINFO_MIP_WHERE_IN_CODE*/
   public static final int    LS_MIP_IN_PRESOLVE                = 0;
   public static final int    LS_MIP_IN_FP_MODE                 = 1;
   public static final int    LS_MIP_IN_HEU_MODE                = 2;
   public static final int    LS_MIP_IN_ENUM                    = 3;
   public static final int    LS_MIP_IN_CUT_ADD_TOP             = 4;
   public static final int    LS_MIP_IN_CUT_ADD_TREE            = 5;
   public static final int    LS_MIP_IN_BANDB                   = 6;


/**
 * @ingroup LSstocOptDataTypes
 */
   /*! Stochastic parameter is an instruction code  */
   public static final int    LS_JCOL_INST                      = -8;
   /*! Stochastic parameter is a RHS upper bound (reserved for future use)*/
   public static final int    LS_JCOL_RUB                       = -7;
   /*! Stochastic parameter is a RHS lower bound (reserved for future use)*/
   public static final int    LS_JCOL_RLB                       = -6;
   /*! Stochastic parameter is a RHS value (belongs to RHS column)      */
   public static final int    LS_JCOL_RHS                       = -5;
   /*! Stochastic parameter is an objective coefficient (belongs to OBJ row)   */
   public static final int    LS_IROW_OBJ                       = -1;
   /*! Stochastic parameter is a variable lower bound (belongs to LO row) */
   public static final int    LS_IROW_VUB                       = -3;
   /*! Stochastic parameter is a variable upper bound (belongs to UP row) */
   public static final int    LS_IROW_VLB                       = -2;
   /*! Stochastic parameter is a variable fixed bound (belongs to FX row) */
   public static final int    LS_IROW_VFX                       = -4;
   /*! Stochastic parameter is an LP matrix entry. */
   public static final int    LS_IMAT_AIJ                       = 0;


/**
 * @ingroup LSstocOptDistribFun
 */
  /* discrete distributions */
   public static final int    LSDIST_TYPE_BINOMIAL              = 701;
   public static final int    LSDIST_TYPE_DISCRETE              = 702;
   public static final int    LSDIST_TYPE_DISCRETE_BLOCK        = 703;
   public static final int    LSDIST_TYPE_NEGATIVE_BINOMIAL     = 704;
   public static final int    LSDIST_TYPE_GEOMETRIC             = 705;
   public static final int    LSDIST_TYPE_POISSON               = 706;
   public static final int    LSDIST_TYPE_LOGARITHMIC           = 707;
   public static final int    LSDIST_TYPE_HYPER_GEOMETRIC       = 708;
   public static final int    LSDIST_TYPE_LINTRAN_BLOCK         = 709;
   public static final int    LSDIST_TYPE_SUB_BLOCK             = 710;
   public static final int    LSDIST_TYPE_SUB                   = 711;
   public static final int    LSDIST_TYPE_USER                  = 712;

   /* continuous distributions */
   public static final int    LSDIST_TYPE_BETA                  = 801;
   public static final int    LSDIST_TYPE_CAUCHY                = 802;
   public static final int    LSDIST_TYPE_CHI_SQUARE            = 803;
   public static final int    LSDIST_TYPE_EXPONENTIAL           = 804;
   public static final int    LSDIST_TYPE_F_DISTRIBUTION        = 805;
   public static final int    LSDIST_TYPE_GAMMA                 = 806;
   public static final int    LSDIST_TYPE_GUMBEL                = 807;
   public static final int    LSDIST_TYPE_LAPLACE               = 808;
   public static final int    LSDIST_TYPE_LOGNORMAL             = 809;
   public static final int    LSDIST_TYPE_LOGISTIC              = 810;
   public static final int    LSDIST_TYPE_NORMAL                = 811;
   public static final int    LSDIST_TYPE_PARETO                = 812;
   public static final int    LSDIST_TYPE_STABLE_PARETIAN       = 813;
   public static final int    LSDIST_TYPE_STUDENTS_T            = 814;
   public static final int    LSDIST_TYPE_TRIANGULAR            = 815;
   public static final int    LSDIST_TYPE_UNIFORM               = 816;
   public static final int    LSDIST_TYPE_WEIBULL               = 817;
   public static final int    LSDIST_TYPE_WILCOXON              = 818;
   public static final int    LSDIST_TYPE_BETABINOMIAL          = 819;
   public static final int    LSDIST_TYPE_SYMMETRICSTABLE       = 820;


/* supported operations modifying the core. */
   public static final int    LS_REPLACE                        = 0;
   public static final int    LS_ADD                            = 1;
   public static final int    LS_SUB                            = 2;
   public static final int    LS_MULTIPLY                       = 3;
   public static final int    LS_DIVIDE                         = 4;


/* scenario indices for special cases */
   public static final int    LS_SCEN_ROOT                      = -1;
   public static final int    LS_SCEN_AVRG                      = -2;
   public static final int    LS_SCEN_MEDIAN                    = -3;
   public static final int    LS_SCEN_USER                      = -4;
   public static final int    LS_SCEN_NONE                      = -5;


/* warmstart rule in optimizing wait-see model */
   public static final int    LS_WSBAS_FREE                     = -1;
   public static final int    LS_WSBAS_NONE                     = 0;
   public static final int    LS_WSBAS_AVRG                     = 1;
   public static final int    LS_WSBAS_LAST                     = 2;


/**
 * @ingroup LSstocOptSolver
 */
  /*! Solve with the method chosen by the solver. */
   public static final int    LS_METHOD_STOC_FREE               = -1;

   /*! Solve the deterministic equivalent (DETEQ).  */
   public static final int    LS_METHOD_STOC_DETEQ              = 0;

   /*! Solve with the Nested Benders Decomposition (NBD) method. */
   public static final int    LS_METHOD_STOC_NBD                = 1;

   /*! Solve with the Augmented Lagrangian Decomposition (ALD) method. */
   public static final int    LS_METHOD_STOC_ALD                = 2;

   /*! Solve with the Heuristic-Search (HS) method. */
   public static final int    LS_METHOD_STOC_HS                 = 4;


/*
 * @ingroup LSstocOptDeteqType
 */
   public static final int    LS_DETEQ_FREE                     = -1;
   public static final int    LS_DETEQ_IMPLICIT                 = 0;
   public static final int    LS_DETEQ_EXPLICIT                 = 1;
   public static final int    LS_DETEQ_CHANCE                   = 2;


/* Distribution functions */
   public static final int    LS_USER                           = 0;
   public static final int    LS_PDF                            = 1;
   public static final int    LS_CDF                            = 2;
   public static final int    LS_CDFINV                         = 3;
   public static final int    LS_PDFDIFF                        = 4;


/* Correlation types */
   public static final int    LS_CORR_TARGET                    = -1;
   public static final int    LS_CORR_LINEAR                    = 0;
   public static final int    LS_CORR_PEARSON                   = 0;
   public static final int    LS_CORR_KENDALL                   = 1;
   public static final int    LS_CORR_SPEARMAN                  = 2;


/* Sampling types */
   public static final int    LS_MONTECARLO                     = 0;
   public static final int    LS_LATINSQUARE                    = 1;
   public static final int    LS_ANTITHETIC                     = 2;


/* Random number generator algorithms */
   public static final int    LS_RANDGEN_FREE                   = -1;
   public static final int    LS_RANDGEN_SYSTEM                 = 0;
   public static final int    LS_RANDGEN_LINDO1                 = 1;
   public static final int    LS_RANDGEN_LINDO2                 = 2;
   public static final int    LS_RANDGEN_LIN1                   = 3;
   public static final int    LS_RANDGEN_MULT1                  = 4;
   public static final int    LS_RANDGEN_MULT2                  = 5;
   public static final int    LS_RANDGEN_MERSENNE               = 6;


/* NCM methods */
   public static final int    LS_NCM_STD                        =   1;
   public static final int    LS_NCM_GA                         = 2;
   public static final int    LS_NCM_ALTP                       = 4;
   public static final int    LS_NCM_L2NORM_CONE                = 8;
   public static final int    LS_NCM_L2NORM_NLP                 = 16;


/* pointer types used */
   public static final int    LS_PTR_ENV                        = 0;
   public static final int    LS_PTR_MODEL                      = 1;
   public static final int    LS_PTR_SAMPLE                     = 2;
   public static final int    LS_PTR_RG                         = 3;


/* multithreading mode */
   public static final int    LS_MTMODE_FREE                    = -1;
   public static final int    LS_MTMODE_EXPLCT                  = 0;
   public static final int    LS_MTMODE_PPCC                    = 1;
   public static final int    LS_MTMODE_PP                      = 2;
   public static final int    LS_MTMODE_CCPP                    = 3;
   public static final int    LS_MTMODE_CC                      = 4;


/* Output file types created by the Sprint code*/
   public static final int    LS_SPRINT_OUTPUT_FILE_FREE        = 0;
   public static final int    LS_SPRINT_OUTPUT_FILE_BIN         = 1;
   public static final int    LS_SPRINT_OUTPUT_FILE_TXT         = 2;


   public static final int    LS_MSW_MODE_TRUNCATE_FREE         = 1;
   public static final int    LS_MSW_MODE_SCALE_REFSET          = 2;
   public static final int    LS_MSW_MODE_EXPAND_RADIUS         = 4;
   public static final int    LS_MSW_MODE_SKEWED_SAMPLE         = 8;
   public static final int    LS_MSW_MODE_BEST_LOCAL_BND        = 16;
   public static final int    LS_MSW_MODE_BEST_GLOBAL_BND       = 32;
   public static final int    LS_MSW_MODE_SAMPLE_FREEVARS       = 64;
   public static final int    LS_MSW_MODE_PRECOLLECT            = 128;
   public static final int    LS_MSW_MODE_POWER_SOLVE           = 256;


   public static final int    LS_GA_CROSS_SBX                   = 101;
   public static final int    LS_GA_CROSS_BLXA                  = 102;
   public static final int    LS_GA_CROSS_BLXAB                 = 103;
   public static final int    LS_GA_CROSS_HEU                   = 104;
   public static final int    LS_GA_CROSS_ONEPOINT              = 201;
   public static final int    LS_GA_CROSS_TWOPOINT              = 202;


  /*! scan for basic solutions for pool */
   public static final int    LS_SOLVER_MODE_POOLBAS            = 1;
  /*! scan for edge solutions for pool */
   public static final int    LS_SOLVER_MODE_POOLEDGE           = 2;
  /*! scan for integer basic solutions */
   public static final int    LS_SOLVER_MODE_INTBAS             = 4;


   public static final int    LS_PARCLASS_BITMASK               = 1;
   public static final int    LS_PARCLASS_MULSTEP               = 2;
   public static final int    LS_PARCLASS_TUNPAR                = 4;
   public static final int    LS_PARCLASS_LP                    = 32;
   public static final int    LS_PARCLASS_QUAD                  = 64;
   public static final int    LS_PARCLASS_NLP                   = 128;
   public static final int    LS_PARCLASS_GOP                   = 256;
   public static final int    LS_PARCLASS_MIP                   = 512;
   public static final int    LS_PARCLASS_ENV                   = 1024;
   public static final int    LS_PARCLASS_MOD                   = 2048;
   public static final int    LS_PARCLASS_HIT                   = 4096;
   public static final int    LS_PARCLASS_ZSTATIC               = 8192;


/* Equivalences */
   public static final int    LS_IINFO_OBJSENSE                 = LS_IPARAM_OBJSENSE;
   public static final int    LS_IINFO_VER_MAJOR                = LS_IPARAM_VER_MAJOR;
   public static final int    LS_IINFO_VER_MINOR                = LS_IPARAM_VER_MINOR;
   public static final int    LS_IINFO_VER_BUILD                = LS_IPARAM_VER_BUILD;
   public static final int    LS_IINFO_VER_REVISION             = LS_IPARAM_VER_REVISION;
/* Conic vs Second-Order-Cone equivalence*/
   public static final int    LS_CONIC                          = LS_SOCP;
   public static final int    LS_MICONIC                        = LS_MISOCP;

/*********************************************************************
 *                   Conversion to version 1.x                       *
 *********************************************************************/
/* old parameter names, changed in 12.x */
   public static final int    LS_INT_PARAMETER_TYPE             = LS_INT_TYPE;
   public static final int    LS_DOUBLE_PARAMETER_TYPE          = LS_DOUBLE_TYPE;

/* old parameter names, changed in 8.x */
   public static final int    LS_IPARAM_NLP_MSW_MAXREF          = LS_IPARAM_NLP_MSW_POPSIZE;
   public static final int    LS_IPARAM_STOC_DEBUG_LEVEL        = LS_IPARAM_STOC_DEBUG_MASK;

/* old parameter names changed in 6.x */
   public static final int    LS_SPRINT_OUTPUT_FILE_DEFAULT     = LS_SPRINT_OUTPUT_FILE_FREE;

/* old parameter names changed in 5.x */
   public static final int    LS_IPARAM_SPLEX_SCALE             = LS_IPARAM_LP_SCALE;
   public static final int    LS_IPARAM_SPLEX_ITRLMT            = LS_IPARAM_LP_ITRLMT;
   public static final int    LS_IPARAM_MIP_USE_ENUM_HEU        = LS_IPARAM_MIP_ENUM_HEUMODE;
   public static final int    LS_IPARAM_SOLVER_USE_CONCURRENT_OPT = LS_IPARAM_SOLVER_CONCURRENT_OPTMODE;
   public static final int    LS_IPARAM_GOP_USEBNDLIM           = LS_IPARAM_GOP_BNDLIM_MODE;
/* old parameter names changed in 4.x or older*/
   public static final int    LSLOC_BANDB                       = LSLOC_MIP;
   public static final int    LS_IPARAM_ITRLMT                  = LS_IPARAM_SPLEX_ITRLMT;
   public static final int    LS_IPARAM_PRICING                 = LS_IPARAM_SPLEX_PPRICING;
   public static final int    LS_IPARAM_SCALE                   = LS_IPARAM_SPLEX_SCALE;
   public static final int    LS_IPARAM_TIMLMT                  = LS_IPARAM_SOLVER_TIMLMT;
   public static final int    LS_DPARAM_CUTOFFVAL               = LS_DPARAM_SOLVER_CUTOFFVAL;
   public static final int    LS_IPARAM_RESTART                 = LS_IPARAM_SOLVER_RESTART;
   public static final int    LS_DPARAM_FEASTOL                 = LS_DPARAM_SOLVER_FEASTOL;
   public static final int    LS_IPARAM_IUSOL                   = LS_IPARAM_SOLVER_IUSOL;
   public static final int    LS_IPARAM_MIPTIMLIM               = LS_IPARAM_MIP_TIMLIM;
   public static final int    LS_IPARAM_MIPAOPTTIMLIM           = LS_IPARAM_MIP_AOPTTIMLIM;
   public static final int    LS_IPARAM_MIPPRELEVEL             = LS_IPARAM_MIP_PRELEVEL;
   public static final int    LS_IPARAM_MIPNODESELRULE          = LS_IPARAM_MIP_NODESELRULE;
   public static final int    LS_DPARAM_MIPINTTOL               = LS_DPARAM_MIP_INTTOL;
   public static final int    LS_DPARAM_MIPRELINTTOL            = LS_DPARAM_MIP_RELINTTOL;
   public static final int    LS_DPARAM_MIP_OPTTOL              = LS_DPARAM_MIP_RELOPTTOL;
   public static final int    LS_DPARAM_MIPOPTTOL               = LS_DPARAM_MIP_OPTTOL;
   public static final int    LS_DPARAM_MIPPEROPTTOL            = LS_DPARAM_MIP_PEROPTTOL;
   public static final int    LS_IPARAM_MIPMAXCUTPASS           = LS_IPARAM_MIP_MAXCUTPASS_TOP;
   public static final int    LS_DPARAM_MIPADDCUTPER            = LS_DPARAM_MIP_ADDCUTPER;
   public static final int    LS_IPARAM_MIPCUTLEVEL             = LS_IPARAM_MIP_CUTLEVEL_TOP;
   public static final int    LS_IPARAM_MIPHEULEVEL             = LS_IPARAM_MIP_HEULEVEL;
   public static final int    LS_IPARAM_MIPPRINTLEVEL           = LS_IPARAM_MIP_PRINTLEVEL;
   public static final int    LS_IPARAM_MIPPREPRINTLEVEL        = LS_IPARAM_MIP_PREPRINTLEVEL;
   public static final int    LS_DPARAM_MIPCUTOFFOBJ            = LS_DPARAM_MIP_CUTOFFOBJ;
   public static final int    LS_IPARAM_MIPSTRONGBRANCHLEVEL    = LS_IPARAM_MIP_STRONGBRANCHLEVEL;
   public static final int    LS_IPARAM_MIPBRANCHDIR            = LS_IPARAM_MIP_BRANCHDIR;
   public static final int    LS_IPARAM_MIPTOPOPT               = LS_IPARAM_MIP_TOPOPT;
   public static final int    LS_IPARAM_MIPREOPT                = LS_IPARAM_MIP_REOPT;
   public static final int    LS_IPARAM_MIPSOLVERTYPE           = LS_IPARAM_MIP_SOLVERTYPE;
   public static final int    LS_IPARAM_MIPKEEPINMEM            = LS_IPARAM_MIP_KEEPINMEM;
   public static final int    LS_DPARAM_MIP_REDCOSTFIXING_CUTOFF = LS_DPARAM_MIP_REDCOSTFIX_CUTOFF;
   public static final int    LS_IPARAM_NLPPRINTLEVEL           = LS_IPARAM_NLP_PRINTLEVEL;
   public static final int    LS_IPARAM_LPPRINTLEVEL            = LS_IPARAM_LP_PRINTLEVEL;
   public static final int    LS_IPARAM_NLPSOLVER               = LS_IPARAM_NLP_SOLVER;
   public static final int    LS_IPARAM_MODEL_CONVEX_FLAG       = LS_IPARAM_NLP_CONVEX;
   public static final int    LS_IPARAM_NLP_SOLVEASLP           = LS_IPARAM_NLP_SOLVE_AS_LP;
   public static final int    LS_DINFO_MIPBESTBOUND             = LS_DINFO_MIP_BESTBOUND;
   public static final int    LS_IINFO_MIPBRANCHCOUNT           = LS_IINFO_MIP_BRANCHCOUNT;
   public static final int    LS_IINFO_MIPSTATUS                = LS_IINFO_MIP_STATUS;
   public static final int    LS_IINFO_MIPNEWIPSOL              = LS_IINFO_MIP_NEWIPSOL;
   public static final int    LS_IINFO_MIPLPCOUNT               = LS_IINFO_MIP_LPCOUNT;
   public static final int    LS_IINFO_MIPACTIVENODES           = LS_IINFO_MIP_ACTIVENODES;
   public static final int    LS_IINFO_MIPLTYPE                 = LS_IINFO_MIP_LTYPE;
   public static final int    LS_IINFO_MIPAOPTTIMETOSTOP        = LS_IINFO_MIP_AOPTTIMETOSTOP;
   public static final int    LS_DINFO_MIPOBJ                   = LS_DINFO_MIP_OBJ;
   public static final int    LS_IPARAM_BARRIER_PROB_TO_SOLVE   = LS_IPARAM_PROB_TO_SOLVE;
   public static final int    LS_IINFO_STATUS                   = LS_IINFO_PRIMAL_STATUS;
   public static final int    LS_GOPSOLSTAT_GLOBAL_OPTIMAL      = LS_STATUS_OPTIMAL;
   public static final int    LS_GOPSOLSTAT_LOCAL_OPTIMAL       = LS_STATUS_LOCAL_OPTIMAL;
   public static final int    LS_GOPSOLSTAT_INFEASIBLE          = LS_STATUS_INFEASIBLE;
   public static final int    LS_GOPSOLSTAT_TOPUNBOUNDED        = LS_STATUS_UNBOUNDED;
   public static final int    LS_GOPSOLSTAT_FEASIBLE            = LS_STATUS_FEASIBLE;
   public static final int    LS_GOPSOLSTAT_UNKNOWN             = LS_STATUS_UNKNOWN;
   public static final int    LS_GOPSOLSTAT_NUMERICAL_ERROR     = LS_STATUS_NUMERICAL_ERROR;
   public static final int    LS_IIS_NORM_NONE                  = LS_IIS_NORM_FREE;
   public static final int    LS_IPARAM_STOC_SAMPLING_METHOD    = LS_IPARAM_STOC_VARCONTROL_METHOD;
   public static final int    LS_DPARAM_GOP_OPTTOL              = LS_DPARAM_GOP_RELOPTTOL;
/* old operator names */
   public static final int    EP_EXT_AND                        = EP_VAND;
   public static final int    EP_EXT_OR                         = EP_VOR;
   public static final int    EP_MULTMULT                       = EP_VMULT;
   public static final int    EP_PUSH_SVAR                      = EP_PUSH_SPAR;

/*********************************************************************

/* Callback Function Definitions  */
/*********************************************************************
 *                                                                   *
 *                        Function Prototypes                        *
 *                                                                   *
 *********************************************************************/

/*********************************************************************
 * Structure Creation and Deletion Routines (4)                      *
 *********************************************************************/


    public static native Object LScreateEnv
                             (int          nErrorcode[],
                              String       szPassword);


    public static native Object LScreateModel
                             (Object       pEnv,
                              int          nErrorcode[]);



    public static native int LSdeleteEnv
                             (Object       pEnv[]);


    public static native int LSdeleteEnv
                             (Object       pEnv);



    public static native int LSdeleteModel
                             (Object       pModel[]);

    public static native int LSdeleteModel
                             (Object       pModel);



    public static native int LSloadLicenseString
                             (String       szFname,
                              StringBuffer achLicense);


    public static native void LSgetVersionInfo
                             (StringBuffer achVernum,
                              StringBuffer achBuildDate);
    public static native int LScopyParam
                             (Object       sourceModel,
                              Object       targetModel,
                              int          mSolverType);


    public static native int LSsetXSolverLibrary
                             (Object       pEnv,
                              int          mVendorId,
                              String       szLibrary);


    public static native int LSgetXSolverLibrary
                             (Object       pEnv,
                              int          mVendorId,
                              String       szLibrary);

/**********************************************************************
 * Model I-O Routines (13)                                            *
 **********************************************************************/

    public static native int LSreadMPSFile
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSwriteMPSFile
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSreadLINDOFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteLINDOFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadLINDOStream
                             (Object       pModel,
                              String       szStream,
                              int          nStreamLen);


    public static native int LSwriteLINGOFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteDualMPSFile
                             (Object       pModel,
                              String       szFname,
                              int          nFormat,
                              int          nObjSense);


    public static native int LSwriteDualLINDOFile
                             (Object       pModel,
                              String       szFname,
                              int          nObjSense);


    public static native int LSwriteSolution
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteNLSolution
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteSolutionOfType
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSwriteIIS
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteIUS
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadMPIFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteMPIFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteMPXFile
                             (Object       pModel,
                              String       szFname,
                              int          mMask);


    public static native int LSwriteWithSetsAndSC
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSreadBasis
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSwriteBasis
                             (Object       pModel,
                              String       szFname,
                              int          nFormat);


    public static native int LSwriteVarPriorities
                             (Object       pModel,
                              String       szFname,
                              int          nMode);


    public static native int LSreadLPFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadLPStream
                             (Object       pModel,
                              String       szStream,
                              int          nStreamLen);


    public static native int LSreadSDPAFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadCBFFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadMPXFile
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadMPXStream
                             (Object       pModel,
                              String       szStream,
                              int          nStreamLen);

    public static native int LSreadNLFile
                             (Object       pModel,
                              String       szFname);


/**********************************************************************
 * Error Handling Routines (3)                                        *
 **********************************************************************/

    public static native int LSgetErrorMessage
                             (Object       pEnv,
                              int          nErrorcode,
                              StringBuffer achMessage);


    public static native int LSgetFileError
                             (Object       pModel,
                              int          nLinenum[],
                              StringBuffer achLinetxt);


    public static native int LSgetErrorRowIndex
                             (Object       pModel,
                              int          iRow[]);


/**********************************************************************
 * Routines for Setting and Retrieving Parameter Values (14)          *
 **********************************************************************/

    public static native int LSsetModelParameter
                             (Object       pModel,
                              int          nParameter,
                              int          nvValue[]);


    public static native int LSgetModelParameter
                             (Object       pModel,
                              int          nParameter,
                              int          nvValue[]);


    public static native int LSsetEnvParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nvValue[]);


    public static native int LSgetEnvParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nvValue[]);



    public static native int LSsetModelParameter
                             (Object       pModel,
                              int          nParameter,
                              double       dvValue[]);




    public static native int LSgetModelParameter
                             (Object       pModel,
                              int          nParameter,
                              double       dvValue[]);





    public static native int LSsetEnvParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dvValue[]);


    public static native int LSgetEnvParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dvValue[]);


    public static native int LSsetModelDouParameter
                             (Object       pModel,
                              int          nParameter,
                              double       dVal);


    public static native int LSgetModelDouParameter
                             (Object       pModel,
                              int          nParameter,
                              double       dVal[]);


    public static native int LSsetModelIntParameter
                             (Object       pModel,
                              int          nParameter,
                              int          nVal);


    public static native int LSgetModelIntParameter
                             (Object       pModel,
                              int          nParameter,
                              int          nVal[]);


    public static native int LSsetEnvDouParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dVal);


    public static native int LSgetEnvDouParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dVal[]);


    public static native int LSsetEnvIntParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nVal);


    public static native int LSgetEnvIntParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nVal[]);


    public static native int LSreadModelParameter
                             (Object       pModel,
                              String       szFname);


    public static native int LSreadEnvParameter
                             (Object       pEnv,
                              String       szFname);


    public static native int LSwriteModelParameter
                             (Object       pModel,
                              String       szFname);


    public static native int LSwriteEnvParameter
                             (Object       pEnv,
                              String       szFname);


    public static native int LSwriteParameterAsciiDoc
                             (Object       pEnv,
                              String       szFileName);


    public static native int LSgetIntParameterRange
                             (Object       pModel,
                              int          nParameter,
                              int          nValMIN[],
                              int          nValMAX[]);


    public static native int LSgetDouParameterRange
                             (Object       pModel,
                              int          nParameter,
                              double       dValMIN[],
                              double       dValMAX[]);


    public static native int LSgetParamShortDesc
                             (Object       pEnv,
                              int          nParam,
                              StringBuffer szDescription);


    public static native int LSgetParamLongDesc
                             (Object       pEnv,
                              int          nParam,
                              StringBuffer szDescription);


    public static native int LSgetParamMacroName
                             (Object       pEnv,
                              int          nParam,
                              StringBuffer szParam);


    public static native int LSgetParamMacroID
                             (Object       pEnv,
                              String       szParam,
                              int          nParamType[],
                              int          nParam[]);


    public static native int LSgetQCEigs
                             (Object       pModel,
                              int          iRow,
                              String       achWhich,
                              double       adEigval[],
                              double       adEigvec[],
                              int          nEigval,
                              int          ncv,
                              double       dTol,
                              int          nMaxIter);


    public static native int LSgetEigs
                             (int          nDim,
                              String       chUL,
                              double       adA[],
                              double       adD[],
                              double       adV[],
                              int          nInfo[]);


    public static native int LSgetEigg
                             (int          nDim,
                              String       chJOBV,
                              double       adA[],
                              double       adWR[],
                              double       adWI[],
                              double       adVRR[],
                              double       adVRI[],
                              double       adVLR[],
                              double       adVLI[],
                              int          nInfo[]);


    public static native int LSgetMatrixTranspose
                             (int          nRows,
                              int          nCols,
                              double       adA[],
                              double       adAT[]);


    public static native int LSgetMatrixInverse
                             (int          nRows,
                              double       adA[],
                              double       adAinv[],
                              int          nInfo[]);


    public static native int LSgetMatrixInverseSY
                             (int          nRows,
                              String       chUpLo,
                              double       adA[],
                              double       adAinv[],
                              int          nInfo[]);


    public static native int LSgetMatrixLUFactor
                             (int          nRows,
                              int          nCols,
                              double       adA[],
                              int          anP[],
                              double       adL[],
                              double       adU[],
                              int          nInfo[]);


    public static native int LSgetMatrixQRFactor
                             (int          nRows,
                              int          nCols,
                              double       adA[],
                              double       adQ[],
                              double       adR[],
                              int          nInfo[]);


    public static native int LSgetMatrixDeterminant
                             (int          nRows,
                              double       adA[],
                              double       adDet[],
                              int          nInfo[]);


    public static native int LSgetMatrixCholFactor
                             (int          nRows,
                              String       chUpLo,
                              double       adA[],
                              double       adUL[],
                              int          nInfo[]);


    public static native int LSgetMatrixSVDFactor
                             (int          nRows,
                              int          nCols,
                              double       adA[],
                              double       adU[],
                              double       adS[],
                              double       adVT[],
                              int          nInfo[]);


    public static native int LSgetMatrixFSolve
                             (String       szuplo,
                              String       sztrans,
                              String       szdiag,
                              int          nRows,
                              double       dAlpha,
                              double       adA[],
                              double       adB[],
                              double       adX[]);


    public static native int LSgetMatrixBSolve
                             (String       szuplo,
                              String       sztrans,
                              String       szdiag,
                              int          nRows,
                              double       dAlpha,
                              double       adA[],
                              double       adB[],
                              double       adX[]);


    public static native int LSgetMatrixSolve
                             (String       szside,
                              String       szuplo,
                              String       sztrans,
                              String       szdiag,
                              int          nRows,
                              int          nRHS,
                              double       dAlpha,
                              double       adA[],
                              int          nLDA,
                              double       adB[],
                              int          nLDB,
                              double       adX[]);


 /*********************************************************************
  * Model Loading Routines (9)                                        *
  *********************************************************************/

    public static native int LSloadLPData
                             (Object       pModel,
                              int          nCons,
                              int          nVars,
                              int          dObjSense,
                              double       dObjConst,
                              double       adC[],
                              double       adB[],
                              String       szConTypes,
                              int          nAnnz,
                              int          aiAcols[],
                              int          anAcols[],
                              double       adAcoef[],
                              int          aiArows[],
                              double       adL[],
                              double       adU[]);


    public static native int LSloadQCData
                             (Object       pModel,
                              int          nQCnnz,
                              int          aiQCrows[],
                              int          aiQCcols1[],
                              int          aiQCcols2[],
                              double       adQCcoef[]);


    public static native int LSloadConeData
                             (Object       pModel,
                              int          nCone,
                              String       szConeTypes,
                              double       adConeAlpha[],
                              int          aiConebegcone[],
                              int          aiConecols[]);


    public static native int LSloadPOSDData
                             (Object       pModel,
                              int          nPOSD,
                              int          aiPOSDdim[],
                              int          aiPOSDbeg[],
                              int          aiPOSDrowndx[],
                              int          aiPOSDcolndx[],
                              int          aiPOSDvarndx[]);


    public static native int LSloadALLDIFFData
                             (Object       pModel,
                              int          nALLDIFF,
                              int          aiAlldiffDim[],
                              int          aiAlldiffL[],
                              int          aiAlldiffU[],
                              int          aiAlldiffBeg[],
                              int          aiAlldiffVar[]);


    public static native int LSloadSETSData
                             (Object       pModel,
                              int          nSETS,
                              String       szSETStype,
                              int          aiCARDnum[],
                              int          aiSETSbegcol[],
                              int          aiSETScols[]);


    public static native int LSloadSemiContData
                             (Object       pModel,
                              int          nSCVars,
                              int          aiVars[],
                              double       adL[],
                              double       adU[]);


    public static native int LSloadVarType
                             (Object       pModel,
                              String       szVarTypes);


    public static native int LSloadNameData
                             (Object       pModel,
                              String       szTitle,
                              String       szObjName,
                              String       szRhsName,
                              String       szRngName,
                              String       szBndname,
                              String       aszConNames[],
                              String       aszVarNames[],
                              String       aszConeNames[]);


    public static native int LSloadNLPData
                             (Object       pModel,
                              int          aiNLPcols[],
                              int          anNLPcols[],
                              double       adNLPcoef[],
                              int          aiNLProws[],
                              int          nNLPobj,
                              int          aiNLPobj[],
                              double       adNLPobj[]);


    public static native int LSloadNLPDense
                             (Object       pModel,
                              int          nCons,
                              int          nVars,
                              int          dObjSense,
                              String       szConTypes,
                              String       szVarTypes,
                              double       adX0[],
                              double       adL[],
                              double       adU[]);


    public static native int LSloadInstruct
                             (Object       pModel,
                              int          nCons,
                              int          nObjs,
                              int          nVars,
                              int          nNumbers,
                              int          anObjSense[],
                              String       szConType,
                              String       szVarType,
                              int          anInstruct[],
                              int          nInstruct,
                              int          aiVars[],
                              double       adNumVal[],
                              double       adVarVal[],
                              int          aiObjBeg[],
                              int          anObjLen[],
                              int          aiConBeg[],
                              int          anConLen[],
                              double       adLB[],
                              double       adUB[]);


    public static native int LSaddInstruct
                             (Object       pModel,
                              int          nCons,
                              int          nObjs,
                              int          nVars,
                              int          nNumbers,
                              int          anObjSense[],
                              String       szConType,
                              String       szVarType,
                              int          anInstruct[],
                              int          nInstruct,
                              int          aiCons[],
                              double       adNumVal[],
                              double       adVarVal[],
                              int          aiObjBeg[],
                              int          anObjLen[],
                              int          aiConBeg[],
                              int          anConLen[],
                              double       adLB[],
                              double       adUB[]);


    public static native int LSloadStringData
                             (Object       pModel,
                              int          nStrings,
                              String       aszStringData[]);


    public static native int LSloadString
                             (Object       pModel,
                              String       szString);


    public static native int LSbuildStringData
                             (Object       pModel);


    public static native int LSdeleteStringData
                             (Object       pModel);


    public static native int LSdeleteString
                             (Object       pModel);


    public static native int LSgetStringValue
                             (Object       pModel,
                              int          iString,
                              double       dValue[]);


    public static native int LSgetConstraintProperty
                             (Object       pModel,
                              int          ndxCons,
                              int          nConptype[]);


    public static native int LSsetConstraintProperty
                             (Object       pModel,
                              int          ndxCons,
                              int          nConptype);


    public static native int LSgetGOPVariablePriority
                             (Object       pModel,
                              int          ndxVar,
                              int          nPriority[]);


    public static native int LSsetGOPVariablePriority
                             (Object       pModel,
                              int          ndxVar,
                              int          nPriority);


    public static native int LSloadMultiStartSolution
                             (Object       pModel,
                              int          nIndex);


    public static native int LSloadGASolution
                             (Object       pModel,
                              int          nIndex);


    public static native int LSaddQCShift
                             (Object       pModel,
                              int          iRow,
                              double       dShift);


    public static native int LSgetQCShift
                             (Object       pModel,
                              int          iRow,
                              double       dShift[]);


    public static native int LSresetQCShift
                             (Object       pModel,
                              int          iRow);


    public static native int LSaddObjPool
                             (Object       pModel,
                              double       adC[],
                              int          mObjSense,
                              int          mRank,
                              double       dRelOptTol);


    public static native int LSremObjPool
                             (Object       pModel,
                              int          nObjIndex);


    public static native int LSfreeObjPool
                             (Object       pModel);


    public static native int LSsetObjPoolInfo
                             (Object       pModel,
                              int          nObjIndex,
                              int          mInfo,
                              double       dValue);


    public static native int LSgetObjPoolNumSol
                             (Object       pModel,
                              int          nObjIndex,
                              int          nNumSol[]);


/**********************************************************************
 * Solver Initialization Routines (6)                                 *
 **********************************************************************/

    public static native int LSloadBasis
                             (Object       pModel,
                              int          anCstatus[],
                              int          anRstatus[]);


    public static native int LSloadVarPriorities
                             (Object       pModel,
                              int          anCprior[]);


    public static native int LSreadVarPriorities
                             (Object       pModel,
                              String       szFname);


    public static native int LSloadVarStartPoint
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSloadVarStartPointPartial
                             (Object       pModel,
                              int          nCols,
                              int          aiCols[],
                              double       adPrimal[]);


    public static native int LSloadMIPVarStartPoint
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSloadMIPVarStartPointPartial
                             (Object       pModel,
                              int          nCols,
                              int          aiCols[],
                              int          aiPrimal[]);


    public static native int LSreadVarStartPoint
                             (Object       pModel,
                              String       szFname);


    public static native int LSloadBlockStructure
                             (Object       pModel,
                              int          nBlock,
                              int          anRblock[],
                              int          anCblock[],
                              int          nType);


    public static native int LSloadIISPriorities
                             (Object       pModel,
                              int          anRprior[],
                              int          anCprior[]);


    public static native int LSloadSolutionAt
                             (Object       pModel,
                              int          nObjIndex,
                              int          nSolIndex);


/**********************************************************************
 * Optimization Routines (3)                                          *
 **********************************************************************/

    public static native int LSoptimize
                             (Object       pModel,
                              int          nMethod,
                              int          nSolStatus[]);


    public static native int LSsolveMIP
                             (Object       pModel,
                              int          nMIPSolStatus[]);


    public static native int LSsolveGOP
                             (Object       pModel,
                              int          nGOPSolStatus[]);


    public static native int LSoptimizeQP
                             (Object       pModel,
                              int          nQPSolStatus[]);


    public static native int LScheckConvexity
                             (Object       pModel);


    public static native int LSsolveSBD
                             (Object       pModel,
                              int          nStages,
                              int          anRowStage[],
                              int          anColStage[],
                              int          nStatus[]);


    public static native int LSsolveMipBnp
                             (Object       pModel,
                              int          nBlock,
                              String       szFname,
                              int          nStatus[]);


/**********************************************************************
 * Solution Query Routines (13)                                       *
 **********************************************************************/

 /* query general model and solver information */
    public static native int LSgetInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nvResult[]);


    public static native int LSgetInfo
                             (Object       pModel,
                              int          nQuery,
                              double       dResult[]);
    public static native int LSgetProfilerInfo
                             (Object       pModel,
                              int          mContext,
                              int          nCalls[],
                              double       dElapsedTime[]);


    public static native String LSgetProfilerContext
                             (Object       pModel,
                              int          mContext);


 /* query continous models */
    public static native int LSgetPrimalSolution
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSgetDualSolution
                             (Object       pModel,
                              double       adDual[]);


    public static native int LSgetReducedCosts
                             (Object       pModel,
                              double       adRedcosts[]);


    public static native int LSgetReducedCostsCone
                             (Object       pModel,
                              double       adRedcosts[]);


    public static native int LSgetSlacks
                             (Object       pModel,
                              double       adSlacks[]);


    public static native int LSgetBasis
                             (Object       pModel,
                              int          anCstatus[],
                              int          anRstatus[]);


    public static native int LSgetSolution
                             (Object       pModel,
                              int          nWhich,
                              double       adVal[]);


    public static native int LSgetNextBestSol
                             (Object       pModel,
                              int          nModStatus[]);


 /* query integer models */
    public static native int LSgetMIPPrimalSolution
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSgetMIPDualSolution
                             (Object       pModel,
                              double       adDual[]);


    public static native int LSgetMIPReducedCosts
                             (Object       pModel,
                              double       adRedcosts[]);


    public static native int LSgetMIPSlacks
                             (Object       pModel,
                              double       adSlacks[]);


    public static native int LSgetMIPBasis
                             (Object       pModel,
                              int          anCstatus[],
                              int          anRstatus[]);


    public static native int LSgetNextBestMIPSol
                             (Object       pModel,
                              int          nIntModStatus[]);


    public static native int LSgetKBestMIPSols
                             (Object       pModel,
                              String       szFname,
                              String       nfMIPCallback,
                              Object       nvCbData,
                              int          nMaxSols);


 /*********************************************************************
  * Model Query Routines (13)                                         *
  *********************************************************************/

    public static native int LSgetLPData
                             (Object       pModel,
                              int          dObjSense[],
                              double       dObjConst[],
                              double       adC[],
                              double       adB[],
                              StringBuffer achConTypes,
                              int          aiAcols[],
                              int          anAcols[],
                              double       adAcoef[],
                              int          aiArows[],
                              double       adL[],
                              double       adU[]);


    public static native int LSgetQCData
                             (Object       pModel,
                              int          aiQCrows[],
                              int          aiQCcols1[],
                              int          aiQCcols2[],
                              double       adQCcoef[]);


    public static native int LSgetQCDatai
                             (Object       pModel,
                              int          iCon,
                              int          nQCnnz[],
                              int          aiQCcols1[],
                              int          aiQCcols2[],
                              double       adQCcoef[]);


    public static native int LSgetVarType
                             (Object       pModel,
                              StringBuffer achVarTypes);


    public static native int LSgetVarStartPoint
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSgetVarStartPointPartial
                             (Object       pModel,
                              int          anCols[],
                              int          aiCols[],
                              double       adPrimal[]);


    public static native int LSgetMIPVarStartPointPartial
                             (Object       pModel,
                              int          anCols[],
                              int          aiCols[],
                              int          aiPrimal[]);


    public static native int LSgetMIPVarStartPoint
                             (Object       pModel,
                              double       adPrimal[]);


    public static native int LSgetSETSData
                             (Object       pModel,
                              int          iNsets[],
                              int          iNtnz[],
                              StringBuffer achSETtype,
                              int          aiCardnum[],
                              int          aiNnz[],
                              int          aiBegset[],
                              int          aiVarndx[]);


    public static native int LSgetSETSDatai
                             (Object       pModel,
                              int          iSet,
                              StringBuffer achSETType,
                              int          iCardnum[],
                              int          iNnz[],
                              int          aiVarndx[]);


    public static native int LSsetSETSStatei
                             (Object       pModel,
                              int          iSet,
                              int          mState);


    public static native int LSgetSemiContData
                             (Object       pModel,
                              int          iNvars[],
                              int          aiVarndx[],
                              double       adL[],
                              double       adU[]);


    public static native int LSgetALLDIFFData
                             (Object       pModel,
                              int          inALLDIFF[],
                              int          aiAlldiffDim[],
                              int          aiAlldiffL[],
                              int          aiAlldiffU[],
                              int          aiAlldiffBeg[],
                              int          aiAlldiffVar[]);


    public static native int LSgetALLDIFFDatai
                             (Object       pModel,
                              int          iALLDIFF,
                              int          iAlldiffDim[],
                              int          iAlldiffL[],
                              int          iAlldiffU[],
                              int          aiAlldiffVar[]);


    public static native int LSgetPOSDData
                             (Object       pModel,
                              int          inPOSD[],
                              int          aiPOSDdim[],
                              int          aiPOSDnnz[],
                              int          aiPOSDbeg[],
                              int          aiPOSDrowndx[],
                              int          aiPOSDcolndx[],
                              int          aiPOSDvarndx[]);


    public static native int LSgetPOSDDatai
                             (Object       pModel,
                              int          iPOSD,
                              int          iPOSDdim[],
                              int          iPOSDnnz[],
                              int          aiPOSDrowndx[],
                              int          aiPOSDcolndx[],
                              int          aiPOSDvarndx[]);


    public static native int LSgetNameData
                             (Object       pModel,
                              StringBuffer achTitle,
                              StringBuffer achObjName,
                              StringBuffer achRhsName,
                              StringBuffer achRngName,
                              StringBuffer achBndname,
                              StringBuffer achConNames[],
                              StringBuffer achConNameData,
                              StringBuffer achVarNames[],
                              StringBuffer achVarNameData);


    public static native int LSgetLPVariableDataj
                             (Object       pModel,
                              int          iVar,
                              StringBuffer achVartype,
                              double       dC[],
                              double       dL[],
                              double       dU[],
                              int          nAnnz[],
                              int          aiArows[],
                              double       adAcoef[]);


    public static native int LSgetVariableNamej
                             (Object       pModel,
                              int          iVar,
                              StringBuffer achVarName);


    public static native int LSgetVariableIndex
                             (Object       pModel,
                              String       szVarName,
                              int          iVar[]);


    public static native int LSgetConstraintNamei
                             (Object       pModel,
                              int          iCon,
                              StringBuffer achConName);


    public static native int LSgetConstraintIndex
                             (Object       pModel,
                              String       szConName,
                              int          iCon[]);


    public static native int LSgetConstraintDatai
                             (Object       pModel,
                              int          iCon,
                              StringBuffer achConType,
                              StringBuffer achIsNlp,
                              double       dB[]);


    public static native int LSgetLPConstraintDatai
                             (Object       pModel,
                              int          iCon,
                              StringBuffer achConType,
                              double       dB[],
                              int          nNnz[],
                              int          iVar[],
                              double       dAcoef[]);


    public static native int LSgetConeNamei
                             (Object       pModel,
                              int          iCone,
                              StringBuffer achConeName);


    public static native int LSgetConeIndex
                             (Object       pModel,
                              String       szConeName,
                              int          iCone[]);


    public static native int LSgetConeDatai
                             (Object       pModel,
                              int          iCone,
                              StringBuffer achConeType,
                              double       dConeAlpha[],
                              int          iNnz[],
                              int          aiCols[]);


    public static native int LSgetNLPData
                             (Object       pModel,
                              int          aiNLPcols[],
                              int          anNLPcols[],
                              double       adNLPcoef[],
                              int          aiNLProws[],
                              int          nNLPobj[],
                              int          aiNLPobj[],
                              double       adNLPobj[],
                              StringBuffer achNLPConTypes);


    public static native int LSgetNLPConstraintDatai
                             (Object       pModel,
                              int          iCon,
                              int          nNnz[],
                              int          aiNLPcols[],
                              double       adNLPcoef[]);


    public static native int LSgetNLPVariableDataj
                             (Object       pModel,
                              int          iVar,
                              int          nNnz[],
                              int          anNLProws[],
                              double       adNLPcoef[]);


    public static native int LSgetNLPObjectiveData
                             (Object       pModel,
                              int          nNLPobjnnz[],
                              int          aiNLPobj[],
                              double       adNLPobj[]);


    public static native int LSgetDualModel
                             (Object          pModel,
                              Object          nDualModel);

    public static native int LSgetInstruct
                             (Object       pModel,
                              int          nObjSense[],
                              StringBuffer achConType,
                              StringBuffer achVarType,
                              int          anCode[],
                              double       adNumVal[],
                              double       adVarVal[],
                              int          anObjBeg[],
                              int          anObjLength[],
                              int          anConBeg[],
                              int          anConLength[],
                              double       adLwrBnd[],
                              double       adUprBnd[]);


    public static native int LScalinfeasMIPsolution
                             (Object       pModel,
                              double       dIntPfeas[],
                              double       nbConsPfeas[],
                              double       dPrimalMipsol[]);


    public static native int LSgetRoundMIPsolution
                             (Object       pModel,
                              double       adPrimal[],
                              double       adPrimalRound[],
                              double       adObjRound[],
                              double       adPfeasRound[],
                              int          nstatus[],
                              int          iUseOpti);


    public static native int LSgetDuplicateColumns
                             (Object       pModel,
                              int          nCheckVals,
                              int          nSets[],
                              int          aiSetsBeg[],
                              int          aiCols[]);


    public static native int LSgetRangeData
                             (Object       pModel,
                              double       adR[]);


    public static native int LSgetJac
                             (Object       pModel,
                              int          nJnonzeros[],
                              int          nJobjnnz[],
                              int          aiJrows[],
                              int          aiJcols[],
                              double       adJcoef[],
                              double       adX[]);


    public static native int LSgetHess
                             (Object       pModel,
                              int          nHnonzeros[],
                              int          aiHrows[],
                              int          aiHcol1[],
                              int          aiHcol2[],
                              double       adHcoef[],
                              double       adX[]);


/**********************************************************************
 *  Model Modification Routines (22)                                  *
 **********************************************************************/

    public static native int LSaddConstraints
                             (Object       pModel,
                              int          nNumaddcons,
                              String       szConTypes,
                              String       aszConNames[],
                              int          aiArows[],
                              double       adAcoef[],
                              int          aiAcols[],
                              double       adB[]);


    public static native int LSaddVariables
                             (Object       pModel,
                              int          nNumaddvars,
                              String       szVarTypes,
                              String       aszVarNames[],
                              int          aiAcols[],
                              int          anAcols[],
                              double       adAcoef[],
                              int          aiArows[],
                              double       adC[],
                              double       adL[],
                              double       adU[]);


    public static native int LSaddCones
                             (Object       pModel,
                              int          nCone,
                              String       szConeTypes,
                              double       adConeAlpha[],
                              String       aszConenames[],
                              int          aiConebegcol[],
                              int          aiConecols[]);


    public static native int LSaddSETS
                             (Object       pModel,
                              int          nSETS,
                              String       szSETStype,
                              int          aiCARDnum[],
                              int          aiSETSbegcol[],
                              int          aiSETScols[]);


    public static native int LSaddQCterms
                             (Object       pModel,
                              int          nQCnonzeros,
                              int          aiQCconndx[],
                              int          aiQCvarndx1[],
                              int          aiQCvarndx2[],
                              double       adQCcoef[]);


    public static native int LSdeleteConstraints
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[]);


    public static native int LSdeleteCones
                             (Object       pModel,
                              int          nCones,
                              int          aiCones[]);


    public static native int LSdeleteSETS
                             (Object       pModel,
                              int          nSETS,
                              int          aiSETS[]);


    public static native int LSdeleteSemiContVars
                             (Object       pModel,
                              int          nSCVars,
                              int          aiSCVars[]);


    public static native int LSdeleteVariables
                             (Object       pModel,
                              int          nVars,
                              int          aiVars[]);


    public static native int LSdeleteQCterms
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[]);


    public static native int LSdeleteAj
                             (Object       pModel,
                              int          iVar1,
                              int          nRows,
                              int          aiRows[]);


    public static native int LSmodifyLowerBounds
                             (Object       pModel,
                              int          nVars,
                              int          aiVars[],
                              double       adL[]);


    public static native int LSmodifyUpperBounds
                             (Object       pModel,
                              int          nVars,
                              int          aiVars[],
                              double       adU[]);


    public static native int LSmodifyRHS
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[],
                              double       adB[]);


    public static native int LSmodifyObjective
                             (Object       pModel,
                              int          nVars,
                              int          aiVars[],
                              double       adC[]);


    public static native int LSmodifyObjConstant
                             (Object       pModel,
                              double       dObjConst);


    public static native int LSmodifyAj
                             (Object       pModel,
                              int          iVar1,
                              int          nRows,
                              int          aiRows[],
                              double       adAj[]);


    public static native int LSmodifyCone
                             (Object       pModel,
                              String       cConeType,
                              int          iConeNum,
                              int          iConeNnz,
                              int          aiConeCols[],
                              double       dConeAlpha);


    public static native int LSmodifySET
                             (Object       pModel,
                              String       cSETtype,
                              int          iSETnum,
                              int          iSETnnz,
                              int          aiSETcols[]);


    public static native int LSmodifySemiContVars
                             (Object       pModel,
                              int          nSCVars,
                              int          aiSCVars[],
                              double       adL[],
                              double       adU[]);


    public static native int LSmodifyConstraintType
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[],
                              String       szConTypes);


    public static native int LSmodifyVariableType
                             (Object       pModel,
                              int          nVars,
                              int          aiVars[],
                              String       szVarTypes);


    public static native int LSaddNLPAj
                             (Object       pModel,
                              int          iVar1,
                              int          nRows,
                              int          aiRows[],
                              double       adAj[]);


    public static native int LSaddNLPobj
                             (Object       pModel,
                              int          nCols,
                              int          aiCols[],
                              double       adColj[]);


    public static native int LSdeleteNLPobj
                             (Object       pModel,
                              int          nCols,
                              int          aiCols[]);


/*********************************************************************
 *   Model & Solution Analysis Routines (10)                         *
 *********************************************************************/

    public static native int LSgetConstraintRanges
                             (Object       pModel,
                              double       adDec[],
                              double       adInc[]);


    public static native int LSgetObjectiveRanges
                             (Object       pModel,
                              double       adDec[],
                              double       adInc[]);


    public static native int LSgetBoundRanges
                             (Object       pModel,
                              double       adDec[],
                              double       adInc[]);


    public static native int LSgetBestBounds
                             (Object       pModel,
                              double       adBestL[],
                              double       adBestU[]);


    public static native int LSfindIIS
                             (Object       pModel,
                              int          nLevel);


    public static native int LSfindIUS
                             (Object       pModel,
                              int          nLevel);


    public static native int LSfindBlockStructure
                             (Object       pModel,
                              int          nBlock,
                              int          nType);


    public static native int LSdisplayBlockStructure
                             (Object       pModel,
                              int          nBlock[],
                              int          anNewColIdx[],
                              int          anNewRowIdx[],
                              int          anNewColPos[],
                              int          anNewRowPos[]);


    public static native int LSgetIIS
                             (Object       pModel,
                              int          nSuf_r[],
                              int          nIIS_r[],
                              int          aiCons[],
                              int          nSuf_c[],
                              int          nIIS_c[],
                              int          aiVars[],
                              int          anBnds[]);


    public static native int LSgetIISInts
                             (Object       pModel,
                              int          nSuf_int[],
                              int          nIIS_int[],
                              int          aiVars[]);


    public static native int LSgetIISSETs
                             (Object       pModel,
                              int          nSuf_set[],
                              int          nIIS_set[],
                              int          aiSets[]);


    public static native int LSgetIUS
                             (Object       pModel,
                              int          nSuf[],
                              int          nIUS[],
                              int          aiVars[]);


    public static native int LSgetBlockStructure
                             (Object       pModel,
                              int          nBlock[],
                              int          anRblock[],
                              int          anCblock[],
                              int          nType[]);


    public static native int LSfindSymmetry
                             (Object       pModel);

/**********************************************************************
 * Advanced Routines (6)                                              *
 **********************************************************************/

    public static native int LSdoBTRAN
                             (Object       pModel,
                              int          cYnz[],
                              int          aiY[],
                              double       adY[],
                              int          cXnz[],
                              int          aiX[],
                              double       adX[]);


    public static native int LSdoFTRAN
                             (Object       pModel,
                              int          cYnz[],
                              int          aiY[],
                              double       adY[],
                              int          cXnz[],
                              int          aiX[],
                              double       adX[]);


 /* function and gradient evaluations */
    public static native int LScalcObjFunc
                             (Object       pModel,
                              double       adPrimal[],
                              double       dObjval[]);


    public static native int LScalcConFunc
                             (Object       pModel,
                              int          iRow,
                              double       adPrimal[],
                              double       adSlacks[]);


    public static native int LScalcObjGrad
                             (Object       pModel,
                              double       adPrimal[],
                              int          nParList,
                              int          aiParList[],
                              double       adParGrad[]);


    public static native int LScalcConGrad
                             (Object       pModel,
                              int          irow,
                              double       adPrimal[],
                              int          nParList,
                              int          aiParList[],
                              double       adParGrad[]);


    public static native int LScheckQterms
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[],
                              int          aiType[]);


    public static native int LSrepairQterms
                             (Object       pModel,
                              int          nCons,
                              int          aiCons[],
                              int          aiType[]);


    public static native int LScomputeFunction
                             (int          inst,
                              double       dInput[],
                              double       dOutput[]);


    public static native int LSfindLtf
                             (Object       pModel,
                              int          anNewColIdx[],
                              int          anNewRowIdx[],
                              int          anNewColPos[],
                              int          anNewRowPos[]);


    public static native int LSapplyLtf
                             (Object       pModel,
                              int          anNewColIdx[],
                              int          anNewRowIdx[],
                              int          anNewColPos[],
                              int          anNewRowPos[],
                              int          nMode);


/**********************************************************************
 * Callback Management Routines (9)                                   *
 **********************************************************************/

    public static native int LSsetCallback
                             (Object       pModel,
                              String       nfCallback,
                              Object       nvCbData);


    public static native int LSsetMIPCallback
                             (Object       pModel,
                              String       nfMIPCallback,
                              Object       nvCbData);

    public static native int LSsetMIPCCStrategy
                             (Object       pModel,
                              Object       MIP_strategy,
                              int          nRunId,
                              String       szParamFile,
                              Object       nvCbData);


    public static native int LSgetCallbackInfo
                             (Object       pModel,
                              int          nLocation,
                              int          nQuery,
                              int          nvValue[]);


    public static native int LSgetCallbackInfo
                             (Object       pModel,
                              int          nLocation,
                              int          nQuery,
                              double       dvValue[]);


    public static native int LSgetProgressInfo
                             (Object       pModel,
                              int          nLocation,
                              int          nQuery,
                              int          nvValue[]);


    public static native int LSgetProgressInfo
                             (Object       pModel,
                              int          nLocation,
                              int          nQuery,
                              double       dvValue[]);


    public static native int LSgetMIPCallbackInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nvValue[]);


    public static native int LSgetMIPCallbackInfo
                             (Object       pModel,
                              int          nQuery,
                              double       dvValue[]);


 /* function evaluation routines for NLP solvers */
    public static native int LSsetGradcalc
                             (Object       pModel,
                              String       nfGrad_func,
                              Object       nvUserData,
                              int          nLenUseGrad,
                              int          nUseGrad[]);


    public static native int LSsetFuncalc
                             (Object       pModel,
                              String       nfFunc,
                              Object       nvFData);


    public static native int LSsetEnvLogfunc
                             (Object       pEnv,
                              String       locFunc,
                              Object       nvPrData);

    public static native int LSsetDbgLogfunc
                             (String       locFunc,
                              Object       nvPrData);


    public static native int LSsetModelLogfunc
                             (Object       pModel,
                              String       locFunc,
                              Object       nvPrData);


    public static native int LSsetUsercalc
                             (Object       pModel,
                              String       nfUser_func,
                              Object       nvUserData);



    public static native int LSsetEnvExitFunc
                             (Object       pEnv,
                              String       pfExitFunc,
                              Object       nvUserData);


    public static native int LSsetGOPCallback
                             (Object       pModel,
                              String       nfGOP_caller,
                              Object       nvPrData);

/**********************************************************************
 *  Memory Related Routines (7)                                       *
 **********************************************************************/

    public static native void LSfreeSolverMemory
                             (Object       pModel);


    public static native void LSfreeHashMemory
                             (Object       pModel);


    public static native void LSfreeSolutionMemory
                             (Object       pModel);


    public static native void LSfreeMIPSolutionMemory
                             (Object       pModel);


    public static native void LSfreeGOPSolutionMemory
                             (Object       pModel);


    public static native int LSsetProbAllocSizes
                             (Object       pModel,
                              int          n_vars_alloc,
                              int          n_cons_alloc,
                              int          n_QC_alloc,
                              int          n_Annz_alloc,
                              int          n_Qnnz_alloc,
                              int          n_NLPnnz_alloc);


    public static native int LSsetProbNameAllocSizes
                             (Object       pModel,
                              int          n_varname_alloc,
                              int          n_rowname_alloc);


    public static native int LSaddEmptySpacesAcolumns
                             (Object       pModel,
                              int          aiColnnz[]);


    public static native int LSaddEmptySpacesNLPAcolumns
                             (Object       pModel,
                              int          aiColnnz[]);



/*********************************************************************
 **
 **    Stochastic Programming Interface
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
/* basic I/O routines */
    public static native int LSwriteDeteqMPSFile
                             (Object       pModel,
                              String       szFilename,
                              int          nFormat,
                              int          iType);


    public static native int LSwriteDeteqLINDOFile
                             (Object       pModel,
                              String       szFilename,
                              int          iType);


    public static native int LSwriteSMPSFile
                             (Object       pModel,
                              String       szCorefile,
                              String       szTimefile,
                              String       szStocfile,
                              int          nCoretype);


    public static native int LSreadSMPSFile
                             (Object       pModel,
                              String       szCorefile,
                              String       szTimefile,
                              String       szStocfile,
                              int          nCoretype);


    public static native int LSwriteSMPIFile
                             (Object       pModel,
                              String       szCorefile,
                              String       szTimefile,
                              String       szStocfile);


    public static native int LSreadSMPIFile
                             (Object       pModel,
                              String       szCorefile,
                              String       szTimefile,
                              String       szStocfile);


    public static native int LSwriteScenarioSolutionFile
                             (Object       pModel,
                              int          jScenario,
                              String       szFname);


    public static native int LSwriteNodeSolutionFile
                             (Object       pModel,
                              int          jScenario,
                              int          iStage,
                              String       szFname);


    public static native int LSwriteScenarioMPIFile
                             (Object       pModel,
                              int          jScenario,
                              String       szFname);


    public static native int LSwriteScenarioMPSFile
                             (Object       pModel,
                              int          jScenario,
                              String       szFname,
                              int          nFormat);


    public static native int LSwriteScenarioLINDOFile
                             (Object       pModel,
                              int          jScenario,
                              String       szFname);


/* parameter routines */
    public static native int LSsetModelStocDouParameter
                             (Object       pModel,
                              int          iPar,
                              double       dVal);


    public static native int LSgetModelStocDouParameter
                             (Object       pModel,
                              int          iPar,
                              double       dVal[]);


    public static native int LSsetModelStocIntParameter
                             (Object       pModel,
                              int          iPar,
                              int          iVal);


    public static native int LSgetModelStocIntParameter
                             (Object       pModel,
                              int          iPar,
                              int          iVal[]);


/* general query routines */
    public static native int LSgetScenarioIndex
                             (Object       pModel,
                              String       szName,
                              int          nIndex[]);


    public static native int LSgetStageIndex
                             (Object       pModel,
                              String       szName,
                              int          nIndex[]);


    public static native int LSgetStocParIndex
                             (Object       pModel,
                              String       szName,
                              int          nIndex[]);


    public static native int LSgetStocParName
                             (Object       pModel,
                              int          nIndex,
                              StringBuffer achName);


    public static native int LSgetScenarioName
                             (Object       pModel,
                              int          nIndex,
                              StringBuffer achName);


    public static native int LSgetStageName
                             (Object       pModel,
                              int          nIndex,
                              StringBuffer achName);


    public static native int LSgetStocInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nParam,
                              int          nResult[]);

    public static native int LSgetStocInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nParam,
                              double       nResult[]);

    public static native int LSgetStocCCPInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nScenarioIndex,
                              int          nCPPIndex,
                              int          nvResult[]);

    public static native int LSgetStocCCPInfo
                             (Object       pModel,
                              int          nQuery,
                              int          nScenarioIndex,
                              int          nCPPIndex,
                              double       nvResult[]);

    public static native int LSloadSampleSizes
                             (Object       pModel,
                              int          anSampleSize[]);


    public static native int LSloadConstraintStages
                             (Object       pModel,
                              int          anStage[]);


    public static native int LSloadVariableStages
                             (Object       pModel,
                              int          anStage[]);


    public static native int LSloadStageData
                             (Object       pModel,
                              int          numStages,
                              int          anRstage[],
                              int          anCstage[]);


    public static native int LSloadStocParData
                             (Object       pModel,
                              int          anSparStage[],
                              double       adSparValue[]);


    public static native int LSloadStocParNames
                             (Object       pModel,
                              int          nSvars,
                              String       aszSVarNames[]);


    public static native Object LSgetDeteqModel
                             (Object       pModel,
                              int          iDeqType,
                              int          nErrorCode[]);


    public static native int LSaggregateStages
                             (Object       pModel,
                              int          anScheme[],
                              int          nLength);


    public static native int LSgetStageAggScheme
                             (Object       pModel,
                              int          anScheme[],
                              int          nLength[]);


    public static native int LSdeduceStages
                             (Object       pModel,
                              int          nMaxStage,
                              int          anRowStagse[],
                              int          anColStages[],
                              int          anSparStage[]);


/* optimization routines */
    public static native int LSsolveSP
                             (Object       pModel,
                              int          nStatus[]);


    public static native int LSsolveHS
                             (Object       pModel,
                              int          nSearchMethod,
                              int          nStatus[]);


/* solution access routines */
    public static native int LSgetScenarioObjective
                             (Object       pModel,
                              int          jScenario,
                              double       nObj[]);


    public static native int LSgetNodePrimalSolution
                             (Object       pModel,
                              int          jScenario,
                              int          iStage,
                              double       adX[]);


    public static native int LSgetNodeDualSolution
                             (Object       pModel,
                              int          jScenario,
                              int          iStage,
                              double       adY[]);


    public static native int LSgetNodeReducedCost
                             (Object       pModel,
                              int          jScenario,
                              int          iStage,
                              double       adX[]);


    public static native int LSgetNodeSlacks
                             (Object       pModel,
                              int          jScenario,
                              int          iStage,
                              double       adY[]);


    public static native int LSgetScenarioPrimalSolution
                             (Object       pModel,
                              int          jScenario,
                              double       adX[],
                              double       nObj[]);


    public static native int LSgetScenarioReducedCost
                             (Object       pModel,
                              int          jScenario,
                              double       adD[]);


    public static native int LSgetScenarioDualSolution
                             (Object       pModel,
                              int          jScenario,
                              double       adY[]);


    public static native int LSgetScenarioSlacks
                             (Object       pModel,
                              int          jScenario,
                              double       adS[]);


    public static native int LSgetNodeListByScenario
                             (Object       pModel,
                              int          jScenario,
                              int          aiNodes[],
                              int          nNodes[]);


    public static native int LSgetProbabilityByScenario
                             (Object       pModel,
                              int          jScenario,
                              double       dProb[]);


    public static native int LSgetProbabilityByNode
                             (Object       pModel,
                              int          iNode,
                              double       dProb[]);


    public static native int LSgetStocParData
                             (Object       pModel,
                              int          aiStages[],
                              double       adVals[]);


/* load stochastic data */
    public static native int LSaddDiscreteBlocks
                             (Object       pModel,
                              int          iStage,
                              int          nRealzBlock,
                              double       adProb[],
                              int          akStart[],
                              int          aiRows[],
                              int          aiCols[],
                              int          aiStvs[],
                              double       adVals[],
                              int          nModifyRule);


    public static native int LSaddScenario
                             (Object       pModel,
                              int          jScenario,
                              int          iParentScen,
                              int          iStage,
                              double       dProb,
                              int          nElems,
                              int          aiRows[],
                              int          aiCols[],
                              int          aiStvs[],
                              double       adVals[],
                              int          nModifyRule);


    public static native int LSaddDiscreteIndep
                             (Object       pModel,
                              int          iRow,
                              int          jCol,
                              int          iStv,
                              int          nRealizations,
                              double       adProbs[],
                              double       adVals[],
                              int          nModifyRule);


    public static native int LSaddParamDistIndep
                             (Object       pModel,
                              int          iRow,
                              int          jCol,
                              int          iStv,
                              int          nDistType,
                              int          nParams,
                              double       adParams[],
                              int          iModifyRule);


    public static native int LSaddUserDist
                             (Object       pModel,
                              int          iRow,
                              int          jCol,
                              int          iStv,
                              String       nfUserFunc,
                              int          nSamples,
                              Object       anSamples[],
                              Object       nvUserData,
                              int          iModifyRule);


    public static native int LSaddChanceConstraint
                             (Object       pModel,
                              int          iSense,
                              int          nCons,
                              int          aiCons[],
                              double       dPrLevel,
                              double       dObjWeight);


    public static native int LSsetNumStages
                             (Object       pModel,
                              int          numStages);


    public static native int LSgetStocParOutcomes
                             (Object       pModel,
                              int          jScenario,
                              double       adVals[],
                              double       adProbs[]);


    public static native int LSloadCorrelationMatrix
                             (Object       pModel,
                              int          nDim,
                              int          nCorrType,
                              int          nQCnnz,
                              int          aiQCcols1[],
                              int          aiQCcols2[],
                              double       adQCcoef[]);


    public static native int LSgetCorrelationMatrix
                             (Object       pModel,
                              int          iFlag,
                              int          nCorrType,
                              int          nQCnnz[],
                              int          aiQCcols1[],
                              int          aiQCcols2[],
                              double       adQCcoef[]);


    public static native Object LSgetStocParSample
                             (Object       pModel,
                              int          iStv,
                              int          iRow,
                              int          jCol,
                              int          nErrorCode[]);


    public static native int LSgetDiscreteBlocks
                             (Object       pModel,
                              int          iEvent,
                              int          nDistType[],
                              int          iStage[],
                              int          nRealzBlock[],
                              double       adProbs[],
                              int          iModifyRule[]);


    public static native int LSgetDiscreteBlockOutcomes
                             (Object       pModel,
                              int          iEvent,
                              int          iRealz,
                              int          nRealz[],
                              int          aiArows[],
                              int          aiAcols[],
                              int          aiStvs[],
                              double       adVals[]);


    public static native int LSgetDiscreteIndep
                             (Object       pModel,
                              int          iEvent,
                              int          nDistType[],
                              int          iStage[],
                              int          iRow[],
                              int          jCol[],
                              int          iStv[],
                              int          nRealizations[],
                              double       adProbs[],
                              double       adVals[],
                              int          iModifyRule[]);


    public static native int LSgetParamDistIndep
                             (Object       pModel,
                              int          iEvent,
                              int          nDistType[],
                              int          iStage[],
                              int          iRow[],
                              int          jCol[],
                              int          iStv[],
                              int          nParams[],
                              double       adParams[],
                              int          iModifyRule[]);


    public static native int LSgetScenario
                             (Object       pModel,
                              int          jScenario,
                              int          iParentScen[],
                              int          iBranchStage[],
                              double       dProb[],
                              int          nRealz[],
                              int          aiArows[],
                              int          aiAcols[],
                              int          aiStvs[],
                              double       adVals[],
                              int          iModifyRule[]);


    public static native int LSgetChanceConstraint
                             (Object       pModel,
                              int          iChance,
                              int          iSense[],
                              int          nCons[],
                              int          aiCons[],
                              double       dProb[],
                              double       dObjWeight[]);


    public static native int LSgetSampleSizes
                             (Object       pModel,
                              int          anSampleSize[]);


    public static native int LSgetConstraintStages
                             (Object       pModel,
                              int          anStage[]);


    public static native int LSgetVariableStages
                             (Object       pModel,
                              int          anStage[]);


    public static native int LSgetStocRowIndices
                             (Object       pModel,
                              int          aiSrows[]);


    public static native int LSsetStocParRG
                             (Object       pModel,
                              int          iStv,
                              int          iRow,
                              int          jCol,
                              Object        pRG);

    public static native Object LSgetScenarioModel
                             (Object       pModel,
                              int          jScenario,
                              int          nErrorcode[]);

/* memory routines */
    public static native void LSfreeStocMemory
                             (Object       pModel);


    public static native void LSfreeStocHashMemory
                             (Object       pModel);


/* stochastic parameter routines */
    public static native int LSgetModelStocParameter
                             (Object       pModel,
                              int          nQuery,
                              int          nvResult[]);

    public static native int LSgetModelStocParameter
                             (Object       pModel,
                              int          nQuery,
                              double       dvResult[]);


    public static native int LSsetModelStocParameter
                             (Object       pModel,
                              int          nQuery,
                              int          nvResult[]);

    public static native int LSsetModelStocParameter
                             (Object       pModel,
                              int          nQuery,
                              double       dvResult[]);


    public static native int LSsetEnvStocParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nvValue[]);

    public static native int LSsetEnvStocParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dvValue[]);


    public static native int LSgetEnvStocParameter
                             (Object       pEnv,
                              int          nParameter,
                              int          nvValue[]);

    public static native int LSgetEnvStocParameter
                             (Object       pEnv,
                              int          nParameter,
                              double       dvValue[]);

/*********************************************************************
 **
 **    Statistical Calculations Interface
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
/* Public Functions */
    public static native Object LSsampCreate
                             (Object       pEnv,
                              int          nDistType,
                              int          nErrorCode[]);


    public static native int LSsampDelete
                             (Object       pSample[]);

    public static native int LSsampDelete
                             (Object       pSample);



    public static native int LSsampSetUserDistr
                             (Object       pSample,
                              String       nUserFunc,
                              Object       nUserData);

    public static native int LSsampSetDistrParam
                             (Object       pSample,
                              int          nIndex,
                              double       dValue);


    public static native int LSsampGetDistrParam
                             (Object       pSample,
                              int          nIndex,
                              double       dValue[]);


    public static native int LSsampEvalDistr
                             (Object       pSample,
                              int          nFuncType,
                              double       dXval,
                              double       dResult[]);


    public static native int LSsampEvalDistrLI
                             (Object       pSample,
                              int          nFuncType,
                              double       dXval,
                              double       dResult[]);


    public static native int LSsampEvalUserDistr
                             (Object       pSample,
                              int          nFuncType,
                              double       adXval[],
                              int          nDim,
                              double       dResult[]);


    public static native int LSsampSetRG
                             (Object       pSample,
                              Object       nRG);


    public static native int LSsampGenerate
                             (Object       pSample,
                              int          nMethod,
                              int          nSize);


    public static native int LSsampGetPointsPtr
                             (Object       pSample,
                              int          nSampSize[],
                              double       dXval[]);


    public static native int LSsampGetPoints
                             (Object       pSample,
                              int          nSampSize[],
                              double       dXval[]);


    public static native int LSsampLoadPoints
                             (Object       pSample,
                              int          nSampSize,
                              double       dXval[]);


    public static native int LSsampGetCIPointsPtr
                             (Object       pSample,
                              int          nSampSize[],
                              double       dXval[]);


    public static native int LSsampGetCIPoints
                             (Object       pSample,
                              int          nSampSize[],
                              double       dXval[]);


    public static native int LSsampInduceCorrelation
                             (Object       aSample[],
                              int          nDim,
                              int          nCorrType,
                              int          nQCnonzeros,
                              int          aiQCvarndx1[],
                              int          aiQCvarndx2[],
                              double       adQCcoef[]);


    public static native int LSsampGetCorrelationMatrix
                             (Object       aSample[],
                              int          nDim,
                              int          iFlag,
                              int          nCorrType,
                              int          nQCnonzeros[],
                              int          aiQCvarndx1[],
                              int          aiQCvarndx2[],
                              double       adQCcoef[]);


    public static native int LSsampLoadDiscretePdfTable
                             (Object       pSample,
                              int          nLen,
                              double       adProb[],
                              double       adVals[]);


    public static native int LSsampGetDiscretePdfTable
                             (Object       pSample,
                              int          nLen[],
                              double       adProb[],
                              double       adVals[]);


    public static native int LSsampGetInfo
                             (Object       pSample,
                              int          nQuery,
                              int          nResult[]);

    public static native int LSsampGetInfo
                             (Object       pSample,
                              int          nQuery,
                              double       nResult[]);

    public static native int LSsampAddUserFuncArg
                             (Object       pSample,
                              Object       pSampleSrc);
    public static native int LSregress
                             (int          nNdim,
                              int          nPdim,
                              double       adU[],
                              double       adX[],
                              double       adB[],
                              double       dB0[],
                              double       adR[],
                              double       adstats[]);

/*********************************************************************
 **
 **    Random Number Generation Interface
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
/* Public functions */
    public static native Object LScreateRG
                             (Object       pEnv,
                              int          nMethod);

    public static native Object LScreateRGMT
                             (Object       pEnv,
                              int          nMethod);

    public static native double LSgetDoubleRV
                             (Object       pRG);


    public static native int LSgetInt32RV
                             (Object       pRG,
                              int          iLow,
                              int          iHigh);


    public static native void LSsetRGSeed
                             (Object       pRG,
                              int          nSeed);


    public static native void LSdisposeRG
                             (Object       pRG);


    public static native void LSdisposeRG
                             (Object       pRG[]);

    public static native int LSsetDistrParamRG
                             (Object       pRG,
                              int          iParam,
                              double       dParam);


    public static native int LSsetDistrRG
                             (Object       pRG,
                              int          nDistType);


    public static native int LSgetDistrRV
                             (Object       pRG,
                              double       dResult[]);

    public static native int LSgetDistrRV
                             (Object       pRG,
                              int          iResult[]);

    public static native int LSgetInitSeed
                             (Object       pRG);


    public static native int LSgetRGNumThreads
                             (Object       pRG,
                              int          nThreads[]);


    public static native int LSfillRGBuffer
                             (Object       pRG);


    public static native int LSgetRGBufferPtr
                             (Object       pRG,
                              int          nBufferLen[]);
/* Auxiliary functions */

    public static native int LSgetHistogram
                             (Object       pModel,
                              int          nSampSize,
                              double       adVals[],
                              double       adWeights[],
                              double       dHistLow,
                              double       dHistHigh,
                              int          nBins[],
                              int          anBinCounts[],
                              double       adBinProbs[],
                              double       adBinLow[],
                              double       adBinHigh[],
                              double       adBinLeftEdge[],
                              double       adBinRightEdge[]);


    public static native int LSsampGetCorrDiff
                             (Object       pModel,
                              Object       aSample[],
                              int          nDim,
                              int          nDiffType,
                              double       dNorm1[],
                              double       dNorm2[],
                              double       dVecNormInf[]);


    public static native int LSgetNnzData
                             (Object       pModel,
                              int          mStat,
                              int          anOutput[]);


/*********************************************************************
 **
 **    Sprint Interface
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
/* Public functions */

    public static native int LSsolveFileLP
                             (Object       pModel,
                              String       szFileNameMPS,
                              String       szFileNameSol,
                              int          nNoOfColsEvaluatedPerSet,
                              int          nNoOfColsSelectedPerSet,
                              int          nTimeLimitSec,
                              int          nSolStatusParam[],
                              int          nNoOfConsMps[],
                              long         pnNoOfColsMps[],
                              long         pnErrorLine[]);


    public static native int LSreadSolutionFileLP
                             (String       szFileNameSol,
                              int          nFileFormat,
                              long         nBeginIndexPrimalSol,
                              long         nEndIndexPrimalSol,
                              int          nSolStatus[],
                              double       dObjValue[],
                              int          nNoOfCons[],
                              long         plNoOfCols[],
                              int          nNoOfColsEvaluated[],
                              int          nNoOfIterations[],
                              double       dTimeTakenInSeconds[],
                              double       adPrimalValues[],
                              double       adDualValues[]);


/*********************************************************************
 **
 **    Date/Time Functions
 **
 **    LINDO API Version 13.0
 **    Copyright (c) 2006-2020
 **
 **    LINDO Systems, Inc.            312.988.7422
 **    1415 North Dayton St.          info@lindo.com
 **    Chicago, IL 60622              http://www.lindo.com
 **
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/
/* Documented Public functions */
    public static native int LSdateDiffSecs
                             (int          nYr1,
                              int          nMon1,
                              int          nDay1,
                              int          nHr1,
                              int          nMin1,
                              double       dSec1,
                              int          nYr2,
                              int          nMon2,
                              int          nDay2,
                              int          nHr2,
                              int          nMin2,
                              double       dSec2,
                              double       dSecdiff[]);


    public static native int LSdateYmdhms
                             (double       dSecdiff,
                              int          nYr1,
                              int          nMon1,
                              int          nDay1,
                              int          nHr1,
                              int          nMin1,
                              double       dSec1,
                              int          nYr2[],
                              int          nMon2[],
                              int          nDay2[],
                              int          nHr2[],
                              int          nMin2[],
                              double       dSec2[],
                              int          nDow[]);


    public static native int LSdateToday
                             (int          nYr1[],
                              int          nMon1[],
                              int          nDay1[],
                              int          nHr1[],
                              int          nMin1[],
                              double       dSec1[],
                              int          nDow[]);


/* Undocumented Public functions */
    public static native int LSdateMakeDate
                             (int          nYYYY);

    public static native int LSdateMakeTime
                             (int          nHH);

    public static native void LSdateSetBaseDate
                             (int          nYYYY);

    public static native int LSdateScalarSec
                             (int          nDate);

    public static native void LSdateScalarSecInverse
                             (double       dSSEC[]);

    public static native int LSdateScalarHour
                             (int          nDate);

    public static native void LSdateScalarHourInverse
                             (double       dSHOUR[]);

    public static native int LSdateJulianSec
                             (int          nDate);

    public static native void LSdateJulianSecInverse
                             (double       dJSEC[]);

    public static native int LSdateJulianHour
                             (int          nDate);

    public static native void LSdateJulianHourInverse
                             (double       dJHOUR[]);

    public static native void LSdateDiff
                             (int          nDate1[]);

    public static native void LSdateNow
                             (int          nDate[]);

    public static native int LSdateIsLeapYear
                             (int          nYear);

    public static native int LSdateJulianDay
                             (int          nDate);

    public static native int LSdateDayOfWeek
                             (int          nDate);

    public static native int LSdateWeekOfYear
                             (int          nDate);

    public static native int LSdateQuarterOfYear
                             (int          nDate);

    public static native int LSdateDayOfYear
                             (int          nDate);

    public static native int LSdateNextWeekDay
                             (int          nDate);

    public static native int LSdatePrevWeekDay
                             (int          nDate);

    public static native int LSdateNextMonth
                             (int          nDate);

    public static native int LSdateDateToDays
                             (int          nDate);

    public static native int LSdateDaysToDate
                             (int          nDays);

    public static native int LSdateTimeToSecs
                             (double       dTime);

    public static native int LSdateSecsToTime
                             (double       dSecs);

    public static native void LSdateFutureDate
                             (int          nDate[]);

    public static native void LSdatePastDate
                             (int          nDate[]);

    public static native int LSdateIsValidDate
                             (int          nDate);

    public static native int LSdateIsValidTime
                             (double       dTime);

    public static native int LSdateIsDateFuture
                             (int          nDate);

    public static native int LSdateIsDatePast
                             (int          nDate);

    public static native int LSdateYear
                             (int          nDate);

    public static native int LSdateMonth
                             (int          nDate);

    public static native int LSdateDay
                             (int          nDate);

    public static native int LSdateHour
                             (double       dTime);

    public static native int LSdateMinute
                             (double       dTime);

    public static native int LSdateSecond
                             (double       dTime);

    public static native int LSdateWeekOfMonth
                             (int          nDate);

    public static native int LSdateLocalTimeStamp
                             (String       szTimeBuffer);

    public static native int LSdateDateNum
                             (int          nDate);

    public static native int LSdateMakeDateNum
                             (int          nYYYY);


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
 **    $Id: Lindo.java 2908 2020-01-15 17:51:57Z mka $
 **
 *********************************************************************/

    public static native int LSrunTuner
                             (Object       pEnv);

    public static native int LSrunTunerFile
                             (Object       pEnv,
                              String       szJsonFile);

    public static native int LSrunTunerString
                             (Object       pEnv,
                              String       szJsonString);

    public static native int LSloadTunerConfigString
                             (Object       pEnv,
                              String       szJsonString);

    public static native int LSloadTunerConfigFile
                             (Object       pEnv,
                              String       szJsonFile);

    public static native int LSclearTuner
                             (Object       pEnv);

    public static native int LSresetTuner
                             (Object       pEnv);

    public static native int LSprintTuner
                             (Object       pEnv);

    public static native int LSsetTunerOption
                             (Object       pEnv,
                              String       szKey,
                              double       dval);

    public static native int LSgetTunerOption
                             (Object       pEnv,
                              String       szkey,
                              double       dval[]);

    public static native int LSgetTunerResult
                             (Object       pEnv,
                              String       szkey,
                              int          jInstance,
                              int          kConfig,
                              double       dval[]);

    public static native int LSgetTunerSpace
                             (Object       pEnv,
                              int          anParamId[],
                              int          numParam[]);

    public static native int LSwriteTunerConfigString
                             (Object       pEnv,
                              String       szJsonString,
                              String       szJsonFile);

    public static native int LSgetTunerConfigString
                             (Object       pEnv,
                              String       szJsonString);

    public static native int LSwriteTunerParameters
                             (Object       pEnv,
                              String       szFile,
                              int          jInstance,
                              int          mCriterion);

    public static native int LSaddTunerInstance
                             (Object       pEnv,
                              String       szFile);

    public static native int LSaddTunerZStatic
                             (Object       pEnv,
                              int          jGroupId,
                              int          iParam,
                              double       dValue);

    public static native int LSaddTunerZDynamic
                             (Object       pEnv,
                              int          iParam);

    public static native int LSaddTunerOption
                             (Object       pEnv,
                              String       szKey,
                              double       dValue);

    public static native int LSdisplayTunerResults
                             (Object       pEnv);


/**********************************************************************
 * Deprecated functions from version 1.x                              *
 **********************************************************************/

 /* Deprecated,  use LSgetInfo() */
    public static native int LSgetLicenseInfo
                             (Object       pModel,
                              int          nMaxcons[],
                              int          nMaxvars[],
                              int          nMaxintvars[],
                              int          nReserved1[],
                              int          nDaystoexp[],
                              int          nDaystotrialexp[],
                              int          nNlpAllowed[],
                              int          nUsers[],
                              int          nBarAllowed[],
                              int          nRuntime[],
                              int          nEdulicense[],
                              StringBuffer achText);


 /* Deprecated,  use LSgetInfo() */
    public static native int LSgetDimensions
                             (Object       pModel,
                              int          nVars[],
                              int          nCons[],
                              int          nCones[],
                              int          nAnnz[],
                              int          nQCnnz[],
                              int          nConennz[],
                              int          nNLPnnz[],
                              int          nNLPobjnnz[],
                              int          nVarNamelen[],
                              int          nConNamelen[],
                              int          nConeNamelen[]);


 /* Deprecated, use LSsolveMIP() */
    public static native int LSbnbSolve
                             (Object       pModel,
                              String       szFname);


 /* Deprecated,  use LSgetInfo() */
    public static native int LSgetDualMIPsolution
                             (Object       pModel,
                              double       adPrimal[],
                              double       adDual[],
                              double       adRedcosts[],
                              int          anCstatus[],
                              int          anRstatus[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetMIPSolutionStatus
                             (Object       pModel,
                              int          nStatus[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetSolutionStatus
                             (Object       pModel,
                              int          nStatus[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetObjective
                             (Object       pModel,
                              double       dObjval[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetSolutionInfo
                             (Object       pModel,
                              int          nMethod[],
                              int          nElapsed[],
                              int          nSpxiter[],
                              int          nBariter[],
                              int          nNlpiter[],
                              int          nPrimStatus[],
                              int          nDualStatus[],
                              int          nBasStatus[],
                              double       dPobjval[],
                              double       dDobjval[],
                              double       dPinfeas[],
                              double       dDinfeas[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetMIPSolution
                             (Object       pModel,
                              double       dPobjval[],
                              double       adPrimal[]);


/* Deprecated,  use LSgetInfo() */
    public static native int LSgetCurrentMIPSolutionInfo
                             (Object       pModel,
                              int          nMIPstatus[],
                              double       dMIPobjval[],
                              double       dBestbound[],
                              double       dSpxiter[],
                              double       dBariter[],
                              double       dNlpiter[],
                              int          nLPcnt[],
                              int          nBranchcnt[],
                              int          nActivecnt[],
                              int          nCons_prep[],
                              int          nVars_prep[],
                              int          nAnnz_prep[],
                              int          nInt_prep[],
                              int          nCut_contra[],
                              int          nCut_obj[],
                              int          nCut_gub[],
                              int          nCut_lift[],
                              int          nCut_flow[],
                              int          nCut_gomory[],
                              int          nCut_gcd[],
                              int          nCut_clique[],
                              int          nCut_disagg[],
                              int          nCut_planloc[],
                              int          nCut_latice[],
                              int          nCut_coef[]);


/* Command Line Parser */
    public static native int LSgetCLOpt
                             (Object       pEnv,
                              int          nArgc,
                              String       szArgv[],
                              String       szOpt);


    public static native int LSgetCLOptArg
                             (Object       pEnv,
                              String       szOptArg[]);


    public static native int LSgetCLOptInd
                             (Object       pEnv,
                              int          nOptInd[]);


    public static native int LSsolveExternally
                             (Object       pModel,
                              int          mSolver,
                              int          nMethod,
                              int          nFileFormat,
                              int          nStatus[]);


    public static native int LSgetMasterModel
                             (Object       pModel);



} /* End of declaration */

