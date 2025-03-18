* -----------------------------------------------------------------
*                    SOLUTION REPORT                               
*                    20250318:211156.901                                    
* LINDO API Version 15.0.6099.225 built on Mar 15 2025 06:32:58
* Barrier Solver Version 0.0.000, Nonlinear Solver Version 3.17L
* Platform Windows 64x86 (B)
*
* Copyright (c) 2024 by LINDO Systems, Inc. Licensed material,
* all rights reserved. Copying except as authorized in license 
* agreement is prohibited.
* -----------------------------------------------------------------
* 
*  PROBLEM NAME     posd.ltx
* 
*   MICONIC GLOBAL OPTIMUM FOUND
* 
*   ITERATIONS BY SIMPLEX METHOD    =           0
*   ITERATIONS BY BARRIER METHOD    =         265
*   ITERATIONS BY NLP METHOD        =           0
*   TOTAL BRANCHES CREATED          =           5
*   TOTAL NUMBER OF LPs SOLVED      =          25
*   NUMBER OF CONTRA CUTS           =           0
*   NUMBER OF OBJECTIVE CUTS        =           1
*   NUMBER OF GUB CUTS              =           0
*   NUMBER OF LIFTING CUTS          =           0
*   NUMBER OF FLOW COVER CUTS       =           0
*   NUMBER OF GOMORY CUTS           =           0
*   NUMBER OF GCD CUTS              =           0
*   NUMBER OF CLIQUE CUTS           =           0
*   NUMBER OF DISAGGREGATION CUTS   =           0
*   NUMBER OF PLANT LOCATION CUTS   =           0
*   NUMBER OF LATTICE CUTS          =           0
*   NUMBER OF COEFF. REDUCTION CUTS =           0
*   TOTAL NUMBER OF CUTS GENERATED  =           1
* 
*   TIME ELAPSED (s)                =           0
* 
*   OBJECTIVE FUNCTION VALUE
* 
*    1)                29.999999938
* 
*                              XMATRIX                  ZMATRIX     MATRIX    MATRIX    MATRIX
* VARIABLES                     VALUE             REDUCED COST      BLOCK       ROW    COLUMN

  X11                      3.999999959              0.000000000          0         0         0
  X12                      0.000000000              0.000000000          0         1         0
  X22                      5.999999989              0.000000000          0         1         1
  X33                      2.000000000              0.000000000          1         0         0
  X34                     -2.000000000              0.000000000          1         1         0
  X44                      2.000000000              0.000000000          1         1         1

* CONSTRAINTS        SLACK OR SURPLUS              DUAL PRICES

  R0000000                 0.000000052              0.000000000
  R0000001                 0.000000011              0.000000000

* XMATRIX    I     J        	  PRIMAL                   DUAL

       0     0     0            4.000000000           -0.000000000
       0     1     0            0.000000000           -0.000000000
       0     1     1            6.000000000            0.000000000

       1     0     0            2.000000000           -1.541420922
       1     1     0           -2.000000000           -1.541420922
       1     1     1            2.000000000           -1.541420922


* END OF REPORT
