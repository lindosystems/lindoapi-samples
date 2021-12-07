@echo off

rem ###########################################################################
rem #
rem # Tests the JNI (Java Native Interface) for LINDO API
rem # Copyright (c) 2002-2010, Lindo Systems Inc.
rem #
rem # $Id: jtestall.bat 121 2010-12-29 20:00:00Z svn $
rem ###########################################################################

@call jtest.bat ex_addrows ..\data\testlp.mps
@call jtest.bat ex_dist
@call jtest.bat ex_iostream ..\data\bm23.ltx
@call jtest.bat ex_lp1
@call jtest.bat ex_modify
@call jtest.bat ex_mps ..\data\bm23.mps
@call jtest.bat ex_mt1
@call jtest.bat ex_nlp1
@call jtest.bat ex_nlp3
@call jtest.bat ex_sp_newsboy
@call jtest.bat ex_sp_smps
@call jtest.bat ex_user
@call jtest.bat ex_gop ..\data\testgop.mpi 2


