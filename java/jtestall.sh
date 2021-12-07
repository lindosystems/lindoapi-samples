#!/bin/sh
###########################################################################
#
# Tests the JNI (Java Native Interface) for LINDO API
# Copyright (c) 2002-2010, Lindo Systems Inc.
#
# $Id: jtestall.sh 29 2010-05-12 17:55:26Z svn $
###########################################################################

sh jtest.sh ex_addrows ../data/testlp.mps
sh jtest.sh ex_dist
sh jtest.sh ex_iostream ../data/bm23.ltx
sh jtest.sh ex_lp1
sh jtest.sh ex_modify
sh jtest.sh ex_mps ../data/bm23.mps
sh jtest.sh ex_mt1
sh jtest.sh ex_nlp1
sh jtest.sh ex_nlp3
sh jtest.sh ex_sp_newsboy
sh jtest.sh ex_user