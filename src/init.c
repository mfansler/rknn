/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "KNN.h"

static const R_CMethodDef cMethods[] = {
    {"knnc", (DL_FUNC) &knnc, 15}, 
    {"knnr", (DL_FUNC) &knnr, 12}, 
    {NULL, NULL, 0}  
};

void  R_init_rknn(DllInfo *info)
{  
    R_registerRoutines(info, cMethods, NULL, NULL, NULL);  
    R_useDynamicSymbols(info, TRUE);
}
