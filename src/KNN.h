#ifndef _KNN_H
#define _KNN_H

#include <R.h>

#define MAX_TIES 100
#define EPS 1e-4		/* relative test of equality of distances */

#define RANDIN  GetRNGstate()
#define RANDOUT PutRNGstate()
#define UNIF unif_rand()

void knnc(const int *kin, 		//k
//			 const int *lin, 		//l
			 const int *pntr, 		//number of training points
			 const int *pnte, 		//number of testing points
			 const int *p,    		//dimension
			 const double *train, 	//training set
			 const int *cl,  		//class label
			 const double *test,    //testing set
			 int *res, 				//prediction
			 double *pr, 			//posterior probability
			 int *votes,			//votes for a sample
			 const int *nc, 		//number of classes
			 const int *cv, 		//boolean
			 const int *use_all, 	//boolean
       		 int   *nn_idx,         //indice of neighbors
       		 double *nn_dist        //distances of neighbors
			 );
			 
void knnr(const int *kin, 		//k
			 const int *pntr, 		//number of training points
			 const int *pnte, 		//number of testing points
			 const int *p,    		//dimension
			 const double *train, 	//training set
			 const double *Y,  		//response
			 const double *test,    //testing set
			 double *res, 			//prediction
			 const int *cv, 		//boolean
			 const int *use_all, 	//boolean
 		   int   *nn_idx,         //indice of neighbors
       double *nn_dist        //distances of neighbors
			 );
#endif
