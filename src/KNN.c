
/******************************************************************************\
* File: KNN.c                                                                *
\******************************************************************************/ 
#include "KNN.h"
  
//classification
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
			 )
{
    const int l = 0;
    int  j1, j2, needed, t, i, index, j, k, k1, kinit = *kin, kn,  mm, npat, ntie,  ntr = *pntr, nte = *pnte, extras;
    double dist, tmp;
    
    int  *pos = (int *) R_alloc(MAX_TIES, sizeof(int)), *ncl = (int *) R_alloc(MAX_TIES, sizeof(int));
    
    double  *nndist = (double *) R_alloc(MAX_TIES, sizeof(double));
  
    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
 */

    for (npat = 0; npat < nte; npat++) {
		kn = kinit;
		for (k = 0; k < kn; k++) nndist[k] = 0.99 * DBL_MAX;
		for (j = 0; j < ntr; j++) {
	    	if ((*cv > 0) && (j == npat))	continue;
	    	dist = 0.0;
	    	for (k = 0; k < *p; k++) {
				tmp = test[npat + k * nte] - train[j + k * ntr];
				dist += tmp * tmp;
	    	}
			/* Use 'fuzz' since distance computed could depend on order of coordinates */
	    	if (dist <= nndist[kinit - 1] * (1 + EPS)){
				for (k = 0; k <= kn; k++){
		    		if (dist < nndist[k]) {
						for (k1 = kn; k1 > k; k1--) {
				    		nndist[k1] = nndist[k1 - 1];
				    		pos[k1] = pos[k1 - 1];
						}
						nndist[k] = dist;
			
						pos[k] = j;
					/* Keep an extra distance if the largest current one ties with current kth */
						if (nndist[kn] <= nndist[kinit - 1])
				    		if (++kn == MAX_TIES - 1)	error("too many ties in knn");
						break;
		    		}
	    	 	}
	    	}
	    	nndist[kn] = 0.99 * DBL_MAX;
		}

		for (k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
			nn_dist[k*nte+npat]= sqrt(nndist[k]);			
			nn_idx[k*nte+npat]=pos[k]+1;	
		}									/*Done for return distances and indice		*/		 
	
	
		for (j = 0; j <= *nc; j++) 	votes[j] = 0;
		if (*use_all) {
	    	for (j = 0; j < kinit; j++)	votes[cl[pos[j]]]++;
	    	extras = 0;
		    for (j = kinit; j < kn; j++) {
				if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
				extras++;
				votes[cl[pos[j]]]++;
	    	}
		} //end of "use all"
		else { /* break ties at random */
	    	extras = 0;
	    	for (j = 0; j < kinit; j++) {
				if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
		    		break;
				votes[cl[pos[j]]]++;
	    	}
	   		j1 = j;
	    	if (j1 == kinit - 1) votes[cl[pos[j1]]]++; /* no ties for largest */				
	    	else {
				/* Use reservoir sampling to choose amongst the tied distances */
				j1 = j;
				needed = kinit - j1;
				for (j = 0; j < needed; j++) ncl[j] = cl[pos[j1 + j]];
				t = needed;
				for (j = j1 + needed; j < kn; j++) {
		    		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
		    		if (++t * UNIF < needed) {
						j2 = j1 + (int) (UNIF * needed);
						ncl[j2] = cl[pos[j]];
		    		}
				}
				for (j = 0; j < needed; j++) votes[ncl[j]]++;
	    	}
		}//end not "not use all"

		/* Use reservoir sampling to choose amongst the tied votes */
		ntie = 1;
		if (l > 0) 	mm = l - 1 + extras;
		else  	mm = 0;
		index = 0;
		for (i = 1; i <= *nc; i++){
	    	if (votes[i] > mm) {
				ntie = 1;
				index = i;
				mm = votes[i];
	    	} else if (votes[i] == mm && votes[i] >= l) {
				if (++ntie * UNIF < 1.0) index = i;
	    	}
    	}
		res[npat] = index;
		pr[npat] = (double) mm / (kinit + extras);
    }
    RANDOUT;
}

//regression
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
			 )
{
    int j1, j2, needed, t, j, k, k1, kinit = *kin, kn, npat, ntr = *pntr, nte = *pnte, extras;
    double dist, tmp;
    	  
    int   *pos = (int *) R_alloc(MAX_TIES, sizeof(int));
        
    double *nY = (double *) R_alloc(MAX_TIES, sizeof(double)), *nndist = (double *) R_alloc(MAX_TIES, sizeof(double));
  
    RANDIN;
/*
    Use a 'fence' in the (k+1)st position to avoid special cases.
    Simple insertion sort will suffice since k will be small.
*/

    for (npat = 0; npat < nte; npat++) {
  		kn = kinit;
  		for (k = 0; k < kn; k++) nndist[k] = 0.99 * DBL_MAX;
  		for (j = 0; j < ntr; j++) {
  	    	if ((*cv > 0) && (j == npat))	continue;
  	    	dist = 0.0;
  	    	for (k = 0; k < *p; k++) {
    				tmp = test[npat + k * nte] - train[j + k * ntr];
    				dist += tmp * tmp;
  	    	}
  			/* Use 'fuzz' since distance computed could depend on order of coordinates */
  	    	if (dist <= nndist[kinit - 1] * (1 + EPS)){
    				for (k = 0; k <= kn; k++){
    		    		if (dist < nndist[k]) {
      						for (k1 = kn; k1 > k; k1--) {
      				    		nndist[k1] = nndist[k1 - 1];
      				    		pos[k1] = pos[k1 - 1];
      						}
      						nndist[k] = dist;
      			
      						pos[k] = j;
      					/* Keep an extra distance if the largest current one ties with current kth */
      						if (nndist[kn] <= nndist[kinit - 1])
      				    		if (++kn == MAX_TIES - 1)	error("too many ties in knn");
      						break;
    		    		}
    	    	 	}
    	    	}
  	    	nndist[kn] = 0.99 * DBL_MAX;
  		}
  		for (k = 0; k < kinit; k++){		/*return distances and indice - Shengqiao Li*/
  			nn_dist[k*nte+npat]= sqrt(nndist[k]);			
  			nn_idx[k*nte+npat]=pos[k]+1;	
  		}									/*Done for return distances and indice		*/		 
  
  		res[npat] = 0;
  		if (*use_all) {
  	    	for (j = 0; j < kinit; j++)	res[npat] += Y[pos[j]];
  	    	extras = 0;
  		    for (j = kinit; j < kn; j++) {
    				if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
    				extras++;
    				res[npat] += Y[pos[j]];
  	    	}
  	    	res[npat] /= kinit + extras;
  		} //end of "use all"
  		else { /* break ties at random */
  	    	extras = 0;
  	    	for (j = 0; j < kinit; j++) {
  				if (nndist[j] >= nndist[kinit - 1] * (1 - EPS))
  		    		break;
  				res[npat] += Y[pos[j]];
  	    	}
  	   		j1 = j; 
  	    	if (j1 == kinit - 1) res[npat] += Y[pos[j1]]; /* no ties for largest */				
  	    	else {
  				/* Use reservoir sampling to choose amongst the tied distances */
  				j1 = j;
  				needed = kinit - j1;
  				for (j = 0; j < needed; j++) nY[j] = Y[pos[j1 + j]];
  				t = needed;
  				for (j = j1 + needed; j < kn; j++) {
  		    		if (nndist[j] > nndist[kinit - 1] * (1 + EPS))	break;
  		    		if (++t * UNIF < needed) {
  						j2 = j1 + (int) (UNIF * needed);
  						nY[j2] = Y[pos[j]];
  		    		}
  				}
  				for (j = 0; j < needed; j++) res[npat] += nY[j];
  	    	}
     			res[npat] /= kinit;
  		}//end not "not use all"
    }
    RANDOUT;
}

