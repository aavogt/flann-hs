#include "flann/flann.h"
typedef struct FLANNParameters FLANNParametersStruct;
/* typedef struct FLANNParameters (*FLANNParametersPtr);  not needed */

/* these are the same as versions without the 2, except these also free the second pointer */
int flann_free_index_int2(flann_index_t index_id, struct FLANNParameters* flann_params);
int flann_free_index_float2(flann_index_t index_id, struct FLANNParameters* flann_params);
int flann_free_index_double2(flann_index_t index_id, struct FLANNParameters* flann_params);
int flann_free_index_byte2(flann_index_t index_id, struct FLANNParameters* flann_params);


