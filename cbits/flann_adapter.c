#include "flann_adapter.h"
#include <stdlib.h>

int flann_free_index_float2(flann_index_t index_id,
                        struct FLANNParameters* flann_params){
    int returncode;
    returncode=flann_free_index_float(index_id, flann_params);
    free(flann_params);
    return returncode;
};

int flann_free_index_double2(flann_index_t index_id,
                        struct FLANNParameters* flann_params){
    int returncode;
    returncode=flann_free_index_double(index_id, flann_params);
    free(flann_params);
    return returncode;
};

int flann_free_index_byte2(flann_index_t index_id,
                        struct FLANNParameters* flann_params){
    int returncode;
    returncode=flann_free_index_byte(index_id, flann_params);
    free(flann_params);
    return returncode;
};

int flann_free_index_int2(flann_index_t index_id,
                        struct FLANNParameters* flann_params){
    int returncode;
    returncode=flann_free_index_int(index_id, flann_params);
    free(flann_params);
    return returncode;
};
