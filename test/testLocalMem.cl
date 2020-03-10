// Test kernels for demonstrating local memory
//
// These kernels calculate partial sums of squares
//  sum of first blocksize elements stored at index 0
//  sum of next blocksize elements stored at index 1 etc.

__kernel void sumSqInt32Test(int N, int M, global int * deviceInt, local int * localInt,
                             global int *blockResults){

  int i = get_global_id(0);
  int j = get_local_id(0);

  localInt[j] = deviceInt[i]*deviceInt[i];
  barrier(CLK_LOCAL_MEM_FENCE);

  if (j==0){

    int temp = 0;
    for (int k=0; k<M; k++){
      temp += localInt[k];
    }

    blockResults[(i/M)] = temp;
  }

}


__kernel void sumSqFloatTest(int N, int M, global float * deviceFloat, local float * localFloat,
                               global float *blockResults){

  int i = get_global_id(0);
  int j = get_local_id(0);

  localFloat[j] = deviceFloat[i]*deviceFloat[i];
  barrier(CLK_LOCAL_MEM_FENCE);

  if (j==0){

    float temp = 0;
    for (int k=0; k<M; k++){
      temp += localFloat[k];
    }

    blockResults[(i/M)] = temp;
  }

}


__kernel void sumSqDoubleTest(int N, int M, global double * deviceDouble, local double * localDouble, 
                               global double *blockResults){

  int i = get_global_id(0);
  int j = get_local_id(0);

  localDouble[j] = deviceDouble[i]*deviceDouble[i];
  barrier(CLK_LOCAL_MEM_FENCE);

  if (j==0){

    double temp = 0;
    for (int k=0; k<M; k++){
      temp += localDouble[k];
    }

    blockResults[(i/M)] = temp;
  }

}
