// File containing OpenCL kernels for Focal test suite

__kernel void setInt32Test(int N, global int * deviceInt){

  int i = get_global_id(0);
  if (i < N) {
    deviceInt[i] = i;
  }

}

__kernel void setFloatTest(int N, global float * deviceFloat){

  int i = get_global_id(0);
  if (i < N) {
    deviceFloat[i] = 1.0*i;
  }

}

__kernel void setDoubleTest(int N, global double * deviceDouble){

  int i = get_global_id(0);
  if (i < N) {
    deviceDouble[i] = 1.0*i;
  }

}
