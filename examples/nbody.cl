#define SOFT 1e-9f

__kernel void bodyForces(const int nBody, const float dt, 
            __global float4 * px, __global float4 * py, __global float4 * pz,
            __global float4 * vx, __global float4 * vy, __global float4 * vz){
 
  int i = get_global_id(0);
  if (i < nBody) {
      
    float4 Fx = 0.0f;
    float4 Fy = 0.0f;
    float4 Fz = 0.0f;

    for (int j=0; j<nBody; j++){

      float4 dx = px[j] - px[i];
      float4 dy = py[j] - py[i];
      float4 dz = pz[j] - pz[i];

      float4 distSqr = dx*dx + dy*dy + dz*dz + SOFT;
      float4 invDist = rsqrt(distSqr);
      float4 invDist3 = invDist * invDist * invDist;

      Fx += dx * invDist3;
      Fy += dy * invDist3;
      Fz += dz * invDist3;

    }
      
    vx[i] += dt*Fx;
    vy[i] += dt*Fy;
    vz[i] += dt*Fz;

  }

}

__kernel void integrateBodies(const int nBody, const float dt, 
            __global float4 * px, __global float4 * py, __global float4 * pz,
            __global float4 * vx, __global float4 * vy, __global float4 * vz){

  int i = get_global_id(0);
  if (i < nBody) {

    px[i] += dt*vx[i];
    py[i] += dt*vy[i];
    pz[i] += dt*vz[i];

  }

}
