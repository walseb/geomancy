#if defined(__i386__) || defined(__x86_64__)

#include <xmmintrin.h>

void Mat4xMat4_SIMD(float *A, float *B, float *O) {
  __m128 row0 = _mm_load_ps(&A[0]);
  __m128 row1 = _mm_load_ps(&A[4]);
  __m128 row2 = _mm_load_ps(&A[8]);
  __m128 row3 = _mm_load_ps(&A[12]);
  for(int i=0; i<4; i++) {
    __m128 brod0 = _mm_set1_ps(B[4*i + 0]);
    __m128 brod1 = _mm_set1_ps(B[4*i + 1]);
    __m128 brod2 = _mm_set1_ps(B[4*i + 2]);
    __m128 brod3 = _mm_set1_ps(B[4*i + 3]);
    __m128 row = _mm_add_ps(
      _mm_add_ps(
        _mm_mul_ps(brod0, row0),
        _mm_mul_ps(brod1, row1)),
      _mm_add_ps(
        _mm_mul_ps(brod2, row2),
        _mm_mul_ps(brod3, row3)));
    _mm_store_ps(&O[4*i], row);
  }
}

void Mat4xVec4_SIMD(float *M, float *V, float *O) {
  __m128 row0 = _mm_load_ps(&M[0]);
  __m128 row1 = _mm_load_ps(&M[4]);
  __m128 row2 = _mm_load_ps(&M[8]);
  __m128 row3 = _mm_load_ps(&M[12]);

  __m128 vec0 = _mm_load_ps(V);

  __m128 vec00 = _mm_shuffle_ps(vec0, vec0, _MM_SHUFFLE(0, 0, 0, 0));
  __m128 vec01 = _mm_shuffle_ps(vec0, vec0, _MM_SHUFFLE(1, 1, 1, 1));
  __m128 vec02 = _mm_shuffle_ps(vec0, vec0, _MM_SHUFFLE(2, 2, 2, 2));
  __m128 vec03 = _mm_shuffle_ps(vec0, vec0, _MM_SHUFFLE(3, 3, 3, 3));

  __m128 res = _mm_mul_ps(row0, vec00);
  res = _mm_add_ps(res, _mm_mul_ps(row1, vec01));
  res = _mm_add_ps(res, _mm_mul_ps(row2, vec02));
  res = _mm_add_ps(res, _mm_mul_ps(row3, vec03));

  _mm_storeu_ps(O, res);
}

#elif defined(__arm__) || defined(__aarch64__)

#include <arm_neon.h>
// Assumes 32bit floats

void Mat4xMat4_SIMD(float *A, float *B, float *O) {

    // Based on the column major ARM example:
    // https://developer.arm.com/documentation/102467/0100/Example---matrix-multiplication
    //
    // NEON intrinsics mapping:
    // https://arm-software.github.io/acle/neon_intrinsics/advsimd.html

    // vfmaq_laneq_f32 -> FMLA
    // Floating-point fused Multiply-Add to accumulator (by element). This
    // instruction multiplies the vector elements in the first source SIMD and
    // FP register by the specified value in the second source SIMD and FP
    // register, and accumulates the results in the vector elements of the
    // destination SIMD and FP register. All the values in this instruction are
    // floating-point values.

    // Contrary to the SSE function above we unroll the loop manually

    // these are the rows of A
    float32x4_t A0 = vld1q_f32(A);
    float32x4_t A1 = vld1q_f32(&A[4]);
    float32x4_t A2 = vld1q_f32(&A[8]);
    float32x4_t A3 = vld1q_f32(&A[12]);

    // these are the rows B
    float32x4_t B0 = vld1q_f32(B);
    float32x4_t B1 = vld1q_f32(&B[4]);
    float32x4_t B2 = vld1q_f32(&B[8]);
    float32x4_t B3 = vld1q_f32(&B[12]);

    // Zero accumulators for output rows
    float32x4_t C0 = vmovq_n_f32(0);
    float32x4_t C1 = vmovq_n_f32(0);
    float32x4_t C2 = vmovq_n_f32(0);
    float32x4_t C3 = vmovq_n_f32(0);

    // Multiply accumulate in 4x1 blocks to output row
    C0 = vfmaq_laneq_f32(C0, A0, B0, 0);
    C0 = vfmaq_laneq_f32(C0, A1, B0, 1);
    C0 = vfmaq_laneq_f32(C0, A2, B0, 2);
    C0 = vfmaq_laneq_f32(C0, A3, B0, 3);
    vst1q_f32(O, C0);

    C1 = vfmaq_laneq_f32(C1, A0, B1, 0);
    C1 = vfmaq_laneq_f32(C1, A1, B1, 1);
    C1 = vfmaq_laneq_f32(C1, A2, B1, 2);
    C1 = vfmaq_laneq_f32(C1, A3, B1, 3);
    vst1q_f32(&O[4], C1);

    C2 = vfmaq_laneq_f32(C2, A0, B2, 0);
    C2 = vfmaq_laneq_f32(C2, A1, B2, 1);
    C2 = vfmaq_laneq_f32(C2, A2, B2, 2);
    C2 = vfmaq_laneq_f32(C2, A3, B2, 3);
    vst1q_f32(&O[8], C2);

    C3 = vfmaq_laneq_f32(C3, A0, B3, 0);
    C3 = vfmaq_laneq_f32(C3, A1, B3, 1);
    C3 = vfmaq_laneq_f32(C3, A2, B3, 2);
    C3 = vfmaq_laneq_f32(C3, A3, B3, 3);
    vst1q_f32(&O[12], C3);

}

void Mat4xVec4_SIMD(float *M, float *V, float *O) {

    // Rows
    float32x4_t M0 = vld1q_f32(M);
    float32x4_t M1 = vld1q_f32(&M[4]);
    float32x4_t M2 = vld1q_f32(&M[8]);
    float32x4_t M3 = vld1q_f32(&M[12]);

    // Col
    float32x4_t VC = vld1q_f32(V);

    // Local output vector
    float32x4_t L0 = vmovq_n_f32(0);

    L0 = vfmaq_laneq_f32(L0, M0, VC, 0);
    L0 = vfmaq_laneq_f32(L0, M1, VC, 1);
    L0 = vfmaq_laneq_f32(L0, M2, VC, 2);
    L0 = vfmaq_laneq_f32(L0, M3, VC, 3);

    vst1q_f32(O, L0);
}

#else

#error "Architecture not supported! Please submit an issue."

#endif
