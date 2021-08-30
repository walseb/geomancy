#include <xmmintrin.h>

void Mat4xMat4_SSE(float *A, float *B, float *O) {
  __m128 row0 = _mm_load_ps(&B[0]);
  __m128 row1 = _mm_load_ps(&B[4]);
  __m128 row2 = _mm_load_ps(&B[8]);
  __m128 row3 = _mm_load_ps(&B[12]);
  for(int i=0; i<4; i++) {
    __m128 brod0 = _mm_set1_ps(A[4*i + 0]);
    __m128 brod1 = _mm_set1_ps(A[4*i + 1]);
    __m128 brod2 = _mm_set1_ps(A[4*i + 2]);
    __m128 brod3 = _mm_set1_ps(A[4*i + 3]);
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

void Mat4xVec4_SSE(float *M, float *V, float *O) {
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
