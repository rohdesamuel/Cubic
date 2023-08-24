#include "random.h"

#ifndef thread_local
# if __STDC_VERSION__ >= 201112 && !defined __STDC_NO_THREADS__
#  define thread_local _Thread_local
# elif defined _WIN32 && ( \
       defined _MSC_VER || \
       defined __ICL || \
       defined __DMC__ || \
       defined __BORLANDC__ )
#  define thread_local __declspec(thread) 
/* note that ICC (linux) and Clang are covered by __GNUC__ */
# elif defined __GNUC__ || \
       defined __SUNPRO_C || \
       defined __xlC__
#  define thread_local __thread
# else
#  error "Cannot define thread_local"
# endif
#endif

uint64_t rol64(uint64_t x, int k) {
	return (x << k) | (x >> (64 - k));
}

static thread_local cbXorshift_ state = { .s=0xE597C8CD6F2FE173 };

void cb_seed(uint64_t s) {
	if (s == 0) {
		state.s = (uint64_t)(rand()) << 32ull | (uint64_t)(rand());
	} else {
		state.s = s;
	}
}

uint64_t cb_rand() {
	return cb_xorshift(&state);
}

uint64_t cb_splitmix(cbSplitmix state) {
	uint64_t result = (state->s += 0x9E3779B97f4A7C15);
	result = (result ^ (result >> 30)) * 0xBF58476D1CE4E5B9;
	result = (result ^ (result >> 27)) * 0x94D049BB133111EB;
	return result ^ (result >> 31);
}

uint64_t cb_xorshift(cbXorshift state) {
	uint64_t x = state->s;
	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	return state->s = x;
}

uint64_t cb_xorshift128p(cbXorshift128p state) {
	uint64_t t = state->s[0];
	const uint64_t s = state->s[1];

	state->s[0] = s;
	t ^= t << 23;		// a
	t ^= t >> 18;		// b -- Again, the shifts and the multipliers are tunable
	t ^= s ^ (s >> 5);	// c
	state->s[1] = t;
	return t + s;
}

uint64_t cb_xoshiro256ss(cbXoshiro256ss state) {
	uint64_t* s = state->s;
	uint64_t const result = rol64(s[1] * 5, 7) * 9;
	uint64_t const t = s[1] << 17;

	s[2] ^= s[0];
	s[3] ^= s[1];
	s[1] ^= s[2];
	s[0] ^= s[3];

	s[2] ^= t;
	s[3] = rol64(s[3], 45);

	return result;
}