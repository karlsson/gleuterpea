#define FRAME_TYPE float
#define FRAME_SIZE sizeof(FRAME_TYPE)


inline double advance_phase(double newphase, double max){
  return(newphase < max)? newphase:(newphase - max);
}

inline int advance_int_phase(int newphase, int max){
  if(newphase < 0) {
    newphase += max;
  }
  return(newphase < max)? newphase:(newphase - max);
}
