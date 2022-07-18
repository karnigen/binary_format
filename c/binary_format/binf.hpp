#ifndef _BINARY_FORMAT_HPP_
#define _BINARY_FORMAT_HPP_

#ifdef __THIS_IS_COMMENT__
//   Copyright 2022 Tony Karnigen

// - must be accepted by C, C++ and FORTRAN (most restrictive)

// - BINF_VAL_LENGTH defines max scalar values storage space
//    - assuming complex requires 2x16B=32B
#endif
#ifndef BINF_VAL_LENGTH
#define BINF_VAL_LENGTH 32
#endif


#ifdef __THIS_IS_COMMENT__
// - max length of variable name
// - max count of dimensions
#endif
#ifndef BINF_VD_MAX_NAME_LENGTH
#define BINF_VD_MAX_NAME_LENGTH 256
#endif
#ifndef BINF_VD_MAX_SHAPE_COUNT
#define BINF_VD_MAX_SHAPE_COUNT 64
#endif



#endif
