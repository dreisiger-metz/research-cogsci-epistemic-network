// =============================================================================
// Filename          : $RCSfile: Endian.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 03 Mar 2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:06:34 $
//
// Security Class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This is the header of a static class that implements
//                     byte-swapping routines for two, four and eight-byte
//                     data types.
// =============================================================================
#ifndef libFGREP__Endian_hpp
#define libFGREP__Endian_hpp

#include <stdlib.h>  // which indirectly includes endian.h


namespace Utilities {

  class Endian {
  public:
    static void swap_2(void *datum) {
      #if BYTE_ORDER == BIG_ENDIAN
        char t;

        t = ((char *) datum)[0];
        ((char *) datum)[0] = ((char *) datum)[1];
        ((char *) datum)[1] = t;
      #endif
    }

    static void swap_4(void *datum) {
      #if BYTE_ORDER == BIG_ENDIAN
        char t;

        t = ((char *) datum)[0];
        ((char *) datum)[0] = ((char *) datum)[3];
        ((char *) datum)[3] = t;
        t = ((char *) datum)[1];
        ((char *) datum)[1] = ((char *) datum)[2];
        ((char *) datum)[2] = t;
      #endif
    }

    static void swap_8(void *datum) {
      #if BYTE_ORDER == BIG_ENDIAN
        char t;

        t = ((char *) datum)[0];
        ((char *) datum)[0] = ((char *) datum)[7];
        ((char *) datum)[7] = t;
        t = ((char *) datum)[1];
        ((char *) datum)[1] = ((char *) datum)[6];
        ((char *) datum)[6] = t;
        t = ((char *) datum)[2];
        ((char *) datum)[2] = ((char *) datum)[5];
        ((char *) datum)[5] = t;
        t = ((char *) datum)[3];
        ((char *) datum)[3] = ((char *) datum)[4];
        ((char *) datum)[4] = t;
      #endif
    }

  };

}

#endif
