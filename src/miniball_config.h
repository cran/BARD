

//    Copright (C) 1999
//    Based on Rev 1.4, Modified for MINGW

//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA,
//    or download the License terms from prep.ai.mit.edu/pub/gnu/COPYING-2.0.
//
//    Original Contact:
//    --------
//    Bernd Gaertner
//    Institut f. Informatik
//    ETH Zuerich
//    ETH-Zentrum
//    CH-8092 Zuerich, Switzerland
//    http://www.inf.ethz.ch/personal/gaertner
//
// 
//   Mingw Modifications: Micah Altman (Micah_Altman@harvard.edu)



#ifndef MINIBALL_CONFIG_H
    #define MINIBALL_CONFIG_H
    #if defined(__sgi) && !defined(__GNUC__)              // assume MIPS-IRIX
        #define MINIBALL_NO_STD_NAMESPACE
    #endif

    #if defined(__GNUC__) && (__GNUC__==2) && (__GNUC_MINOR__<=90)         // assume old GNU
        #define MINIBALL_NO_STD_NAMESPACE
    #endif

    #ifdef LDOUBLE
		typedef long double mbdouble;
    #else 
		typedef double mbdouble;
    #endif
		

    #if !defined(__sgi) && !defined(__GNUC__)             // assume Visual C++
        #include<cstdlib>
	using std::srand;
	using std::rand;
        inline void random_seed (unsigned int seed) {srand(seed);}
        inline double random_double () {return double(rand())/RAND_MAX;}
    #else                                                 // no Visual C++
        #ifndef MINIBALL_NO_STD_NAMESPACE
            #include<cstdlib>
            using std::srand; 
            using std::rand;
	    #ifdef __GNUC__
        	inline void random_seed (unsigned int seed) {srand(seed);}
        	inline double random_double () {return double(rand())/RAND_MAX;}
	    #else
            inline void random_seed (unsigned int seed){std::srand48(seed);}
            inline double random_double () {return std::drand48();}
	    #endif
        #else
            /*#include<cstdlib>*/
            #include<stdlib.h> 
	    #ifdef MINGW
            inline void random_seed (unsigned int seed){srand(seed);}
            inline double random_double () {return rand();}
	    #else
	    inline void random_seed (unsigned int seed){srand48(seed);}
            inline double random_double () {return drand48();}
	    #endif
        #endif
    #endif
#endif

