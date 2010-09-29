

//    Copright (C) 1999
//    $Revision: 1.3 $
//    $Date: 2004/08/28 01:19:41 $
//
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
//    Contact:
//    --------
//    Bernd Gaertner
//    Institut f. Informatik
//    ETH Zuerich
//    ETH-Zentrum
//    CH-8092 Zuerich, Switzerland
//    http://www.inf.ethz.ch/personal/gaertner
//


#ifdef MINIBALL_NO_STD_NAMESPACE
    #include <list.h>
#else
    #include <list>
#endif

#if defined __sun
       #include<cstdlib>
//       inline void random_seed (unsigned int seed) {std::srand(seed);}
//       inline double random_double () {return double(std::rand())/RAND_MAX;}
#elif !defined(__sgi) && !defined(__GNUC__)             // assume Visual C++
       #include<cstdlib>
#endif
#include "miniball_wrapped_array.h"

    template <int d> class Miniball;
    template <int d> class Basis;

    // Miniball
    // --------
    
    template <int d>
    class Miniball {
        public:
            // types
            typedef Wrapped_array<d>                            Point;
            typedef typename std::list<Point>::iterator         It;
            typedef typename std::list<Point>::const_iterator   Cit;
    
        private:
            // data members
            std::list<Point> L;         // STL list keeping the points
            Basis<d>    B;              // basis keeping the current ball
            It          support_end;    // past-the-end iterator of support set
    
            // private methods
            void        mtf_mb (It k);
            void        pivot_mb (It k);
            void        move_to_front (It j);
            mbdouble      max_excess (It t, It i, It& pivot) const;
            mbdouble      abs (mbdouble r) const {return (r>0)? r: (-r);}
            mbdouble      sqr (mbdouble r) const {return r*r;}
    
        public:
            // construction
            Miniball() {}
            void        check_in (const Point& p);
            void        build (bool pivoting = true);
    
            // access
            Point       center() const;
            mbdouble      squared_radius () const;
            int         nr_points () const;
            Cit         points_begin () const;
            Cit         points_end () const;
            int         nr_support_points () const;
            Cit         support_points_begin () const;
            Cit         support_points_end () const;
    
            // checking
            mbdouble      accuracy (mbdouble& slack) const;
            bool        is_valid (mbdouble tolerance = 1e-15) const;
     };
    
    

    // Basis
    // -----
    
    template <int d>
    class Basis {
        private:
            // types
            typedef Wrapped_array<d>            Point;
    
            // data members
            int                 m, s;   // size and number of support points
            mbdouble              q0[d];
    
            mbdouble              z[d+1];
            mbdouble              f[d+1];
            mbdouble              v[d+1][d];
            mbdouble              a[d+1][d];
    
            mbdouble              c[d+1][d];
            mbdouble              sqr_r[d+1];
    
            mbdouble*             current_c;      // points to some c[j]
            mbdouble              current_sqr_r;
    
            mbdouble              sqr (mbdouble r) const {return r*r;}
    
        public:
            Basis();
    
            // access
            const mbdouble*       center() const;
            mbdouble              squared_radius() const;
            int                 size() const;
            int                 support_size() const;
            mbdouble              excess (const Point& p) const;
    
            // modification
            void                reset(); // generates empty sphere with m=s=0
            bool                push (const Point& p);
            void                pop ();
    
            // checking
            mbdouble              slack() const;
    };
    
    

    #include "miniball_src.h"

