

//    Copright (C) 1999
//    $Revision: 1.2 $
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
   #include<assert.h>
#else
   #include<cassert>
   
#endif

   // Miniball
   // --------
   
   template <int d>
   void Miniball<d>::check_in (const Point& p)
   {
       L.push_back(p);
   }
   
   
   template <int d>
   void Miniball<d>::build (bool pivoting)
   {
       B.reset();
       support_end = L.begin();
       if (pivoting)
           pivot_mb (L.end());
       else
           mtf_mb (L.end());
   }
   
   
   template <int d>
   void Miniball<d>::mtf_mb (It i)
   {
       support_end = L.begin();
       if ((B.size())==d+1) return;
       for (It k=L.begin(); k!=i;) {
           It j=k++;
           if (B.excess(*j) > 0) {
               if (B.push(*j)) {
                   mtf_mb (j);
                   B.pop();
                   move_to_front(j);
               }
           }
       }
   }
   
   template <int d>
   void Miniball<d>::move_to_front (It j)
   {
       if (support_end == j)
           support_end++;
       L.splice (L.begin(), L, j);
   }
   
   
   template <int d>
   void Miniball<d>::pivot_mb (It i)
   {
       It t = ++L.begin();
       mtf_mb (t);
       mbdouble max_e, old_sqr_r;
       old_sqr_r=-1;
       do {
           It pivot;
           max_e = max_excess (t, i, pivot);
           if (max_e > 0) {
               t = support_end;
               if (t==pivot) ++t;
               old_sqr_r = B.squared_radius();
               B.push (*pivot);
               mtf_mb (support_end);
               B.pop();
               move_to_front (pivot);
           }
       } while ((max_e > 0) && (B.squared_radius() > old_sqr_r));
   }
   
   
   template <int d>
   mbdouble Miniball<d>::max_excess (It t, It i, It& pivot) const
   {
       const mbdouble *c = B.center(), sqr_r = B.squared_radius();
       mbdouble e, max_e = 0;
       for (It k=t; k!=i; ++k) {
           const mbdouble *p = (*k).begin();
           e = -sqr_r;
           for (int j=0; j<d; ++j)
               e += sqr(p[j]-c[j]);
           if (e > max_e) {
               max_e = e;
               pivot = k;
           }
       }
       return max_e;
    }
   
   
   
   template <int d>
   typename Miniball<d>::Point Miniball<d>::center () const
   {
       return Point(B.center());
   }
   
   template <int d>
   mbdouble Miniball<d>::squared_radius () const
   {
       return B.squared_radius();
   }
   
   
   template <int d>
   int Miniball<d>::nr_points () const
   {
       return L.size();
   }
   
   template <int d>
   typename Miniball<d>::Cit Miniball<d>::points_begin () const
   {
       return L.begin();
   }
   
   template <int d>
   typename Miniball<d>::Cit Miniball<d>::points_end () const
   {
       return L.end();
   }
   
   
   template <int d>
   int Miniball<d>::nr_support_points () const
   {
       return B.support_size();
   }
   
   template <int d>
   typename Miniball<d>::Cit Miniball<d>::support_points_begin () const
   {
       return L.begin();
   }
   
   template <int d>
   typename Miniball<d>::Cit Miniball<d>::support_points_end () const
   {
       return support_end;
   }
   
   
   
   template <int d>
   mbdouble Miniball<d>::accuracy (mbdouble& slack) const
   {
       mbdouble e, max_e = 0;
       int n_supp=0;
       Cit i;
       for (i=L.begin(); i!=support_end; ++i,++n_supp)
           if ((e = abs (B.excess (*i))) > max_e)
               max_e = e;
   
       // you've found a non-numerical problem if the following ever fails
       assert (n_supp == nr_support_points());
   
       for (i=support_end; i!=L.end(); ++i)
          if ((e = B.excess (*i)) > max_e)
               max_e = e;
   
       slack = B.slack();
       return (max_e/squared_radius());
   }
   
   
   template <int d>
   bool Miniball<d>::is_valid (mbdouble tolerance) const
   {
       mbdouble slack;
       return ( (accuracy (slack) < tolerance) && (slack == 0) );
   }
   
   

   // Basis
   // -----
   
   template <int d>
   const mbdouble* Basis<d>::center () const
   {
       return current_c;
   }
   
   template <int d>
   mbdouble Basis<d>::squared_radius() const
   {
       return current_sqr_r;
   }
   
   template <int d>
   int Basis<d>::size() const
   {
       return m;
   }
   
   template <int d>
   int Basis<d>::support_size() const
   {
       return s;
   }
   
   template <int d>
   mbdouble Basis<d>::excess (const Point& p) const
   {
       mbdouble e = -current_sqr_r;
       for (int k=0; k<d; ++k)
          e += sqr(p[k]-current_c[k]);
       return e;
   }
   
   
   
   template <int d>
   void Basis<d>::reset ()
   {
       m = s = 0;
       // we misuse c[0] for the center of the empty sphere
       for (int j=0; j<d; ++j)
           c[0][j]=0;
       current_c = c[0];
       current_sqr_r = -1;
   }
   
   
   template <int d>
   Basis<d>::Basis ()
   {
       reset();
   }
   
   
   template <int d>
   void Basis<d>::pop ()
   {
       --m;
   }
   
   
   template <int d>
   bool Basis<d>::push (const Point& p)
   {
       int i, j;
       mbdouble eps = 1e-32;
       if (m==0) {
           for (i=0; i<d; ++i)
               q0[i] = p[i];
           for (i=0; i<d; ++i)
               c[0][i] = q0[i];
           sqr_r[0] = 0;
       } else {
          // set v_m to Q_m
          for (i=0; i<d; ++i)
               v[m][i] = p[i]-q0[i];
   
          // compute the a_{m,i}, i< m
          for (i=1; i<m; ++i) {
               a[m][i] = 0;
               for (j=0; j<d; ++j)
                   a[m][i] += v[i][j] * v[m][j];
               a[m][i]*=(2/z[i]);
          }
   
          // update v_m to Q_m-\bar{Q}_m
          for (i=1; i<m; ++i) {
               for (j=0; j<d; ++j)
                   v[m][j] -= a[m][i]*v[i][j];
          }
   
          // compute z_m
          z[m]=0;
          for (j=0; j<d; ++j)
               z[m] += sqr(v[m][j]);
          z[m]*=2;
   
          // reject push if z_m too small
          if (z[m]<eps*current_sqr_r) {
               return false;
          }
   
          // update c, sqr_r
          mbdouble e = -sqr_r[m-1];
          for (i=0; i<d; ++i)
               e += sqr(p[i]-c[m-1][i]);
          f[m]=e/z[m];
   
          for (i=0; i<d; ++i)
               c[m][i] = c[m-1][i]+f[m]*v[m][i];
          sqr_r[m] = sqr_r[m-1] + e*f[m]/2;
       }
       current_c = c[m];
       current_sqr_r = sqr_r[m];
       s = ++m;
       return true;
   }
   
   
   
   template <int d>
   mbdouble Basis<d>::slack () const
   {
       mbdouble l[d+1], min_l=0;
       l[0] = 1;
       for (int i=s-1; i>0; --i) {
           l[i] = f[i];
           for (int k=s-1; k>i; --k)
               l[i]-=a[k][i]*l[k];
           if (l[i] < min_l) min_l = l[i];
           l[0] -= l[i];
       }
       if (l[0] < min_l) min_l = l[0];
       return ( (min_l < 0) ? -min_l : 0);
   }
   
