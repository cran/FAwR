/* 	$Id: chambers-1980.h 3551 2010-02-03 22:58:10Z hamannj $	 */

/****************************************************************************/
/* Charles J. Chambers 1980. Emperical growth and yield tables for the      */
/* Douglas-fir zone.  DNR Report No. 41.  Washington Department of Natural  */
/* Resources. Olympia WA. 98504                                             */
/****************************************************************************/

/*
  Emperical yield functions for Douglas-fir from:

         Min  Max     Mean      SD
  Age     9    99    38.05   17.35
  SI     38   165   107.39   21.42 (Kings 1966)
  TPA               178.06  111.48
  BA                135.93   78.37
  CFV              4463.1  3353.87
  QMD                12.35    3.36
  normBA              0.82    0.38

*/


#ifndef _CHAMBERS_1980_
#define _CHAMBERS_1980_



/* double chambers_1980_normal_basal_area( */
/*     double   *site_index, */
/*     double   *total_age          ); */

void chambers_1980_normal_basal_area(
   double   *site_index,
   double   *total_age,
   double *ret_val );

/* double chambers_1980_normal_tpa_dnr( */
/*     double   *site_index,  */
/*     double   *total_age, */
/*     double   *pnba                ); */


void chambers_1980_normal_tpa_dnr(
    double   *site_index,
    double   *total_age,
    double   *pnba,
    double   *ntpa_dnr );

/* double chambers_1980_normal_tpa( */
/*     double   *site_index,  */
/*     double   *total_age, */
/*     double   *pnba                ); */


void chambers_1980_normal_tpa(
    double   *site_index,
    double   *total_age,
    double   *pnba,
    double   *ntpa );


/* double chambers_1980_avg_dbh_dnr( */
/*     double *site_index,  */
/*     double *total_age,  */
/*     double *pnba                  );  */

void chambers_1980_avg_dbh_dnr(
    double *site_index,
    double *total_age,
    double *pnba,
    double *avg_dbh_dnr );

/* double chambers_1980_avg_dbh( */
/*     double *site_index, */
/*     double *total_age, */
/*     double *pnba                  ); */

void chambers_1980_avg_dbh(
    double *site_index,
    double *total_age,
    double *pnba,
    double *avg_dbh );

/* double chambers_1980_average_stand_tarif( */
/*     double *site_index,  */
/*     double *total_age,  */
/*     double *pnba                  ); */

void chambers_1980_average_stand_tarif(
    double *site_index,
    double *total_age,
    double *pnba,
    double *avg_stand_tarif );

/* double chambers_1980_cubic_foot_volume( */
/*     double *site_index,  */
/*     double *total_age,  */
/*     double *pnba                  ); */

void chambers_1980_cubic_foot_volume(
    double *site_index,
    double *total_age,
    double *pnba,
    double *cubic_foot_volume );

/* these next two functions still don't check out */
/* double chambers_1980_scribner_16_volume( */
/*     double *site_index,  */
/*     double *total_age,  */
/*     double *pnba                  );  */

void chambers_1980_scribner_16_volume(
    double *site_index,
    double *total_age,
    double *pnba,
    double *scibner_16_volume );

/* double chambers_1980_scribner_32_volume( */
/*     double *site_index,  */
/*     double *total_age,  */
/*     double *pnba                  ); */

void chambers_1980_scribner_32_volume(
   double *site_index,
   double *total_age,
   double *pnba,
   double *scribner_32_volume );

/* double chambers_1980_cubic_volume_dbh_basal_area( */
/*     double   *dbh, */
/*     double   *basal_area          ); */

void chambers_1980_cubic_volume_dbh_basal_area(
    double   *dbh,
    double   *basal_area,
    double *cubic_volume_dbh_basal_area);

    
/* double chambers_1980_average_tarif_dbh_basal_area( */
/*     double   *dbh, */
/*     double   *basal_area          ); */

void chambers_1980_average_tarif_dbh_basal_area(
    double   *dbh,
    double   *basal_area,
    double *average_tarif_dbh_basal_area );
     
/* double chambers_1980_board_foot_cubic_foot_ratio( */
/*     double   *dbh, */
/*     double   *tarif               ); */

void chambers_1980_board_foot_cubic_foot_ratio(
    double   *dbh,
    double   *tarif,
   double *board_foot_cubic_foot_ratio);

/* double chambers_1980_stand_mean_height( */
/*     double   *site_index, */
/*     double   *total_age          ); */

void chambers_1980_stand_mean_height(
   double   *site_index,
   double   *total_age,
   double *stand_mean_height );


/* double chambers_1980_net_basal_area_growth( */
/*    double   *total_age, */
/*    double   *basal_area, */
/*    double   *site_index ); */

void chambers_1980_net_basal_area_growth(
   double   *total_age,
   double   *basal_area,
   double   *site_index,
   double *net_basal_area_growth );



#endif /* _CHAMBERS_1980_ */
