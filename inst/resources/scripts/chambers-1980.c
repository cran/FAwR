/* 	$Id: chambers-1980.c 3569 2010-02-09 01:35:10Z hamannj $	 */
                                                                                
/****************************************************************************/
/* Charles J. Chambers 1980. Emperical growth and yield tables for the      */
/* Douglas-fir zone.  DNR Report No. 41.  Washington Department of Natural  */
/* Resources. Olympia WA. 98504                                             */
/****************************************************************************/

#ifndef lint
static char vcid[] = "$Id: chambers-1980.c 3569 2010-02-09 01:35:10Z hamannj $";
#endif /* lint */

#include "chambers-1980.h"
#include <math.h>

static double chambers_1980_breast_height_age(
  double *site_index, 
  double *total_age   );

static unsigned long chambers_1980_years_2_bh( 
  const unsigned long    *species,
  const double             *site_index, 
  const unsigned long    *bha );

/****************************************************************************/
/*  breast_height_age                                                       */
/****************************************************************************/
static double chambers_1980_breast_height_age(
  double *site_index, 
  double *total_age   ) 
{

  double  ret_val;
  unsigned long  y2bh;

  ret_val = 0.0;

  y2bh = chambers_1980_years_2_bh( 0, site_index, 0 );

  ret_val = (*total_age) - y2bh;

  if( ret_val < 0.0 ) 
  {
    ret_val = 0.0;
  }

  return (double)ret_val;

}

/* do you really need bha in here... */
static unsigned long chambers_1980_years_2_bh( 
  const unsigned long    *species,
  const double             *site_index, 
  const unsigned long    *bha )
{

  if( (*site_index) <= 74.0 )
  {
    return 10;
  }
  if( (*site_index) > 74.0 && (*site_index) <= 94.0 )
  {
    return 9;
  }
  if( (*site_index) > 94.0 && (*site_index) <= 114.0 )
  {
    return 8;
  }
  if( (*site_index) > 114.0 && (*site_index) <= 134.0 )
  {
    return 7;
  }
  if( (*site_index) > 134.0 )
  {
    return 6;
  }

}


/****************************************************************************/
/*  normal_basal_area                                                       */
/*  table 1. page 4.                                                        */
/****************************************************************************/
//double chambers_1980_normal_basal_area(
void chambers_1980_normal_basal_area(
  double   *site_index,
  double   *total_age,
   double *normal_basal_area )
{

   double  ret_val;
  double  bha;

  ret_val = 0.0;
  *normal_basal_area = 0.0;

  bha     = chambers_1980_breast_height_age( site_index, total_age ); 

  if( (*site_index) <= 0.0f || bha <= 0.0f ) 
  {
     //return -1.0f;
     //*ret_val = -1.0f;
     *normal_basal_area = -1.0;
     return;
  }

  ret_val   =     -901.67920  + 
    301.38721   * log10( bha ) +   
    296.87085   * log10( (*site_index) );

  if( ret_val <= 0.0 )
  {
    ret_val = 0.0f;
  }

  *normal_basal_area = ret_val;

  //return (double)ret_val;

}


/****************************************************************************/
/*  trees per acre for the dnr ownership                                    */
/*  table 2. page 5.                                                        */
/****************************************************************************/
//double chambers_1980_normal_tpa_dnr(
void chambers_1980_normal_tpa_dnr(
   double   *site_index, 
   double   *total_age,
   double   *pnba,
   double *normal_tpa_dnr ) 
{
    
  double  pred_nba;
  double  pred_dbh;
  double  ret_val;

  if( (*site_index)  <= 0.0f || (*total_age)   <= 0.0f ) 
  {
     //return -1.0f;
     *normal_tpa_dnr = -1.0;
     return;
  }

  //pred_nba = chambers_1980_normal_basal_area( site_index, total_age );
  chambers_1980_normal_basal_area( site_index, total_age, &pred_nba );
  //pred_dbh = chambers_1980_avg_dbh_dnr( site_index, total_age, pnba );
  chambers_1980_avg_dbh_dnr( site_index, total_age, pnba, &pred_dbh );

  ret_val =   ( *pnba * pred_nba ) / ( 0.0054541539 * (pred_dbh) * (pred_dbh) );

  if( 0.0f > ret_val )
  {
     //ret_val = 0.0f;
     *normal_tpa_dnr = 0.0f;
  }

  //return (double)ret_val;
  *normal_tpa_dnr = ret_val;

}



/****************************************************************************/
/*  trees per acre                                                          */
/*  table 3. page 5.                                                        */
/****************************************************************************/
//double chambers_1980_normal_tpa(
void chambers_1980_normal_tpa(
  double   *site_index, 
  double   *total_age,
  double   *pnba,
   double *normal_tpa ) 
{
    
  double  pred_nba;
  double  pred_dbh;
  double  ret_val;

  if( *site_index  <= 0.0f || *total_age   <= 0.0f ) 
  {
     //return -1.0f;
    *normal_tpa = -1.0f;
    return;
  }

  //pred_nba = chambers_1980_normal_basal_area( site_index, total_age );
  chambers_1980_normal_basal_area( site_index, total_age, &pred_nba );
  //pred_dbh = chambers_1980_avg_dbh( site_index, total_age, pnba );
  chambers_1980_avg_dbh( site_index, total_age, pnba, &pred_dbh );

  ret_val =   ( *pnba * pred_nba ) / ( 0.0054541539 * (pred_dbh) * (pred_dbh) );

  if( 0.0f > ret_val )
  {
     ret_val = 0.0f;
  }

  //return (double)ret_val;
  *normal_tpa = ret_val;

}


/****************************************************************************/
/*  average stand diameter                                                  */
/*  table 5, page 6.                                                        */
/*  QMD in trees 7 inches or larger DBH from pure even-aged second growth   */
/****************************************************************************/
//double chambers_1980_avg_dbh_dnr(
void chambers_1980_avg_dbh_dnr(
  double *site_index, 
  double *total_age, 
  double *pnba,
  double *avg_dbh_dnr ) 
{

  double  bha;
  double  qmd;

  if( *site_index  <=  0.0f ||
      *total_age   <=  0.0f ||
      *pnba        <=  0.0f    )
  {
     //return 0.0;
     *avg_dbh_dnr = 0.0;
     return;
  }

  bha = chambers_1980_breast_height_age( site_index, total_age );
  //chambers_1980_breast_height_age( site_index, total_age, &bha );

  qmd =       6.92634 
    +   0.00170 * bha * (*site_index) 
    -   0.03591 * bha * (*pnba)
    - 0.0000022293 * pow( bha, 3.0 );

  if( qmd <= 0.0 )
  {
    qmd = 0.0f;
  }

  //return (double)qmd;

  *avg_dbh_dnr = qmd;

}


/****************************************************************************/
/*  average stand diameter                                                  */
/*  table 5, page 6.                                                        */
/*  QMD in trees 7 inches or larger DBH from pure even-aged second growth   */
/****************************************************************************/
//double chambers_1980_avg_dbh(
void chambers_1980_avg_dbh(
  double *site_index, 
  double *total_age, 
  double *pnba,
   double *avg_dbh ) 
{

  double  bha;
  double  qmd;

  if( *site_index  <=  0.0f ||
      *total_age   <=  0.0f ||
      *pnba        <=  0.0f    )
  {
     //return 0.0;
     *avg_dbh = 0.0;
     return;
  }

  bha = chambers_1980_breast_height_age( site_index, total_age );

  qmd =   5.09819 + 
    0.00175928  * bha * (*site_index) - 
    0.000395468 * bha * (*site_index) * (*pnba);

  if( qmd <= 0.0 )
  {
    qmd = 0.0f;
  }


  // return (double)qmd;
  *avg_dbh = qmd;

}


/****************************************************************************/
/*  average stand tarif                                                     */
/*  table 6, page 7.                                                        */
/*  7.0 inches DBH and larger DBH                                           */
/****************************************************************************/
//double chambers_1980_average_stand_tarif(
void chambers_1980_average_stand_tarif(
  double *site_index, 
  double *total_age, 
  double *pnba,
   double *average_stand_tarif ) 
{

  double  ret_val;
  double  bha;

  bha = chambers_1980_breast_height_age( site_index, total_age );

  ret_val =   554.02612
    + 51.4646 * pow( log10( bha * (*site_index) ), 2.0 ) 
    - 0.00213 * bha * bha
    - 330.23315 * log10( bha * (*site_index) )
    + 0.09154 * bha * (*pnba)
    - 114.17606 * ( 1.0 / bha )
    + 0.63984 * ( 1.0 / (*pnba) );

  if( ret_val <= 0.0 )
  {
    ret_val = 0.0f;
  }

  //return (double)ret_val;
  *average_stand_tarif = ret_val;

}


/****************************************************************************/
/*  cubic foot volume per acre                                              */
/*  table 7, page 7.                                                        */
/*  7.0 inches DBH and larger DBH                                           */
/****************************************************************************/
void chambers_1980_cubic_foot_volume(
   double *site_index, 
   double *total_age, 
   double *pnba,
   double *cubic_foot_volume ) 
{
   
   double  ret_val;
   double  bha;
   
   bha = chambers_1980_breast_height_age( site_index, total_age );
   
   ret_val =   -   938.33423 
      +   2.01933  * bha * (*site_index) * (*pnba) 
      -   21.28009 * bha * (*pnba)
      +   41.49121 * bha
      -    0.51870 * bha * bha
      - 1567.56665 * (*pnba);
   
   if( ret_val <= 0.0 )
   {
      ret_val = 0.0f;
   }
   
   //return (double)ret_val;
   *cubic_foot_volume = ret_val;

}


/****************************************************************************/
/*  scribner board foot volume per acre                                     */
/*  table 8, page 8.                                                        */
/*  7.0 inches DBH and larger DBH, 6 inch top 16 foot logs                  */
/****************************************************************************/
//double chambers_1980_scribner_16_volume(
void chambers_1980_scribner_16_volume(
  double *site_index, 
  double *total_age, 
  double *pnba,
   double *scribner_16_volume ) 
{

  double  ret_val;
  double  cfv;
  double  scribner_cvts;
  double  pred_dbh;
  double  pred_avg_tarif;

  //pred_dbh        = chambers_1980_avg_dbh(  site_index, 
  chambers_1980_avg_dbh(  site_index, 
                                            total_age, 
                                            pnba, 
					    &pred_dbh);

  // pred_avg_tarif  = chambers_1980_average_stand_tarif(  site_index, 
  chambers_1980_average_stand_tarif(  site_index, 
				      total_age, 
				      pnba,
				      &pred_avg_tarif ); 

  //cfv             = chambers_1980_cubic_foot_volume(    site_index, 
  chambers_1980_cubic_foot_volume(    site_index, 
				      total_age, 
				      pnba,
				      &cfv );
  
  //scribner_cvts   = (double)chambers_1980_board_foot_cubic_foot_ratio(    
  chambers_1980_board_foot_cubic_foot_ratio(    
     &pred_dbh, 
     &pred_avg_tarif,
     &scribner_cvts );
  
  ret_val = cfv * scribner_cvts;
  
  if( ret_val <= 0.0 )
  {
    ret_val = 0.0f;
  }

  //return (double)ret_val;
  *scribner_16_volume = ret_val;
  
}


/****************************************************************************/
/*  scribner board foot volume per acre                                     */
/*  table 9, page 8.                                                        */
/*  7.0 inches DBH and larger DBH, 6 inch top 16 foot logs                  */
/****************************************************************************/
//double chambers_1980_scribner_32_volume(
void chambers_1980_scribner_32_volume(
  double *site_index, 
  double *total_age, 
  double *pnba,
  double *scribner_32_volume ) 
{

  double  ret_val;
  double  r_32_16;
  double  sv_16;
  double  pred_dbh;
  double  pred_avg_tarif;


  //sv_16 = chambers_1980_scribner_16_volume( site_index, 
  chambers_1980_scribner_16_volume( site_index, 
				    total_age, 
				    pnba, 
				    &sv_16  );

  //pred_dbh        = chambers_1980_avg_dbh(  site_index, 
  chambers_1980_avg_dbh(  site_index, 
			  total_age, 
			  pnba,
			  &pred_dbh );

  //pred_avg_tarif  = chambers_1980_average_stand_tarif(  site_index, 
  chambers_1980_average_stand_tarif(  site_index, 
				      total_age, 
				      pnba,
				      &pred_avg_tarif ); 
  
  r_32_16 =   1.001491 
     -6.924097 / (pred_avg_tarif)
     + 0.00001315 * (pred_dbh) * (pred_dbh);

  ret_val = (sv_16) * r_32_16;

  if( ret_val <= 0.0 )
  {
    ret_val = 0.0f;
  }

  //return (double)ret_val;

  *scribner_32_volume = ret_val;

}


/****************************************************************************/
/*  cubic_volume_dbh_basal_area                                             */
/*  table 10, page 9.                                                       */
/*  7.0 inches dbh and larger                                               */
/****************************************************************************/
//double chambers_1980_cubic_volume_dbh_basal_area(
void chambers_1980_cubic_volume_dbh_basal_area(
   double   *dbh,
   double   *basal_area,
   double *cubic_volume_dbh_basal_area )
{

  double  ret_val;

  ret_val =   pow( 10.0, 1.11618
                   + 0.09251 * log10( (*basal_area) ) * log10( (*basal_area) )
                   + 0.01696 * (*dbh)
                   + 0.43675 * log10( (*basal_area) * (*basal_area) ) );
    
  if( ret_val <= 0.0 ) 
  {
    ret_val = 0.0;
  }

  //return (double)ret_val;

  *cubic_volume_dbh_basal_area = ret_val;

}


/****************************************************************************/
/*  average tarif                                                           */
/*  table 12, page 9.                                                       */
/*  7.0 inches dbh and larger                                               */
/****************************************************************************/
//double chambers_1980_average_tarif_dbh_basal_area(
void chambers_1980_average_tarif_dbh_basal_area(
   double   *dbh,
   double   *basal_area,
   double *average_tarif_dbh_basal_area )
{

  double  ret_val;

  ret_val =   11.27328
    + 5.40054 * ( log10( (*basal_area) ) * log10( (*dbh) ) )
    + 0.00056 * (*dbh) * (*dbh) * (*dbh) 
    + 0.01782 * (*basal_area) 
    + 0.00131 * ( (*basal_area) * (*dbh) );
    
  if( ret_val <= 0.0 ) 
  {
    ret_val = 0.0;
  }


  //return (double)ret_val;

  *average_tarif_dbh_basal_area = ret_val;
}


/****************************************************************************/
/*  board_foot_cubic_foot_ratio                                             */
/*  table 13, page 9.                                                       */
/*  scribner 6-inch top / total cubic foot volume                           */
/****************************************************************************/
//double chambers_1980_board_foot_cubic_foot_ratio(
void chambers_1980_board_foot_cubic_foot_ratio(
   double   *dbh,
   double   *tarif,
   double *board_foot_cubic_foot_ratio )
{
   
  double  ret_val;

  ret_val =   -9.53626
    + 14.85941 * log10( (*dbh) )
    - 5.10189 * ( (*dbh) / (*tarif) )
    - 0.00204 * (*tarif) * (*dbh);
    
  if( ret_val <= 0.0 ) 
  {
    ret_val = 0.0;
  }

  //return (double)ret_val;
  
  *board_foot_cubic_foot_ratio = ret_val;

}


/****************************************************************************/
/*  stand mean height                                                       */
/*  table 16, page 10.                                                      */
/*  trees 5.0 inches and larger                                             */
/****************************************************************************/
//double chambers_1980_stand_mean_height(
void chambers_1980_stand_mean_height(
   double   *site_index,
   double   *total_age,
   double *stand_mean_height )
{
   
  double  ret_val;
  double  bha;

  bha = chambers_1980_breast_height_age( site_index, total_age );

  /* this is a lame fix */
  if( bha < 1.0 ) 
  {
    bha = 1.0;
  }

  ret_val =   2202.75000
    + 205.41618 * pow( log10( bha * (*site_index) ), 2.0 )
    - 1323.88477 * log10( bha * (*site_index) )
    - 0.00671 * bha * bha
    - 383.35889 * ( 1.0 / bha );

  if( ret_val <= 0.0 ) 
  {
    ret_val = 0.0;
  }

  //return (double)ret_val;

  *stand_mean_height = ret_val;
}


/****************************************************************************/
/*  net_basal_area_growth                                                   */
/****************************************************************************/
//double chambers_1980_net_basal_area_growth(
void chambers_1980_net_basal_area_growth(
  double   *total_age,
  double   *basal_area,
  double   *site_index,
   double *net_basal_area_growth )
{

  double  ret_val;
  double  bha;

  bha = chambers_1980_breast_height_age( site_index, total_age ) + 10.0;

  ret_val =   1.77548
    + 14.13342 * ( (*basal_area) / ( bha * bha ) )
    - 955.048 / ( (*site_index) * (*site_index) )
    + 1181.21191 / ( bha * bha )
    - 72.90152 / (*basal_area);

  if( ret_val <= 0.0 ) 
  {
    ret_val = 0.0;
  }

  //return (double)ret_val;
  *net_basal_area_growth = ret_val;

}



