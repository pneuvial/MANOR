#include "nem_typ.h"    /* DataT, ... */
#include "nem_ver.h"    /* NemVersionStrC */
#include "nem_arg.h"    /* NemArg, ... */
#include "nem_alg.h"    /* ClassifyByNem, ... */
#include "nem_rnd.h"    /* RandomInteger, ... */
#include "lib_io.h"     /* ReadOpeningComments, ... */
#include "genmemo.h"    /* GenAlloc, ... */

#include <stdio.h>      /* printf, ... */
#include <stdlib.h>     /* srand */
#ifdef __TURBOC__
#include <alloc.h>      /* coreleft, ... */ 
#endif
#include <string.h>     /* strncpy, ... */
#include <math.h>       /* sqrt, ... */
#include <R.h>


#ifndef MAXFLOAT
#define MAXFLOAT  3.40282347e+38F
#endif

#ifndef MINFLOAT
#define MINFLOAT   1.17549435e-38F
#endif



/* ==================== LOCAL FUNCTION PROTOTYPING =================== */




    static int SetVisitOrder
        ( 
	     int         Npt,
	     OrderET     VisitOrder,
	     int**       SiteVisitVP
	) ;
                                     


    static int  MakeErrinfo
         ( 
             const char* RefName,
             int         N,
             int         Kc,
	     TieET       TieRule,
	     ErrinfoT*   ErrinfoP,
	     ErrcurT*    ErrcurP
         ) ;



    static int  SetImageNeigh
                (   
                    NeighET     NeighSpec,
                    char*       NeiDescS,
                    NeighDataT* NeighDataP
                ) ;


/* Called by MakeErrinfo */

static int factorial(int n);

static int compute_permutations    /* ret 0 if OK, -1 if memory error */
(
 const int Startval,         /* I : start value of integer suite */
 const int K,                /* I : size of integer suite to permute > 0 */
 int*      perms_Kfact_K_p[] /* O : matrix to store permuted values */
) ;


/* Called by compute_permutations */

static int rec_permutations        /* ret 0 if OK, -1 if memory error */
(
 const int array_A[],       /* I : remaining array to permute */
 const int A,               /* I : length of the remaining array : 0..K */
 const int K,               /* I : length of original array */
 int       offset,          /* I : first line of storage to use */
 int       perms_Kfact_K[]  /* O : matrix to store permuted values, use
			       lines :   offset -> offset + A! - 1
			       columns : K - A  -> K - 1 */
) ;


/*Called by nem*/
static int GetInputParaPHUPE
(
          	DataT         *DataP,         /* O and allocated */
          	NemParaT      *NemParaP,      /* O */
          	SpatialT      *SpatialP,      /* O and allocated */
          	StatModelT    *StatModelP,    /* O and allocated */
	ErrinfoT      *ErrinfoP,      /* O and allocated */
	ErrcurT       *ErrcurP,       /* O and allocated */
          	float         **ClassifMP     /* O and allocated */
);

 static int  ReadNeiFilePHUPE
(
             NeighET     NeighSpec,          /* I */
             char*       NeiDescS,           /* O [LEN_LINE+1] */
             SpatialT    *SpatialP           /* I/O and allocated */
);

static int SaveResultsPHUPE
(
          const int          Npt,                   /* I */
          const int          Nd,                    /* I */
          const float*       ClassifM,              /* I */
          const SpatialT*    SpatialP,              /* I */
          const NemParaT*    NemParaP,              /* I */
          const StatModelT*  ModelP,                /* I */
          const CriterT*     CriterP ,               /* I */ /*V1.03-d*/
          int *classificationR
);


/* Called by */



/* ==================== LOCAL FUNCTION DEFINITION =================== */



/* ------------------------------------------------------------------- */
static int SetVisitOrder   /*V1.04-e*/
        (
	     int         Npt,          /* I */
	     OrderET     VisitOrder,   /* I */
	     int**       SiteVisitVP   /* O and allocated (Npt) */
	)
/* ------------------------------------------------------------------- */
{
  int ivis ;


  if (( *SiteVisitVP = GenAlloc( Npt, sizeof( int ), 0,
				 "SetVisitOrder", "SiteVisitVP" ) ) == NULL )
      return STS_E_MEMORY ;

  for ( ivis = 0 ; ivis < Npt ; ivis ++ )
    (*SiteVisitVP)[ ivis ] = ivis ;

  if ( VisitOrder == ORDER_RANDOM )
    {
      RandomPermutationAlgo( (*SiteVisitVP) , Npt ) ;
    }

  return STS_OK ;
}






/* ------------------------------------------------------------------- */
static int  MakeErrinfo
         ( 
             const char* RefName,       /* I : filename of reference class */
             int         N,             /* I : number of objects */
             int         Kc,            /* I : user number of classes */
	     TieET       TieRule,       /* I : specified MAP tie rule */
	     ErrinfoT*   ErrinfoP,      /* O and allocated */
	     ErrcurT*    ErrcurP        /* O and allocated */
         )
/* ------------------------------------------------------------------- */
{
  StatusET err ;
  int      *tmpV = 0 ;
  int      ipt ;

  if ( strcmp( RefName, "" ) != 0 ) {
    ErrinfoP->Kc = Kc ;


    /* Check all reference labels ok */
    for ( ipt = 0, err = STS_OK ; 
	  ( ipt < N ) && ( err == STS_OK ) ; ipt ++ ) {
      if ( ( tmpV[ ipt ] <= 0 ) || ( tmpV[ ipt ] > ErrinfoP->Kr ) ) {
	Rprintf("Reference class for point %d not in 1..%d \n", 
		 ipt + 1, ErrinfoP->Kr ) ;
	err = STS_E_FILE ;
      }
    }
    GenFree( tmpV ) ; tmpV = NULL ;
    if ( err != STS_OK )
      return err ;

    /* Compute greatest number of classes and permutations of classes */
    ErrinfoP->Km = ( ErrinfoP->Kc > ErrinfoP->Kr ) ? 
      ErrinfoP->Kc : ErrinfoP->Kr ;

    ErrinfoP->Kmfac = factorial( ErrinfoP->Km ) ;

    ErrinfoP->TieRule = TieRule ;

    compute_permutations( 0, ErrinfoP->Km, & ErrinfoP->Perm_Kmfac_Km ) ;

    /* Allocate and initialize later computed stuff */
    if ( ( ErrcurP->Agree_Km_Km = 
	   GenAlloc( ErrinfoP->Km * ErrinfoP->Km, sizeof( float ),
		     0, "MakeErrinfo", "Agree_Km_Km" ) ) == NULL )
        return STS_E_MEMORY ;

    if ( ( ErrcurP->Loclas_N_Kc = 
	   GenAlloc( N * ErrinfoP->Kc, sizeof( float ),
		     0, "MakeErrinfo", "Loclas_N_Kc" ) ) == NULL )
        return STS_E_MEMORY ;

    ErrcurP->Ibestpermut = -1 ;
    ErrcurP->Errorrate   = -2.0 ;

    return STS_OK ;
  }
  else {
    ErrinfoP->Kr = (int)NULL ;
    ErrinfoP->Refclas_N_Kr = NULL ;
    ErrcurP->Errorrate     = -1.0 ;
    return STS_OK ;
  }

}  /* end of MakeErrinfo() */





/* ------------------------------------------------------------------- */
static int factorial(int n)
/* ------------------------------------------------------------------- */
{
  int result = 1;

  for (; n>0; n--)
    result *= n;

  return result;
}

/* ------------------------------------------------------------------- */
static int compute_permutations    /* ret 0 if OK, -1 if memory error */
(
 const int Startval,         /* I : start value of integer suite */
 const int K,                /* I : size of integer suite to permute > 0 */
 int*      perms_Kfact_K_p[] /* O : matrix to store permuted values */
)
/* ------------------------------------------------------------------- */
{
  int   ik ;      /* current index and value of integer suite : 0..K-1 */
  int* array_K ; /* integer suite to permute : [ 0 ... (K-1) ] */
  int   Kfact ;   /* K! */
  int   err ;     /* 0 if OK, -1 if memory error */

  if ( K <= 0 )
    return 1 ;

  Kfact = factorial( K ) ;

  if( ( (*perms_Kfact_K_p) = malloc( Kfact * K * sizeof( int ) ) ) == NULL )
    return -1 ;

  if( ( array_K = malloc( K * sizeof( int ) ) ) == NULL ) {
    free( (*perms_Kfact_K_p) ) ;
    (*perms_Kfact_K_p) = NULL ;
    return -1 ;
  }

  for ( ik = 0 ; ik < K ; ik ++ )
    array_K[ ik ] = Startval + ik ;

  err = rec_permutations( array_K, K, K, 0, (*perms_Kfact_K_p) ) ;

  free( array_K ) ;

  return err ;
}


/* ------------------------------------------------------------------- */
static int rec_permutations        /* ret 0 if OK, -1 if memory error */
(
 const int array_A[],       /* I : remaining array to permute */
 const int A,               /* I : length of the remaining array : 0..K */
 const int K,               /* I : length of original array */
 int       offset,          /* I : first line of storage to use */
 int       perms_Kfact_K[]  /* O : matrix to store permuted values, use
			       lines :   offset -> offset + A! - 1
			       columns : K - A  -> K - 1 */
)
/* ------------------------------------------------------------------- */
{
  int    err ;         /* 0 if currently OK */
  int    ia ;          /* array element currently removed : 0..A-1 */
  int    ja ;          /* array element currently copied : 0..A-1 (neq ia) */
  int    am1fact ;     /* (A-1)! */
  int    iam1fact ;    /* current perms line (without offset) : 0..am1fact-1 */
  int*   redarr_am1 ;  /* array with one element removed (A-1) */

  am1fact = factorial( A - 1 ) ;

  /* Check against out of bounds offset */
  if ( ( offset < 0 ) || ( factorial( K ) < ( offset + A * am1fact ) ) )
    return 1 ;
  
  if ( ( redarr_am1 = malloc( ( A - 1 ) * sizeof( int ) ) ) == NULL ) 
    return -1;

  /* For each element of given array */
  for ( ia = 0, err = 0 ; ( ia < A ) && ( err == 0 ) ; ia ++ ) {

    /* Copy (A-1)! times this element into the column ( K - A ) of perms, 
       starting from line ( offset + ia * (A-1)! ) (skip previous ia's) */
    for ( iam1fact = 0 ; iam1fact < am1fact ; iam1fact ++ ) 
      perms_Kfact_K[ ( offset + ia * am1fact + iam1fact ) * K + ( K - A ) ] =
	array_A[ ia ] ;

    /* Copy array into reduced array without this element */
    for ( ja = 0 ; ja < ia ; ja ++ )
      redarr_am1[ ja ] = array_A[ ja ] ;
    for ( ja = ia + 1 ; ja < A ; ja ++ )
      redarr_am1[ ja - 1 ] = array_A[ ja ] ;

    /* Recursive call with reduced array */
    err = rec_permutations( redarr_am1, A-1, K, offset + ia * am1fact,
			    perms_Kfact_K ) ;
  }

  free( redarr_am1 ) ;
  return err ;
}




/* ------------------------------------------------------------------- */
static int  SetImageNeigh
            (
                NeighET     NeighSpec,          /* I */
                char*       NeiDescS,           /* O [LEN_LINE+1] */
                NeighDataT* NeighDataP          /* O and allocated */
            )
/* ------------------------------------------------------------------- */
{
    INeighT   *neighV;  /* to be allocated */

    switch( NeighSpec )
    {
    case NEIGH_FOUR :
        if ( ( neighV = GenAlloc( 4, sizeof( INeighT ),
				  0, "SetImageNeigh", "neighV" ) ) == NULL )
        {
            Rprintf( "Could not allocate %d image neighbours\n",
                     4 ) ;
            return STS_E_MEMORY ;
        }
        NeighDataP->Image.NeighsV = neighV ;
        NeighDataP->Image.NbNeigh = 4 ;

        neighV[ 0 ].Dl = -1 ;
        neighV[ 0 ].Dc = 0 ;
        neighV[ 0 ].Weight = 1.0 ;

        neighV[ 1 ].Dl = 0 ;
        neighV[ 1 ].Dc = -1 ;
        neighV[ 1 ].Weight = 1.0 ;

        neighV[ 2 ].Dl = 0 ;
        neighV[ 2 ].Dc = +1 ;
        neighV[ 2 ].Weight = 1.0 ;

        neighV[ 3 ].Dl = +1 ;
        neighV[ 3 ].Dc = 0 ;
        neighV[ 3 ].Weight = 1.0 ;

        strncpy( NeiDescS , 
                 "  Default 1st-order neighbors (horizontal and vertical)\n",
                 LEN_LINE ) ;
        break ;

    default :
        Rprintf( "Unknown neighborhood type (%d)\n", NeighSpec ) ;
        return STS_E_FUNCARG ;
    }

    return STS_OK ;
}   /* end of SetNeigh() */






  /*=================================================================*/
 /*ajout de Philippe Hup�*/
 /*interface avec R*/



static int GetInputParaPHUPE
    (
          DataT         *DataP,         /* O and allocated */
          NemParaT      *NemParaP,      /* O */
          SpatialT      *SpatialP,      /* O and allocated */
          StatModelT    *StatModelP,    /* O and allocated */
	  ErrinfoT      *ErrinfoP,      /* O and allocated */
	  ErrcurT       *ErrcurP,       /* O and allocated */
          float         **ClassifMP     /* O and allocated */
    )
/* ------------------------------------------------------------------- */
{
    StatusET    err ;
    /*char        datadescS[ LEN_LINE + 1 ] ;*/
    char        neidescS[ LEN_LINE + 1 ] ;



    /* Allocate and set sites visit order */
    if ( ( err = SetVisitOrder( DataP->NbPts,   /*V1.04-e*/
				NemParaP->VisitOrder,
				& DataP->SiteVisitV ) ) != STS_OK )
      return err ;



    /* Eventually read reference class file */  /*V1.04-f*/
    if ( ( err = MakeErrinfo( NemParaP->RefName, DataP->NbPts,
			      StatModelP->Spec.K, NemParaP->TieRule,
			      ErrinfoP, ErrcurP ) ) != STS_OK )
      return err ;

    /* Read neighborhood file */
    if ( SpatialP->Type != TYPE_NONSPATIAL )
    {
    	Rprintf("\n************************************************\n" ) ;
	Rprintf("*** Spatial Classification with EM algorithm ***\n" ) ;
	Rprintf("************************************************\n\n" ) ;
        if ( ( err = ReadNeiFilePHUPE( NemParaP->NeighSpec , neidescS, SpatialP ) ) != STS_OK )
                return err ;
    }
    else
    {
        StatModelP->Para.Beta = 0.0 ; /*V1.06-a*/
        SpatialP->MaxNeighs   = 0 ;
    }

    Rprintf( "\nData : " ) ;
    

    Rprintf( "  nb points   = %10d\n", DataP->NbPts ) ;
    if ( SpatialP->Type == TYPE_IMAGE )
    {
     Rprintf( "  grid size =  %4d rows, %4d columns\n", SpatialP->NeighData.Image.Nc,
             SpatialP->NeighData.Image.Nl ) ;
    }
    if ( DataP->NbMiss > 0 ) /*V1.05-a*/
    {
    Rprintf( "  %d missing values / %d\n",
	     DataP->NbMiss, DataP->NbPts * DataP->NbVars ) ;
    }

    Rprintf("\n");

    if ( SpatialP->Type != TYPE_NONSPATIAL )
    {
    Rprintf( "Neighborhood system :\n  max neighb =  %10d\n",
             SpatialP->MaxNeighs ) ;
    Rprintf( "%s\n", neidescS ) ;
    }

    Rprintf( "\n" ) ;
    Rprintf( "NEM parameters :\n" ) ;
    Rprintf( "  beta       =  %10.2f   |   nk                    = %3d\n",
             StatModelP->Para.Beta, StatModelP->Spec.K) ;
    Rprintf( "\n" ) ;


    return STS_OK ;

} /* end of GetInputParaPHUPE() */





/* ------------------------------------------------------------------- */
static int  ReadNeiFilePHUPE
         (
             NeighET     NeighSpec,          /* I */
	     char*       NeiDescS,           /* O [LEN_LINE+1] */
             SpatialT    *SpatialP           /* I/O and allocated */
         )
/* ------------------------------------------------------------------- */
{

    	StatusET    err = STS_OK ;


       	err = SetImageNeigh( NeighSpec, NeiDescS, &SpatialP->NeighData ) ;
        SpatialP->MaxNeighs = SpatialP->NeighData.Image.NbNeigh ;


    return err ;

}  /* end of ReadNeiFilePHUPE() */



static int SaveResultsPHUPE
        (
          const int          Npt,                   /* I */
          const int          Nd,                    /* I */
          const float*       ClassifM,              /* I */
          const SpatialT*    SpatialP,              /* I */
          const NemParaT*    NemParaP,              /* I */
          const StatModelT*  ModelP,                /* I */
	  const CriterT*     CriterP ,               /* I */ /*V1.03-d*/
	  int *classificationR
        )
/* ------------------------------------------------------------------- */
{
    int         ipt ;           /* point counter : 0..Npt */
    int         nk = ModelP->Spec.K ;
    StatusET    err = STS_OK ;



    if ( NemParaP->Format == FORMAT_HARD )
      {
        int*   kmaxesV;  /* classes having same maximum probabilites */

        if ( ( kmaxesV = GenAlloc( nk, sizeof( int ),
				   0, "SaveResultsPHUPE", "kmaxesV" ) ) == NULL )
            return STS_E_MEMORY ;

        /* For each record */
        for ( ipt = 0 ; ( ipt < Npt ) && ( err == STS_OK ) ; ipt ++ )
          {

            /* Compute its MAP class */
            classificationR[ipt] = ComputeMAP( ClassifM, ipt, nk, NemParaP->TieRule,
			       kmaxesV ) +1;

          } /* end for ( ipt ... ) */


        GenFree( kmaxesV ) ;

      } /* end if Format == HARD */


    return err ;

}

 /* end of SaveResultsPHUPE() */


void nem (float *vector, int *Nl, int *Nc, int *Nvars, int *Nclasses, float *beta, int *iters, int *res)
{
    	static  DataT           Data = {0} ;
             static  SpatialT        Spatial = {{{0}}} ;
	static  StatModelT      StatModel = {{0}} ;
	static  NemParaT        NemPara = {0} ;
	static  float           *ClassifM = 0;
	CriterT                 Criteria = {0} ;


	/*Settings of algorithm parameters*/
	NemArgsPHUPE(Nl,Nc,Nvars,Nclasses,&StatModel,&NemPara,&Data,&Spatial.NeighData.Image,&Spatial.Type);
	StatModel.Para.Beta = *beta;
	NemPara.NbIters = *iters;

	/*data values*/
	Data.PointsM = vector;
	Data.NbMiss = 0 ;

	GetInputParaPHUPE (&Data, &NemPara, &Spatial, &StatModel,&Criteria.Errinfo,&Criteria.Errcur, &ClassifM);

	srand( (unsigned) NemPara.Seed ) ;

	ClassifM =  GenAlloc( Data.NbPts * StatModel.Spec.K, sizeof( float ), 0, "nem", "ClassifMP" ) ;
	
	ClassifyByNem( &NemPara, &Spatial, &Data, &StatModel, ClassifM, &Criteria ) ;

	Rprintf("\n");

	SaveResultsPHUPE(Data.NbPts,Data.NbVars,ClassifM, &Spatial, &NemPara, &StatModel, &Criteria, res);

	/*FreeAllocatedData( &Data, &Spatial, &StatModel.Para, &Criteria, ClassifM ) ;*/
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~ END OF FILE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */




