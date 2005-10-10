#include "nem_typ.h"    /* DataT, ... */



void NemArgsPHUPE
        (
	  int *Nrow,
	  int *Ncol,
	  int *Nvars,
	  int *Nclasses,
	  StatModelT    *StatModelP,    /* O */
          NemParaT      *NemParaP,      /* O */
	  DataT*        DataP,          /* O */
	  ImageNeighT*  ImageP,         /* O */
	  TypeET*       SpatialTypeP    /* O */
        );


extern const char *CritStrVC[ CRIT_NB ] ;
