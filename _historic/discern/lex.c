/* File: lex.c
 *
 * The lexicon of DISCERN
 * Risto Miikkulainen 5/7/93
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "discerndefs.h"
#include "globals.c"

/* Funtion prototypes */

void input_lexicon(int index);
void output_lexicon(int index);
void translate(int index, float inpwordrep[], int inpmod, int inpnets,
 struct LEXUNIT inpunits[MAXLSNETS][MAXLSNETS], struct WORDSTRUCT inpwords[],
 int inpnrep, int inpnwords, int outmod, int outnets, 
 struct LEXUNIT outunits[MAXLSNETS][MAXLSNETS], 
 struct WORDSTRUCT outwords[], int outnrep, int outnwords, 
 float assoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS]);
void lex_init();
void lex_read_params(FILE *fp);
void lex_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp);
void lex_clear_values(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets);
void lex_clear_prevvalues(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets);

void readfun(FILE *fp, float *place, float par1, float par2);
float distance(float v1[], float v2[], int nrep);
void collect_stats(int modi, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int nwords);
void display_lex(int neti, int nets, struct LEXUNIT units[MAXLSNETS][MAXLSNETS]);
void display_lex_error(int neti, int index, struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int besti, int bestj, int nwords, struct WORDSTRUCT words[], int nrep);
void handle_events();
void wait_for_run();
void fgl(FILE *fp);

void input_lexicon(int index)
{
  int i;
  float inpwordrep[MAXREP];

  for(i=0; i<nlrep; i++)
    inpwordrep[i]=lwords[index].rep[i];	/* form input */
  translate(index,inpwordrep,LINPMOD,lnets,lunits,lwords,nlrep,nlwords,
	    SOUTMOD,snets,sunits,swords,nsrep,nswords,lsassoc);
  if(chained)
    for(i=0; i<nsrep; i++)
      swordrep[i] = sunits[abesti][abestj].comp[i];	/* activate result */
}  
  
void output_lexicon(int index)
{
  int i;
  float inpwordrep[MAXREP];

  if(chained)
    for(i=0; i<nsrep; i++)
      inpwordrep[i]=swordrep[i];  /* use current sword */
  else
    for(i=0; i<nsrep; i++)
      inpwordrep[i]=swords[index].rep[i];
  translate(index,inpwordrep,SINPMOD,snets,sunits,swords,nsrep,nswords,
	    LOUTMOD,lnets,lunits,lwords,nlrep,nlwords,slassoc);
}  
  

void translate(int index, float inpwordrep[], int inpmod, int inpnets,
 struct LEXUNIT inpunits[MAXLSNETS][MAXLSNETS], struct WORDSTRUCT inpwords[],
 int inpnrep, int inpnwords, int outmod, int outnets, 
 struct LEXUNIT outunits[MAXLSNETS][MAXLSNETS], 
 struct WORDSTRUCT outwords[], int outnrep, int outnwords, 
 float assoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS])
{
  int i,j,ii,jj,besti,bestj;
  float best, second;
  int lowi, lowj, highi, highj,secondi,secondj,thirdi,thirdj,fourthi,fourthj;
  float highest,third,fourth;

  for(i=0; i<inpnets; i++)
    for(j=0; j<inpnets; j++)
      inpunits[i][j].prevvalue = inpunits[i][j].value;

  second=LARGEFLOAT;
  best=LARGEFLOAT;
  for(i=0; i<inpnets; i++)
    for(j=0; j<inpnets; j++)
      {
	inpunits[i][j].value = distance(inpwordrep, inpunits[i][j].comp, inpnrep);
	if (inpunits[i][j].value == best) second=best;
	if (inpunits[i][j].value < best)
	  {
	    besti=i; bestj=j; best=inpunits[i][j].value;
	  }
      }
  if (babbling && second == best)
    printf("\nWarning: input image for %s is not unique: %f\n", inpwords[index].chars, best);
  collect_stats(inpmod, inpunits[besti][bestj].comp, 1, &index, inpwords, inpnrep, inpnwords);

  for(ii=0; ii<outnets; ii++)
    for(jj=0; jj<outnets; jj++)
      {
	outunits[ii][jj].prevvalue=outunits[ii][jj].value;
	outunits[ii][jj].value=0.0;
      }
  
  if (besti-ANC>=0) lowi=besti-ANC;
  else lowi=0;
  if (besti+ANC<inpnets) highi=besti+ANC;
  else highi=inpnets-1;
  if (bestj-ANC>=0) lowj=bestj-ANC;
  else lowj=0;
  if (bestj+ANC<inpnets) highj=bestj+ANC;
  else highj=inpnets-1;

  fourth=third=second=best=LARGEFLOAT;
  highest=(-1.0);
  for(i=lowi; i<=highi; i++)
    for(j=lowj; j<=highj; j++)
      {
	if (inpunits[i][j].value < best)
	  {
	    fourthi=thirdi; fourthj=thirdj; fourth=third;
	    thirdi=secondi; thirdj=secondj; third=second;
	    secondi=besti; secondj=bestj; second=best;
	    besti=i; bestj=j; best=inpunits[i][j].value;
	  }
	else if (inpunits[i][j].value < second)
	  {
	    fourthi=thirdi; fourthj=thirdj; fourth=third;
	    thirdi=secondi; thirdj=secondj; third=second;
	    secondi=i; secondj=j; second=inpunits[i][j].value;
	  }	    
	else if (inpunits[i][j].value < third)
	  {
	    fourthi=thirdi; fourthj=thirdj; fourth=third;
	    thirdi=i; thirdj=j; third=inpunits[i][j].value;
	  }	    
	else if (inpunits[i][j].value < fourth)
	  {
	    fourthi=i; fourthj=j; fourth=inpunits[i][j].value;
	  }	    
	if(inpunits[i][j].value>highest)
	  highest=inpunits[i][j].value;
      }
  
  for(i=0; i<inpnets; i++)
    for(j=0; j<inpnets; j++)
      if((i==besti && j==bestj) ||
	 (i==secondi && j==secondj) ||
	 (i==thirdi && j==thirdj) ||
	 (i==fourthi && j==fourthj))
	{
	  inpunits[i][j].value=1.0-(inpunits[i][j].value-best)/(highest-best);
	  for(ii=0; ii<outnets; ii++)
	    for(jj=0; jj<outnets; jj++)
	      outunits[ii][jj].value += inpunits[i][j].value * assoc[i][j][ii][jj];
	}
      else
	inpunits[i][j].value=0.0;
  
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();      
      display_lex(inpmod, inpnets, inpunits);
      display_lex_error(inpmod, index, inpunits, besti, bestj,
			inpnwords, inpwords, inpnrep);
    }

  second=(-1.0);
  best=(-1.0);
  for(ii=0; ii<outnets; ii++)
    for(jj=0; jj<outnets; jj++)
      {
	if(outunits[ii][jj].value==best) second=best;
	if(outunits[ii][jj].value>best)
	  {
	    abesti=ii; abestj=jj; best=outunits[ii][jj].value;
	  }
      }
  if (babbling && second == best)
    printf("\nWarning: assoc image for %s is not unique: %f\n", outwords[index].chars, best);
  collect_stats(outmod, outunits[abesti][abestj].comp, 1, &index, outwords, outnrep, outnwords);
  
  for(ii=0; ii<outnets; ii++)
    for(jj=0; jj<outnets; jj++)
      {
	outunits[ii][jj].value=outunits[ii][jj].value/best;
	if (outunits[ii][jj].value<0.1) /* avoid screen update */
	  outunits[ii][jj].value=0.0;
      }	

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();      
      display_lex(outmod, outnets, outunits);
      display_lex_error(outmod, index, outunits, abesti, abestj,
			outnwords, outwords, outnrep);
    }
}



/*********************  initializations ******************************/

void lex_init()
{
  int doo;
  FILE *fp;
  if((fp=fopen(lexfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", lexfile);
       exit(1);
     }
  printf("Reading lexicon from %s...", lexfile);
  lex_read_params(fp);
  fscanf(fp,"%d",&doo);
  lex_iterate_weights(readfun, fp);
  fclose(fp);
  printf("Done.\n");
}


void lex_read_params(FILE *fp)
{
  char s[100];
  register int i;
  int doo;
  float foo;
  /* simulation parameters */
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%s", s); fgl(fp);

  fscanf(fp,"%d %d %d %d", &doo,&doo,&doo,&doo); fgl(fp);
  fscanf(fp,"%f %f %f %f", &foo,&foo,&foo,&foo); fgl(fp);
  fscanf(fp,"%f %f %f %f", &foo,&foo,&foo,&foo); fgl(fp);
  fscanf(fp,"%d %d %d %d %d %d %f %f", &lnets,&doo,&doo,&doo,&doo,&doo,
	                               &foo,&foo); fgl(fp);
  fscanf(fp,"%d %d %d %d %d %d %f %f", &snets,&doo,&doo,&doo,&doo,&doo,
	                            &foo,&foo); fgl(fp);
  fscanf(fp,"%d %d %d %d %d %f %f", &doo,&doo,&doo,&doo,&doo,
	                            &foo,&foo); fgl(fp);
  
  /* saving info */
  fscanf(fp,"%d", &doo);
  for(i=0; i<MAXSNAPS && doo<SNAPSHOTEND; i++)
    fscanf(fp,"%d", &doo);
  fgl(fp);
}


void lex_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp)
{
  register int i,j,k,ii,jj;

  for(i=0; i<lnets; i++)
    for(j=0; j<lnets; j++)
      for(k=0; k<nlrep; k++)
	(*dofun)(fp, &lunits[i][j].comp[k],0,0);

  for(i=0; i<snets; i++)
    for(j=0; j<snets; j++)
      for(k=0; k<nsrep; k++)
	(*dofun)(fp, &sunits[i][j].comp[k],0,0);

  for(i=0; i<lnets; i++)
    for(j=0; j<lnets; j++)
      for(ii=0; ii<snets; ii++)
	for(jj=0; jj<snets; jj++)
	  {
	    (*dofun)(fp, &lsassoc[i][j][ii][jj],0,0);
	    (*dofun)(fp, &slassoc[ii][jj][i][j],0,0);
	  }  
}  

void lex_clear_values(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets)
{
  register int i,j;
  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      units[i][j].value = 0.0;
}

void lex_clear_prevvalues(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets)
{
  register int i,j;
  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      units[i][j].prevvalue = -1.0;
}
