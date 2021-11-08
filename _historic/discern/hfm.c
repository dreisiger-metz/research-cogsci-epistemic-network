/* File: hfm.c
 *
 * Hierarchical feature maps in DISCERN
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

void presentmem(int mod, int slots[]);
void classify();
void classify_and_retrieve();
void form_inpvector(int slots[]);
void form_outvector(int mod);
void hfm_init();
void hfm_read_params(FILE *fp);
void hfm_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp);
void hfm_null_values();
void hfm_null_prevvalues();

void readfun(FILE *fp, float *place, float par1, float par2);
float distance(float v1[], float v2[], int nrep);
float seldistance(int indeces[], float v1[], float v2[], int ncomp);
void store_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS],
		 int dim, int indeces[]);
void retrieve_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS],
		    int dim, int indeces[]);
void collect_stats(int modi, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int nwords);
void display_hfm_values();
void display_hfm_error(int neti, float outvector[], int slots[]);
void display_hfm_top();
void display_hfm_sub(int i, int j);
void handle_events();
void wait_for_run();
void printcomment(char *beginning, char *s, char *ending);
void fgl(FILE *fp);

void presentmem(int mod, int slots[])
{
  int i;

  form_inpvector(slots);

  if(mod==STOREMOD)
    {
      classify();
      store_trace(bunits[besti0][bestj0][besti1][bestj1],
		  nblines[besti0][bestj0][besti1][bestj1],
		  indeces2[besti0][bestj0][besti1][bestj1]);
    }
  else if(mod==RETMOD)
    classify_and_retrieve();

  form_outvector(mod);
  if (!(mod==RETMOD && bestvalue<aliveact))
    collect_stats(mod, outvector, nslot, slots, swords, nsrep, nswords);
  if (displaying) 
    {
      if (stepping) wait_for_run();
      handle_events();
      display_hfm_error(mod, outvector, slots);
    }
  if(chained)
    for(i=0; i<nslotrep; i++)
      slotrep[i]= outvector[i];	 /* activate result */
}


void classify()
{
  register int i,j;
  float best=LARGEFLOAT, worst=(-1.0);

  printcomment("\n", "storing into episodic memory:", "");

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      hfm_null_values();
      display_hfm_values();
    }
  

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      {
	hfmunits[i][j].prevvalue = hfmunits[i][j].value;
	hfmunits[i][j].value =
	  distance(inpvector,
		   hfmunits[i][j].comp,
		   nslotrep);
      /* check if this unit's response is best so far encountered */
	if (hfmunits[i][j].value < best)
	  {
	    besti0=i; bestj0=j;
	    best=hfmunits[i][j].value;
	  }
	if (hfmunits[i][j].value > worst) worst=hfmunits[i][j].value;
      }

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
	hfmunits[i][j].value = (worst - hfmunits[i][j].value)/(worst-best);
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_hfm_top();
    }

  best=LARGEFLOAT;  worst=(-1.0);
  for(i=0; i<subnets; i++)
    for(j=0; j<subnets; j++)
      {
	subunits[besti0][bestj0][i][j].prevvalue =
	  subunits[besti0][bestj0][i][j].value;
	subunits[besti0][bestj0][i][j].value =
	  seldistance(indeces[besti0][bestj0], inpvector,
		      subunits[besti0][bestj0][i][j].comp,
		      nsublines[besti0][bestj0]);
	if (subunits[besti0][bestj0][i][j].value < best)
	  {
	    besti1=i; bestj1=j;
	    best=subunits[besti0][bestj0][i][j].value;
	  }
	if (subunits[besti0][bestj0][i][j].value > worst)
	  worst=subunits[besti0][bestj0][i][j].value;
      }
  for(i=0; i<subnets; i++)
    for(j=0; j<subnets; j++)
	subunits[besti0][bestj0][i][j].value =
	  (worst - subunits[besti0][bestj0][i][j].value)/(worst-best);
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_hfm_sub(besti0, bestj0);
    }

  best=LARGEFLOAT;
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      {
	bunits[besti0][bestj0][besti1][bestj1][i][j].prevvalue =
	  bunits[besti0][bestj0][besti1][bestj1][i][j].value;
	bunits[besti0][bestj0][besti1][bestj1][i][j].value =
	  seldistance(indeces2[besti0][bestj0][besti1][bestj1], inpvector,
		      bunits[besti0][bestj0][besti1][bestj1][i][j].comp,
		      nblines[besti0][bestj0][besti1][bestj1]);
	if (bunits[besti0][bestj0][besti1][bestj1][i][j].value < best)
	  {
	    besti2=i; bestj2=j;
	    best=bunits[besti0][bestj0][besti1][bestj1][i][j].value;
	  }
      }
}

void classify_and_retrieve()
{
  int i,j,ii,jj,ci0,cj0,ci1,cj1,ci2,cj2;
  float hfmbest=LARGEFLOAT, hfmworst=(-1.0);
  float subbest=LARGEFLOAT, subworst=(-1.0);
  float bestcvalue=(-2.0);
  char s[MAXSTRL];

  printcomment("\n", "retrieving from episodic memory:", "");

  hfm_null_values();
  if(displaying) display_hfm_values();

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      {
	hfmunits[i][j].prevvalue= hfmunits[i][j].value;
	hfmunits[i][j].value= distance(inpvector,hfmunits[i][j].comp,nslotrep);
/*	sprintf(s, "hfmunit (%d,%d): %.3f", i, j, hfmunits[i][j].value);
	printcomment("\n", s, "");*/
	if (hfmunits[i][j].value < hfmbest)  hfmbest=hfmunits[i][j].value;
	if (hfmunits[i][j].value > hfmworst) hfmworst=hfmunits[i][j].value;
      }

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
	hfmunits[i][j].value =
	  (hfmworst - hfmunits[i][j].value)/(hfmworst-hfmbest);
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_hfm_top();
    }

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      if(hfmworst-(hfmworst-hfmbest)*hfmunits[i][j].value < hfmsearch
	 && nsublines[i][j]>0)
	{
	  for(ii=0; ii<subnets; ii++)
	    for(jj=0; jj<subnets; jj++)
	      {
		subunits[i][j][ii][jj].prevvalue =
		  subunits[i][j][ii][jj].value;
		subunits[i][j][ii][jj].value =
		  seldistance(indeces[i][j], inpvector,
			      subunits[i][j][ii][jj].comp,
			      nsublines[i][j]);
/*		sprintf(s, "subunit (%d,%d), (%d,%d): %.3f", i, j, ii, jj,
			subunits[i][j][ii][jj].value);
		printcomment("\n", s, "");*/
		if (subunits[i][j][ii][jj].value < subbest)
		  subbest=subunits[i][j][ii][jj].value;
		if (subunits[i][j][ii][jj].value > subworst)
		  subworst=subunits[i][j][ii][jj].value;
	      }
	  
	  for(ii=0; ii<subnets; ii++)
	    for(jj=0; jj<subnets; jj++)
	      subunits[i][j][ii][jj].value =
		(subworst - subunits[i][j][ii][jj].value)/(subworst-subbest);
	  if (displaying)
	    {
	      if (stepping) wait_for_run();
	      handle_events();
	      display_hfm_sub(i,j);
	    }
	  
	  for(ii=0; ii<subnets; ii++)
	    for(jj=0; jj<subnets; jj++)
	      if(subworst-(subworst-subbest)*subunits[i][j][ii][jj].value
		 < subsearch && nblines[i][j][ii][jj]>0)
		{
		  sprintf(s, "\n%smap (%d,%d), (%d,%d):",
			  BEGIN_COMMENT, i, j, ii, jj);
		  if (babbling) printf("%s", s);
		  besti0=i; bestj0=j; 
		  besti1=ii; bestj1=jj; 		  
		  retrieve_trace(bunits[i][j][ii][jj],
				 nblines[i][j][ii][jj],
				 indeces2[i][j][ii][jj]);
		  if (babbling) printf("%s", END_COMMENT);
		  if(bestvalue>bestcvalue)
		    {
		      ci0=i; cj0=j;
		      ci1=ii; cj1=jj;
		      ci2=besti2; cj2=bestj2;
		      bestcvalue=bestvalue;
		    }		  
		}
	}
  
  besti0=ci0; bestj0=cj0; 
  besti1=ci1; bestj1=cj1; 
  besti2=ci2; bestj2=cj2; 
  bestvalue=bestcvalue;
}
  
void form_inpvector(int slots[])
{
  register int i,j;

  if(chained)
    for(i=0; i<nslotrep; i++)
      inpvector[i] = slotrep[i];	/* use current story */
  else
    for(i=0; i<nslot; i++)
      for(j=0; j<nsrep; j++)
	inpvector[i*nsrep+j] = swords[slots[i]].rep[j];
}


void form_outvector(int mod)
{
  int i, babbling_save;
  char s[MAXSTRL];

  if(mod==RETMOD && bestvalue<aliveact)
    {
      if(babbling || print_mistakes)
	{
	  babbling_save=babbling;
	  babbling=1;
	  printcomment("\n", "oops: no image found", "\n");
	  babbling=babbling_save;
	}
      for(i=0; i<nslotrep; i++)
	outvector[i] = 0.0;
    }
  else
    {
      sprintf(s, "image units (%d,%d), (%d,%d), (%d,%d):",
	      besti0, bestj0, besti1, bestj1, besti2, bestj2);
      printcomment("\n", s, "\n");
      for(i=0; i<nslotrep; i++)
	outvector[i] = hfmunits[besti0][bestj0].comp[i];
      for (i=0; i<nsublines[besti0][bestj0]; i++)
	outvector[ indeces[besti0][bestj0][i] ] =
	  subunits[besti0][bestj0][besti1][bestj1].comp[i];
      for (i=0; i<nblines[besti0][bestj0][besti1][bestj1]; i++)
	outvector[ indeces2[besti0][bestj0][besti1][bestj1][i] ] =
	  bunits[besti0][bestj0][besti1][bestj1][besti2][bestj2].comp[i];
    }
}

/*********************  initializations ******************************/

void hfm_init()
{
  int doo, i,j,ii,jj,k;
  FILE *fp;

  if((fp=fopen(hfmfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", hfmfile);
       exit(1);
     }
  printf("Reading hierarchical feature maps from %s...", hfmfile);
  hfm_read_params(fp);
  fscanf(fp,"%d",&doo);
  hfm_iterate_weights(readfun, fp);
  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      {
	fscanf(fp,"%d", &nsublines[i][j]);
/* 	    printf("Unit %d, %d passes %d lines\n",i,j,nsublines[i][j]); */
	for(k=0; k<nsublines[i][j]; k++)
	  fscanf(fp,"%d", &indeces[i][j][k]);
      }
  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(ii=0; ii<subnets; ii++)
	for(jj=0; jj<subnets; jj++)
	  {
	    fscanf(fp,"%d\n", &nblines[i][j][ii][jj]);
/*		printf("Unit %d, %d, %d, %d passes %d lines\n",i,j,ii,jj,nblines[i][j][ii][jj]);*/
	    for(k=0; k<nblines[i][j][ii][jj]; k++)
	      fscanf(fp,"%d\n", &indeces2[i][j][ii][jj][k]);
	  }
  printf("Done.\n");
}

void hfm_read_params(FILE *fp)
{
  char s[MAXSTRL];
  register int i;
  int doo;
  float foo;
  /* simulation parameters */
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%s", hfminpfile); fgl(fp);
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%d %d %d %d %f %f %f %f %f %f",
	 &doo,&doo,&doo,&doo,&foo,&foo,&foo,&foo,&foo,&foo); fgl(fp);
  fscanf(fp,"%d%d%d%d%d%f%f", &hfmnets,&doo,&doo,&doo,&doo,&foo,&foo); fgl(fp);
  fscanf(fp,"%d%d%d%d%d%d%f%f", &subnets,&doo,&doo,&doo,&doo,&doo,&foo,&foo);
  fgl(fp);
  fscanf(fp,"%d%d%d%d%d%d%f%f",	 &bnets,&doo,&doo,&doo,&doo,&doo,&foo,&foo);
  fgl(fp);
  
  /* saving info */
  fscanf(fp,"%d", &doo);
  for(i=0; i<MAXSNAPS && doo<SNAPSHOTEND; i++)
    fscanf(fp,"%d", &doo);
  fgl(fp);
}


void hfm_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp)
{
  register int i,j,k,ii,jj,iii,jjj;

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(k=0; k<nslotrep; k++)
        (*dofun)(fp, &hfmunits[i][j].comp[k],0,0);

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(ii=0; ii<subnets; ii++)
	for(jj=0; jj<subnets; jj++)
	  for(k=0; k<nslotrep; k++)
	    (*dofun)(fp, &subunits[i][j][ii][jj].comp[k],0,0);

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(ii=0; ii<subnets; ii++)
	for(jj=0; jj<subnets; jj++)
	  for(iii=0; iii<bnets; iii++)
	    for(jjj=0; jjj<bnets; jjj++)
	      for(k=0; k<nslotrep; k++)
		(*dofun)(fp, &bunits[i][j][ii][jj][iii][jjj].comp[k],0,0);
}  

void hfm_null_values()
{
  register int i,j,ii,jj,iii,jjj;

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      {
	hfmunits[i][j].prevvalue=hfmunits[i][j].value;
	hfmunits[i][j].value=0.0;
	for(ii=0; ii<subnets; ii++)
	  for(jj=0; jj<subnets; jj++)
	    {
	      subunits[i][j][ii][jj].prevvalue=subunits[i][j][ii][jj].value;
	      subunits[i][j][ii][jj].value=0.0;
	      for(iii=0; iii<bnets; iii++)
		for(jjj=0; jjj<bnets; jjj++)
		  {
		    bunits[i][j][ii][jj][iii][jjj].prevvalue=
		      bunits[i][j][ii][jj][iii][jjj].value;
		    bunits[i][j][ii][jj][iii][jjj].value=0.0;
		  }
	    }
      }
}

void hfm_null_prevvalues()
{
  register int i,j,ii,jj,iii,jjj;

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      {
	hfmunits[i][j].prevvalue=-1.0;
	for(ii=0; ii<subnets; ii++)
	  for(jj=0; jj<subnets; jj++)
	    {
	      subunits[i][j][ii][jj].prevvalue=-1.0;
	      for(iii=0; iii<bnets; iii++)
		for(jjj=0; jjj<bnets; jjj++)
		  bunits[i][j][ii][jj][iii][jjj].prevvalue=-1.0;
	    }
      }
}
