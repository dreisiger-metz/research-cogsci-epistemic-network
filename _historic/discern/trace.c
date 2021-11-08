/* File: trace.c
 *
 * Trace feature maps in DISCERN
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

void store_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim, int indeces[]);
void retrieve_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim, int indeces[]);
void compute_lat_responses(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim,int indeces[]);
float mem_unit_resp(int indeces[], struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS],int ui, int uj, int dim);
float ext_resp(int indeces[], float comp[], int dim);
float sigmoid(float activity);
void clear_traces();

void display_hfm_b(int i, int j, int ii, int jj);
void handle_events();
void wait_for_run();
void printwordout(char s[], char ss[]);


void store_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim, int indeces[])
{
  register int i,j, ii, jj;
  int lowi, lowj, highi, highj, foobesti2, foobestj2;
  float lowest, highest;

  /* need to use a different activity function
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      table[i][j].value = sigmoid(ext_resp(indeces, table[i][j].comp, dim));*/

  if (besti2<tracenc)
    foobesti2=tracenc;
  else if(besti2>bnets-1-tracenc)
    foobesti2=bnets-1-tracenc;
  else
    foobesti2=besti2;

  if (bestj2<tracenc)
    foobestj2=tracenc;
  else if(bestj2>bnets-1-tracenc)
    foobestj2=bnets-1-tracenc;
  else
    foobestj2=bestj2;

  lowi=foobesti2-tracenc;
  highi=foobesti2+tracenc;
  lowj=foobestj2-tracenc;
  highj=foobestj2+tracenc;
  
  lowest=LARGEFLOAT;
  highest=(-1.0);
  for(i=lowi; i<=highi; i++)
    for(j=lowj; j<=highj; j++)
      {
	if (table[i][j].value < lowest)
	  lowest=table[i][j].value;
	if (table[i][j].value > highest)
	  highest=table[i][j].value;
      }
  
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      if(i>=lowi && i<=highi && j>=lowj && j<=highj)
	table[i][j].value = 1.0-(table[i][j].value-lowest)/(highest-lowest);
      else
	table[i][j].value = 0.0;

  /* modify weights toward the input in a neighborhood of the maximally 
     responding unit */
  for(i=lowi; i<=highi; i++)
    for(j=lowj; j<=highj; j++)
      for(ii=0; ii<bnets; ii++)
	for(jj=0; jj<bnets; jj++)
	  if((ii>=lowi && ii<=highi && jj>=lowj && jj<=highj) &&
	     ((table[ii][jj].value > table[i][j].value+epsilon) ||
	      (ii == i && jj == j)))
	    table[i][j].latweights[ii][jj] = gammaexc*table[i][j].value;
	  else
	    table[i][j].latweights[ii][jj] = gammainh*table[i][j].value;

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();      
      display_hfm_b(besti0,bestj0,besti1,bestj1);
    }
}

void retrieve_trace(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim, int indeces[])
{
  register int i,j,ts;

  /* for each input, start with a blank network
     because the similarity measure is different */
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      {
	table[i][j].prevvalue = -1.0;
	table[i][j].value = 0.0;
      }
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();      
      display_hfm_b(besti0,bestj0,besti1,bestj1);
    }

  bestvalue=1.0;
  for(ts=1; (ts<=tsettle && bestvalue>=aliveact) || ts<=tsettle/2; ts++)
    compute_lat_responses(table,dim,indeces);
}

void compute_lat_responses(struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS], int dim,int indeces[])
{
  char s[MAXSTRL];
  register int i,j;
  float best=(-1.0);
  float mem_unit_resp();
  /* copy the current response to be the previous response */
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      table[i][j].prevvalue = table[i][j].value;

  /* compute the new responses */
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      {
	table[i][j].value = sigmoid(mem_unit_resp(indeces, table, i,j, dim));
	if (table[i][j].value >= best)
	  {
	    besti2=i; bestj2=j; best=table[i][j].value;
	  }
      }
  bestvalue=best;
  if(babbling)
    {
      sprintf(s, " ->(%d,%d):%.3f",besti2,bestj2,best);
      printwordout(s, "");
    }
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();      
      display_hfm_b(besti0,bestj0,besti1,bestj1);
    }
}

float mem_unit_resp(int indeces[], struct TRACEUNITDATA table[MAXTRACENETS][MAXTRACENETS],
int ui, int uj, int dim)
{
  /* compute the response of a single unit */
  register int i,j;
  float resp=0.0, ext_resp();

  /* external input activation */
  resp = ext_resp(indeces, table[ui][uj].comp, dim);

  /* these give different values!!
  printf("resp: %.10f\n",1.0 -
	 seldistance(indeces,inpvector, table[ui][uj].comp, dim)/
	 sqrt((float)dim));
  printf("resp: %.10f\n",ext_resp(indeces, table[ui][uj].comp, dim));*/

  /* lateral interaction */
  for(i=0; i<bnets; i++)
    for(j=0; j<bnets; j++)
      resp = resp + table[i][j].latweights[ui][uj] * table[i][j].prevvalue;
  return(resp);
}

float ext_resp(int indeces[], float comp[], int dim)
{
  float seldistance();
  if(dim==0) return(0.0);
  else
    return(1.0 - seldistance(indeces, inpvector, comp, dim)/sqrt((float)dim));
}

float sigmoid(float activity)
{
  /* transform the activity to a sigmoid response between 0 and maximum */
  return(1.0/(1.0+exp(maxact*(minact-activity))));
}

/*********************  initializations ******************************/

void clear_traces()
{
  int i,j,ii,jj,iii,jjj,li,lj;

  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(ii=0; ii<subnets; ii++)
	for(jj=0; jj<subnets; jj++)
	  for(iii=0; iii<bnets; iii++)
	    for(jjj=0; jjj<bnets; jjj++)
	      for(li=0; li<bnets; li++)
		for(lj=0; lj<bnets; lj++)
		  bunits[i][j][ii][jj][iii][jjj].latweights[li][lj] = gammainh;
}
