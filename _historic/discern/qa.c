/* File: qa.c
 *
 * Fgrep modules of DISCERN
 * Risto Miikkulainen 2/28/93
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "discerndefs.h"
#include "globals.c"
#include "xglobals.c"

/* Funtion prototypes */

void parse_story();
void parse_sentence(struct SENTSTRUCT sent);
void gener_story();
void gener_sentence(struct SENTSTRUCT sent);
void formcue();
void produceanswer();
void forward_prop(int neti);
void qa_init();
void qa_read_params(FILE *fp);
void qa_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp, float par1,float par2,float par3,float par4);

void readfun(FILE *fp, float *place, float par1, float par2);
void display_labeled_layer(int neti, int nas, float rep[], int nums[], int x, int y, int labeloffset);
void display_layer(int neti, int nas, float layer[], int layerx, int layery, int nrep);
void display_assembly(int neti, int startx, int starty, float assembly[], int nrep);
void display_sequence(int neti, int x, int y);
void display_error(int neti, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int step);
void collect_stats(int modi, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int nwords);
void printwordout(char s[], char ss[]);
void printcomment(char *beginning, char *s, char *ending);
void input_lexicon(int index);
void output_lexicon(int index);
void handle_events();
void wait_for_run();
void fgl(FILE *fp);
int find_nearest_s(float rep[], struct WORDSTRUCT words[], int nrep, int nwords);

void parse_story()
{
  register int i,j,k, senti, step=0, neti=STORYPARSMOD;
  
  printcomment("\n", "parsing input story:", "\n");
  for(i=0; i<nslot; i++) targets[neti][i]=story.slots[i];
  for(i=0; i<nslot; i++)
    for(j=0; j<nsrep; j++)
      tchrep[neti][i*nsrep+j] = swords[targets[neti][i]].rep[j];
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, net[neti].noutitem, tchrep[neti],
			    targets[neti],
			    net[neti].tchx, net[neti].tchy, BELOW);
    }

  for(i=0; i<nhidrep[neti]; i++)
    prevhidrep[neti][i]=0;
  for(senti=0; senti<nsent; senti++)
    if(story.sents[senti].included>0)
      {
	parse_sentence(story.sents[senti]);
	for(i=0; i<ncase; i++)
	  inputs[neti][i]=story.sents[senti].caseroles[i];
	if (chained)
	  for(i=0; i<ninpunits[neti]; i++)
	    inprep[neti][i] = caserep[i];	/* use active sentence */
	else
	  for(i=0; i<ncase; i++)
	    for(j=0; j<nsrep; j++)
	      inprep[neti][i*nsrep+j] =	swords[inputs[neti][i]].rep[j];
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_labeled_layer(neti, net[neti].ninpitem, inprep[neti],
				  inputs[neti],
				  net[neti].inpx, net[neti].inpy, ABOVE);
	    display_assembly(neti, net[neti].prevx, net[neti].prevy,
			     prevhidrep[neti], nhidrep[neti]);
	  }
	forward_prop(neti);
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].hidx, net[neti].hidy,
			     hidrep[neti], nhidrep[neti]);
	    display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
				  targets[neti],
				  net[neti].outx, net[neti].outy, BELOW2);
	    display_error(neti, outrep[neti], net[neti].noutitem,
			  targets[neti], swords, nsrep, ++step);
	  }
	for(k=0; k<nhidrep[neti]; k++)
	  prevhidrep[neti][k]=hidrep[neti][k];
      }
  printcomment("\n", "into internal rep:", "\n");
  collect_stats(neti, outrep[neti], nslot, targets[neti],
		swords, nsrep, nswords);

  if (chained)
    for (i=0; i<nslotrep; i++)
      slotrep[i] = outrep[neti][i]; /* activate result */
}

void parse_sentence(struct SENTSTRUCT sent)
{
  int i,j,k, wordi, step=0, neti=SENTPARSMOD;
  sprintf(net[neti].sequence, "%s", "");
  sprintf(net[neti].newitem, "%s", "");
  
  for(i=0; i<ncase; i++)
    targets[neti][i]=sent.caseroles[i];
  for(i=0; i<ncase; i++)
    for(j=0; j<nsrep; j++)
      tchrep[neti][i*nsrep+j] = swords[targets[neti][i]].rep[j];
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, net[neti].noutitem, tchrep[neti],
			    targets[neti],
			    net[neti].tchx, net[neti].tchy, BELOW);
    }
  
  for(i=0; i<nhidrep[neti]; i++)
    prevhidrep[neti][i]=0;
  for(wordi=0; wordi<nword; wordi++)
    if(sent.words[wordi]>(-1))
      {
	if (withlex)
	  input_lexicon(sent.words[wordi]);
	else
	  if(babbling) printwordout(swords[sent.words[wordi]].chars, " ");
	inputs[neti][0]=sent.words[wordi];
	if (chained && withlex)
	  for(i=0; i<nsrep; i++)
	    inprep[neti][i] = swordrep[i];	/* use active concept */
	else
	  for(i=0; i<nsrep; i++)
	    inprep[neti][i] = swords[inputs[neti][0]].rep[i];
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    sprintf(net[neti].sequence,"%s %s",
		    net[neti].sequence, net[neti].newitem);
	    display_sequence(neti, net[neti].inpx, net[neti].inpy);
	    display_labeled_layer(neti, net[neti].ninpitem, inprep[neti],
				  inputs[neti],
				  net[neti].inpx, net[neti].inpy, ABOVE);
	    display_assembly(neti, net[neti].prevx, net[neti].prevy,
			     prevhidrep[neti], nhidrep[neti]);
	  }
	    forward_prop(neti);
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].hidx, net[neti].hidy,
			     hidrep[neti], nhidrep[neti]);
	    display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
				  targets[neti],
				  net[neti].outx, net[neti].outy, BELOW2);
	    display_error(neti, outrep[neti], net[neti].noutitem,
			  targets[neti],
			  swords, nsrep, ++step);
	  }
	for(k=0; k<nhidrep[neti]; k++)
	  prevhidrep[neti][k]=hidrep[neti][k];
      }
  if(babbling) printf("\n");
  collect_stats(neti, outrep[neti], ncase, targets[neti],
		swords, nsrep, nswords);

  if(chained)
    for (i=0; i<ncaserep; i++)
      caserep[i] = outrep[neti][i]; /* activate result */
}


void gener_story()
{
  register int i,j,k,senti, step=0, neti=STORYGENMOD;
  
  printcomment("\n", "generating paraphrase:", "\n");
  for (i=0; i<nslot; i++) inputs[neti][i] = story.slots[i];
  for (i=0; i<nslot; i++)
    for(j=0; j<nsrep;j++)
      if(chained)
	inprep[neti][i*nsrep+j] = slotrep[i*nsrep+j]; /* use active story */
      else
	inprep[neti][i*nsrep+j] = swords[inputs[neti][i]].rep[j];

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, net[neti].ninpitem, inprep[neti],
			    inputs[neti],
			    net[neti].inpx, net[neti].inpy, BELOW);
    }

  for(i=0; i<nhidrep[neti]; i++)
    prevhidrep[neti][i]=0;
  for(senti=0; senti<nsent; senti++)
    if(story.sents[senti].included>(-1))
      {
	for (i=0; i<ncase; i++) targets[neti][i] =
	  story.sents[senti].caseroles[i];
	for (i=0; i<ncase; i++)
	  for(j=0; j<nsrep; j++)
	    tchrep[neti][i*nsrep+j] = swords[targets[neti][i]].rep[j];
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].prevx, net[neti].prevy,
			     prevhidrep[neti], nhidrep[neti]);
	  }
	forward_prop(neti);
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].hidx, net[neti].hidy,
			     hidrep[neti], nhidrep[neti]);
	    display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
				  targets[neti],
				  net[neti].outx, net[neti].outy, ABOVE2);
	    display_layer(neti, net[neti].noutitem, tchrep[neti],
			  net[neti].tchx, net[neti].tchy, nsrep);
	    display_error(neti, outrep[neti], net[neti].noutitem,
			  targets[neti],swords,nsrep,++step);
	  }
	for(k=0; k<nhidrep[neti]; k++)
	  prevhidrep[neti][k]=hidrep[neti][k];
	if (chained)
	  for (i=0; i<ncaserep; i++)
	    caserep[i] = outrep[neti][i]; /* activate result */
  
	collect_stats(neti, outrep[neti], ncase, targets[neti],
		      swords, nsrep, nswords);
	gener_sentence(story.sents[senti]);
      }      
}


void gener_sentence(struct SENTSTRUCT sent)
{
  register int i,j,k, wordi, step=0, neti=SENTGENMOD;
  
  sprintf(net[neti].sequence, "%s", "");
  sprintf(net[neti].newitem, "%s", "");
  for (i=0; i<ncase; i++) inputs[neti][i] = sent.caseroles[i];
  for (i=0; i<ncase; i++)
    for(j=0; j<nsrep;j++)
      if(chained)
	inprep[neti][i*nsrep+j] = caserep[i*nsrep+j]; /* use active sent */
      else
	inprep[neti][i*nsrep+j] = swords[inputs[neti][i]].rep[j];

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, net[neti].ninpitem, inprep[neti],
			    inputs[neti],
			    net[neti].inpx, net[neti].inpy, BELOW);
    }

  for(i=0; i<nhidrep[neti]; i++)
    prevhidrep[neti][i]=0;
  for(wordi=0; wordi<nword; wordi++)
    if(sent.words[wordi]>(-1) || text_question)
      {
	targets[neti][0] = sent.words[wordi];
	for(j=0; j<nsrep; j++)
	  tchrep[neti][j] = swords[targets[neti][0]].rep[j];
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].prevx, net[neti].prevy,
			     prevhidrep[neti], nhidrep[neti]);
	  }
	forward_prop(neti);
	if (displaying)
	  {
	    if (stepping) wait_for_run();
	    handle_events();
	    display_assembly(neti, net[neti].hidx, net[neti].hidy,
			     hidrep[neti], nhidrep[neti]);
	    sprintf(net[neti].sequence,"%s %s",
		    net[neti].sequence, net[neti].newitem);
	    display_sequence(neti, net[neti].tchx, net[neti].tchy);
	    display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
				  targets[neti],
				  net[neti].outx, net[neti].outy, ABOVE2);
	    display_layer(neti, net[neti].noutitem, tchrep[neti],
			  net[neti].tchx, net[neti].tchy, nsrep);
	    display_error(neti, outrep[neti], net[neti].noutitem,
			  targets[neti],
			  swords, nsrep, ++step);
	  }
	for(k=0; k<nhidrep[neti]; k++)
	  prevhidrep[neti][k]=hidrep[neti][k];
	collect_stats(neti, outrep[neti], 1, targets[neti],
		      swords, nsrep, nswords);
	if(chained && withlex)
	  for(j=0; j<nsrep; j++)
	    swordrep[j]=outrep[neti][j];	/* activate result */
	if (withlex)
	  output_lexicon(sent.words[wordi]);
	if (text_question &&
	    find_nearest_s(outrep[neti], swords, nsrep, nswords)
	    == periodindex)
	  break;
      }      
  if(babbling) printf("\n");
}


void formcue()
{
  register int i,j, step=0, neti=CUEFORMMOD;
  
  printcomment("\n", "parsing question:", "\n");
  for(i=0; i<nslot; i++) targets[neti][i] = qa.slots[i];
  for(i=0; i<nslot; i++)
    for(j=0; j<nsrep; j++)
      tchrep[neti][i*nsrep+j] = swords[ targets[neti][i] ].rep[j];
  parse_sentence(qa.question);
  
  for (i=0; i<ncase; i++) inputs[neti][i] = qa.question.caseroles[i];
  if(chained)
    for (i=0; i<ninpunits[neti]; i++)
      inprep[neti][i]=caserep[i];		/* use active sent */
  else
    for (i=0; i<ncase; i++)
      for(j=0; j<nsrep;j++)
	inprep[neti][i*nsrep+j] =
	  swords[ inputs[neti][i] ].rep[j];
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, net[neti].ninpitem, inprep[neti],
			    inputs[neti],
			    net[neti].inpx, net[neti].inpy, ABOVE);
    }
  
  forward_prop(neti);
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_assembly(neti, net[neti].hidx, net[neti].hidy,
		       hidrep[neti], nhidrep[neti]);
      display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
			    targets[neti],
			    net[neti].outx, net[neti].outy, BELOW2);
      display_layer(neti, net[neti].noutitem, tchrep[neti],
		    net[neti].tchx, net[neti].tchy, nsrep);
      display_error(neti, outrep[neti], net[neti].noutitem, targets[neti],
		    swords, nsrep, ++step);
    }

  printcomment("\n", "into cue:", "\n");
  collect_stats(neti, outrep[neti], nslot, targets[neti],
		swords, nsrep, nswords);

  if(chained && withhfm)
    for(i=0; i<nslotrep; i++)
      slotrep[i]=outrep[neti][i];	/* activate result */
}

void produceanswer()
{
  register int i,j, step=0, neti=ANSWERPRODMOD;
   
  for(i=0; i<ncase; i++) targets[neti][i] = qa.answer.caseroles[i];
  for(i=0; i<ncase; i++)
    for(j=0; j<nsrep; j++)
      tchrep[neti][i*nsrep+j] = swords[ targets[neti][i] ].rep[j];
  
  for (i=0; i<ncase; i++) inputs[neti][i] = qa.question.caseroles[i];
  for (i=0; i<nslot; i++) inputs[neti][ncase+i] = qa.slots[i];
  if(chained)
    {
      for (i=0; i<noutunits[0]; i++)
	inprep[neti][i]=caserep[i];	/* use active sent */
      for (i=0; i<noutunits[1]; i++)
	inprep[neti][i+noutunits[0]] = slotrep[i]; /* use active slots */
    }	
  else
    for (i=0; i<ncase+nslot; i++)
      for(j=0; j<nsrep;j++)
	inprep[neti][i*nsrep+j] = swords[ inputs[neti][i] ].rep[j];

  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_labeled_layer(neti, ncase, inprep[neti], inputs[neti],
			    net[neti].inpx, net[neti].inpy, BELOW);
      display_labeled_layer(neti, nslot, &inprep[neti][ncase*nsrep],
			    &inputs[neti][ncase],
			    net[neti].inp1x, net[neti].inp1y, BELOW);
    }
  forward_prop(neti);
  if (displaying)
    {
      if (stepping) wait_for_run();
      handle_events();
      display_assembly(neti, net[neti].hidx, net[neti].hidy,
		       hidrep[neti], nhidrep[neti]);
      display_labeled_layer(neti, net[neti].noutitem, outrep[neti],
			    targets[neti],
			    net[neti].outx, net[neti].outy, ABOVE2);
      display_layer(neti, net[neti].noutitem, tchrep[neti],
		    net[neti].tchx, net[neti].tchy, nsrep);
      display_error(neti, outrep[neti], net[neti].noutitem,
		    targets[neti],
		    swords, nsrep, ++step);
    }

  printcomment("\n", "generating answer:", "\n");
  collect_stats(neti, outrep[neti], ncase, targets[neti],
		swords, nsrep, nswords);
  if(chained)
    for(i=0; i<ncaserep; i++)
      caserep[i] = outrep[neti][i];	/* activate result */
  gener_sentence(qa.answer);
}


/*********************  propagation ******************************/

void forward_prop(int neti)
{
  register int i,k,p;
  
  for(k=0; k<nhidrep[neti]; k++)
    hidrep[neti][k]=hidbias[neti][k];
  if (neti<4)
    for(p=HIDREPSTEP-1; p<nhidrep[neti]; p+=HIDREPSTEP)
      for(k=0; k<nhidrep[neti]; k++)
	hidrep[neti][k] += prevhidrep[neti][p]*wph[neti][p][k]
	  + prevhidrep[neti][p-1]*wph[neti][p-1][k]
	  + prevhidrep[neti][p-2]*wph[neti][p-2][k]
	  + prevhidrep[neti][p-3]*wph[neti][p-3][k]
	  + prevhidrep[neti][p-4]*wph[neti][p-4][k];
  for(i=INPUNITSTEP-1; i<ninpunits[neti]; i+=INPUNITSTEP)
    for(k=0; k<nhidrep[neti]; k++)
      hidrep[neti][k] += inprep[neti][i]*wih[neti][i][k]
	+ inprep[neti][i-1]*wih[neti][i-1][k]
	+ inprep[neti][i-2]*wih[neti][i-2][k]
	+ inprep[neti][i-3]*wih[neti][i-3][k];
  for(k=0; k<nhidrep[neti]; k++)
    hidrep[neti][k]= 1.0/(1.0+exp(-hidrep[neti][k]));

  for(i=0; i<noutunits[neti]; i++)
    outrep[neti][i]=outbias[neti][i];
  for(k=HIDREPSTEP-1; k<nhidrep[neti]; k+=HIDREPSTEP)
    for(i=0; i<noutunits[neti]; i++)
      outrep[neti][i] += hidrep[neti][k]*who[neti][i][k]
	+ hidrep[neti][k-1]*who[neti][i][k-1]
	+ hidrep[neti][k-2]*who[neti][i][k-2]
	+ hidrep[neti][k-3]*who[neti][i][k-3]
	+ hidrep[neti][k-4]*who[neti][i][k-4];
  for(i=0; i<noutunits[neti]; i++)
    outrep[neti][i] = 1.0/(1.0+exp(-outrep[neti][i]));
}      



/*********************  initializations ******************************/

void qa_init()
{
  int doo, neti;
  float foo;
  FILE *fp;

  ninpunits[0] = noutunits[3] = nsrep;
  noutunits[0] = ninpunits[1]  = ninpunits[4] = noutunits[5] = noutunits[2]
    = ninpunits[3] = ncase*nsrep;
  noutunits[1] = noutunits[4] = ninpunits[2] = nslot*nsrep;
  ninpunits[5] = (ncase+nslot)*nsrep;

  if((fp=fopen(qafile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", qafile);
       exit(1);
     }
  printf("Reading FGREP modules from %s...", qafile);
  qa_read_params(fp);
  fscanf(fp,"%d",&doo);
  for(neti=FIRSTQAMOD; neti<FIRSTQAMOD+nqanets; neti++)
    fscanf(fp, "%f", &foo);
  qa_iterate_weights(readfun, fp, 0, 0, 0, 0);
  fclose(fp);
  printf("Done.\n");
}


void qa_read_params(FILE *fp)
{
  char s[MAXSTRL];
  int i, neti, nphase, doo;
  float foo;

  /* simulation parameters */
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%s", s); fgl(fp);
  fscanf(fp,"%d %d", &nqanets, &doo); 
  for(neti=FIRSTQAMOD; neti<FIRSTQAMOD+nqanets; neti++)
    fscanf(fp,"%d", &nhidrep[neti]); fgl(fp);
  for(neti=FIRSTQAMOD; neti<FIRSTQAMOD+nqanets; neti++)
    fscanf(fp,"%d", &doo); fgl(fp);
  fscanf(fp, "%d %d %d %d",&doo,&doo,&doo,&nphase); fgl(fp);
  for(i=0; i<nphase; i++)
    fscanf(fp,"%d",&doo); fgl(fp);
  for(i=0; i<nphase; i++)
    fscanf(fp,"%f",&foo); fgl(fp);
  
  /* saving info */
  fscanf(fp,"%d", &doo);
  for(i=0; i<MAXSNAPS && doo<SNAPSHOTEND; i++)
    fscanf(fp,"%d", &doo);
  fgl(fp);
}


void qa_iterate_weights(void (*dofun)(FILE *, float *, float, float), FILE *fp,
			float par1, float par2, float par3, float par4)
{
  register int neti,i,j,k;
  float foo;

  for(i=0; i<nswords; i++)
    for(j=0; j<nsrep; j++)
      (*dofun)(fp, &foo,par3,par4);
  
  for(neti=FIRSTQAMOD; neti<FIRSTQAMOD+nqanets; neti++)
    {
      for(i=0; i<ninpunits[neti]; i++)
	for(k=0; k<nhidrep[neti]; k++)
	  (*dofun)(fp, &wih[neti][i][k],par1,par2);
      
      for(k=0; k<nhidrep[neti]; k++)
	(*dofun)(fp, &hidbias[neti][k],par1,par2);
      
      if (neti<4)
	for(j=0; j<nhidrep[neti]; j++)
	  for(k=0; k<nhidrep[neti]; k++)
	    (*dofun)(fp, &wph[neti][j][k],par1,par2);
      
      for(i=0; i<noutunits[neti]; i++)
	(*dofun)(fp, &outbias[neti][i],par1,par2);

      for(i=0; i<noutunits[neti]; i++)
	for(k=0; k<nhidrep[neti]; k++)
	  (*dofun)(fp, &who[neti][i][k],par1,par2);
    }
}  
