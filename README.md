# GFW-Scripts

Shark Sanctuary Workflow - Longline

1.	Pull 2019 AIS data from server: 

      		2019_AISDataPull.R

2.	Write GAMM for hooks~hours relationship:

		2019_HooksHrs_GAMM.R

3.	Filter AIS data to only reflect effort within a given sanctuary, and predict hooks in the sanctuary (at 0.25x0.25) using the GAMM. 
    	This is done at the sanctuary specific level. I included scripts for Cook Islands as an example.
    
	    	HookProj_SANCTUARY.R
      
  	Notes: To filter AIS data, I use EEZ shapefiles that were downloaded and saved on my computer. I shared that folder here: 
  	https://drive.google.com/drive/folders/1Vw2t1sM775izoAAi-ZP0LtjQXwTRcoWD?usp=sharing
  
4.	Standardizing CPUE data - not sure if you wanted to see this part or not, I included the script, and the data.
		
		WCPFC bycatch_GAMMs_V3_.R
		WCPFC_bycatch.csv


5.	Sanctuary-specific prediction loops for catch and mortality.

      		CatchMort_SANCTUARY_V5.R
