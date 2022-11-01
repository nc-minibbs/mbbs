# Mini-Bird Breeding Survey

This repository contains code to download and analyze data from the [Mini-Bird Breeding Surveys](http://minibbs.us) conducted in Orange, Chatham, and Durham counties.

[View the results website](http://minibbs.us)



## Notes on the analysis data files

* Prio


### Instructions for updating the data
After a fresh new year and a new series of surveys, here's the walkthrough of how to update this repository with the latest information

STEP1: Checking and QC'ing the ebird data
You'll want the 'NC Mini BBS Route Runners' google sheet up to check which routes ought to have been run, and who you should get in touch with if they haven't been. You'll check the routes on all three ebird accounts.
 - Check that all the routes are accounted for 
 - Check that all routes have 20 stops 
	-Potential errors: 
		-More than one checklist for the same stop
		-Checklist missing for one or more stops
			-Read the comments on the first stop, there should be an explanation ie: "road work, stops 2-4 not surveyed". If there's no explanation, check the surveyor's ebird lists to see if the stop checklist exists and hasn't been shared to the account or if the stop checklist is fully missing. Follow up with the surveyor and make a note in the sampling events tab of 'NC Mini BBS Route Runners'

 - Does Stop 1 have environmental data? Are the types of data written correctly? The only required information is the observers=
EX: observers=Allen Hurlbert, Sarah Pollack; weather=55 F, clear; notes=big thunderstorm last night, everything wet; vehicles=3 (or v=3); habitat=B,H (or h=B,H)
	-Potential errors
		-misspellings in the data type ie "wether" instead of "weather": edit the comments to the correct spelling
		-no "observer(s)=" in the first stop: check the submitter's ebird account or the 'NC Mini BBS Route Runners' to see who to credit with running that route. Add the information
		-data types seperated by a comma "," instead of semi-colon ";": edit the comments to change commas to semi-colons

 - Do the other stops have comments? Are the comments formatted correctly?
	-Potential errors
		-missing semi-colon between data types (ie v=3 h=b,bh) or comma instead of semi-colon: add semi-colons 
		-non-data comment, add notes= before it
 - Are all the routes sorted under the right county? 
	-Potential error
		-a Durham checklist has been shared to Chatham - if Durham already has the checklist and it's been shared twice, delete the duplicate checklist in the Chatham account. If Durham does NOT have the checklist, share the checklist to Durham and then delete the checklist from the Chatham account, or if not possible, get in touch with the surveyor to share the checklist to the Durham account instead. All ebird accounts can Only have checklists from the correct county for the data to process correctly.

STEP2: Download the ebird data from all the accounts and add it to mbbs -> inst -> extdata. You'll rename the files to MyEbirdData_[COUNTY]_[YYYYMMDD]. IMPORTANT! DO NOT open the files in excel, if you want to check the data open the .csv in R. Opening the files in excel may change the date format and cause errors when processing the data. 