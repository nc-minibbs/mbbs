# Mini-Bird Breeding Survey

This repository contains code to download and analyze data from the [Mini-Bird Breeding Surveys](http://minibbs.us) conducted in Orange, Chatham, and Durham counties.

[View the results website](http://minibbs.us)


### Instructions for updating the data
After a fresh new year and a new series of surveys, here's the walkthrough of how to update this repository with the latest information

#### STEP1: Checking and QC'ing the ebird data
You'll want the 'NC Mini BBS Route Runners' google sheet up to check which routes ought to have been run, and who you should get in touch with if they haven't been. You'll check the routes on all three ebird accounts. Update the 'sampling events' tab with comments as needed.
 1. Check that all the routes are accounted for 
 2. Check that all routes have 20 stops 
    - Potential errors: 
      - More than one checklist for the same stop
      - Checklist missing for one or more stops
      - Read the comments on the first stop, there should be an explanation ie: "road work, stops 2-4 not surveyed". If there's no explanation, check the surveyor's ebird lists to see if the stop checklist exists and hasn't been shared to the account or if the stop checklist is fully missing. Follow up with the surveyor and make a note in the sampling events tab of 'NC Mini BBS Route Runners'

 3. Does Stop 1 have environmental data? Are the types of data written correctly? The only required information is the *observers=*

Here's an example of what this environmental data might look like:

*observers=Allen Hurlbert, Sarah Pollack; weather=55 F, clear; notes=big thunderstorm last night, everything wet; vehicles=3 (or v=3); habitat=B,H (or h=B,H).*

Habitat data may only be recorded if something's changed from the last years.

   - Potential errors
     - misspellings in the data type ie "wether" instead of "weather". To fix, edit the comments to the correct spelling
     - no *"observer(s)="* in the first stop. To fix, check the submitter's ebird account or the 'NC Mini BBS Route Runners' to see who to credit with running that route. Add the information
     - data types seperated by a comma "," instead of semi-colon ";" To fix, edit the comments to change commas to semi-colons

 4. Do the other stops have comments? Are the comments formatted correctly?
    - Potential errors
      - missing semi-colon between data types (ie v=3 h=b,bh) or comma instead of semi-colon. To fix, add semi-colons 
      - non-data comment. To fix, add "notes=" before the comment
      - vehicle information on most but not all routes. To fix this use your discretion, as the surveyor may not have checked for vehicles at all stops, but if all the other spots have v>1, these are likely v=0 stops. If so add v=0 to comments.

 5. Are all the routes sorted under the right county? 
    - Potential error
      - Example: a Durham checklist has been shared to Chatham - if Durham already has the checklist and it's been shared twice, delete the duplicate checklist in the Chatham account. Ebird will make this seem scary! If the checklist is on both accounts it will only be deleted from the one account. If Durham does NOT have the checklist, share the checklist to Durham and then delete the checklist from the Chatham account, or if not possible, get in touch with the surveyor to share the checklist to the Durham account instead. All ebird accounts can Only have checklists from the correct county for the data to process correctly.

#### STEP2: Download the ebird data 
Now that the data is QC'd, download the ebird data from all the accounts (it will be sent to Allen's email initially) and add it to mbbs/inst/extdata. You'll rename the files to `MyEbirdData_[COUNTY]_[YYYYMMDD]`. _IMPORTANT!_ DO NOT open the files in excel. If you want to check the data open the .csv in R. Opening the files in excel may change the date format and cause errors when processing the data. If you get an error later on relating to an invalid date format, redownload the data.

#### STEP3: Update the taxonomy
Download the latest version of the eBird taxonomy CSV to the inst/taxonomy folder. You can find the latest version of the taxonomy at 

	https://www.birds.cornell.edu/clementschecklist/download/

The file should be named with the format `ebird_taxonomy_vYYYY` (it should download in this format)

#### STEP4: Update the repository
Run the `import_data.R` file located in mbbs/inst. Every other R code in the project contributes to this file, and it's the one thing that needs to run to update the data. You can see it calls library(mbbs) to use all the functions contained in the package. If you can't load library(mbbs) run the following lines:

	install.packages('devtools')
	devtools::install_github('nc-minibbs/mbbs')
	library(mbbs) 

To run the new data, first you'll need to replace the .csv files being read in to create the Orange, Durham, and Chatham dataframes. You'll replace the dates in lines 18,33,and 48 ie:

        import_ebird_data("inst/extdata/MyEBirdData_Orange_20220913.csv")

with the dates for the new ebird downloads that you added to inst/extdata in step 2. 
ie:

        import_ebird_data("inst/extdata/MyEBirdData_Orange_YYYYMMDD.csv")

Ensure in changing the code that each line is still reading in correct county's .csv, and that for example the mbbs_orange variable is not now getting the new Durham csv.

In lines 61-63 if everything's gone well, we've now updated the mbbs datasets with the new year's info. Open up your file explorer and make sure the date modified matches your current date and that they've been changed successfully. Because we want to create a backup of this specific download, the R script will also automatically create a new csv version labeled with the day's date stored in inst/analysis_data. 

If you get an error at any point, the two other key R scripts are found in the mbbs/R folder. `prepare_mbbs_data` formats the data from 1999-2010 from the old website and is unlikely to be a source of error. `import_ebird_data` is where you can see how the data's being processed and where you'll be able to find an explanation for your error.