# Instructions for updating the data

After a fresh new year and a new series of surveys,
here's the walkthrough of how to update this repository
with the latest information.

## STEP 1: Checking and QC'ing the ebird data

You'll want the
['NC Mini BBS Route Runners' google sheet](https://docs.google.com/spreadsheets/d/1KRM57UewvwcbkVERUV_X7faAmgKuWSfVO5Ty3SU7qLE/edit?usp=sharing)[^rr]
up to check which routes ought to have been run,
and who you should get in touch with if they haven't been.
You'll check the routes on all three ebird accounts.

[^rr]: The sheet is view-only unless it has been shared with your account.

Update the 'sampling events' tab with comments as needed.

1. Check that all the routes are accounted for.
2. Check that all routes have 20 stops and are formatted correctly
    * Potential errors:
      * More than one checklist for the same stop
      * Checklist missing for one or more stops.
        To fix, read the comments on the first stop,
        there should be an explanation ie: "road work, stops 2-4 not surveyed".
        Add the checklist to `data/stop_deviations.yml`,
        following the formatting example of previous entries.
        If there's no explanation,
        check the surveyor's ebird lists to see if the stop checklist exists
        and hasn't been shared to the account
        or if the stop checklist is fully missing.
        Follow up with the surveyor
        and make a note
        in the sampling events tab of 'NC Mini BBS Route Runners'.
      * Checklist for stop 1 does not have some marker that it is stop 1.
      * Checklist does not contain the county name (Durham|Chatham|Orange)

3. Does Stop 1 have environmental data?
   Are the types of data written correctly?
   The only required information is the *observers=*

  Here's an example of what this environmental data might look like:

  *observers=Allen Hurlbert, Sarah Pollack; weather=55 F, clear; notes=big thunderstorm last night, everything wet; vehicles=3 (or v=3); habitat=B,H (or h=B,H).*

  Habitat data may only be recorded if something's changed from the last years.

    * Potential errors:
      * misspellings in the data type ie "wether" instead of "weather".
        To fix, edit the comments to the correct spelling.
      * no *"observer(s)="* in the first stop.
        To fix, check the submitter's ebird account
        or the 'NC Mini BBS Route Runners'
        to see who to credit with running that route.
        Add the information.
      * data types separated by a comma "," instead of semi-colon ";". 
        To fix, edit the comments to change commas to semi-colons.

4. Do the other stops have comments? Are the comments formatted correctly?

    * Potential errors:
      * missing semi-colon between data types (ie v=3 h=b,bh)
        or comma instead of semi-colon.
        To fix, add semi-colons.
      * non-data comment. 
        To fix, add "notes=" before the comment.
      * vehicle information on most but not all routes.
        To fix this use your discretion,
        as the surveyor may not have checked for vehicles at all stops,
        but if all the other stops have v>1, these are likely v=0 stops.
        If so add v=0 to comments.

5. Are all the routes sorted under the right county?

    * Potential errors
      * Example: a Durham checklist has been shared to Chatham -
        if Durham already has the checklist and it's been shared twice,
        delete the duplicate checklist in the Chatham account.
        Ebird will make this seem scary!
        If the checklist is on both accounts,
        it will only be deleted from the one account.
        If Durham does NOT have the checklist,
        share the checklist to Durham,
        and then delete the checklist from the Chatham account,
        or if not possible,
        get in touch with the surveyor
        to share the checklist to the Durham account instead.
        Determine what county the checklist
        belongs to based on the name of the checklist
        i.e. "MBBS, Durham, Route 1-1"
        rather than based on the location
        that ebird gives in the county column on the website.
        Sometimes checklists that are along county borders
        are put in the wrong location by ebird (ie: Wake co.) 
        but mbbs_county is extracted from the location name.
      * Example: The location of the checklist does not include the county 
        i.e.: "MBBS, Route=8,,Stop1" 
        rather than something like "MBBS, Orange, Route=8,,Stop1". 
        The dataset column mbbs_county is extracted from the checklist location, 
        and this will cause an error where the data receives an N/A county. 
        To fix, edit the location to the same route/stop, 
        but one of the location names that includes the county.  

## STEP 2: Download the ebird data

Now that the data is QC'd,
download the ebird data from all the accounts
(it will be sent to Allen's email initially)
and add it to `data/ebird`.
You'll rename the files to `MyEbirdData_[COUNTY]_[YYYYMMDD]`.
**IMPORTANT!**
DO NOT open the files in excel.
If you want to check the data open the .csv in R.
Opening the files in excel may change the date format
and cause errors when processing the data.
If you get an error later on relating to an invalid date format, 
redownload the data.

## STEP 3: Update the taxonomy

Download the latest version of the eBird taxonomy CSV
to the `data/taxonomy` folder.
You can find the
[latest version of the taxonomy here](https://www.birds.cornell.edu/clementschecklist/download/)

The file should be named with the format `ebird_taxonomy_vYYYY`
(it should download in this format).
Leave previous versions in the directory.

If conform_taxonomy() later stops the update because it's flagged that there's a common name in the historical data that is NOT present in the ebird data,
eg. taxonomy has changed for that species and been updated on ebird, 
add the species to the case_when()'s in taxonomy.R/conform_taxonomy()
eg. "House Wren ~ Northern House Wren" 

## STEP 4: Run the update locally

Now, you'll run the update locally.
this will enable to you to do two things:

1) check for errors and
2) give input on any new observers.

To run the update locally:
`devtools::load_all()` while you have the mbbs project open in RStudio.
If you don't have devtools installed: `install.packages('devtools')`.
The functions for creating
both the stop-level and the route-level dataset are in the `R/mbbs` folder.
Every other R code in the project contributes to this file,
and it's the one thing that needs to run to update the data.
Run this function: `create_mbbs_counts()`.
  
As you import the data for each county,
warning messages may appear in the console.
E.g.: "The following year/route don't have either 1 or 20 checklists:".
Follow up on these messages -
first check for a note in the NC Mini BBS Route Runners sheet
(in the sampling events tab),
then fix any errors,
and finally redownload the data from ebird as necessary.

You will get INFO notices for changes occuring in the background,
ARN notices for warnings that should be fixed,
and ERROR notices for large errors that must to be fixed.

If there are any new observers,
or any previous observers whose names
have been entered with typos in the new data,
you will be prompted to give input on their names.

Are there any checklists which,
after discussion,
need to be removed from the data?
Add their checklist ID to `data/excluded_submissions.yml`.
Examples include duplicate checklists and pre-count owling checklists,
which for the moment are excluded from the data.
Leave a note about the county, route, year, and why the checklist was excluded.

If everything's gone well,
we've now updated the mbbs datasets with the new year's info.

## STEP 5: Update the version number

Once you've confirmed that the update is running smoothly locally,
update the version number in the DESCRIPTION file.

## STEP 6: Push update to github

Push your commits to github.

The data available for download on the website will be automatically updated.
Download it and check that the latest year of data is available in the dataset.
Voilà!
