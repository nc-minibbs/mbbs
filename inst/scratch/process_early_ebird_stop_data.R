#for processing stop-level information from ebird checklists

#problem 1: that data is not currently stored in any column. Is it removed when we filter the ebird checklists?

#comes from column Observation.Details, need that to be added back in to import_ebird_data. Added

#problem 2: now need to re-run the data imports to include those columns

#problem 3: some of the unicode characters have been changed. 

#nice thing 1: don't have to worry about coding for new types of data imputs in these columns. Thankfully, data is done by stop_level now so it just needs if/then regex statements to account for the currently existing variation 

#perchance really nice to write the function to add all the data into one really quick, same as copy/pasting it again for use now

