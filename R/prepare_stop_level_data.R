#
# top-of-script function should:
# -> generate df of routes with stop info by step from species_comments
# -> add info to that df extracted from when information is by quarter route in notes
# -> add to that info the dataset from the transcription work
# -> add the stop num 1 in for routes that have all the 19 stops aside from stop 1. 
# -> Allen's proposal, to maintain the highest level of granuarity in the one mbbs dataset, is to replace entries in the mbbs with stop_level information where possible
# then, when generating the two kinda 'end-user' datasets, you're really using both functions (this happens anyway) where you summarize by route, and where you extract just the information that has stop number information
# Allen's proposal avoids what my solution would not: disagreements between the mbbs by route and mbbs by stop - here, they HAVE to agree to pass the qc check. I'm manually approving them to go forward. 
# So yes, Allen's proposal where then information is granularized in mbbs when matches agree, and report when they don't. Depending on how many issues there are, either manually resolve this incongruences or discard one part of the information. 