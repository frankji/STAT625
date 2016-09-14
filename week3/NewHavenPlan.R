#
# STAT 325/625: Fall 2016
#
# You can work alone.  We'd recommend forming small teams of 2-3.
# We want to keep 325 and 625 groups independent (because independence
# is a good thing), so please respect this boundary as you team up.
# We'll try to facilitate via Piazza.
#
# Goal: a CSV file, basically building on what we currently have with 6
# columns, one per team.  Be ready for validation on Thursday, Sept 15,
# at 9 AM.  Choose a "team leader" to upload the file to his/her dropbox,
# with file name like 325_netid1_netid2_netid3.csv or 625_netid1_netid2.csv.
#
# (Note: At first glance, the number of variables we are asking you to 
#        scrape will seem overwhelming. Upon closer inspection, you will
#        see that many of the variables can be processed using the
#        same exact approach.)
# 
# New Haven Housing!  Ultimately, we want the following.
#    We should all use the same variable names for coding efficiency:
#
#    - Location and appraised value (from HW1)
#      'pid', 'location', 'totval'
#    - Owner address (may be useful for zip code, used with caution)
#      'address'
#    - Any sale dates and sale prices (say, up to the 5 most recent),
#      along with the name of the owner on that same line.
#      * you decide how to deal with this information *
#    - Year built
#      'yearbuilt'
#    - Living area (this is in square feet)
#      'sqft'
#    - Replacement cost
#      'replcost'
#    - Building percent good
#      'pctgood'
#    - Style, model, grade, occupancy, AC Type, bedrooms, bathrooms,
#      half baths, bath style, kitchen style
#      'style', 'model', 'grade', 'occupancy', 'actype', 'bedrooms',
#      'bathrooms, 'halfbaths', 'bathstyle', 'kstyle'
#    - The sum of the value of any extra features
#      'exval'
#    - Land size (in acres)
#      'acres'
#    - Land use zone (like RS1, RS2, ...)
#      'zone'
#    - Neighborhood (like 0200, which should not be a number)
#      'neighborhood'
#    - Appraised value of the land
#      'landval'
#    - Gross area of anything that seems like a 'garage'
#      'garagesqft'
