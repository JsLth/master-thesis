# Master's thesis

This repository holds the code associated to the Master's thesis called "Estimating contextual determinants of attitudes towards global environmental change: A semantic text modelling approach using georeferenced tweets". The goal of the thesis is to determine the spatial relationship of attitudes expressed in tweets and certain contextual variables on a local level. To do this, the tweet texts are scaled depending on their attitudinal position (either supporting or opposing) using methods of Natural Language Processing.

Data files were either removed due to copyright restrictions or dehydrated to only contain tweet IDs and geo-information.


# Structure

The code files are divided into workflow files and supporting functional files. Workflow files evaluate code while functional files provide the functions for workflow files.

### Workflow files
- **main.R**: Computes the thesis results based on the tweet packages created from the code files below
- **collect_tweets.R**: Collects tweets for the earliest possible day and saves them to ./data/tweets
- **geocode.R**: Geocodes the earliest non-geocoded tweet package present in ./data/tweets and saves them to ./data/geo

### Functional files
- **boundaries.R**: Functions to collect data on administrative boundaries
- **read_destatis.R**: Functions to read data from destatis
- **datatable.R**: Functions to read (and filter) tweet packages as lazy datatables
- **dictionaries.R**: Contains dictionaries and other lists (sourced in many other files)
- **packages.R**: Loads all the packages needed for any of the code files (sourced in each file)
- **hydrate.R**: Functions to get tweets from IDs
- **search_tweets.R**: Functions to query the Twitter API
- **tf_idf.R**: Function to calculate tf-idf values for a character vector
- **photon.R**: Functions to control and query photon and filter the results
- **utils.R**: Various utility functions
