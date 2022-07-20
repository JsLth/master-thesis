# Master's thesis

This repository holds the code associated to the Master's thesis called "Estimating contextual determinants of attitudes towards global environmental change: A semantic text modelling approach using georeferenced tweets". The goal of the thesis is to determine the spatial relationship of attitudes expressed in tweets and certain contextual variables on a local level. To do this, the tweet texts are scaled depending on their attitudinal position (either supporting or opposing) using methods of Natural Language Processing.

Data files were either removed due to copyright restrictions or dehydrated to only contain tweet IDs and geo-information.


# Structure

The code files are structured as follows:

- **workflow.R**: Evaluated code that is used for computing the results of the thesis. Sources all code files below.
- **collect_tweets.R**: Evaluated code to extract tweets on a daily basis.
- **boundaries.R**: Collects geodata describing the boundaries of Germany's administrative levels
- **datatable.R**: Converts collected tweets to the efficient data.table format and filters out unwanted tweets
- **dictionaries.R**: Contains dictionaries that can be used to, e.g., filter out words from a corpus (sourced in many other files)
- **hydrate.R**: Hydrates tweet IDs with all relevant information on a tweet
- **packages.R**: Loads all the packages needed for any of the code files (sourced in each file)
- **search_tweets.R**: Searches Twitter for all tweets on a particular day
- **tf_idf.R**: Calculates tf-idf scores for a vector of tweets
