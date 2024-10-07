################################################################################
# stage1_08_finalise_coding.R
#
# This script constructs a "finalised" data set, which resolves differences
# between coders for each article, and also removes some articles due to
# changes in their badge status.

# Remove the following articles from the data set:
#
# a001347, DOI:10.1080/00224545.2017.1389684 (The Journal of Social Psychology)
# This paper was corrected to remove its open data badge according to the
# Correction published 14 May 2019 (DOI:10.1080/00224545.2019.1599547)
#
# a001526, DOI:10.1111/ajps.12302 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges, but (i) the PDF does not have such badges and (ii)
# the website does not show any badges. Interestingly, the article does have
# a data availability statement, but there is no link. This article was either
# added to the database in error, or the journal website in 2018 erroneously
# indicated that the article did have badges.
#
# a000685, DOI:10.1111/ajps.12360 (American Journal of Political Science)
# This paper was awarded an open data badge when originally entered into
# database, but the data was only available upon request. According to the
# journal website and PDF, it has an Open Data badge, but according to the 
# Harvard Dataverse page for the article, it only has an Open Materials badge.
# This could be interpreted either way, but better to remove it and mention it
# in the paper.
