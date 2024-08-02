Codes and analysis for the submission to Applied Network Science - Reconstructing collaboration networks from text using Large Language Models

In this work, we use Large Language models (LLMs from OpenAI's GPT 3.5 and 4 family) to reconstruct networks of relationships from unstructured text. We use the data from online wiki lab notebooks of teams participating in the iGEM synthetic biology competition. These notebooks contain 2 types of information encoded as free text - that are of our specific interest. 

1. Inter-team collaboration: Teams report on the wiki about which other teams they collaborated with during the competition. THey also mention the context of the collaborations.
2. Intra-team contribitions: Teams report which member contributed to which tasks of their project.

These are the two networks we reconstruct in this work. For more context and information on the iGEM competition and an overview of the data collected from teams - please refer to: Santolini et. al., 2023 (https://arxiv.org/abs/2310.19858) and Blondel et. al., 2024 (https://zenodo.org/records/11072818)

The Folder "notebooks" contains the R and python scripts used in the analysis: 

1. The python notebooks (.ipynb) are used for api calls to OpenAI. The scripts are numbered in the order of execution, and separate for inter- and intra- team network curation. The "0_create_batches_...ipynb" processes the full text from team wiki pages into batches and selects pages which contain the intra- or inter- team information. The notebooks with the prefix "1_...ipynb" passes the processed text and the prompt to OpenAI chatComplete endpoint to get a table of relationships.  The python notebooks with the prefix "2_..." matches the team names collected from the previous step to their corresponding identifiers (for the inter-team case) and user names to their IDs (for the intra-team case). The notebooks with the prefix "3_.." matches the context of the inter- and intra- team relationships to a dictionary of standard relationships defined by the authors. 

We also have a version of all 3 steps where we use the older "Completions" endpoint instead of the "chatComplete" endpoint of OpenAI. These notebooks have the term  "completions" in the title. We do this for inter-team and not for the intra-team relationships due to poorer performance, as only older models are accessable and has since been depracated. (https://platform.openai.com/docs/guides/completions)

The notebook with the prefix "4_..." uses a fuzzy matching approach to identify the inter-team collaborations and creates a test set to validate the GPT output against. 

2. The R notebooks (.Rmd) are used for analysis and making the figures presented in the paper. They are also numbered and separate for intra- and inter- team. The notebooks with the prefix "0_.." use the processed data from the all the previous python notebooks to create a dataframe where each user, team are matched to their identifiers and tasks/contexts matched to the corresponding standard list. The R notebooks with prefix "1_..." evaluates the effect of changing OpenAI parameter values. "2_..." prefix is for notebooks which compare the GPT output against the reference sets - in the inter-team case - against manually curated random sample and against the fuzzy matching sample. For the intra-team case - it is just against the manually curated random sample of teams. The "3_..." prefix for the inter-team case compares the GPT and fuzzy output against a sample set curated by iGEM team waterloo in 2015 (http://2016.igem.org/Team:Waterloo/Integrated_Practices/Networks). The "3_.." prefix for the intra-team case constructs bipartite networks from the user-task relationships and compares network properties like nestedness and modularity for the GPT networks vs the manually curated team networks.  

The figures made in the R notebooks are contained in the "figures" folder. There is also a "custom_functions.R" script which contains functions for processing and visualisation that are used across different notebooks. 

Folder "data" contains all the data used in the analysis and processing. It has 2 sub-folders, "raw" and "processed". The "raw" data folder contains all the data used which were collected and processed previously. The "raw" folder has a subfolder "full text" which has the full text of team wikis from 2008-2018, organised by year and by team. Any analysis and processing performed in the work are contained in the "processed" folder. It also contains the manually curated random samples for the inter- and intra- team cases.

For more information/clarification - please email to "jeyaram.rathin@cri-paris.org"
