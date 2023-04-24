##
# If using RefmanagR

# ```{r bib_setup, include=FALSE, message=FALSE, warning=FALSE}
# file.name <- system.file("Bib", "BrunaPublications.bib", package = "RefManageR") 
# bib <- ReadBib("./BrunaPublications.bib", check = FALSE) 
# ```
# #       
# `r NoCite(bib = bib, title = "*")` 
# #       
# \begingroup  
# \setlength{\leftskip}{.2cm}   
# ```{r results = "asis", echo = FALSE} 
# PrintBibliography(bib, .opts = list(check.entries = FALSE, sorting = "ydnt",max.names=1000)) 
# ``` 
# \endgroup


<macro name="sort-key">
  <choose>
  <if type="Software">
    <text value="1"/>
      </if>
      <else-if type="Software">
        <text value="2"/>
          </else-if>
          <else>
          <text value="3"/>
            </else>
            </choose>
            </macro>


            <key macro="sort-key" sort="ascending"/>

# https://www.aggieerin.com/post/updating-your-cv-with-packages/


library(scholar)
emilio_scholar <- get_publications("mgXvbcYAAAAJ&hl", #my scholar id
                            cstart = 0, #get everything
                            pagesize = 1000, #max amount of things to get
                            flush = F) #caching parameter

# pull information from orcid ---------------------------------------------
library(rorcid)
emilio_orcid <- works(orcid_id("0000-0003-3381-8477")) #my orcid
#note you will have to authorize your orcid for each folder you use this file in

# # set up dois -------------------------------------------------------------
# set up the ORC pages to merge with the Scholar pages. 
# Unfortunately, the ORCID one pulls in a column that’s a list within each cell,
# so I converted that into a doi (the only variable I wanted) column separately.

#blank doi spot
emilio_orcid$doi = NA 

#loop over the list to get just the dois
for (i in 1:nrow(emilio_orcid)){
  
  #grab the list
  temp = emilio_orcid$`external-ids.external-id`[[i]]
  
  #see if the doi is there 
  if (length(temp) > 0 && length(temp[ temp$`external-id-type` == "doi" ,  "external-id-value" ] > 0)) {
    emilio_orcid$doi[i] = temp[ temp$`external-id-type` == "doi" ,  "external-id-value" ]
  } #close if, this handles no dois and multiple ids
  
} #close for loop

#take out the duplicates
emilio_orcid <- emilio_orcid[!duplicated(emilio_orcid$title.title.value) , ]


# Now that we have the two datasets and have extracted the dois,
# let’s merge them together.

# merge two pub lists -----------------------------------------------------

emilio_orcid$title <- emilio_orcid$title.title.value
final <- merge(emilio_scholar, emilio_orcid, by = "title", all = T)


# The last piece here is to compare that with my curated list. 
# First, I pulled out all the specific ids that Scholar and ORCID have given my paper. 
# Sometimes, they get multiple ids, so I included them in my curated excel file by 
# doing something like: id1, id2, which makes them easy to split up later.

# compare with curated list -------------------------------------------------
c_pubid = na.omit(unlist(strsplit(gsub(" ", "", curated$pubid), ",")))
c_putcode = na.omit(unlist(strsplit(gsub(" ", "", curated$`put-code`), ",")))
f_pubid = na.omit(unlist(strsplit(gsub(" ", "", as.character(final$pubid)), ",")))
f_putcode = na.omit(unlist(strsplit(gsub(" ", "", final$`put-code`), ",")))


# Next, I compared those to the actual ones in Scholar and ORCID:
  
#find ones that aren't in the curated set from google
google_diff = setdiff(f_pubid, c_pubid)

#find ones that aren't in the curated set from orcid
orc_diff = setdiff(f_putcode, c_putcode)

newrows = final[ final$pubid %in% google_diff | 
                   final$`put-code` %in% orc_diff, ]

#create an output for cutting and pasting
curated_check = matrix(NA, nrow = nrow(newrows), ncol = ncol(curated))
colnames(curated_check) = colnames(curated)
curated_check = as.data.frame(curated_check)

curated_check$doi = newrows$doi
curated_check$title = newrows$title
curated_check$author = newrows$author
curated_check$journal = newrows$journal
curated_check$volume = newrows$number
curated_check$year = newrows$year
curated_check$pubid = newrows$pubid
curated_check$`put-code` = newrows$`put-code`

#write.csv(curated_check, "check_these.csv", row.names = F)


# setdiff takes the ones that are in the final dataset (f) 
# but aren’t in the curated dataset (c). Then I grabbed the rows in 
# the final dataset that were in the “haven’t seen these yet” 
# pile using %in%. newrows is a list of information found in both of 
# these services that aren’t in your CV. I use the word “new” loosely here. 
# Let’s see what it found, as I’ve just updated my cv.

newrows$title