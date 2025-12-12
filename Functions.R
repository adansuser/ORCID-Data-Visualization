rorcid_paks <- function() {
  install.packages('dplyr')
  install.packages('tibble')
  install.packages('tidyr')
  install.packages('purrr')
  install.packages('readr')
  install.packages('stringr')
  install.packages('jsonlite')
  install.packages('lubridate')
  install.packages('ggplot2')
  install.packages('httr')
  install.packages('forcats')
  install.packages('rorcid')
  install.packages('usethis')
  install.packages('anytime')
  install.packages('janitor')
  install.packages('glue')
  install.packages('remotes')
  remotes::install_github("ropensci/rcrossref")
  install.packages('roadoi')
  install.packages('inops')
  install.packages("rdatacite")
  install.packages("data.table")
  
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(jsonlite)
  library(lubridate)
  library(ggplot2)
  library(httr)
  library(forcats)
  library(usethis)
  library(anytime)
  library(janitor)
  library(glue)
  library(rorcid)
  library(rcrossref)
  library(roadoi)
  library(inops)
  library(rdatacite)
  library(data.table)
  library(geonames)
}

#--------------------------


shiny_paks <- function(){
  install.packages("shiny")  
  install.packages("readr") 
  install.packages("dplyr")  
  install.packages("ggplot2")
  install.packages("bslib")  
  install.packages("bsicons")  
  install.packages("plotly")   
  install.packages("leaflet") 
  install.packages("htmltools") 
  install.packages("DT")      
  install.packages("stringr") 
  install.packages("tidyr")    
  
  library(shiny)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(bslib)
  library(bsicons)
  library(plotly)
  library(leaflet)
  library(htmltools)
  library(DT)
  library(stringr)
  library(tidyr)
}

#------------------------

accessToken <- function() {
  orcidRequest <- POST(url  = "https://orcid.org/oauth/token",
                       config = add_headers(`Accept` = "application/json",
                                            `Content-Type` = "application/x-www-form-urlencoded"),
                       body = list(grant_type = "client_credentials",
                                   scope = "/read-public",
                                   client_id = orcidClientId,
                                   client_secret = orcidClientSecret),
                       encode = "form")
  orcidResponse <- content(orcidRequest)
  print(orcidResponse$accessToken)
  usethis::edit_r_environ()
}

#----------------------------

employment <- function(){
  # get the employments from the orcidIdentifierPath column
  ##### TIME: be patient, this may take a long time (e.g. for Temple University's data [~3500 IDs], this took ~8 minutes)
  myEmployment <- rorcid::orcidEmployments(myOrcidsData$orcidIdentifierPath)

  ##### WRITE/READ JSON uncomment to work with this data outside of R or read it back in later
  #to_write<-toJSON(myEmployment, na="null")
  #write(to_write,"./data/employment.json")

  # read it back in, if necessary
  #myEmployment <- read_json("./data/processed/employment.json", simplifyVector = TRUE)
  ##### WRITE/READ JSON

  # extract the employment data and mutate the dates
  myEmploymentData <- myEmployment %>%
    purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
    purrr::flatten_dfr() %>%
    janitor::clean_names() %>%
    dplyr::mutate(employmentSummaryEndDate = anytime::anydate(employmentSummaryEndDate/1000),
                  employmentSummaryCreatedDateValue = anytime::anydate(employmentSummaryCreatedDateValue/1000),
                  employmentSummaryLastModifiedDateValue = anytime::anydate(employmentSummaryLastModifiedDateValue/1000))

  # clean up the column names
  names(myEmploymentData) <- names(myEmploymentData) %>%
    stringr::str_replace(., "employment_summary_", "") %>%
    stringr::str_replace(., "source_source_", "") %>%
    stringr::str_replace(., "organization_disambiguated_", "")

  # view the unique institutions in the organization names columns
  # keep in mind this will include all institutions a person has in their employments section
  myOrganizations <- myEmploymentData %>%
    group_by(organizationName) %>%
    count() %>%
    arrange(desc(n))

  # view the variation in organization names by looking at my_organization_filtered (will open a new tab)
  # view(myOrganizations)

  # Note that this will give you employment records only. 
  # In other words, each row represents a single employment record for an individual.
  # the name_value variable refers specifically to the name of the person or system
  # that wrote the record, NOT the name of the individual. 

  # To get that, you must first get all the unique ORCID iDs from the dataset:
  
  # There is no distinct value identifying the orcid ID of the person.
  # The orcidPath value corresponds to the path of the person who added the employment record (which is usually, but not always the same)
  # Therefore you have to strip out the ORCID iD from the 'path' variable first and put it in it's own value and use it
  # We do this using str_sub from the stringr package
  # While we are at it, we can select and reorder the columns we want to keep
  currentEmploymentAll <- myEmploymentData %>%
    mutate(orcidIdentifier = str_sub(path, 2, 20)) %>%
    select(any_of(c("orcidIdentifier",
                    "organizationName",
                    "organizationAddressCity",
                    "organizationAddressRegion",
                    "organizationAddressCountry",
                    "organizationIdentifier",
                    "organization_disambiguated_organizationIdentifier",
                    "departmorganizationDisambiguationSourceentName",
                    "departmentName",
                    "roleTitle",
                    "urlValue",
                    "displayIndex",
                    "visibility",
                    "createdDateValue",
                    "startDateYearValue",
                    "startDateMonthValue",
                    "startDateDayValue",
                    "endDateYearValue",
                    "endDateMonthValue",
                    "endDateDayValue")))

  # next, create a new vector uniqueOrcids that includes only unique ORCID iDs from our filtered dataset.     
  uniqueOrcids <- unique(currentEmploymentAll$orcidIdentifier) %>%
    na.omit(.) %>%
    as.character()
  
  # then run the following expression to get all biographical information for those iDs.
  ##### TIME: This may take anywhere from a few seconds to a few minutes (e.g. for Temple University's data [~700 IDs], this took ~1.5 minutes)
  myOrcidPerson <- rorcid::orcid_person(uniqueOrcids)
  
  # then we construct a data frame from the response. 
  # See more at https://ciakovx.github.io/rorcid.html#Getting_the_data_into_a_data_frame for this.
  myOrcidPersonData <- myOrcidPerson %>% {
    dplyr::tibble(
      givenName = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
      createdDate = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_integer_),
      last_modified_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_character_),
      family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
      credit_name = purrr::map_chr(., purrr::pluck, "name", "credit-name", "value", .default=NA_character_),
      other_names = purrr::map(., purrr::pluck, "other-names", "other-name", "content", .default=NA_character_),
      orcidIdentifierPath = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
      biography = purrr::map_chr(., purrr::pluck, "biography", "content", .default=NA_character_),
      researcher_urls = purrr::map(., purrr::pluck, "researcher-urls", "researcher-url", .default=NA_character_),
      emails = purrr::map(., purrr::pluck, "emails", "email", "email", .default=NA_character_),
      keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", .default=NA_character_),
      external_ids = purrr::map(., purrr::pluck, "external-identifiers", "external-identifier", .default=NA_character_))
  } %>%
    dplyr::mutate(createdDate = anytime::anydate(as.double(createdDate)/1000),
                  last_modified_date = anytime::anydate(as.double(last_modified_date)/1000))
  
  # Join it back with the employment records so that the employment data now includes organization city, region, country
  orcidPersonEmploymentJoin <- myOrcidPersonData %>%
    left_join(currentEmploymentAll, by = c("orcidIdentifierPath" = "orcidIdentifier"))
  
  ##### WRITE/READ CSV uncomment to save this data and read it back in later
  #write_csv(orcidPersonEmploymentJoin, "./data/orcid_employment_file.csv")
  
  # read it back in, if necessary
  #orcidPersonEmploymentJoin <- read_csv("./data/orcid_employment_file.csv", col_types = cols(.default = "c"))
  ##### WRITE/READ CSV


}

#-----------------------------
  

Work_Data <- function() {
  # create a vector of unique, unduplicated ORCID IDs from that file
  myOrcids <- orcidPersonEmploymentJoin %>%
    filter(!duplicated(orcidIdentifierPath)) %>%
    pull(orcidIdentifierPath) %>%
    na.omit() %>%
    as.character()
  
  # Call the orcidWorks function to collect all works associated with each ID
  ##### TIME: This may take anywhere from a few seconds to a few minutes (e.g. for Temple University's data [~700 IDs], this took ~2.5 minutes)
  myWorks <- rorcid::orcidWorks(myOrcids)
  
  ##### WRITE/READ JSON uncomment to work with this data outside of R or read it back in later
  #to_write<-toJSON(myWorks, na="null")
  #write(to_write,"./data/myWorks.json")
  
  # read it back in, if necessary
  #myWorks <- read_json("./data/myWorks.json", simplifyVector = TRUE)
  ##### WRITE/READ JSON
  
  # turn the JSON file into a unique data frame by looping through the file,
  # extracting ("pluck") the object, bind the rows together with(this is the "_dfr" part of map_dfr)
  # then clean column names
  # and convert the dates from Unix time to yyyy-mm-dd
  myWorksData <- myWorks %>%
    purrr::map_dfr(pluck, "works") %>%
    janitor::clean_names() %>%
    dplyr::mutate(createdDateValue = anytime::anydate(createdDateValue/1000),
                  last_modified_date_value = anytime::anydate(last_modified_date_value/1000))
  
  # we only want to keep works that have an external identifier
  # (specifically, a DOI), so we first filter to keep only objects that have an external_id value
  # then unnest those: in other words expand to include a row for every work + external id value
  # (in other words, one work might be linked to a DOI, a PubMed ID, an ISSN, etc.)
  myWorksExternalIDs <- myWorksData %>% 
    dplyr::filter(!purrr::map_lgl(external_ids_external_id, purrr::is_empty)) %>% 
    tidyr::unnest(external_ids_external_id) %>%
    clean_names()
  
  # From those unnested external IDs, we want to keep only those with a DOI, as that is the 
  # value we'll use to look up the items in Crossref.
  # We then select a few relevant columns, and finally create a new column DOI that takes the external_id_value column
  # and coerces it to lower case, and the orcidIdentifier column which strips out the ORCID ID
  # from the path variable.
  dois <- myWorksExternalIDs %>%
    filter(external_id_type == "doi") %>%
    select(type, path, title_title_value, external_id_type, external_id_value, external_id_relationship,
           urlValue, publicationDateYearValue, publication_date_month_value, publication_date_day_value,
           journal_title_value) %>%
    mutate(doi = tolower(external_id_value),
           orcidIdentifier = str_sub(path, 2, 20))
  
  # there are some duplicated values here: we can't just look at duplicate DOIs because some of these
  # works were co-authored, and we want to keep that data (i.e. unique orcid + doi combinations)
  # This function will let you look at observations where both the orcid ID and the DOI are duplicated in 
  # case you want to review them more closely. 
  # In our case below, we just keep the first appearance of a unique orcid + doi combination and discard
  # all subsequent ones.
  dupes <- dois %>%
    get_dupes(orcidIdentifier, doi)
  
  # Here we are preparing the orcid dataset for merging to publications. 
  # We keep only Orcid ID, first name and last name, remove duplicates, and rename orcidIdentifier
  orcidEmplMerge <- orcidPersonEmploymentJoin %>%
    select(orcidIdentifierPath, givenName, family_name) %>%
    filter(!duplicated(orcidIdentifierPath)) %>%
    rename(orcidIdentifier = orcidIdentifierPath)
  
  # Finally, we remove the duplicates by creating a new variable that is a combination of
  # the orcid ID and the DOI, and keeping only the first instance. We then join that to our 
  # cleaned orcid ID file and write to csv
  doisUnduped <- dois %>%
    mutate(orcid_doi = paste0(orcidIdentifier, doi)) %>%
    filter(!duplicated(orcid_doi)) %>%
    left_join(orcidEmplMerge, by = "orcidIdentifier")
  
  ##### WRITE/READ CSV uncomment to save this data and read it back in later
  #write_csv(doisUnduped, "./data/orcid_dois.csv")
  
  # read it back in, if necessary
  #doisUnduped <- read_csv("./data/orcid_dois.csv")
  ##### WRITE/READ CSV
  
}

#------------------------------------

CrossRef_Data <- function() {
  doisSinceYear <- doisUnduped %>%
    filter(publicationDateYearValue >= myYear)
  
  metadataSinceYear <- map(doisSinceYear$doi, function(z) {
    print(z)
    o <- cr_works(dois = z)
    return(o)
  })
  
  metadataSinceYear_df <- metadataSinceYear %>%
    map_dfr(., pluck("data")) %>%
    clean_names() %>%
    filter(!duplicated(doi))
  
  orcidMerge <- doisSinceYear %>%
    select(orcidIdentifier, doi, givenName, family_name)
  
  crMerge <- metadataSinceYear_df %>%
    select(any_of(c("doi",
                    "title",
                    "published_print", 
                    "published_online", 
                    "issued", 
                    "container_title",
                    "issn",
                    "volume",
                    "issue",
                    "page",
                    "publisher",
                    "language",
                    "isbn",
                    "url",
                    "type",
                    "subject",
                    "reference_count",
                    "is_referenced_by_count",
                    "subject",
                    "alternative_id",
                    "author",
                    "pdf_url")))
  
  crMerge$reference_count <- as.numeric(as.character(crMerge$reference_count))
  crMerge$is_referenced_by_count <- as.numeric(as.character(crMerge$is_referenced_by_count))
  
}

orcidID_refTable <- function() {
  orcidPersonEmploymentJoin$fullname <- with(orcidPersonEmploymentJoin, paste(givenName,family_name))
  orcidPersonEmploymentJoin$fullname <- str_replace_all(orcidPersonEmploymentJoin$fullname, "[^[:alnum:]]", " ")
  orcidPersonEmploymentJoin$fullname<-str_replace_all(orcidPersonEmploymentJoin$fullname, fixed(" "), "")
  
  master_names <- orcidPersonEmploymentJoin %>%
    select(any_of(c("fullname",
                    "orcidIdentifierPath",
                    "departmentName",
                    "organizationName",
                    "organizationAddressCity",
                    "organizationAddressRegion",
                    "organizationAddressCountry"
    )))
  master_names <- master_names[!duplicated(master_names$orcidIdentifierPath),]
  
  credit_names <- orcidPersonEmploymentJoin %>%
    filter(!is.na(credit_name)) %>%
    select(any_of(c("credit_name",
                    "orcidIdentifierPath",
                    "departmentName",
                    "organizationName",
                    "organizationAddressCity",
                    "organizationAddressRegion",
                    "organizationAddressCountry"
    ))) %>%
    rename(fullname = credit_name)
  
  credit_names$fullname <- str_replace_all(credit_names$fullname, "[^[:alnum:]]", " ")
  credit_names$fullname<-str_replace_all(credit_names$fullname, fixed(" "), "")
  
  credit_names <- credit_names[!duplicated(credit_names$orcidIdentifierPath),]
  names_df <- rbind(master_names,credit_names)
}



DataCit_data <- function(){
  `%ni%` <- Negate(`%in%`)
  
  # Make a list of all dois not found through CrossRef
  filtered_dois <- dplyr::filter(doisSinceYear, doisSinceYear$doi %ni% metadataSinceYear_df$doi)
  
  # retrieving metadata from datacite
  metadataSinceYear <- map(filtered_dois$doi, function(z){
    print(z)
    o <- dc_dois(z,detail=TRUE)
    return(o)    
  })
  
  dc_metadataSinceYear_df <- metadataSinceYear %>%
    map_dfr(., pluck("data")) %>%
    clean_names() %>%
    dplyr::filter(!duplicated(attributes$doi)) 
  
  # unnesting dates, titles, and authors information
  dc_metadataSinceYear_df <- dc_metadataSinceYear_df %>%
    unnest_wider(attributes) %>%
    unnest_wider(dates, names_sep = "_") %>%
    unnest_wider(dates_1, names_sep = "_") %>%
    unnest_wider(titles, names_sep = "_") %>%
    unnest_wider(titles_1, names_sep = "_") %>%
    unnest_wider(creators, names_sep = "_") %>%
    unnest_wider(types, names_sep = "_") 
  
  # duplicate published column to match with crMerge
  dc_metadataSinceYear_df <- dc_metadataSinceYear_df %>%
    mutate(published_print = published)
  
  # rename columns  
  dc_metadataSinceYear_df <- dc_metadataSinceYear_df %>%
    rename(
      date = dates_1_date,
      dateType = dates_1_dateType,
      title = titles_1_title,
      author = creators_1,
      data_type = type,
      type = types_citeproc,
      reference_count = referenceCount,
      is_referenced_by_count = citationCount,
      alternative_id = id,
      published_online = published
    )
  
  # add some columns for issued, available and submitted dates info
  dc_metadataSinceYear_df[,c("issued", "available", "submitted")] = NA
  
  # function to populate new columns
  create_date_columns <- function(df) {
    
    for (i in 1:nrow(df)) {
      dates <- unlist(df$date[i])
      print(dates)
      types <- unlist(df$dateType[i])
      print(types)
      
      for (j in 1:length(types)) {
        if (types[j] == "Issued") {
          df$issued[i] <- dates[j]
          print("Issued")
        }
        if (types[j] == "Available") { #maybe only keep issued then??
          df$available[i] <- dates[j]
          print("Available")
        }
        if (types[j] == "Submitted") {
          df$submitted[i] <- dates[j]
          print("Submitted")
        }
        if (types[j] == "Published") {
          df$published[i] <- dates[j]
          print("Published")
        }
      }
    }
    return(df)
  }
  
  
  # populate the columns
  dc_metadataSinceYear_df <- dc_metadataSinceYear_df %>%
    create_date_columns() 
  
  # select relevant columns
  dc_merge <- dc_mdata_since_year_df %>%
    select(any_of(c("doi",
                    "title",
                    "published_print", 
                    "published_online", 
                    "issued", 
                    #"container_title",
                    "issn",
                    "volume",
                    "issue",
                    "page",
                    "publisher",
                    "language",
                    "isbn",
                    "url",
                    "type",
                    #"subject", 
                    "reference_count",
                    "is_referenced_by_count",
                    #"subjects",
                    "alternative_id",
                    "author",
                    "pdf_url")))
}


#----------------------------

CoAuth_Info <- function() {
  # The authors for each DOI in the crMerge dataframe are in a nested list. 
  # In order to collect information about them, we must unnest the list,
  # Then we will build a list of home author, co-author pairs and try ti fill in any unknown ORCID
  # and location info about the co-authors
  
  # unnest the author list for each DOI
  whatAuths <- crMerge %>% unnest(author)
  
  # left join this DOI authors list to our list of home authors by DOI
  # this gives us a df where there is an individual row for each home author and co-author on a  DOI
  authlistAll <- whatAuths %>%
    left_join(orcidMerge, by = "doi")
  
  # when multiple home authors have collaborated on a DOI there will be several sets of
  # rows for that DOI in the data frame - one set for each home author
  # we keep these because we're counting each home author and all their collaborations, including within institution
  
  # we do want to remove rows produced by the join where the home author (orcidIdentifier) is 
  # the same as the co-author (ORCID) - so where orcidIdentifier = str_sub(ORCID , 18, 37)
  # AND where the home author / co-author names are exactly the same
  # this will miss slight variations in names when there is no ORCID ID on the cross ref record (e.g. Bradley Baker vs. Bradley J. Baker)
  
  # add some columns to authlistAll to help with this deduplicating
  authlistAll$orcid_coauth <- with(authlistAll, 
                                   ifelse(is.na(ORCID),'',str_sub(ORCID , 18, 37))
  )
  
  # fullname identifier for the home author, striped of punctuation and whitespace
  authlistAll$anchorfullname <- with(authlistAll, paste(givenName,family_name))
  authlistAll$anchorfullname <- str_replace_all(authlistAll$anchorfullname, "[^[:alnum:]]", " ")
  authlistAll$anchorfullname<-str_replace_all(authlistAll$anchorfullname, fixed(" "), "")
  
  # fullname identifier for the co-author, striped of punctuation and whitespace
  authlistAll$coauthfullname <- with(authlistAll, paste(given,family))
  authlistAll$coauthfullname <- str_replace_all(authlistAll$coauthfullname, "[^[:alnum:]]", " ")
  authlistAll$coauthfullname<-str_replace_all(authlistAll$coauthfullname, fixed(" "), "")
  
  ## create a new df with the identical entries removed
  authlistNodups <- subset(authlistAll, (orcidIdentifier != orcid_coauth))
  authlistNodups <- subset(authlistNodups, (anchorfullname != coauthfullname))
  
  # next it would be good to fill in ORCID if there is a co-author name variation that 
  # we are already aware of and logged in names_df, our author ORCID ID reference table
  # when there are author name variations that we are not aware of, and there is no ORCID ID
  # there is just no way to resolve them, so the occasional row where home author and co-author are the same will persist 
  
  # left join to add ORCIDs from our reference table to the author list
  myJoin <- left_join(authlistNodups,names_df,by=c("coauthfullname" = "fullname"))
  
  # fill in the joined ORCID where orcid_coauth is blank
  myJoin[ myJoin$orcid_coauth == "", "orcid_coauth" ] <- myJoin[ myJoin$orcid_coauth == "", "orcidIdentifierPath" ]
  
  # this reintroducies NA values into the data fram, so replace those with blanks again
  myJoin <- myJoin %>% 
    mutate_at('orcid_coauth', ~replace_na(.,""))
  
  # do another pass to eliminate rows with the same anchor author and co-author ORCID from the ones we just filled in
  authlistNodups <- subset(myJoin, (orcidIdentifier != orcid_coauth))
  
  
  # now that we tried to fill in co-author ORCID IDs we can also fill in 
  # co-author current affiliations and location information that we have in the reference table names_df
  
  # but we have to use a version of the names_df where orcid is unique
  orcidDf <- names_df
  
  # remove duplicate orcid rows
  orcidDf <- orcidDf[!duplicated(orcidDf$orcidIdentifierPath),]
  
  myJoin <- left_join(authlistNodups,orcidDf,by=c("orcid_coauth" = "orcidIdentifierPath"))
  
  # fill in the joined location fields where any co-author locations are blank
  myJoin <- myJoin %>% 
    mutate(departmentName.x = coalesce(departmentName.x,departmentName.y),
           organizationName.x = coalesce(organizationName.x,organizationName.y),
           organizationAddressCity.x = coalesce(organizationAddressCity.x,organizationAddressCity.y),
           organizationAddressRegion.x = coalesce(organizationAddressRegion.x,organizationAddressRegion.y),
           organizationAddressCountry.x = coalesce(organizationAddressCountry.x,organizationAddressCountry.y)
    )
  
  # drop some columns we don't need
  authlistNodups <- subset(myJoin, select = -c(orcidIdentifierPath,departmentName.y,organizationName.y, organizationAddressCity.y, organizationAddressRegion.y, organizationAddressCountry.y))
  
  # now we have authlistNodups, a dataframe where there is a row for every co-author on a DOI except for the home author duplicate (ideally),
  # and each row also includes the home author's name and ORCID ID, and as much info about the co-author as we have so far
  
}

Build_Output <- function() {
  # we eventually want to output a CSV with these columns:
  # fname1, lname1, orcid1, affiliation1, org1, city1, region1, country1, fname2, lname2, orcid2, affiliation2, org2, city2, region2, country2, DOI
  
  # create a dataframe with the columns we need
  coAuthors <- authlistNodups %>%
    select(any_of(c("doi",
                    "issued",
                    "givenName",
                    "family_name", 
                    "orcidIdentifier", 
                    "given", 
                    "family",
                    "orcid_coauth",
                    "affiliation.name",
                    "organizationName.x",
                    "organizationAddressCity.x",
                    "organizationAddressRegion.x",
                    "organizationAddressCountry.x"
    )))
  
  # rename some columns
  coAuthors <- coAuthors %>% 
    rename(
      fname1 = givenName,
      lname1 = family_name,
      orcid1 = orcidIdentifier,
      fname2 = given,
      lname2 = family,
      orcid2 = orcid_coauth,
      affiliation2 = affiliation.name,
      org2 = organizationName.x,
      city2 = organizationAddressCity.x,
      region2 = organizationAddressRegion.x,
      country2 = organizationAddressCountry.x
    )
  
  # add in columns of home author info affiliation and location info
  # join the info in from our orcidDf reference table
  coAuthors <- left_join(coAuthors,orcidDf,by=c("orcid1" = "orcidIdentifierPath"))
  
  # rename the joined affiliation and location fields for the home author
  coAuthors <- coAuthors %>% 
    rename(
      affiliation1 = departmentName,
      org1 = organizationName,
      city1 = organizationAddressCity,
      region1 = organizationAddressRegion,
      country1 = organizationAddressCountry
    )
  
  # move the columns around
  coAuthors <- coAuthors %>% relocate(affiliation1, org1, city1, region1, country1, .after = orcid1)
  
  # fill in with static values if there are blanks -- there realy shouldn't be any but just in case
  coAuthors$org1[coAuthors$org1 == "" | coAuthors$org1 == " " | is.na(coAuthors$org1)]<- anchorOrg
  coAuthors$city1[coAuthors$city1 == "" | coAuthors$city1 == " " | is.na(coAuthors$city1)]<- anchorCity
  coAuthors$region1[coAuthors$region1 == "" | coAuthors$region1 == " " | is.na(coAuthors$region1)]<- anchorRegion
  coAuthors$country1[coAuthors$country1 == "" | coAuthors$country1 == " " | is.na(coAuthors$country1)]<- anchorCountry
  
  
  # though we might have filled in a few pieces of co-author info for some of the co-authors from the same institution above,
  # we stil need city, region, and country for many of the co-authors. we can try to retrive this if we have the co-authors ORCID ID
  # we'll make a unique list of co-author's who have ORCID IDs and get their CURRENT affiliation
  # we chose to get their current affiliation because this is the same way we treat home authors 
  # (they are a home author because of their current affiliation, 
  # even though they may have published a DOI in the past when affiliated with a different organization)
  coAuthIds <- coAuthors$orcid2
  coAuthIdsUnduped <- unique(coAuthIds[coAuthIds != ""])
  
  # if a value in coAuthIdsUnduped gives an error when you try to generate myCoAuthsEmployment below
  # (like that it is locked and cannot be edited)
  # remove it from the list by filling in the problem ORCID ID (format XXXX-XXXX-XXXX-XXXX), uncommenting, and running this statement
  # then try to generate myCoAuthsEmployment again
  #coAuthIdsUnduped <- coAuthIdsUnduped[ coAuthIdsUnduped != "enter problem ORCID ID here in format XXXX-XXXX-XXXX-XXXX"]
  
  # get the co-authors employment data from their ORCID profile
  ##### TIME: This may take anywhere from a few seconds to a few minutes (e.g. for Temple University's 2022 data [>850 IDs], this took ~2 minutes)
  myCoAuthsEmployment <- rorcid::orcidEmployments(coAuthIdsUnduped)
  
  ##### JSON
  # you can write the file to json if you want to work with it outside of R
  #to_write<-toJSON(myCoAuthsEmployment, na="null")
  #write(to_write,"./data/co_auths_employment.json")
  
  # read it back in, if necessary
  #myCoAuthsEmployment <- read_json("./data/co_auths_employment.json", simplifyVector = TRUE)
  ##### JSON
  
  # extract the employment data and mutate the dates
  myCoAuthsEmploymentData <- myCoAuthsEmployment %>%
    purrr::map(., purrr::pluck, "affiliation-group", "summaries") %>% 
    purrr::flatten_dfr() %>%
    janitor::clean_names() %>%
    dplyr::mutate(employmentSummaryEndDate = anytime::anydate(employmentSummaryEndDate/1000),
                  employmentSummaryCreatedDateValue = anytime::anydate(employmentSummaryCreatedDateValue/1000),
                  employmentSummaryLastModifiedDateValue = anytime::anydate(employmentSummaryLastModifiedDateValue/1000))
  
  # clean up column names
  names(myCoAuthsEmploymentData) <- names(myCoAuthsEmploymentData) %>%
    stringr::str_replace(., "employment_summary_", "") %>%
    stringr::str_replace(., "source_source_", "") %>%
    stringr::str_replace(., "organization_disambiguated_", "")
  
  # some rows have orcidPath = NA, for these put the ORCID ID back with substring of path
  myCoAuthsEmploymentData <- myCoAuthsEmploymentData %>% 
    mutate(orcidPath = coalesce(orcidPath,substring(path,2,20)))
  
  # get the co-authors' current affiliations
  # this will miss co-authors who have no current employment line (with no end date) in their ORCID profile
  myCoAuthsEmploymentDataFilteredCurrent <- myCoAuthsEmploymentData %>%
    dplyr::filter(is.na(endDateYearValue))
  
  # some co-authors may have multiple "current" affiliations
  # seperate out those with no start date year value and those that do have start dates
  myCoAuthsCurrentEmpNodate <- subset(myCoAuthsEmploymentDataFilteredCurrent, is.na(startDateYearValue))
  myCoAuthsCurrentEmpDate <- subset(myCoAuthsEmploymentDataFilteredCurrent, !is.na(startDateYearValue))
  
  # for those with a start date, choose the row with the most recent year
  latestDates <- myCoAuthsCurrentEmpDate %>%
    group_by(orcidPath) %>%
    slice(which.max(startDateYearValue)) %>%
    arrange(startDateYearValue)
  
  coAuthsLatestEmp <- rbind(myCoAuthsCurrentEmpNodate,latestDates)
  
  # there will STILL be duplicates because of people with a mix of undated and dated ORCID profile employment entries, 
  # group again and use the latest entry date
  coAuthsVeryLatestEmp  <- coAuthsLatestEmp  %>%
    group_by(orcidPath) %>%
    slice(which.max(createdDateValue)) %>%
    arrange(createdDateValue)
  
  # be double sure that we removed duplicate orcid rows
  coAuthsVeryLatestEmp <- coAuthsVeryLatestEmp[!duplicated(coAuthsVeryLatestEmp$orcidPath),]
  
  # for the co-authors that had ORCID profiles and for whom we now have a current employment data point, join them back to the coAuthors dataframe
  coAuthorsFullInfo <- left_join(coAuthors,coAuthsVeryLatestEmp,by=c("orcid2" = "orcidPath"))
  
  # If org2, city2, region2, country2 had been NA in the dataframe we are building to output, fill from the joined table fields 
  coAuthorsFullInfo <- coAuthorsFullInfo %>% 
    mutate(org2 = coalesce(org2,organizationName),
           city2 = coalesce(city2,organizationAddressCity),
           region2 = coalesce(region2,organizationAddressRegion),
           country2 = coalesce(country2,organizationAddressCountry)
    )
  
  # drop some columns we don't need
  coAuthorsFullInfo <- coAuthorsFullInfo %>% select(doi:country2)
  
  ##### Code improvement
  # from here you could do yet ANOTHER round of recording co-author fullnames and ORCID IDs to the reference dataframe, 
  # then fill in blanks in the full_info df
  # when the code that does that is pulled out into its own function, that won't take a lot of space to do
  ##### Code improvement
  
  # get rid of NA values
  coAuthorsFullInfo[is.na(coAuthorsFullInfo)] <- ""
  
  
  # clean up US state names
  # set up a dataframe of state names and abbreviations
  states_df<- data.frame(state.abb, state.name, paste0(state.name,'US'))
  colnames(states_df) <- c('abb','name','id')
  
  # Do the same for Canadian provinces
  provinces_df <- data.frame(
    abb = c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"),
    name = c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", 
             "Nova Scotia", "Northwest Territories", "Nunavut", "Ontario", "Prince Edward Island", 
             "Quebec", "Saskatchewan", "Yukon"),
    id = paste0(c("Alberta", "British Columbia", "Manitoba", "New Brunswick", 
                  "Newfoundland and Labrador", "Nova Scotia", "Northwest Territories", 
                  "Nunavut", "Ontario", "Prince Edward Island", "Quebec", 
                  "Saskatchewan", "Yukon"), ' CA')
  )
  colnames(provinces_df) <- c('abb','name','id')
  
  # left join the correct state abbreviation for only US states with the full state name spelled out
  # starting with the home authors' region1
  coAuthorsFullInfo$state1<-with(coAuthorsFullInfo,paste0(region1,country1))
  coAuthorsFullInfo <- left_join(coAuthorsFullInfo,states_df,by=c("state1" = "id"))
  
  # overwrite the full state names with the abbreviations where they occur
  coAuthorsFullInfo$region1 <- ifelse(is.na(coAuthorsFullInfo$abb), coAuthorsFullInfo$region1, coAuthorsFullInfo$abb )
  
  # drop the joined columns
  coAuthorsFullInfo <- coAuthorsFullInfo %>% select(doi:country2)
  
  # do the same for the region2, the coAuthors' US state names
  coAuthorsFullInfo$state2<-with(coAuthorsFullInfo,paste0(region2,country2))
  coAuthorsFullInfo <- left_join(coAuthorsFullInfo,states_df,by=c("state2" = "id"))
  coAuthorsFullInfo$region2 <- ifelse(is.na(coAuthorsFullInfo$abb), coAuthorsFullInfo$region2, coAuthorsFullInfo$abb )
  coAuthorsFullInfo <- coAuthorsFullInfo %>% select(doi:country2)
  
  # Now handle Canadian provinces
  # Create a combined province and country identifier
  coAuthorsFullInfo$province1 <- with(coAuthorsFullInfo, paste0(region1, country1))
  coAuthorsFullInfo <- left_join(coAuthorsFullInfo, provinces_df, by = c("province1" = "id"))
  
  # Overwrite full province names with abbreviations where they occur
  coAuthorsFullInfo$region1 <- ifelse(is.na(coAuthorsFullInfo$abb), coAuthorsFullInfo$region1, coAuthorsFullInfo$abb)
  
  # Drop the joined columns
  coAuthorsFullInfo <- coAuthorsFullInfo %>% select(doi:country2)
  
  # Repeat the same process for the second region
  coAuthorsFullInfo$province2 <- with(coAuthorsFullInfo, paste0(region2, country2))
  coAuthorsFullInfo <- left_join(coAuthorsFullInfo, provinces_df, by = c("province2" = "id"))
  coAuthorsFullInfo$region2 <- ifelse(is.na(coAuthorsFullInfo$abb), coAuthorsFullInfo$region2, coAuthorsFullInfo$abb)
  coAuthorsFullInfo <- coAuthorsFullInfo %>% select(doi:country2)
  
  ##### WRITE/READ CSV uncomment to save this data and read it back in later
  #write_csv(coAuthorsFullInfo, "./data/orcid-data.csv")
  
  # read it back in, if necessary
  #coAuthorsFullInfo <- read_csv("./data/orcid-data.csv", col_types = cols(.default = "c"))
  ##### WRITE/READ CSV
  
  # get latitude and longitude data for each location:
  # the sourced script (Geonames_Get_Lat_Long.R) will generate a final CSV file: "./data/orcid_data_latlng.csv"
  source("./Geonames_Get_Lat_Long.R")
}
