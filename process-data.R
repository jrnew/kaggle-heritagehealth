process_members <- function(
  members
) {
  members$Sex[members$Sex == ""] <- "Missing"
  members$Sex <- as.factor(members$Sex)
  members$AgeAtFirstClaim <- as.factor(members$AgeAtFirstClaim)
  members$AgeAtFirstClaimNum <- as.integer(ifelse(members$AgeAtFirstClaim == "0-9", 5,
                                                  ifelse(members$AgeAtFirstClaim == "10-19", 15,
                                                         ifelse(members$AgeAtFirstClaim == "20-29", 25,
                                                                ifelse(members$AgeAtFirstClaim == "30-39", 35,
                                                                       ifelse(members$AgeAtFirstClaim == "40-49", 45,
                                                                              ifelse(members$AgeAtFirstClaim == "50-59", 55,
                                                                                     ifelse(members$AgeAtFirstClaim == "60-69", 65,
                                                                                            ifelse(members$AgeAtFirstClaim == "70-79", 75,
                                                                                                   ifelse(members$AgeAtFirstClaim == "80+", 85,
                                                                                                          NA))))))))))
  return(members)
}

process_claims <- function(
  claims
) {
  # Add identifier to get around error for duplicate rows
  claims$ClaimID <- seq_along(claims$MemberID) 
  # Convert factors to numbers
  claims <- process_for_DSFS(claims)
  claims$PayDelayNum <- as.integer(ifelse(claims$PayDelay == "162+", 
                                          162, claims$PayDelay))       
  claims$LengthOfStayNum <- as.integer(ifelse(claims$LengthOfStay == "1 day", 1,
                                              ifelse(claims$LengthOfStay == "2 days", 2,
                                                     ifelse(claims$LengthOfStay == "3 days", 3,
                                                            ifelse(claims$LengthOfStay == "4 days", 4,
                                                                   ifelse(claims$LengthOfStay == "5 days", 5,
                                                                          ifelse(claims$LengthOfStay == "6 days", 6,
                                                                                 ifelse(claims$LengthOfStay == "1- 2 weeks", 11,
                                                                                        ifelse(claims$LengthOfStay == "2- 4 weeks", 21,
                                                                                               ifelse(claims$LengthOfStay == "4- 8 weeks", 42,
                                                                                                      ifelse(claims$LengthOfStay == "26+ weeks", 180,
                                                                                                             NA)))))))))))                     
  claims$CharlsonIndexNum <- as.integer(ifelse(claims$CharlsonIndex == "0", 0,
                                               ifelse(claims$CharlsonIndex == "1-2", 2,
                                                      ifelse(claims$CharlsonIndex == "3-4", 4, 6))))
  # Create a column for each category in Specialty, PlaceSvc, 
  # PrimaryConditionGroup, ProcedureGroup
  claims <- claims %>% 
    mutate(SpecialtyNew = ifelse(Specialty == "", "Missing", Specialty)) %>%
    mutate(PlaceSvcNew = ifelse(PlaceSvc == "", "Missing", PlaceSvc)) %>%
    mutate(PrimaryConditionGroupNew = ifelse(PrimaryConditionGroup == "", 
                                             "Missing", PrimaryConditionGroup)) %>%
    mutate(ProcedureGroupNew = ifelse(ProcedureGroup == "", "Missing", ProcedureGroup)) %>%
    mutate(SpecialtyNew = paste0("Specialty.", gsub(" ", "", SpecialtyNew))) %>%
    mutate(PlaceSvcNew = paste0("PlaceSvc.", gsub(" ", "", PlaceSvcNew))) %>%
    mutate(PrimaryConditionGroupNew = paste0("PrimaryConditionGroup.", 
                                             gsub(" ", "", PrimaryConditionGroupNew))) %>%
    mutate(ProcedureGroupNew = paste0("ProcedureGroup.", gsub(" ", "", ProcedureGroupNew))) %>%
    mutate(Value = 1) %>% spread(SpecialtyNew, Value) %>%
    mutate(Value = 1) %>% spread(PlaceSvcNew, Value) %>%
    mutate(Value = 1) %>% spread(PrimaryConditionGroupNew, Value) %>%
    mutate(Value = 1) %>% spread(ProcedureGroupNew, Value)
  varnames <- names(claims)[grepl("Specialty\\.|PlaceSvc\\.|PrimaryConditionGroup\\.|ProcedureGroup\\.", 
                                  names(claims))]
  varnamesnew <- gsub("\\.", "", varnames)
  # Remove period in column names and convert NAs to 0
  for (i in seq_along(varnames))
    claims <- claims %>%
    mutate_(.dots = setNames(paste0("ifelse(is.na(", varnames[i], "), 0, ", varnames[i], ")"),
                             varnamesnew[i])) %>%
    select_(paste0("-", varnames[i]))
  # Aggregate data by member-year
  claims_by_member_year <- claims %>% 
    group_by(MemberID, Year) %>%
    summarise(ClaimCount = length(MemberID),
              ClaimLOSCount = length(MemberID[LengthOfStayNum > 0]),
              ProviderIDCount = length(unique(ProviderID)),
              VendorCount = length(unique(Vendor)),
              PCPCount = length(unique(PCP)),
              SpecialtyCount = length(unique(Specialty)),
              PlaceSvcCount = length(unique(PlaceSvc)),
              PrimaryConditionGroupCount = length(unique(PrimaryConditionGroup)),
              CharlsonIndexCount = length(unique(CharlsonIndex)),
              ProcedureGroupCount = length(unique(ProcedureGroup)),
              LOSValid = ifelse(any(SupLOS == 0 & LengthOfStay != ""), 1, 0),
              LOSSup = ifelse(any(SupLOS == 1), 1, 0),
              LOSNull = ifelse(any(SupLOS == 0 & LengthOfStay == ""), 1, 0),
              LengthOfStayTotal = sum(LengthOfStayNum, na.rm = TRUE),
              LengthOfStayMin = min(LengthOfStayNum, na.rm = TRUE),
              LengthOfStayMax = max(LengthOfStayNum, na.rm = TRUE),
              LengthOfStayMean = mean(LengthOfStayNum, na.rm = TRUE),
              LengthOfStaySD = ifelse(sum(!is.na(LengthOfStayNum)) > 1, 
                                      sd(LengthOfStayNum, na.rm = TRUE), 
                                      ifelse(sum(!is.na(LengthOfStayNum)) == 1, 0, NA)),
              LengthOfStayRange = ifelse(sum(!is.na(LengthOfStayNum)) > 1, 
                                         LengthOfStayMax - LengthOfStayMin, 
                                         ifelse(sum(!is.na(LengthOfStayNum)) == 1, 0, NA)),
              DSFSTotal = sum(DSFSNum, na.rm = TRUE),
              DSFSMin = min(DSFSNum, na.rm = TRUE),
              DSFSMax = max(DSFSNum, na.rm = TRUE),
              DSFSMean = mean(DSFSNum, na.rm = TRUE),
              DSFSSD = ifelse(sum(!is.na(DSFSNum)) > 1, 
                              sd(DSFSNum, na.rm = TRUE), 
                              ifelse(sum(!is.na(DSFSNum)) == 1, 0, NA)),
              DSFSRange = ifelse(sum(!is.na(DSFSNum)) > 1, 
                                 DSFSMax - DSFSMin, 
                                 ifelse(sum(!is.na(DSFSNum)) == 1, 0, NA)),
              CharlsonIndexTotal = sum(CharlsonIndexNum, na.rm = TRUE),
              CharlsonIndexMin = min(CharlsonIndexNum, na.rm = TRUE),
              CharlsonIndexMax = max(CharlsonIndexNum, na.rm = TRUE),
              CharlsonIndexMean = mean(CharlsonIndexNum, na.rm = TRUE),
              CharlsonIndexSD = ifelse(sum(!is.na(CharlsonIndexNum)) > 1, 
                                       sd(CharlsonIndexNum, na.rm = TRUE), 
                                       ifelse(sum(!is.na(CharlsonIndexNum)) == 1, 0, NA)),
              CharlsonIndexRange = ifelse(sum(!is.na(CharlsonIndexNum)) > 1, 
                                          CharlsonIndexMax - CharlsonIndexMin, 
                                          ifelse(sum(!is.na(CharlsonIndexNum)) == 1, 0, NA)),
              PayDelayTotal = sum(PayDelayNum, na.rm = TRUE),
              PayDelayMin = min(PayDelayNum, na.rm = TRUE),
              PayDelayMax = max(PayDelayNum, na.rm = TRUE),
              PayDelayMean = mean(PayDelayNum, na.rm = TRUE),
              PayDelaySD = ifelse(sum(!is.na(PayDelayNum)) > 1, 
                                  sd(PayDelayNum, na.rm = TRUE), 
                                  ifelse(sum(!is.na(PayDelayNum)) == 1, 0, NA)),
              PayDelayRange = ifelse(sum(!is.na(PayDelayNum)) > 1, 
                                     PayDelayMax - PayDelayMin, 
                                     ifelse(sum(!is.na(PayDelayNum)) == 1, 0, NA))) %>%
    replace_NA_with_mean_by_year # Convert NAs to mean by year for quantitative variables
  # Convert NAs to 0 for qualitative variables
  varnamesnew <- c(varnamesnew, "SupLOS")
  for (i in seq_along(varnames)) {
    claims_by_member_year_temp <- claims %>%
      group_by(MemberID, Year) %>%
      summarise_(.dots = setNames(paste0("sum(", varnamesnew[i], ")"),
                                  paste0(varnamesnew[i], "Count")))
    claims_by_member_year <- claims_by_member_year %>% 
      left_join(claims_by_member_year_temp)
  }
  
  return(claims_by_member_year)
}

process_claims2 <- function(# Keep as claim-level data
  claims
) {
  # Add identifier to get around error for duplicate rows
  claims$ClaimID <- seq_along(claims$MemberID) 
  # Convert factors to numbers
  claims <- process_for_DSFS(claims)
  claims$PayDelayNum <- as.integer(ifelse(claims$PayDelay == "162+", 
                                          162, claims$PayDelay))       
  claims$LengthOfStayNum <- as.integer(ifelse(claims$LengthOfStay == "1 day", 1,
                                              ifelse(claims$LengthOfStay == "2 days", 2,
                                                     ifelse(claims$LengthOfStay == "3 days", 3,
                                                            ifelse(claims$LengthOfStay == "4 days", 4,
                                                                   ifelse(claims$LengthOfStay == "5 days", 5,
                                                                          ifelse(claims$LengthOfStay == "6 days", 6,
                                                                                 ifelse(claims$LengthOfStay == "1- 2 weeks", 11,
                                                                                        ifelse(claims$LengthOfStay == "2- 4 weeks", 21,
                                                                                               ifelse(claims$LengthOfStay == "4- 8 weeks", 42,
                                                                                                      ifelse(claims$LengthOfStay == "26+ weeks", 180,
                                                                                                             NA)))))))))))                     
  claims$CharlsonIndexNum <- as.integer(ifelse(claims$CharlsonIndex == "0", 0,
                                               ifelse(claims$CharlsonIndex == "1-2", 2,
                                                      ifelse(claims$CharlsonIndex == "3-4", 4, 6))))
  # Create a column for each category in Specialty, PlaceSvc, 
  # PrimaryConditionGroup, ProcedureGroup
  claims <- claims %>% 
    mutate(SpecialtyNew = ifelse(Specialty == "", "Missing", Specialty)) %>%
    mutate(PlaceSvcNew = ifelse(PlaceSvc == "", "Missing", PlaceSvc)) %>%
    mutate(PrimaryConditionGroupNew = ifelse(PrimaryConditionGroup == "", 
                                             "Missing", PrimaryConditionGroup)) %>%
    mutate(ProcedureGroupNew = ifelse(ProcedureGroup == "", "Missing", ProcedureGroup)) %>%
    mutate(SpecialtyNew = paste0("Specialty.", gsub(" ", "", SpecialtyNew))) %>%
    mutate(PlaceSvcNew = paste0("PlaceSvc.", gsub(" ", "", PlaceSvcNew))) %>%
    mutate(PrimaryConditionGroupNew = paste0("PrimaryConditionGroup.", 
                                             gsub(" ", "", PrimaryConditionGroupNew))) %>%
    mutate(ProcedureGroupNew = paste0("ProcedureGroup.", gsub(" ", "", ProcedureGroupNew))) %>%
    mutate(Value = 1) %>% spread(SpecialtyNew, Value) %>%
    mutate(Value = 1) %>% spread(PlaceSvcNew, Value) %>%
    mutate(Value = 1) %>% spread(PrimaryConditionGroupNew, Value) %>%
    mutate(Value = 1) %>% spread(ProcedureGroupNew, Value) %>%
    select(-DSFS, -PayDelay, -LengthOfStay, -CharlsonIndex) %>%
    rename(DSFS = DSFSNum, PayDelay = PayDelayNum, LengthOfStay = LengthOfStayNum, 
           CharlsonIndex = CharlsonIndexNum)
  varnames <- names(claims)[grepl("Specialty\\.|PlaceSvc\\.|PrimaryConditionGroup\\.|ProcedureGroup\\.", 
                                  names(claims))]
  varnamesnew <- gsub("\\.", "", varnames)
  # Remove period in column names and convert NAs to 0
  for (i in seq_along(varnames))
    claims <- claims %>%
    mutate_(.dots = setNames(paste0("ifelse(is.na(", varnames[i], "), 0, ", varnames[i], ")"),
                             varnamesnew[i])) %>%
    select_(paste0("-", varnames[i]))
  return(claims)
}

process_for_DSFS <- function(
  data
) {
  data$DSFSNum <- as.integer(ifelse(data$DSFS == "0- 1 month", 1,
                                    ifelse(data$DSFS == "1- 2 months", 2,
                                           ifelse(data$DSFS == "2- 3 months", 3,
                                                  ifelse(data$DSFS == "3- 4 months", 4,
                                                         ifelse(data$DSFS == "4- 5 months", 5,
                                                                ifelse(data$DSFS == "5- 6 months", 6,
                                                                       ifelse(data$DSFS == "6- 7 months", 7,
                                                                              ifelse(data$DSFS == "7- 8 months", 8,
                                                                                     ifelse(data$DSFS == "8- 9 months", 9,
                                                                                            ifelse(data$DSFS == "9-10 months", 10,
                                                                                                   ifelse(data$DSFS == "10-11 months", 11,
                                                                                                          ifelse(data$DSFS == "11-12 months", 12,
                                                                                                                 NA)))))))))))))
  return(data)
}

process_drugs <- function(
  drugs
) {
  drugs <- process_for_DSFS(drugs)
  drugs_by_member_year <- drugs %>% 
    mutate(DrugCountNum = as.integer(ifelse(DrugCount == "7+", 7, DrugCount))) %>%
    group_by(MemberID, Year) %>%
    summarise(DrugClaimCount = length(DrugCountNum),
              DrugCountTotal = sum(DrugCountNum),
              DrugCountMin = min(DrugCountNum),
              DrugCountMax = max(DrugCountNum),
              DrugCountMean = mean(DrugCountNum),
              DrugCountRange = DrugCountMax - DrugCountMin,
              DrugCountSD = sd(DrugCountNum),
              DrugCountLast = max(DrugCountNum[DSFSNum == max(DSFSNum)]))
  return(drugs_by_member_year)
}

process_labs <- function(
  labs
) {
  labs <- process_for_DSFS(labs)
  labs_by_member_year <- labs %>% 
    mutate(LabCountNum = as.integer(ifelse(LabCount == "10+", 7, LabCount))) %>%
    group_by(MemberID, Year) %>%
    summarise(LabClaimCount = length(LabCountNum),
              LabCountTotal = sum(LabCountNum),
              LabCountMin = min(LabCountNum),
              LabCountMax = max(LabCountNum),
              LabCountMean = mean(LabCountNum),
              LabCountRange = LabCountMax - LabCountMin,
              LabCountSD = sd(LabCountNum),
              LabCountLast = max(LabCountNum[DSFSNum == max(DSFSNum)]))
  return(labs_by_member_year)
}

replace_NA_with_mean_by_year <- function(
  data
) { 
  # Replace NA values with 0
  check_NA <- sapply(data, function(x) any(is.na(x)))
  varnames_NA <- names(check_NA)[check_NA]
  # Remove period in column names and convert NAs to 0
  data <- as.data.frame(data)
  for (i in seq_along(varnames_NA)) {
    mean_by_year <- by(data, factor(data$Year), function(x) mean(x[[varnames_NA[i]]], na.rm = TRUE))
    indices <- is.na(data[, varnames_NA[i]])
    data[indices, varnames_NA[i]] <- mean_by_year[data$Year[indices]]
  }
  return(data)
}

rmse <- function(predicted, actual) {
  sqrt(mean((predicted - actual)^2, na.rm = TRUE))
}