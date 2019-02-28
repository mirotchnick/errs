library(redcapAPI)

redcap_api_url = "https://redcap.braincode.ca/api/"
token = "2F15DD1A28FA0DA0710F0D49D0257D03"

rcon <- redcapConnection(redcap_api_url, token)

ethics <- exportRecords(rcon, factors = FALSE, labels = FALSE)

prog <- "EPL"

#ethics restriction report
library(XLConnect)
library(Hmisc)
library(dplyr)
library(stringr)
library(tidyr)

recs <- ethics[which(ethics$program == prog),]

err <- data.frame(recs$record_id,
                  recs$study_title,
                  recs$study_code,
                  recs$reb_aprvl_rcvd,
                  recs$reb_expiry_date,
                  recs$reb_bc_transfer_approval,
                  recs$mod_did_ohip,
                  recs$mod_did_dob,
                  recs$mod_did_postcd,
                  recs$mod_did_face,
                  recs$site_rstrcn_hsc,
                  recs$site_rstrcn_uto,
                  recs$site_rstrcn_tgh,
                  recs$site_rstrcn_twh,
                  recs$site_rstrcn_lhr,
                  recs$site_rstrcn_lhs,
                  recs$site_rstrcn_cho,
                  recs$site_rstrcn_mcu,
                  recs$site_rstrcn_mch,
                  recs$site_rstrcn_hhs,
                  recs$site_rstrcn_hgh,
                  recs$site_rstrcn_weu,
                  recs$site_rstrcn_hsc_deid,
                  recs$site_rstrcn_uto_deid,
                  recs$site_rstrcn_tgh_deid,
                  recs$site_rstrcn_twh_deid,
                  recs$site_rstrcn_lhr_deid,
                  recs$site_rstrcn_lhs_deid,
                  recs$site_rstrcn_cho_deid,
                  recs$site_rstrcn_mcu_deid,
                  recs$site_rstrcn_mch_deid,
                  recs$site_rstrcn_hhs_deid,
                  recs$site_rstrcn_hgh_deid,
                  recs$site_rstrcn_weu_deid,
                  recs$third_party_share,
                  recs$encrypt_ohip_link,
                  recs$other_methods_link, 
                  recs$braincode_study_tracking_sheet_data_transfer_form_complete,
                  stringsAsFactors = FALSE)

err <- 
  err %>%
  mutate(recs.reb_aprvl_rcvd = ifelse(err$recs.reb_aprvl_rcvd == 1, "Yes", "No"),
         recs.reb_bc_transfer_approval = ifelse(recs.reb_bc_transfer_approval == 1, "Yes", "No"),
         recs.third_party_share = ifelse(recs.third_party_share == 1, "Yes", "No"),
         recs.encrypt_ohip_link = ifelse(recs.encrypt_ohip_link == 1, "Yes", "No"),
         recs.other_methods_link = ifelse(recs.other_methods_link == 1, "Yes", "No"))

err$recs.mod_did_ohip <- str_replace_all(err$recs.mod_did_ohip, "encrypted_ohip", "Yes")
err$recs.mod_did_ohip <- str_replace_all(err$recs.mod_did_ohip, "no_ohip", "No")
err$recs.mod_did_dob <- str_replace_all(err$recs.mod_did_dob, "age", "Age")
err$recs.mod_did_dob <- str_replace_all(err$recs.mod_did_dob, "full_dob", "Full DOB")
err$recs.mod_did_dob <- str_replace_all(err$recs.mod_did_dob, "no_dob", "No")
err$recs.mod_did_postcd <- str_replace_all(err$recs.mod_did_postcd, "3dig_postcd", "First 3 digits")
err$recs.mod_did_postcd <- str_replace_all(err$recs.mod_did_postcd, "full_postcd", "Full PC")
err$recs.mod_did_postcd <- str_replace_all(err$recs.mod_did_postcd, "no_postcd", "No")
err$recs.mod_did_face <- str_replace_all(err$recs.mod_did_face, "deface", "Deface")
err$recs.mod_did_face <- str_replace_all(err$recs.mod_did_face, "full_face", "Full Face")
err$recs.mod_did_face <- str_replace_all(err$recs.mod_did_face, "no_img", "Imaging not collected")
err$recs.braincode_study_tracking_sheet_data_transfer_form_complete[err$recs.braincode_study_tracking_sheet_data_transfer_form_complete == 0] <-
  "Incomplete"
err$recs.braincode_study_tracking_sheet_data_transfer_form_complete[err$recs.braincode_study_tracking_sheet_data_transfer_form_complete == 1] <-
  "Unverified"
err$recs.braincode_study_tracking_sheet_data_transfer_form_complete[err$recs.braincode_study_tracking_sheet_data_transfer_form_complete == 2] <-
  "Complete"

err[,11:(ncol(err)-4)][!is.na(err[,11:(ncol(err)-4)])] <- "Yes"
err[,11:(ncol(err)-4)][is.na(err[,11:(ncol(err)-4)])] <- ""
err$recs.reb_expiry_date <- as.character(err$recs.reb_expiry_date)

template <- loadWorkbook("epltemplate.xlsx")
xlerr = readWorksheet(template, sheet = getSheets(template)[1])
for(i in 1:nrow(err)){
  colcount <- 1
  for(j in names(xlerr)){
    xlerr[i,j] <- err[i,colcount]
    colcount <- colcount + 1
  }
}

xlerr$Col3 <- as.integer(xlerr$Col3)
xlerr$Col5 <- as.Date(xlerr$Col5, format = "%Y-%m-%d")
xlerr$Col5 <- format(xlerr$Col5, "%d/%m/%Y")

writeWorksheet(template, xlerr, sheet = getSheets(template)[1], startRow = 3, header = F)
today <- toupper(format(Sys.Date(), "%d%h%Y"))
errname <- paste(prog, "_ERR_", today, ".xlsx", sep = "")
saveWorkbook(template, errname)