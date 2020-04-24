library(readr)


sub_times <- list.files("virus_data/expanded_git_dir")


folder <- paste0("virus_data/expanded_git_dir", "/", sub_times[1])

zlist <- list()
blist <- list()

for (t in sort(sub_times)){
    
    unix_time <- strsplit(t, "-")[[1]][1]
    folder <- paste0("virus_data/expanded_git_dir", "/", t)
    boro <- paste0(folder, "/", "boro.csv")
    zcta <- paste0(folder, "/", "tests-by-zcta.csv")
    
    if (file.exists(zcta)) {
        df = read_csv(zcta)[-1,c("MODZCTA","Positive", "Total")]
        df$UNIX_TIME <- unix_time
        zlist[[unix_time]] <- df
    }
    
}

z_df <- do.call(rbind, zlist)




colnames(z_df)[1] <- "ZIP"
# write_csv(z_df, "cleaned_data/zip_count_rate_temp.csv")



# read zip 

zip_dict <- read_csv("virus_data/ZIP-COUNTY-FIPS_2017-06.csv")
z_demo_county <- read_csv("virus_data/county_data.csv") %>% mutate(STCOUNTYFP = paste0(statefip, fipcode))
# z_demo <- read_csv("Demographic_Statistics_By_Zip_Code.csv")
# colnames(z_demo)[1] <- "ZIP"
# z_state <- read_csv("state_data.csv")


library(dplyr)
dft <-left_join(z_df, zip_dict)
dft <- left_join(dft, z_demo_county)
write_csv(dft, "cleaned_data/zip_count_rate_temp.csv")

