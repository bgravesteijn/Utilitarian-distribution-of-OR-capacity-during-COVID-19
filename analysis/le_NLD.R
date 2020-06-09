library(HMDHFDplus)

user <- "e.krijkamp@erasmusmc.nl" # as registered with HMD.
pw <- "1514918326"
# password as provided in an email from HMD possibly changed by you.
df_le <- readHMDweb(CNTRY = "NLD", item = "bltper_1x1",   username = user, password = pw)
df_le_HMD <- df_le[which(df_le$Year == 2018), ] # select all columns for the year 2017
write.csv(df_le_HMD, "data/NLD_bltper_1x1_2018.csv", row.names = FALSE)


#df_le <- read.csv("../data/data/NLD_bltper_1x1_2018.csv")

