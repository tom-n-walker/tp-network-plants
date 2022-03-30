loadd()
write.table(
  x = rbind(
    CH_Calanda$meta,
    CH_Lavey$meta,
    CN_Gongga$meta,
    DE_Grainau$meta,
    FR_AlpeHuez$meta,
    FR_Lautaret$meta,
    IT_MatschMazia2$meta,
    NO_Skjellingahaugen$meta,
    NO_Ulvhaugen$meta,
    US_Arizona$meta,
    US_Colorado$meta,
    US_Montana$meta
  ),
  file = "site_metadata.csv",
  sep = ",",
  row.names = F
)


