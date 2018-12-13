#### Construct data import drake plan ####

Import_Gongga <- function(){
  
  ### IMPORT DATA
  ## CN_Gongga
  metaCN_Gongga_raw = get(load(file = file_in("data/metaCN_Gongga.Rdata")))
  metaCommunityCN_Gongga_raw = get(load(file = file_in("data/metaCommunityCN_Gongga_2012_2016.Rdata")))
  communityCN_Gongga_raw = get(load(file = file_in("data/cover_thin_CH_2012_2016.Rdata")))
  traitCN_Gongga_raw = get(load(file = file_in("data/traits_2015_2016_China.Rdata")))

  ### CLEAN DATA SETS
  ## CN_Gongga
  metaCN_Gongga = CleanChinaMeta(metaCN_Gongga_raw)
  metaCommunityCN_Gongga = CleanChinaMetaCommunity(metaCommunityCN_Gongga_raw)
  communityCN_Gongga = CleanChinaCommunity(communityCN_Gongga_raw)
  traitCN_Gongga = CleanChinaTrait(traitCN_Gongga_raw)
  
  CN_Gongga = list(meta = metaCN_Gongga,
                   metaCommunity = metaCommunityCN_Gongga,
                   community = communityCN_Gongga,
                   trait = traitCN_Gongga)

  
  return(CN_Gongga)
}



# make an import drake plan
# dataImport_plan = drake_plan(
#   strings_in_dots = "literals",
  
  #### IMPORT & CLEAN DATA
  
  # ## NORWAY
  # metaNO_raw = get(load(file = file_in("data/metaNO.Rdata"))),
  # metaCommunityNO_raw = get(load(file = file_in("data/metaCommunityNO_2016.Rdata"))),
  # traitNO_raw = target(
  #   drop_and_load.csv(myfile = "transplant/USE THIS DATA/Norway/traitdata_NO.csv",
  #                     localpath = "data/traitdata_NO.csv"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/traitdata_NO.csv")$content_hash)
  # ),
  # communityNO_raw = target(
  #   drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx",
  #                      localpath = "data/funcab_composition_2016.xlsx"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/funcab_composition_2016.xlsx")$content_hash)
  # ),
  # spNO = target(
  #   drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/Norway/systematics_species.xlsx",
  #                      localpath = "data/fsystematics_species.xlsx"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/systematics_species.xlsx")$content_hash)
  # ),
  # fluxNO_raw = target(
  #   drop_and_load(myfile = "transplant/USE THIS DATA/Norway/standardControlFluxNO_2016.Rdata",
  #                 localpath = "data/standardControlFluxNO_2016.Rdata"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Norway/standardControlFluxNO_2016.Rdata")$content_hash)
  # ),
  # 
  # 
  # ## COLORADO
  # metaCO_raw = get(load(file = file_in("data/metaCO.Rdata"))),
  # metaCommunityCO_raw = get(load(file = file_in("data/metaCommunityCO_2016.Rdata"))),
  # communityCO_raw = target(
  #   drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv",
  #                     localpath = "data/CO_gradient_2016_Species_Cover.csv"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/CO_gradient_2016_Species_Cover.csv")$content_hash)
  # ),
  # traitCO_raw = target(
  #   drop_and_load.csv(myfile = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv",
  #                     localpath = "data/rmbl_trait_data_master.csv"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/rmbl_trait_data_master.csv")$content_hash)
  # ),
  # fluxCO_raw = target(
  #   drop_and_load(myfile = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata",
  #                 localpath = "data/standardControlFluxCO_2016.Rdata"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/Colorado/standardControlFluxCO_2016.Rdata")$content_hash)
  # ),
  # 
  # ### META BIOCLIM ###
  # #metaAll = get(load(file = file_in("data/metaAllC.Rdata"))),
  # metaBioclim_raw = target(
  #   drop_and_load.xlsx(myfile = "transplant/USE THIS DATA/PFTC/MetaBioclimAllCountriesVPD_PET.xlsx",
  #                      localpath = "data/MetaBioclimAllCountriesVPD_PET.xlsx"),
  #   trigger = trigger(change = drop_get_metadata(path = "transplant/USE THIS DATA/PFTC/MetaBioclimAllCountriesVPD_PET.xlsx")$content_hash)
  # ),
  
  
  #### CLEAN DATA SETS
  # metaNO = CleanNorwayMeta(metaNO_raw), 
  # metaCommunityNO = CleanNorwayMetaCommunity(metaCommunityNO_raw),
  # communityNO = CleanNorwayCommunity(communityNO_raw, spNO),
  # traitNO = CleanNorwayTrait(traitNO_raw),
  # fluxNO = CleanNorwayFlux(fluxNO_raw),
  # 
  # metaCO  = CleanColoradoMeta(metaCO_raw),  
  # communityCO = CleanColoradoCommunity(communityCO_raw),
  # traitCO = CleanColoradoTrait(traitCO_raw),
  # metaCommunityCO = CleanColoradoMetaCommunity(metaCommunityCO_raw),
  # fluxCO = CleanColoradoFlux(fluxCO_raw),
  # 
  # metaBioclim = CleanMetaBioclim(metaBioclim_raw),
  
  
  # make a list with all data sets
  # GradientList = MakeGradientList(metaCN_Gongga, metaCommunityCN_Gongga, communityCN_Gongga, traitCN_Gongga
                                # metaNO, metaCommunityNO, communityNO, traitNO,
                                # metaCO, metaCommunityCO, communityCO, traitCO
                                # )
  
  
  # ## Combine meta data
  # metaAll = metaCH %>% 
  #   bind_rows(metaPE, metaSV, metaNO, metaCO),
  # 
  # metaCommunityAll = metaCommunityCH %>% 
  #   bind_rows(metaCommunityPE, metaCommunitySV, metaCommunityNO, metaCommunityCO)
  
# )
