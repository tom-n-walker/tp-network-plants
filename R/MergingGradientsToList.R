#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH_Lavey, metaCommunityCH_Lavey, communityCH_Lavey, traitCH_Lavey,
                            metaCH_Calanda, metaCommunityCH_Calanda, communityCH_Calanda, traitCH_Calanda,
                            metaNO_Ulvehaugen, metaCommunityNO_Ulvehaugen, communityNO_Ulvehaugen, traitNO_Ulvehaugen,
                            metaNO_Lavisdalen, metaCommunityNO_Lavisdalen, communityNO_Lavisdalen, traitNO_Lavisdalen,
                            metaNO_Gudmedalen, metaCommunityNO_Gudmedalen, communityNO_Gudmedalen, traitNO_Gudmedalen,
                            metaNO_Skjellingahaugen, metaCommunityNO_Skjellingahaugen, communityNO_Skjellingahaugen, traitNO_Skjellingahaugen
                            ){
  
  CountryList <- list(CH_Lavey = list(meta = metaCH_Lavey,
                                   metaCommunity = metaCommunityCH_Lavey,
                                   community = communityCH_Lavey,
                                   trait = traitCH_Lavey),
                      
                      CH_Calanda = list(meta = metaCH_Calanda,
                                  metaCommunity = metaCommunityCH_Calanda,
                                  community = communityCH_Calanda,
                                  trait = traitCH_Calanda),
                      
                      NO_Ulvehaugen = list(meta = metaNO_Ulvehaugen,
                                      metaCommunity = metaCommunityNO_Ulvehaugen,
                                      community = communityNO_Ulvehaugen,
                                      trait = traitNO_Ulvehaugen),
                      
                      NO_Lavisdalen = list(meta = metaNO_Lavisdalen,
                                    metaCommunity = metaCommunityNO_Lavisdalen,
                                    community = communityNO_Lavisdalen,
                                    trait = traitNO_Lavisdalen),
                      
                      NO_Gudmedalen = list(meta = metaNO_Gudmedalen,
                                      metaCommunity = metaCommunityNO_Gudmedalen,
                                      community = communityNO_Gudmedalen,
                                      trait = traitNO_Gudmedalen),
                      
                      NO_Skjellingahaugen = list(meta = metaNO_Skjellingahaugen,
                                           metaCommunity = metaCommunityNO_Skjellingahaugen,
                                           community = communityNO_Skjellingahaugen,
                                           trait = traitNO_Skjellingahaugen)
                      )
  return(CountryList)
  
}

