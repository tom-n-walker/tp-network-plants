#### MAKE A LIST FOR ALL COUNTRIES ####

MakeCountryList <- function(metaCH, metaCommunityCH, communityCH, traitCH, fluxCH,
                            metaPE, metaCommunityPE, communityPE, traitPE, fluxPE,
                            metaSV, metaCommunitySV, communitySV, traitSV, fluxSV,
                            metaNO, metaCommunityNO, communityNO, traitNO, fluxNO,
                            metaCO, metaCommunityCO, communityCO, traitCO, fluxCO
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

