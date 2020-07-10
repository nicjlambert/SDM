## ========================================================
## System Dynamcis Model - General Practitioner Workforce
## ========================================================

options(scipen = 999)

func <- function(times, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    
    ## ====================
    ## Converters 
    ## ====================
    
    x.SystemPressureWorkYear <- seq(0, 2.5, by = .25)
    y.EffectSystemPressureWorkYear <- c(0.75, 0.79, 0.84, 0.90, 1.0, 1.09, 1.17, 1.23, 1.25, 1.25, 1.25)
    func.WorkYear <- approxfun(x = x.SystemPressureWorkYear,
                               y = y.EffectSystemPressureWorkYear,
                               method = "linear",
                               yleft = 0.75, yright = 1.25)
    
    x.SystemPressureProductivity <- seq(0, 2.0, by = .2)
    y.EffectSystemPressureProductivity <- c(0.62, 0.65, 0.84, 0.79, 0.89, 1.0, 1.14, 1.24, 1.32, 1.37, 1.4)
    func.Productivity <- approxfun(x = x.SystemPressureProductivity,
                                   y = y.EffectSystemPressureProductivity,
                                   method = "linear",
                                   yleft = 0.62, yright = 1.4) 

    ## ====================
    ## Demographic Sector
    ## ====================
    
    # total population
    sPopulation <- sPopulation0_14 + sPopulation15_39 + sPopulation40_64 + sPopulation65_plus
    
    # aging chain with delay constants
    fRateC1C2 <- sPopulation0_14/D1
    fRateC2C3 <- sPopulation15_39/D2
    fRateC3C4 <- sPopulation40_64/D3
    
    # flow which moves a material between stocks. For the population, fBirths is an inflow 
    fBirths <- sPopulation * aCrudeBirthRate 
    
    # flow which moves a material between stocks. For the population, fDeaths is an outflow 
    fDeaths <- sPopulation * aCrudeDeathRate
    
    # total GP visitations
    TotalGPVisits0_14 <- sPopulation0_14 * aAverageGPVisits0_14
    TotalGPVisits15_39 <- sPopulation15_39 * aAverageGPVisits15_39
    TotalGPVisits40_64 <- sPopulation40_64 * aAverageGPVisits40_64
    TotalGPVisits65_plus <- sPopulation65_plus * aAverageGPVisits65plus
    
    # population cohorts
    sPopulation0_14 <- fBirths - fRateC1C2
    sPopulation15_39 <- fRateC1C2 - fRateC2C3
    sPopulation40_64 <-  fRateC2C3 - fRateC3C4
    sPopulation65_plus <-  fRateC3C4 - fDeaths
    
    RetirementRate <- sGeneralPractitioners / aAverageGPCareerDuration
  
    GeneralPractitionerDemand <- TotalGPVisits0_14 + TotalGPVisits15_39 + TotalGPVisits40_64 + TotalGPVisits65_plus
    
    ## ====================
    ## Supply Sector
    ## ====================
    
    DesiredGPs <- sPopulation * aDesiredGPsPer1000sPopulation
    
    AdjustmentforGPs <- (DesiredGPs - sGeneralPractitioners) / aAdjustmentTime
    
    RecruitmentRate <- pmax(0, ExpectedRetirement + AdjustmentforGPs)
    
    
    ## ====================
    ## Delivery Sector
    ## ====================
    
    fPatientVisits <- GeneralPractitionerDemand
    
    # available capacity can be calculated as the number of standard annual visits that are feasible based on available capacity
    StandardAnnualCompletedVisits <- sGeneralPractitioners * aStandardGPWorkYear * aStandardGPProductivity
    
    DesiredCompletedVisits <- sPatientsBeingTreated / aTargetCompletionTime
    
    # this provides information to formulate the outflow of the stock
    SystemPressure <- DesiredCompletedVisits / StandardAnnualCompletedVisits
  
    # effect the work year when system pressure flag active
    WorkYear <- if(SystemPressureFlag == 1) 
      func.WorkYear(SystemPressure) * aStandardGPWorkYear else{
        aStandardGPWorkYear
      }
    # effect the productivity when system pressure flag active
    Productivity <- if(SystemPressureFlag == 1) 
      func.Productivity(SystemPressure) * aStandardGPProductivity else{
        aStandardGPProductivity
      }
    
    # effect the productivity when system pressure flag active
    Productivity <- func.Productivity(SystemPressure) * aStandardGPProductivity
    
    PotentialCompletedVisits <- sGeneralPractitioners * Productivity * WorkYear
    
    CompletedVisits <- pmin(PotentialCompletedVisits, DesiredCompletedVisits)
    
    sPatientsBeingTreated <- fPatientVisits - CompletedVisits
    
    sGeneralPractitioners <- RecruitmentRate - RetirementRate
    
    Discrepancy <- RetirementRate - ExpectedRetirement
    CERR <- Discrepancy / DC
    ExpectedRetirement <- CERR

    return (list(c(sPopulation0_14, sPopulation15_39, sPopulation40_64, sPopulation65_plus, sGeneralPractitioners, ExpectedRetirement, sPatientsBeingTreated), # initial conditions vector
                 fRateC1C2 = fRateC1C2,
                 fRateC2C3 = fRateC2C3,
                 fRateC3C4 = fRateC3C4,
                 fBirths = fBirths,
                 fDeaths = fDeaths,
                 sPopulation = sPopulation,
                 DesiredGPs = DesiredGPs,
                 RetirementRate = RetirementRate,
                 RecruitmentRate = RecruitmentRate,
                 AdjustmentforGPs = AdjustmentforGPs,
                 StandardAnnualCompletedVisits = StandardAnnualCompletedVisits,
                 GeneralPractitionerDemand = GeneralPractitionerDemand,
                 fPatientVisits = fPatientVisits,
                 SystemPressure = SystemPressure,
                 PotentialCompletedVisits = PotentialCompletedVisits,
                 DesiredCompletedVisits = DesiredCompletedVisits,
                 CompletedVisits = CompletedVisits,
                 WorkYear = WorkYear,
                 Productivity = Productivity
                 

    ))   
    
  })
}

