## ========================================================
## System Dynamcis Model - General Practitioner Workforce
## ========================================================


# file: SDM_GPPC.R
# title: System Dynamics Model of General Practitioner Primary Care
# summary: Predictive modelling (what will happen next if...?) and simulation (what could happen...?)
# model type: conditional, imprecise projections of dynamic behaviour

pacman::p_load('deSolve',
               'ggplot2',
               'tictoc',
               'scales',
               #'raustats',  
               'here')

source(here::here("settings.yaml"))

# create time vector
simtime <- seq(begin, end, by = timeStep)

# model function
model <- function(simtime, stocks, auxs){
  with(as.list(c(stocks, auxs)),{ 
    # graphing function to set up the relationship between x and y
    # effect of system pressure on workdays
    x.SystemPressure <- seq(0, 2.5, by = .25)
    y.EffectSystemPressure <- c(0.75, 0.79, 0.84, 0.90, 1.0, 1.09, 1.17, 1.23, 1.25, 1.25, 1.25)
    plot(x.SystemPressure, y.EffectSystemPressure)
    # min(o$SystemPressure)
    abline(v=0.7982064, col="blue", lty=2)
    # max(o$SystemPressure)
    abline(v=1.495762, col="red", lty=2)
    # Return a list of points which approx. linearly interpolation of given data points, or a function performing the linear (or constant) interpolation.
    # Effect of system pressure on work year
    func.WorkYear <- approxfun(x = x.SystemPressure,
                               y = y.EffectSystemPressure,
                               method = "linear",
                               yleft = 0.75, yright = 1.25)    
    # effect of system pressure on productivity
    x.SystemPressure <- seq(0, 2.0, by = .2)
    y.EffectSystemPressureProductivity <- c(0.62, 0.65, 0.84, 0.79, 0.89, 1.0, 1.14, 1.24, 1.32, 1.37, 1.4)
    plot(x.SystemPressure, y.EffectSystemPressureProductivity)
    # min(o$SystemPressure)
    abline(v=0.7982064, col="blue", lty=2)
    # max(o$SystemPressure)
    abline(v=1.495762, col="red", lty=2)
    func.Productivity <- approxfun(x = x.SystemPressure,
                                   y = y.EffectSystemPressureProductivity,
                                   method = "linear",
                                   yleft = 0.62, yright = 1.4) 
    
    
    x.time <- ages_list_0_14$time
    y.values <- ages_list_0_14$values
    
    func.erp <- approxfun(x = x.time,
                          y = y.values,
                          method = "linear",
                          yleft = min(y.values), yright = max(y.values))  
    
    
    erp <- func.erp(simtime)
    
    
    # flow which moves a material between stocks. For sPopulation, fDeaths is an outflow 
    fDeaths <- sPopulation * aCrudeDeathRate
    # flow which moves a material between stocks. For sPopulation, fBirths is an inflow 
    fBirths <- sPopulation * aCrudeBirthRate
    # flow which moves a material between stocks. For sPopulation, fNetOverseasMigration is an inflow 
    fNetOverseasMigration <- sPopulation * aCrudefNetOverseasMigrationRate
    # variable which can be dynamical calculated or a constant 
    # this variable represents rate at which general practitioners exit the system 
    # Units: Persons/Year
    fExits <- sGeneralPractitioners / aAverageGPCareerDuration
    # simple expressions that defines variable in terms of another that are causally connected
    # Demand for GP services grows with the sPopulation
    GeneralPractitionerDemand <- sPopulation * aAverageGPVisits
    
    
    # patient visits - 
    # Units: Person/Year
    fPatientVisits <- GeneralPractitionerDemand
    # alternatively could be dDesiredfCompletedVisits / aStandardGPProductivity
    DesiredGeneralPractitioners <- sPopulation * aDesiredGeneralPractitionersPer1000sPopulation
    # ie we try and correct decrepancies between desired and actual workforce in 5 years
    WorkforceCorrectionForGeneralPractitioners <- (DesiredGeneralPractitioners - sGeneralPractitioners) / aAdjustmentTime
    # to maintain system capacity you need to (at least) train as many as you lose to retirement
    # plus workforce correction (the difference b/w desired workforce and 'actual' workforce over time period to replace)
    
    
    fEntries <- pmax(0, fExits + WorkforceCorrectionForGeneralPractitioners)
    
    # fBirths increases sPopulation, and fDeaths decreases sPopulation
    # aCrudeBirthRate > aCrudeDeathRate so sPopulation will grow
    sPopulation <- fBirths - fDeaths + fNetOverseasMigration
    # available capacity can be calculated as the number of standard annual visits that are feasible based on available capacity
    StandardAnnualfCompletedVisits <- sGeneralPractitioners * aStandardGPWorkYear * aStandardGPProductivity
    # the potential completed visits is the product of GPs, productivity and work year
    # this provides information to formulate the outflow of the stock
    SystemPressure <- fPatientVisits / StandardAnnualfCompletedVisits
    
    
    # flag for system pressure
    # schedule pressure is the ratio of desired to normal production and
    # measures the need for 'overtime'
    if(SystemPressure < .5) 
      SystemPressureFlag <- 0 else{
        SystemPressureFlag <- 1
      }
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
    PotentialfCompletedVisits <- sGeneralPractitioners * Productivity * WorkYear
    # this value is the number of patient visits that would be completed if there were no
    # resource constraints on the system. In effect, this value represents the number ofd patients
    # who need to be treated in any given year
    
    # PotentialfCompletedVisits - Person/Year
    
    # number of general practitioners
    sGeneralPractitioners <- fEntries - fExits     
    
    
    
    
    DesiredfCompletedVisits <- sPatientsBeingTreated/aTargetCompletionTime
    fCompletedVisits <-  pmin(PotentialfCompletedVisits, DesiredfCompletedVisits)
    
    
    
    
    sPatientsBeingTreated <- fPatientVisits - fCompletedVisits
    Discrepancy <- fExits - sExpectedRetirement
    CERR <- Discrepancy / DC
    sExpectedRetirement <- CERR
    
    # 
    AverageDelay = fPatientVisits / fCompletedVisits
    WorkYearRatio <- WorkYear / aStandardGPWorkYear
    ProductivityRatio <- Productivity / aStandardGPProductivity
    
    # minimum of desired completed visits and potential completed visits. This ensures
    # that the stock will never go negative, and that the outflow cannot exceed the 
    
    return (list(c(sPopulation, sGeneralPractitioners, sPatientsBeingTreated, sExpectedRetirement), # initial conditions vector
                 fBirths = fBirths, 
                 fDeaths = fDeaths, 
                 fNOM = fNetOverseasMigration,
                 GeneralPractitionerDemand = GeneralPractitionerDemand, 
                 fPatientVisits = fPatientVisits,
                 fExits = fExits, 
                 fEntries = fEntries,
                 SystemPressure = SystemPressure, 
                 SystemPressureFlag = SystemPressureFlag,
                 GF = aCrudeBirthRate,DF = aCrudeDeathRate, 
                 GP = aDesiredGeneralPractitionersPer1000sPopulation, 
                 WorkYear = WorkYear,
                 Productivity = Productivity,
                 SystemCapacity = StandardAnnualfCompletedVisits, 
                 PotentialfCompletedVisits = PotentialfCompletedVisits,
                 DesiredfCompletedVisits = DesiredfCompletedVisits,
                 fCompletedVisits = fCompletedVisits,
                 WorkYearRatio = WorkYearRatio,
                 ProductivityRatio = ProductivityRatio,
                 DesiredGPs = DesiredGeneralPractitioners,
                 AdjustmentForGPs = WorkforceCorrectionForGeneralPractitioners,
                 AverageDelay = AverageDelay,
                 erp = erp
    ))   
    
  })
}
# in order to simulate the model needs a set of equations that describe the relationship. The are
# defined in the model above and called in the ode 'func'
# The Model function, takes 4 arguments from ode()
o <-data.frame(ode(y=stocks, times=simtime, func = model, 
                   parms=auxs, method="euler"))

o_tidyr <- o %>% pivot_longer(-time, names_to = "key", values_to = "value")


# Plots and output
o_tidyr %>% filter(key %in% c("DesiredfCompletedVisits","PotentialfCompletedVisits")) %>% 
  ggplot()+
  geom_line(aes(time, value, color=key))+
  scale_y_continuous(labels = comma)+
  ylab("#")+
  xlab("Year") +
  labs(color="")+
  theme(legend.position="top")