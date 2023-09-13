#LOOKING AT THE DATA ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

RailDS=read.csv("C:/Users/dcali/Downloads/Rail_Equipment_Accident_Incident_Data.csv")
str(RailDS)
View(RailDS)

#Start Year+Month Of Data
OGY1975=subset(RailDS,RailDS$Report.Year==1975)
View(OGY1975)
table(OGY1975$Accident.Month)

#End Year+Month of Data
OGY2022=subset(RailCleanDS,RailCleanDS$Report.Year==2022)
View(OGY2022)
table(OGY2022$Accident.Month)

#Data Description: Train Accidents Not Involving People in Vehicles (ex: Equipment Malfunctions)

#CLEANING THE DATA ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Columns of Interest For Testing Correlations & Possibly As Potential Predictors:
RailDS$Reporting.Railroad.Code
RailDS$Date
RailDS$Time
RailDS$Accident.Type
RailDS$State.Abbreviation
RailDS$County.Name
RailDS$Temperature
RailDS$Visibility
RailDS$Weather.Condition
RailDS$Track.Type 
RailDS$Track.Class #Based On Speed Limit At Accident Site... Will Make Another Column to Signify This
RailDS$Equipment.Type
RailDS$Gross.Tonnage
RailDS$Train.Speed
RailDS$Accident.Cause

#Early Thoughts For Hypothesis/Prediction: We could predict accident type based on weather conditions.  Ex: If it is cold or it snowed, it may be more likely that there is a derailment as opposed to regular conditions.  The call-to-action would be: because we acknowledge a correlation (not causation), we can be more attentive to certain equipment during specific weather conditions to prevent future accidents.

#Cleaning Missing & NA Values For Columns I Am Interested In:
#RailDS3 = RailDS[!is.na(RailDS$Equipment.Type),]
#str(RailDS3)
#I Had To Replace Missing Values of "" as NA values Because R Would Not Recognize Them
library(dplyr)
RailDS = RailDS %>%
  mutate(Reporting.Railroad.Code = replace(Reporting.Railroad.Code, Reporting.Railroad.Code == "",NA)) %>%
  mutate(Date=replace(Date,Date=="",NA)) %>%
  mutate(Time=replace(Time,Time=="",NA)) %>%
  mutate(Accident.Type=replace(Accident.Type,Accident.Type=="",NA)) %>%
  mutate(State.Abbreviation=replace(State.Abbreviation,State.Abbreviation=="",NA)) %>% 
  mutate(County.Name=replace(County.Name,County.Name=="",NA)) %>%
  mutate(Temperature=replace(Temperature,Temperature=="",NA)) %>%
  mutate(Visibility=replace(Visibility,Visibility=="",NA)) %>%
  mutate(Weather.Condition=replace(Weather.Condition,Weather.Condition=="",NA)) %>%
  mutate(Track.Type=replace(Track.Type,Track.Type=="",NA)) %>%
  mutate(Track.Class=replace(Track.Class,Track.Class=="",NA)) %>%
  mutate(Train.Speed=replace(Train.Speed,Train.Speed=="",NA)) %>%
  mutate(Total.Damage.Cost=replace(Total.Damage.Cost,Total.Damage.Cost=="",NA)) %>%
  mutate(Loaded.Passenger.Cars=replace(Loaded.Passenger.Cars,Loaded.Passenger.Cars=="",NA)) %>%
  mutate(Empty.Passenger.Cars=replace(Empty.Passenger.Cars,Empty.Passenger.Cars=="",NA)) %>%
  mutate(Primary.Accident.Cause.Code=replace(Primary.Accident.Cause.Code,Primary.Accident.Cause.Code=="",NA))
  
#RailDS[!is.na(RailDS$Reporting.Railroad.Code),] & RailDS[!is.na(RailDS$Date),] => I Couldn't Figure Out Using is.na or na.omit So I Used dplyr package

library(tidyr) 
RailCleanDS = RailDS %>%
  drop_na(Reporting.Railroad.Code) %>%
  drop_na(Date) %>%
  drop_na(Time) %>%
  drop_na(Accident.Type) %>%
  drop_na(State.Abbreviation) %>%
  drop_na(County.Name) %>%
  drop_na(Temperature) %>%
  drop_na(Visibility) %>%
  drop_na(Weather.Condition) %>%
  drop_na(Track.Type) %>%
  drop_na(Track.Class) %>%
  drop_na(Equipment.Type) %>%
  drop_na(Train.Speed) %>%
  drop_na(Accident.Cause) %>%
  drop_na(Total.Damage.Cost) %>%
  drop_na(Loaded.Passenger.Cars) %>%
  drop_na(Empty.Passenger.Cars) %>%
  drop_na(Primary.Accident.Cause.Code)
str(RailCleanDS)
View(RailCleanDS)

#If Any Problems Arise In Predicting Categorial Variables, Use trim.ws()

#ADDING NEW COLUMNS/SUBSETTING DATA -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Cleaning: Removing NA Values From This New Column (Either Originally Came With Dataset, Or The 30 Rows That Do Not Fit Into the Defined Categories):
RailCleanDS = RailCleanDS %>%
  drop_na(Track.Speed.Limit)
str(RailCleanDS)
View(RailCleanDS)

#Cleaning: Primary Accident Cause Codes That We Cannot Analyze:
#According To The Manual, "M599" Signifies A Primary Accident Cause Not Defined By Them & Is Explained Under "Narrative" Column.  I must omit this.  I am limited in the fact that I can only analyze strings that fit a certain criteria.
OtherCause=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Code=="M599")
str(OtherCause)
#We Must Also Omit "M505" As It Is An Accident Still Under Investigation.
UICause=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Code=="M505")
str(UICause)
#We must also omit "M507" as it is an accident with an undetermined cause although the investigation is completed.
NoKnownCause=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Code=="M507")
str(NoKnownCause)
RailCleanDS = RailCleanDS %>%
  mutate(Primary.Accident.Cause.Code=replace(Primary.Accident.Cause.Code,Primary.Accident.Cause.Code=="M599",NA)) %>%
  mutate(Primary.Accident.Cause.Code=replace(Primary.Accident.Cause.Code,Primary.Accident.Cause.Code=="M505",NA)) %>%
  mutate(Primary.Accident.Cause.Code=replace(Primary.Accident.Cause.Code,Primary.Accident.Cause.Code=="M507",NA))
View(RailCleanDS)
emptyCode=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Code==NA)
View(emptyCode)
is.na(RailCleanDS$Primary.Accident.Cause.Code)
RailCleanDS = RailCleanDS[!is.na(RailCleanDS$Primary.Accident.Cause.Code),]
is.na(RailCleanDS$Primary.Accident.Cause.Code)
str(RailCleanDS)
View(RailCleanDS)

#Importing Another Dataset That Has Aggragated Categories & Titles of Primary Accident Cause Codes:
CauseCodesDS=read.csv("C:/Users/dcali/Downloads/Appendix C - Train Accident Cause Codes.csv")
View(CauseCodesDS)
colnames(CauseCodesDS)=c("Primary.Accident.Cause.Code","Primary.Accident.Cause","Primary.Accident.Cause.Category","Primary.Accident.Cause.Title")
View(CauseCodesDS)

#Merging Aggregated Categories of Accidents From Appendix C To The Cleaned Dataset:
RailCleanDS$Primary.Accident.Cause.Category=with(RailCleanDS, ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H021" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H020" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H025" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H017" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H022" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H018" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H019" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H008" |
                                                RailCleanDS$Primary.Accident.Cause.Code=="H099"),"Brakes, Use of",
                                              ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H822" |
                                                        RailCleanDS$Primary.Accident.Cause.Code=="H821" |
                                                        RailCleanDS$Primary.Accident.Cause.Code=="H824" |
                                                        RailCleanDS$Primary.Accident.Cause.Code=="H823" |
                                                        RailCleanDS$Primary.Accident.Cause.Code=="H899"), "Cab Signals",
                                                     ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H104" |
                                                               RailCleanDS$Primary.Accident.Cause.Code=="H199" |
                                                               RailCleanDS$Primary.Accident.Cause.Code=="H103" |
                                                               RailCleanDS$Primary.Accident.Cause.Code=="H101" |
                                                               RailCleanDS$Primary.Accident.Cause.Code=="H102"),"Employee Physical Condition",
                                                            ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H221" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H222" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H201" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H202" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H218" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H217" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H220" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H219" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H206" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H205" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H208" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H207" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H209" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H299" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H210" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H212" |
                                                                      RailCleanDS$Primary.Accident.Cause.Code=="H211"),"Flagging, Fixed, Hand and Radio Signals",
                                                                   ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H301" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H302" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H303" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H310" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H309" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H304" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H317" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H305" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H318" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H316" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H311" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H399" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H312" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H315" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H314" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H313" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H306" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H307" |
                                                                             RailCleanDS$Primary.Accident.Cause.Code=="H308"),"General Switching Rules",
                                                                          ifelse((RailCleanDS$Primary.Accident.Cause.Code=="M399"),"Loading Procedures",
                                                                          ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H401" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H402" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H403" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H499" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H404" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H405" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H406" |
                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="H499"),"Main Track Authority",
                                                                                 ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H99E" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H99D" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H995" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H994" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H99A" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H99B" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H99C" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H993" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H997" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H992" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H999" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H996" |
                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="H991"),"Miscellaneous",
                                                                                        ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H601" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H605" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H607" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H699" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H602" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H603" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H606" |
                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="H604"),"Speed",
                                                                                        ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H705" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H707" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H701" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H702" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H706" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H703" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H704" |
                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="H799"),"Switches, Use of",
                                                                                               ifelse((RailCleanDS$Primary.Accident.Cause.Code=="H511" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H512" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H510" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H513" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H503" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H504" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H518" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H520" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H517" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H521" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H519" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H524" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H526" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H514" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H516" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H515" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H502" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H509" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H508" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H501" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H525" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H507" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H505" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H506" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H599" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H522" |
                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="H523"),"Train Handling / Train Make-Up",
                                                                                                      ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E51C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E51L" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E52C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E52L" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E53C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E53L" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E54C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E54L" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E55C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E55L" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E59C" |
                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="E59L"),"Axles and Journal Bearings",
                                                                                                             ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E32L" |
                                                                                                                       RailCleanDS$Primary.Accident.Cause.Code=="E33C" |
                                                                                                                       RailCleanDS$Primary.Accident.Cause.Code=="E34C" |
                                                                                                                       RailCleanDS$Primary.Accident.Cause.Code=="E32L"),"Coupler and Draft System",
                                                                                                                    ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E80C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E81C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E82C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E83C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E84C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E85C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E86C" |
                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="E89C"),"Doors",
                                                                                                                           ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E99C" |
                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="E99L"),"General Mechanical Electrical Failures",
                                                                                                                                  ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E70L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E71L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E72L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E73L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E74L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E75L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E76L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E77L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E78L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E79L" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E7AL" |
                                                                                                                                            RailCleanDS$Primary.Accident.Cause.Code=="E7BL"),"Locomotives",
                                                                                                                                         ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E40C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E40L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E41C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E41L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E42C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E43C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E43L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E44C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E44L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E45C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E45L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E46C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E46L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E47C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E47L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E48C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E48L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E49C" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E49L" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E4AC" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E4BC" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E4TC" |
                                                                                                                                                   RailCleanDS$Primary.Accident.Cause.Code=="E4TL"),"Truck Components",
                                                                                                                                                ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E60C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E60L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E61C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E61L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E62C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E62L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E63C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E63L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E64C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E64L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E65C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E65L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E66C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E66L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E67C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E67L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E68C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E68L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E69C" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E69L" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E6AC" |
                                                                                                                                                          RailCleanDS$Primary.Accident.Cause.Code=="E6AL"),"Wheels",
                                                                                                                                                       ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E20C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E20L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E21C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E22L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E23C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E23L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E24C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E24L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E25C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E25L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E26C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E26L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E27C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E27L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E29C" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E29L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E21L" |
                                                                                                                                                                 RailCleanDS$Primary.Accident.Cause.Code=="E22C"),"Body",
                                                                                                                                                              ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E00C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E00L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E01C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E01L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E02C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E02L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E03C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E03L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E04C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E04L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E05C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E05L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E06C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E06L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E07C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E07L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E08C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E08L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E09C" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E09L" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E0HC" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E0HL" |
                                                                                                                                                                        RailCleanDS$Primary.Accident.Cause.Code=="E10L"),"Brake",
                                                                                                                                                                     ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E30C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E30L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E31C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E31L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E32C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E33L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E34L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E35C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E36C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E36L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E37C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E37L" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E39C" |
                                                                                                                                                                               RailCleanDS$Primary.Accident.Cause.Code=="E39L"),"Coupler and Draft System",
                                                                                                                                                                            ifelse((RailCleanDS$Primary.Accident.Cause.Code=="E11C" |
                                                                                                                                                                                      RailCleanDS$Primary.Accident.Cause.Code=="E12C" |
                                                                                                                                                                                      RailCleanDS$Primary.Accident.Cause.Code=="E13C" |
                                                                                                                                                                                      RailCleanDS$Primary.Accident.Cause.Code=="E19C"),"Trailer Or Container On Flatcar",
                                                                                                                                                                                   ifelse((RailCleanDS$Primary.Accident.Cause.Code=="M101" |
                                                                                                                                                                                             RailCleanDS$Primary.Accident.Cause.Code=="M102" |
                                                                                                                                                                                             RailCleanDS$Primary.Accident.Cause.Code=="M103" |
                                                                                                                                                                                             RailCleanDS$Primary.Accident.Cause.Code=="M104" |
                                                                                                                                                                                             RailCleanDS$Primary.Accident.Cause.Code=="M105" |
                                                                                                                                                                                             RailCleanDS$Primary.Accident.Cause.Code=="M199"),"Environment Conditions",
                                                                                                                                                                                          ifelse((RailCleanDS$Primary.Accident.Cause.Code=="M201" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M202" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M203" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M204" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M206" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M207" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M208" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M299" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M301" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M302" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M303" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M304" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M305" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M306" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M307" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M308" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M401" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M402" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M403" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M404" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M405" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M406" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M407" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M408" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M409" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M410" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M411" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M501" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M502" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M503" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M504" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M505" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M506" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M507" |
                                                                                                                                                                                                    RailCleanDS$Primary.Accident.Cause.Code=="M599"),"Loading Procedures",
                                                                                                                                                                                                 ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T101" |
                                                                                                                                                                                                           RailCleanDS$Primary.Accident.Cause.Code=="T109"),"Track Geometry",
                                                                                                                                                                                                        ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T301" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T302" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T303" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T304" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T305" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T306" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T307" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T308" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T309" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T310" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T311" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T312" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T313" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T314" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T315" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T316" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T317" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T318" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T319" |
                                                                                                                                                                                                                  RailCleanDS$Primary.Accident.Cause.Code=="T399"),"Frogs, Switches and Track Appliances",
                                                                                                                                                                                                               ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T401" |
                                                                                                                                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="T402" |
                                                                                                                                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="T403" |
                                                                                                                                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="T404" |
                                                                                                                                                                                                                         RailCleanDS$Primary.Accident.Cause.Code=="T499"),"Other Way and Structure",
                                                                                                                                                                                                                      ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T201" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T202" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T203" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T204" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T205" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T206" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T207" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T208" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T210" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T211" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T212" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T213" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T214" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T215" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T216" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T217" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T218" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T219" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T220" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T221" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T222" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T223" |
                                                                                                                                                                                                                                RailCleanDS$Primary.Accident.Cause.Code=="T299"),"Rail, Joint Bar and Rail Anchoring",
                                                                                                                                                                                                                             ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T001" |
                                                                                                                                                                                                                                       RailCleanDS$Primary.Accident.Cause.Code=="T002" |
                                                                                                                                                                                                                                       RailCleanDS$Primary.Accident.Cause.Code=="T099"),"Roadbed",
                                                                                                                                                                                                                                    ifelse((RailCleanDS$Primary.Accident.Cause.Code=="T102" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T103" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T104" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T105" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T106" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T107" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T108" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T110" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T111" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T112" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T113" |
                                                                                                                                                                                                                                              RailCleanDS$Primary.Accident.Cause.Code=="T199"),"Track Geometry",
                                                                                                                                                                                                                                           ifelse((RailCleanDS$Primary.Accident.Cause.Code=="S001" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S002" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S003" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S004" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S005" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S006" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S007" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S008" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S009" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S010" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S011" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S012" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S013" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S014" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S015" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S016" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S099" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S101" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S102" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S103" |
                                                                                                                                                                                                                                                     RailCleanDS$Primary.Accident.Cause.Code=="S104"),"Signal and Communication",NA))))))))))))))))))))))))))))))))

#Cleaning: Removing NA Values That Don't Fit Into the Categories of Codes Identified
View(RailCleanDS)
is.na(RailCleanDS$Primary.Accident.Cause.Category)
RailCleanDS = RailCleanDS[!is.na(RailCleanDS$Primary.Accident.Cause.Category),]
is.na(RailCleanDS$Primary.Accident.Cause.Category)
str(RailCleanDS)
View(RailCleanDS)

#Merging Aggregated Titles of Accidents From Appendix C To The Cleaned Dataset:
RailCleanDS$Primary.Accident.Cause.Title=with(RailCleanDS,ifelse((RailCleanDS$Primary.Accident.Cause.Category=="Axles and Journal Bearings" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Coupler and Draft System" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Doors" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="General Mechanical Electrical Failures" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Locomotives" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Truck Components" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Wheels" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Body" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Brake" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Coupler and Draft System" |
                                                                    RailCleanDS$Primary.Accident.Cause.Category=="Trailer Or Container On Flatcar"),"Mechanical and Electrical Failures",
                                                                 ifelse((RailCleanDS$Primary.Accident.Cause.Category=="Environment Conditions" |
                                                                           RailCleanDS$Primary.Accident.Cause.Category=="Loading Procedures"),"Miscellaneous Cause Not Otherwise Listed",
                                                                        ifelse((RailCleanDS$Primary.Accident.Cause.Category=="Track Geometry" |
                                                                                  RailCleanDS$Primary.Accident.Cause.Category=="Frogs, Switches and Track Appliances" |
                                                                                  RailCleanDS$Primary.Accident.Cause.Category=="Other Way and Structure" |
                                                                                  RailCleanDS$Primary.Accident.Cause.Category=="Rail, Joint Bar and Rail Anchoring" |
                                                                                  RailCleanDS$Primary.Accident.Cause.Category=="Roadbed"),"Rack, Roadbed and Structures",
                                                                               ifelse((RailCleanDS$Primary.Accident.Cause.Category=="Signal and Communication"),"Signal and Communication",
                                                                                      ifelse((RailCleanDS$Primary.Accident.Cause.Category=="Brakes, Use of" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Cab Signals" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Employee Physical Condition" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Flagging, Fixed, Hand and Radio Signals" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="General Switching Rules" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Loading Procedures" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Main Track Authority" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Miscellaneous" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Speed" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Switches, Use of" |
                                                                                                RailCleanDS$Primary.Accident.Cause.Category=="Train Handling / Train Make-Up"),"Train operation - Human Factors",NA))))))
#Cleaning: No Need Since This Is Aggregated of Cleaned Column

#Subsetting Title of Cause:
HumanError=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Train operation - Human Factors")
MechElectError=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Mechanical and Electrical Failures")
MiscError=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Miscellaneous Causes Not Otherwise Listed")
RailError=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Rack, Roadbed and Structures")
SignalError=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Signal and Communication")

#Start Year+Month Of Data
Y1975=subset(RailCleanDS,RailCleanDS$Report.Year==1975)
View(Y1975)
table(Y1975$Accident.Month)

#End Year+Month of Data
Y2022=subset(RailCleanDS,RailCleanDS$Report.Year==2022)
View(Y2022)
table(Y2022$Accident.Month)

#DATA VISUALIZATION ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#What is the leading type of accident across the entire dataset (since 1975)?
table(RailCleanDS$Accident.Type)
HiLoAccTypeNames=c("Derailment","Side Coll.","Other Impacts","Hwy-Rail Crossing","Other Non-Impacts","Rear-End Coll.","Raking Coll.","Obstruction","Fire/Violent Rupture","Head-On Coll.","Broken Train Coll.","Explosion","RR Grade Crossing")
HiLoAccTypeNum=c(125443,18813,18486,10717,3989,3768,3753,3691,2377,1956,608,64,46)
library(RColorBrewer)
b=barplot(HiLoAccTypeNum,
          xlab = expression(italic("Type of Accident")),
          ylab = expression(italic("Number of Accidents Since 1975")),
          ylim=c(0,140000),
          cex.names = 0.6, font = 2,
          cex.main = 2.3,
          cex.lab = 1.2,
          main = "Train Accidents in The U.S. Since 1975",
          names.arg = HiLoAccTypeNames,
          col=c("Red","Violet","Cyan","Orange","Purple","Green","Gold","Maroon","Grey","Light Green","Pink","Tan","Dark Green"),
          col.main = "Dark Blue",
          col.lab = "Maroon")

# Rounded Count Labels on Top of The Bars:
numTypeAcc=round(HiLoAccTypeNum, digits = 1)
text(x=b, y=numTypeAcc, pos=3, label=numTypeAcc, cex=0.6,col="Black", font=1)

#What is the leading type of accident 2015 onwards?
RailCleanDS1522=subset(RailCleanDS,(RailCleanDS$Accident.Year==15 | RailCleanDS$Accident.Year==16 | RailCleanDS$Accident.Year==17 | RailCleanDS$Accident.Year==18 | RailCleanDS$Accident.Year==19 | RailCleanDS$Accident.Year==20 | RailCleanDS$Accident.Year==21 | RailCleanDS$Accident.Year==22))
table(RailCleanDS1522$Accident.Type)
HiLoAccTypeNames2=c("Derailment","Other Impacts","Hwy-Rail Crossing","Other Non-Impacts","Side Coll.","Obstruction","Fire/Violent Rupture","Raking Coll.","Rear-End Coll.","Head-On Coll.","Broken Train Coll.","RR Grade Crossing")
HiLoAccTypeNum2=c(10544,2675,1803,1105,896,850,404,286,162,57,36,7)
library(RColorBrewer)
b2=barplot(HiLoAccTypeNum2,
          xlab = expression(italic("Type of Accident")),
          ylab = expression(italic("Number of Accidents Since 2015")),
          ylim=c(0,12000),
          cex.names = 0.6, font = 2,
          cex.main = 2.3,
          cex.lab = 1.2,
          main = "Train Accidents in The U.S. Since 2015",
          names.arg = HiLoAccTypeNames2,
          col=c("Red","Violet","Cyan","Orange","Purple","Green","Gold","Maroon","Grey","Light Green","Pink","Tan"),
          col.main = "Dark Blue",
          col.lab = "Maroon")

# Rounded Count Labels on Top of The Bars:
numTypeAcc2=round(HiLoAccTypeNum2, digits = 1)
text(x=b2, y=numTypeAcc2, pos=3, label=numTypeAcc2, cex=0.6,col="Black", font=1)

#How many derailments involved a speeding train?
#Adding New Column: The Difference Between Speed & Speed Limit (Positive Value Means Under Limit, 0 Means At Limit, Negative Value Means Over Limit)
typeof(RailCleanDS$Track.Speed.Limit)
typeof(RailCleanDS$Train.Speed)
RailCleanDS$Speed.Under.Over.Limit=(RailCleanDS$Train.Speed-as.integer(RailCleanDS$Track.Speed.Limit))
View(RailCleanDS)
#Adding New Column: Assigning a Binary Option Potentially For Conditional Trees Later On...
RailCleanDS$Was.It.Speeding=with(RailCleanDS,ifelse((RailCleanDS$Speed.Under.Over.Limit>0),"Yes","No"))
#Subsetting By "Yes" To "Speeding" To See How Many Are Speeding:
Speeding=subset(RailCleanDS,RailCleanDS$Was.It.Speeding=="Yes")
str(Speeding) #Turned Out To Be A Very Small Minority (6296/194759 = 3.323%)
#Plotting It:
Derailment=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment")
table(Derailment$Was.It.Speeding=="Yes")
YesNoNames=c("No","Yes")
YesNoNum=c(121371,4072)
b5=barplot(YesNoNum,
           xlab = expression(italic("Was It Speeding?")),
           ylab = expression(italic("Number of Accidents")),
           ylim=c(0,140000),
           cex.names = 1, font = 2,
           cex.main = 2.3,
           cex.lab = 1.2,
           main = "Derailments in The U.S. Since 1975 Based On Speed",
           names.arg = YesNoNames,
           col=c("Red","Green"),
           col.main = "Dark Blue",
           col.lab = "Maroon")

# Rounded Count Labels on Top of The Bars:
numYesNo=round(YesNoNum, digits = 1)
text(x=b5, y=numYesNo, pos=3, label=numYesNo, cex=0.6,col="Black", font=1)

#What is the leading primary cause of derailments across the entire dataset (since 1975)?
table(Derailment$Primary.Accident.Cause.Title)
HiLoDerailCauseName=c("Rack, Roadbed and Structures","Train Operation - Human Error","Mechanical and Electrical Failures","Misc. Cause","Signal and Communication")
HiLoDerailCauseNum=c(60424,30476,22467,11187,889)
b3=barplot(HiLoDerailCauseNum,
           xlab = expression(italic("Primary Cause of Derailments")),
           ylab = expression(italic("Number of Derailments Since 1975")),
           ylim=c(0,62000),
           cex.names = 0.9, font = 2,
           cex.main = 2.3,
           cex.lab = 1.2,
           main = "Primary Cause of Derailments in The U.S. Since 1975",
           names.arg = HiLoDerailCauseName,
           col=c("Red","Cyan","Gold","Purple","Green"),
           col.main = "Dark Blue",
           col.lab = "Maroon")

# Rounded Count Labels on Top of The Bars:
numDerailCause=round(HiLoDerailCauseNum, digits = 1)
text(x=b3, y=HiLoDerailCauseNum, pos=3, label=numDerailCause, cex=0.7,col="Black", font=1)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# # Median Total Damage Cost Based on Cause of Derailment:
# summary(RailCleanDS$Total.Damage.Cost)
# table(Derailment$Primary.Accident.Cause.Title)
# med1=median(Derail.Structures$Total.Damage.Cost)
# med2=median(Derail.MechElectFail$Total.Damage.Cost)
# med3=median(Derail.HumErr$Total.Damage.Cost)
# med4=median(Derail.Misc$Total.Damage.Cost)
# med5=median(Derail.SigComm$Total.Damage.Cost)
# med1
# med2
# med3
# med4
# med5
# HiLoDerailCauseName=c("Rack, Roadbed and Structures","Train Operation - Human Error","Mechanical and Electrical Failures","Misc. Cause","Signal and Communication")
# HiLoDerailCauseNum=c(60424,30476,22467,11187,889)
# b3=barplot(HiLoDerailCauseNum,
#            xlab = expression(italic("Primary Cause of Derailments")),
#            ylab = expression(italic("Number of Derailments Since 1975")),
#            ylim=c(0,62000),
#            cex.names = 0.9, font = 2,
#            cex.main = 2.3,
#            cex.lab = 1.2,
#            main = "Primary Cause of Derailments in The U.S. Since 1975",
#            names.arg = HiLoDerailCauseName,
#            col=c("Red","Cyan","Gold","Purple","Green"),
#            col.main = "Dark Blue",
#            col.lab = "Maroon")

#BAYESIAN REASONING ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Subsetting Cause of Derailment:
table(RailCleanDS$Accident.Type)
table(RailCleanDS$Primary.Accident.Cause.Title)
Derail.HumErr=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment" & RailCleanDS$Primary.Accident.Cause.Title=="Train operation - Human Factors")
Derail.SigComm=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment" & RailCleanDS$Primary.Accident.Cause.Title=="Signal and Communication")
Derail.MechElectFail=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment" & RailCleanDS$Primary.Accident.Cause.Title=="Mechanical and Electrical Failures")
Derail.Misc=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment" & RailCleanDS$Primary.Accident.Cause.Title=="Miscellaneous Cause Not Otherwise Listed")
Derail.Structures=subset(RailCleanDS, RailCleanDS$Accident.Type=="Derailment" & RailCleanDS$Primary.Accident.Cause.Title=="Rack, Roadbed and Structures")
str(Derail.HumErr)
30476/125443
str(Derail.SigComm)
899/125443
str(Derail.MechElectFail)
22467/125443
str(Derail.Misc)
11187/125443
str(Derail.Structures)
60424/125443
str(Derailment)

#Let's say I Wanted To Find The Probability That The Primary Cause is A Structural Rail Problem (Rack, Roadbed and Structures) Given Accident Type was a Derailment?
# #P(H|E) = P(E|H)*P(H) / P(E)
# #P(Rack.Roadbed.Structures|Derailment) = P(Derailment|Rack.Roadbed.Structures)*P(Rack.Roadbed.Structures) / P(Derailment)
str(RailCleanDS$Primary.Accident.Cause.Title)
table(RailCleanDS$Accident.Type)
PE=125443/193711
table(RailCleanDS$Primary.Accident.Cause.Title)
PH=60424/193711
RRS=subset(RailCleanDS,RailCleanDS$Primary.Accident.Cause.Title=="Rack, Roadbed and Structures")
DerailGivenStructures=count(RRS,RRS$Accident.Type=="Derailment")
DerailGivenStructures
PE.H=60424/(60424+3224)
PH.E=PE.H*PH/PE
PH.E*100
#45.72858% chance that the primary cause is structural rail problem given it was derailment.

#Let's Add Another Piece of Evidence:
# #P(H|E) = P(E|H)*P(H) / P(E)
# #P(Rack.Roadbed.Structures|Derailment+) = P(Derailment+Speeding|Rack.Roadbed.Structures)*P(Rack.Roadbed.Structures) / P(Derailment+Speeding)
str(RailCleanDS$Primary.Accident.Cause.Title)
Derail.Speed=subset(RailCleanDS,(RailCleanDS$Accident.Type=="Derailment")&(RailCleanDS$Was.It.Speeding=="Yes"))
str(Derail.Speed)
PE2=4072/193711
PH
DerailGivenStrucutres.Speed=count(RRS,(RRS$Accident.Type=="Derailment"&RRS$Was.It.Speeding=="Yes"))
DerailGivenStrucutres.Speed
PE.H2=1473/(1473+62175)
PH.E2=PE.H2*PH/PE2
PH.E2*100
#34.34153% chance that the primary cause is structural given it was derailment and the train was speeding.









#END OF SUBMITTED WORK (HERE ARE OTHER THINGS I TRIED AND SPENT A LOT OF TIME ON) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Adding New Column "Freight.Or.Passenger":
RailCleanDS$Freight.Or.Passenger=with(RailCleanDS,ifelse((RailCleanDS$Loaded.Passenger.Cars==0 & RailCleanDS$Empty.Passenger.Cars==0),"Freight","Passenger"))
str(RailCleanDS$Freight.Or.Passenger)
View(RailCleanDS)
#Adding New Column "Track.Speed.Limit" Based on Track.Class & Freight.Or.Passenger
RailCleanDS$Track.Speed.Limit=with(RailCleanDS,ifelse((RailCleanDS$Track.Class=="X" & RailCleanDS$Freight.Or.Passenger=="Freight"),10,
                                                      ifelse((RailCleanDS$Track.Class=="X" & RailCleanDS$Freight.Or.Passenger=="Passenger"),NA,
                                                             ifelse((RailCleanDS$Track.Class==1 & RailCleanDS$Freight.Or.Passenger=="Freight"),10,
                                                                    ifelse((RailCleanDS$Track.Class==1 & RailCleanDS$Freight.Or.Passenger=="Passenger"),15,
                                                                           ifelse((RailCleanDS$Track.Class==2 & RailCleanDS$Freight.Or.Passenger=="Freight"),25,
                                                                                  ifelse((RailCleanDS$Track.Class==2 & RailCleanDS$Freight.Or.Passenger=="Passenger"),30,
                                                                                         ifelse((RailCleanDS$Track.Class==3 & RailCleanDS$Freight.Or.Passenger=="Freight"),40,
                                                                                                ifelse((RailCleanDS$Track.Class==3 & RailCleanDS$Freight.Or.Passenger=="Passenger"),60,
                                                                                                       ifelse((RailCleanDS$Track.Class==4 & RailCleanDS$Freight.Or.Passenger=="Freight"),60,
                                                                                                              ifelse((RailCleanDS$Track.Class==4 & RailCleanDS$Freight.Or.Passenger=="Passenger"),80,
                                                                                                                     ifelse((RailCleanDS$Track.Class==5 & RailCleanDS$Freight.Or.Passenger=="Freight"),80,
                                                                                                                            ifelse((RailCleanDS$Track.Class==5 & RailCleanDS$Freight.Or.Passenger=="Passenger"),90,
                                                                                                                                   ifelse((RailCleanDS$Track.Class==6),110,
                                                                                                                                          ifelse((RailCleanDS$Track.Class==7),125,
                                                                                                                                                 ifelse((RailCleanDS$Track.Class==8),160,
                                                                                                                                                        ifelse((RailCleanDS$Track.Class==9),220,NA)))))))))))))))))

#HYPOTHESIS TESTING ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# #Alt Hyp: If A Train Is Speeding, it is more likely to be a derailment than other accident types?
# YesSpeeding=subset(RailCleanDS,RailCleanDS$Speed.Under.Over.Limit>0)
# NotSpeeding=subset(RailCleanDS,RailCleanDS$Speed.Under.Over.Limit<=0)
# table(RailCleanDS$Accident.Type)
# 
# meanSpeeding=mean(Derailment$Speed.Under.Over.Limit>0)
# meanNotSpeeding=mean(NotSpeeding$Speed.Under.Over.Limit)
# sd.Speeding=sd(YesSpeeding$Speed.Under.Over.Limit)
# sd.NotSpeeding=sd(NotSpeeding$Speed.Under.Over.Limit)
# num.Speeding=length(YesSpeeding$Speed.Under.Over.Limit)
# num.NotSpeeding=length(NotSpeeding$Speed.Under.Over.Limit)
# #SD Based on the 2 Samples:
# sd.Speeding.NotSpeeding=sqrt((sd.Speeding^2/num.Speeding)+(sd.NotSpeeding^2/num.NotSpeeding))
# z.score=(meanSpeeding-meanNotSpeeding)/sd.Speeding.NotSpeeding
# z.score
# 
# #Calculate P-Value:
# plot(x=seq(from=-22,to=22,by=0.1),y=dnorm(seq(from=-22,to=22,by=0.1),mean=0),type="l",xlab="Mean difference", ylab="Possibility")
# abline(v=z.score,col="red")
# p=1-pnorm(z.score)
# p #0
# #Since p=0 < 0.05, we can reject the NULL hypothesis.

#BAYESION REASONING -----------------------------------------------------------------------------------------------------------

# # #What is the probability that the train is speeding given human error was the primary cause for the accident?
# # #P(H|E) = P(E|H)*P(H) / P(E)
# # #P(Speeding|Human.Error) = P(Human.Error|Speeding)*P(Speeding) / P(Human.Error)
# str(RailCleanDS$Was.It.Speeding)
# table(RailCleanDS$Was.It.Speeding=="Yes")
# PE=6254/193711
# table(RailCleanDS$Primary.Accident.Cause.Title=="Train operation - Human Factors")
# PH=69771/193711
# View(HumanError)
# HumErr.Speeding=count(Speeding,Speeding$Primary.Accident.Cause.Title=="Train operation - Human Factors")
# HumErr.Speeding
# PE.H=1883/(1883+4371)
# PH.E=PE.H*PH/PE
# PH.E*100
# 
# (PE.H*PH)/PE
# #30.10873% chance that the primary cause is human error given the train was speeding.

#Probability It Is A Derailment Given That It Was Human Error & Speeding
# SpeedingAndHumErr=subset(RailCleanDS, (RailCleanDS$Primary.Accident.Cause.Title=="Train operation - Human Factors" & RailCleanDS$Was.It.Speeding=="Yes"))
# str(SpeedingAndHumErr)

#What is the probability that it's a derailment given that it was due to human error and the train was speeding?
# Speeding=subset(RailCleanDS,RailCleanDS$Was.It.Speeding=="Yes")
# str(Speeding)
# View(Speeding)


#PREDICTION USING RPART SINCE QUALITATIVE -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(rpart)
# library(rpart.plot)
# ind=sample(2,nrow(RailCleanDS),replace=TRUE,prob=c(0.7,0.3))
# train_data=RailCleanDS[ind==1,]
# test_data=RailCleanDS[ind==2,]
# View(train_data)
# #Need To Save Them As CSVs Since It Will Be Random Every Time:
# write.csv(train_data,file="RailCleanDS.train.data.csv")
# write.csv(test_data,file="RailCleanDS.test.data.csv")
# 
# #tree1=rpart(Total.Damage.Cost~Primary.Accident.Cause.Title+Was.It.Speeding,data=train_data,method="class")
# 
# typeSpeed=predict(tree1,test_data)
# test_data$PredictedCost1=typeSpeed
# #View(test_data)
# table(test_data$PredictedCost1)
# table(test_data$Total.Damage.Cost)
# 
# regr.error <- function(predicted,actual){
#   #MAE
#   mae <- mean(abs(actual-predicted))
#   #MSE
#   mse <- mean((actual-predicted)^2)
#   #RMSE
#   rmse <- sqrt(mean((actual-predicted)^2))
#   #MAPE
#   mape <- mean(abs((actual-predicted)/actual))
#   errors <- c(mae,mse,rmse,mape)
#   names(errors) <- c("mae","mse","rmse","mape")
#   return(errors)
# }
# regr.error(typeSpeed,test_data$Total.Damage.Cost)
# 
# plot(test_data$PredictedCost1,test_data$Total.Damage.Cost)
# abline(tree1,col="red")

#END -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------