{-# LANGUAGE OverloadedStrings #-}

module Model.Elements where

import Metamodel.UfoA
import Metamodel.UfoAInst

gibn = getInstanceByName

ePerson = mkKind "Person"
iPerson1 = OUElementInst ePerson (Just "person1")
iPerson2 = OUElementInst ePerson (Just "person2")
iPerson3 = OUElementInst ePerson (Just "person3")
eDonorRegistry = mkKind "Donor Registry"
iDonorRegistry = OUElementInst eDonorRegistry Nothing
eDonorCentre = mkKind "Donor Centre"
iDonorCentre = OUElementInst eDonorCentre Nothing
eHLALaboratory = mkKind "HLA Laboratory"
iHLALaboratory = OUElementInst eHLALaboratory Nothing
eTransplantCentre = mkKind "TransplantCentre"
iTransplantCentre = OUElementInst eTransplantCentre Nothing
eDonorAspirant = mkRole "Aspirant Donor"
iDonorAspirant = OUElementInst eDonorAspirant Nothing
eDonor = mkRole "Donor"
iDonor = OUElementInst eDonor Nothing
eRecruitment = mkRelator "Recruitment"
iRecruitment = OUElementInst eRecruitment Nothing
eRegistration = mkRelator "Registration"
iRegistration = OUElementInst eRegistration Nothing
eDonorAvailable = mkPhase "Available Donor"
iDonorAvailable = OUElementInst eDonorAvailable Nothing
eDonorTempDeferred = mkPhase "Temporary\nDeferred\nDonor"
iDonorTempDeferred = OUElementInst eDonorTempDeferred Nothing
eDonorPermDeferred = mkPhase "Permanently\nDeferred\nDonor"
iDonorPermDeferred = OUElementInst eDonorPermDeferred Nothing
eDonorPotential = mkRole "Potential Donor"
iDonorPotential = OUElementInst eDonorPotential Nothing
eNotSelected = mkPhase "not Selected\nfor Examination"
iNotSelected = OUElementInst eNotSelected Nothing
eSelectedV = mkPhase "Selected and Available\nfor Verification"
iSelectedV = OUElementInst eSelectedV Nothing
eSelectedET = mkPhase "Selected and Available\nfor Extended Typing"
iSelectedET = OUElementInst eSelectedET Nothing
eExaminationUnavailable = mkPhase "Unavailable\nfor Examination"
iExaminationUnavailable = OUElementInst eExaminationUnavailable Nothing
eDonorVerified = mkRole "Verified\nDonor"
iDonorVerified = OUElementInst eDonorVerified Nothing
eDonorClearance = mkMode "Donor\nClearance"
iDonorClearance = OUElementInst eDonorClearance Nothing
eDonorClearanceSub = mkMode "Donor Clearance\nfor Subsequent Donation"
iDonorClearanceSub = OUElementInst eDonorClearanceSub Nothing
eDonorReserved = mkRole "Reserved\nDonor"
iDonorReserved = OUElementInst eDonorReserved Nothing
eDonorReservation = mkRelator "Donor\nReservation"
iDonorReservation = OUElementInst eDonorReservation Nothing
eDonorChosen = mkRole "Donor Chosen\nfor Donation"
iDonorChosen = OUElementInst eDonorChosen Nothing
eDonorActivated = mkPhase "Donor Activated\nfor Workup"
iDonorActivated = OUElementInst eDonorActivated Nothing
eDonorAvailableForWorkup = mkPhase "Donor Available\nfor Workup"
iDonorAvailableForWorkup = OUElementInst eDonorAvailableForWorkup Nothing
eDonorUnavailableForWorkup = mkPhase "Donor Unavailable\nfor Workup"
iDonorUnavailableForWorkup = OUElementInst eDonorUnavailableForWorkup Nothing
eSchedule = mkMode "Schedule"
iSchedule = OUElementInst eSchedule Nothing
eSpecification = mkMode "Donor Clereance\nSpecification"
iSpecification = OUElementInst eSpecification Nothing
eCollectionRequest = mkRelator "Collection Request"
iCollectionRequest = OUElementInst eCollectionRequest Nothing
eDonorCleared = mkRole "Final Cleared\nDonor"
iDonorCleared = OUElementInst eDonorCleared Nothing
eDonorAccepted = mkRole "Donor Accepted\nfor Collection"
iDonorAccepted = OUElementInst eDonorAccepted Nothing
eDonorClearanceAcc = mkRelator "Donor Clearance\nAcceptance"
iDonorClearanceAcc = OUElementInst eDonorClearanceAcc Nothing
eDonorPBSCPrepared = mkRole "PBSC Prepared\nDonor"
iDonorPBSCPrepared = OUElementInst eDonorPBSCPrepared Nothing
eDonorPBSC = mkRole "PBSC Donor"
iDonorPBSC = OUElementInst eDonorPBSC Nothing
eDonorBM = mkRole "BM\nDonor"
iDonorBM = OUElementInst eDonorBM Nothing
eDonorSubsequent = mkRole "Donor Chosen for\nSubsequent Donation"
iDonorSubsequent = OUElementInst eDonorSubsequent Nothing
eDonorActivatedForWorkupSub = mkPhase "Donor Activated\nforSubsequent Workup"
iDonorActivatedForWokupSub = OUElementInst eDonorActivatedForWorkupSub Nothing
eDonorAvailableForWorkupSub = mkPhase "Donor Available\nfor Subsequent Workup"
iDonorAvailableForWorkupSub = OUElementInst eDonorAvailableForWorkupSub Nothing
eDonorUnavailableForWorkupSub = mkPhase "Donor Unavailable\nfor Subsequent Workup"
iDonorUnavailableForWorkupSub = OUElementInst eDonorUnavailableForWorkupSub Nothing
eScheduleSub = mkMode "Schedule\nfor Subsequent Donation"
iScheduleSub = OUElementInst eScheduleSub Nothing
eSpecificationSub = mkMode "Donor Clereance\nSpecification\nfor Subsequent Donation"
iSpecificationSub = OUElementInst eSpecificationSub Nothing
eDonorSubsequentRel = mkRelator "Collection Request\nfor Subsequent Donation"
iDonorSubsequentRel = OUElementInst eDonorSubsequentRel Nothing
eExpertStatement = mkMode "Expert Statement"
iExpertStatement1 = OUElementInst eExpertStatement (Just "es1")
iExpertStatement2 = OUElementInst eExpertStatement (Just "es2")
iExpertStatement3 = OUElementInst eExpertStatement (Just "es3")
eExamination = mkRelator "Examination"
iExamination = OUElementInst eExamination Nothing
eInitialExamination = mkRelator "Initial Examination"
iInitialExamination = OUElementInst eInitialExamination Nothing
eExtendedExamination = mkRelator "Extended Typing\nwithin New Blood Sample"
iExtendedExamination = OUElementInst eExtendedExamination Nothing
eVerificationExamination = mkRelator "Verification Examination"
iVerificationExamination = OUElementInst eVerificationExamination Nothing
eBloodSampleDraw = mkRelator "Blood Sample Draw"
iBloodSampleDraw1 = OUElementInst eBloodSampleDraw (Just "initial")
iBloodSampleDraw2 = OUElementInst eBloodSampleDraw (Just "extended")
iBloodSampleDraw3 = OUElementInst eBloodSampleDraw (Just "verification")
iBloodSampleDraw4 = OUElementInst eBloodSampleDraw (Just "workup")
iBloodSampleDraw5 = OUElementInst eBloodSampleDraw (Just "subsequent workup")
eBloodSample = mkQuantity "Blood Sample"
iBloodSample1 = OUElementInst eBloodSample (Just "bs1")
iBloodSample2 = OUElementInst eBloodSample (Just "bs2")
iBloodSample3 = OUElementInst eBloodSample (Just "bs3")
iBloodSample4 = OUElementInst eBloodSample (Just "bs4")
iBloodSample5 = OUElementInst eBloodSample (Just "bs5")
eBSFresh = mkPhase "Fresh BS"
iBSFresh1 = OUElementInst eBSFresh (Just "bs1f")
iBSFresh2 = OUElementInst eBSFresh (Just "bs2f")
iBSFresh3 = OUElementInst eBSFresh (Just "bs3f")
eBSStored = mkPhase "Stored BS"
iBSStored1 = OUElementInst eBSStored (Just "bs1s")
iBSStored2 = OUElementInst eBSStored (Just "bs2s")
iBSStored3 = OUElementInst eBSStored (Just "bs3s")
eBSDisposed = mkPhase "Disposed BS"
iBSDisposed1 = OUElementInst eBSDisposed (Just "bs1d")
iBSDisposed2 = OUElementInst eBSDisposed (Just "bs2d")
iBSDisposed3 = OUElementInst eBSDisposed (Just "bs3d")
iBSDisposed4 = OUElementInst eBSDisposed (Just "bs4d")
iBSDisposed5 = OUElementInst eBSDisposed (Just "bs5d")
eDNAIsolation = mkRelator "DNA Isolation"
iDNAIsolation1 = OUElementInst eDNAIsolation (Just "inital")
iDNAIsolation2 = OUElementInst eDNAIsolation (Just "extended")
iDNAIsolation3 = OUElementInst eDNAIsolation (Just "verification")
eDNASample = mkQuantity "DNA Sample"
iDNASample1 = OUElementInst eDNASample (Just "ds1")
iDNASample2 = OUElementInst eDNASample (Just "ds2")
iDNASample3 = OUElementInst eDNASample (Just "ds3")
eDSFresh = mkPhase "Fresh DS"
iDSFresh1 = OUElementInst eDSFresh (Just "ds1f")
iDSFresh2 = OUElementInst eDSFresh (Just "ds2f")
iDSFresh3 = OUElementInst eDSFresh (Just "ds3f")
eDSStored = mkPhase "Stored DS"
iDSStored1 = OUElementInst eDSStored (Just "ds1s")
iDSStored2 = OUElementInst eDSStored (Just "ds2s")
iDSStored3 = OUElementInst eDSStored (Just "ds3s")
eDSDisposed = mkPhase "Disposed DS"
iDSDisposed1 = OUElementInst eDSDisposed (Just "ds1d")
iDSDisposed2 = OUElementInst eDSDisposed (Just "ds2d")
iDSDisposed3 = OUElementInst eDSDisposed (Just "ds3d")
eHLATyping = mkRelator "HLA Typing"
iHLATyping1 = OUElementInst eHLATyping (Just "initial")
iHLATyping2 = OUElementInst eHLATyping (Just "extended")
iHLATyping3 = OUElementInst eHLATyping (Just "verification")
eDonorClearedSub = mkRole "Final Cleared\nDonor\nfor Subsequent Donation"
iDonorClearedSub = OUElementInst eDonorClearedSub Nothing
eDonorAcceptedSub = mkRole "Donor Accepted\nfor Subsequent Collection"
iDonorAcceptedSub = OUElementInst eDonorAcceptedSub Nothing
eDonorClearanceSubAcc = mkRelator "Donor Clearance\nfor Subsequent Donation\nAcceptance"
iDonorClearanceSubAcc = OUElementInst eDonorClearanceSubAcc Nothing
eDonorSubsequentHSC = mkRole "Subsequent\nHSC Donor"
iDonorSubsequentHSC = OUElementInst eDonorSubsequentHSC Nothing

eDonorsTypingResults = mkMode "Donor's\nTyping Results"
iDonorsTypingResults1 = OUElementInst eDonorsTypingResults (Just "initial")
iDonorsTypingResults2 = OUElementInst eDonorsTypingResults (Just "extended")
iDonorsTypingResults3 = OUElementInst eDonorsTypingResults (Just "verification")

eGenotype = mkMode "Person's Genotype"
iGenotype1 = OUElementInst eGenotype (Just "Donor's Genotype")
iGenotype2 = OUElementInst eGenotype (Just "Patient's Genotype")

eGenotypeUnknown = mkPhase "Unknown Genotype"
iGenotypeUnknown1 = OUElementInst eGenotypeUnknown (Just "gup1")
iGenotypeUnknown2 = OUElementInst eGenotypeUnknown (Just "gup2")

eGenotypeEvaluated = mkPhase "Evaluated Genotype"
iGenotypeEvaluated1 = OUElementInst eGenotypeEvaluated (Just "ge1")
iGenotypeEvaluated2 = OUElementInst eGenotypeEvaluated (Just "ge2")

eGenotypeValue = mkQuality "Genotype value"
iGenotypeValue1 = OUElementInst eGenotypeValue (Just "gv1")
iGenotypeValue2 = OUElementInst eGenotypeValue (Just "gv2")

eGeneAssignment = mkRelator "Gene Assignment"
iGeneAssignment1 = OUElementInst eGeneAssignment (Just "initial")
iGeneAssignment2 = OUElementInst eGeneAssignment (Just "extended")
iGeneAssignment3 = OUElementInst eGeneAssignment (Just "verification")

ePatient = mkRole "Patient"
iPatient = OUElementInst ePatient Nothing
ePBSCPatient = mkRole "PBSC\nRecipient"
iPBSCPatient = OUElementInst ePBSCPatient Nothing
eBMPatient = mkRole "BM\nRecipient"
iBMPatient = OUElementInst eBMPatient Nothing
eDLIPatient = mkRole "Subsequent HSC\nRecipient"
iDLIPatient = OUElementInst eDLIPatient Nothing

ePatientRegistration = mkRelator "Patient Registration"
iPatientRegistration = OUElementInst ePatientRegistration Nothing
eFoundDonors = mkCollective "Found Donors"
iFoundDonors = OUElementInst eFoundDonors Nothing
eSearch = mkRelator "Search"
iSearch = OUElementInst eSearch Nothing
eInfectionTesting = mkRelator "Infection Markers\nDisease Testing"
iInfectionTesting = OUElementInst eInfectionTesting Nothing
eInfectionTestingV = mkRelator "Verification\nInfection Markers\nDisease Testing"
iInfectionTestingV = OUElementInst eInfectionTestingV Nothing
eInfectionTestingW = mkRelator "Workup\nInfection Markers\nDisease Testing"
iInfectionTestingW1 = OUElementInst eInfectionTestingW Nothing
iInfectionTestingW2 = OUElementInst eInfectionTestingW (Just "subsequent")
eInfectionMarkers = mkMode "Donor's Infection\nDisease Markers\nResults"
iInfectionMarkers = OUElementInst eInfectionMarkers Nothing
eInfectionMarkersV = mkMode "Donor's Infection\nDisease Markers\nResults\nfor Verification"
iInfectionMarkersV = OUElementInst eInfectionMarkersV Nothing
eInfectionMarkersW = mkMode "Donor's Infection\nDisease Markers\nResults\nfor Workup"
iInfectionMarkersW1 = OUElementInst eInfectionMarkersW Nothing
iInfectionMarkersW2 = OUElementInst eInfectionMarkersW (Just "subsequent")
eMedicalAssessment = mkRelator "Medical\nAssessment"
iMedicalAssessment = OUElementInst eMedicalAssessment Nothing
eMedicalAssessment1 = mkRelator "Initial\nMedical\nAssessment"
iMedicalAssessment1 = OUElementInst eMedicalAssessment1 Nothing
eMedicalAssessment2 = mkRelator "Verification\nMedical\nAssessment"
iMedicalAssessment2 = OUElementInst eMedicalAssessment2 Nothing
eMedicalAssessment3 = mkRelator "Workup\nMedical\nAssessment"
iMedicalAssessment3_1 = OUElementInst eMedicalAssessment3 Nothing
iMedicalAssessment3_2 = OUElementInst eMedicalAssessment3 (Just "subsequent")
eMedicalAssessmentResults = mkMode "Medical\nAssessment\nResults"
iMedicalAssessmentResults = OUElementInst eMedicalAssessmentResults Nothing
eMedicalAssessmentResults1 = mkMode "Initial\nMedical\nAssessment\nResults"
iMedicalAssessmentResults1 = OUElementInst eMedicalAssessmentResults1 Nothing
eMedicalAssessmentResults2 = mkMode "Verification\nMedical\nAssessment\nResults"
iMedicalAssessmentResults2 = OUElementInst eMedicalAssessmentResults2 Nothing
eMedicalAssessmentResults3 = mkMode "Workup\nMedical\nAssessment\nResults"
iMedicalAssessmentResults3_1 = OUElementInst eMedicalAssessmentResults3 Nothing
iMedicalAssessmentResults3_2 = OUElementInst eMedicalAssessmentResults3 (Just "subsequent")
eWorkup = mkRelator "Workup Examination"
iWorkup1 = OUElementInst eWorkup Nothing
iWorkup2 = OUElementInst eWorkup (Just "subsequent")

eHSC = mkQuantity "Hematopoietic\nStem Cells"
iHSC = OUElementInst eHSC Nothing
ePBSC = mkQuantity "PBSC"
iPBSC = OUElementInst ePBSC Nothing
eBM = mkQuantity "BM"
iBM = OUElementInst eBM Nothing
eDLI = mkQuantity "DLI"
iDLI = OUElementInst eDLI Nothing

eCollection = mkRelator "Collection"
iCollection = OUElementInst eCollection Nothing

ePBSCCollection = mkRelator "PBSC Collection"
iPBSCCollection = OUElementInst ePBSCCollection Nothing
eBMCollection = mkRelator "BM Collection"
iBMCollection = OUElementInst eBMCollection Nothing
eDLICollection = mkRelator "DLI Collection"
iDLICollection = OUElementInst eDLICollection Nothing

eCollectedHSC = mkRole "Collected HSC"
iCollectedHSC = OUElementInst eCollectedHSC Nothing
eCollectedPBSC = mkRole "Collected PBSC"
iCollectedPBSC = OUElementInst eCollectedPBSC Nothing
eCollectedBM = mkRole "Collected BM"
iCollectedBM = OUElementInst eCollectedBM Nothing

eTransport = mkRelator "Transport"
iTransportPBSC = OUElementInst eTransport (Just "PBSC")
iTransportBM = OUElementInst eTransport (Just "BM")
iTransportHSC = OUElementInst eTransport (Just "HSC")

eDeliveredHSC = mkRole "Delivered HSC"
iDeliveredHSC = OUElementInst eDeliveredHSC Nothing
eDeliveredPBSC = mkRole "Delivered PBSC"
iDeliveredPBSC = OUElementInst eDeliveredPBSC Nothing
eDeliveredBM = mkRole "Delivered BM"
iDeliveredBM = OUElementInst eDeliveredBM Nothing

eTransplantation = mkRelator "Transplantation"
iTransplantation = OUElementInst eTransplantation Nothing

ePBSCTransplantation = mkRelator "PBSC Transplantation"
iPBSCTransplantation = OUElementInst ePBSCTransplantation Nothing
eBMTransplantation = mkRelator "BM Transplantation"
iBMTransplantation = OUElementInst eBMTransplantation Nothing

eTransplantedHSC = mkRole "Transplanted HSC"
iTransplantedHSC = OUElementInst eTransplantedHSC Nothing
eTransplantedPBSC = mkRole "Transplanted PBSC"
iTransplantedPBSC = OUElementInst eTransplantedPBSC Nothing
eTransplantedBM = mkRole "Transplanted BM"
iTransplantedBM = OUElementInst eTransplantedBM Nothing

ePBSCPreparation = mkRelator "PBSC Donation\nPreparation"
iPBSCPreparation = OUElementInst ePBSCPreparation Nothing
eDLIPreparation = mkRelator "DLI Donation\nPreparation"
iDLIPreparation = OUElementInst eDLIPreparation Nothing

eDonation = mkRelator "HSC Donation"
iDonation = OUElementInst eDonation Nothing

ePBSCDonation = mkRelator "PBSC Donation"
iPBSCDonation = OUElementInst ePBSCDonation Nothing
eBMDonation = mkRelator "BM Donation"
iBMDonation = OUElementInst eBMDonation Nothing
eDLIDonation = mkRelator "DLI Donation"
iDLIDonation = OUElementInst eDLIDonation Nothing

eCollectionCentre = mkKind "Collection\nCentre"
iCollectionCentre = OUElementInst eCollectionCentre Nothing

eCollectionReasoning = mkMode "Reasoning\nof Collection\nof Noneligible\nDonor"
iCollectionReasoning = OUElementInst eCollectionReasoning Nothing

eCourier = mkRole "Courier"
iCourier = OUElementInst eCourier Nothing
eCourierTraining = mkRelator "Courier Training"
iCourierTraining = OUElementInst eCourierTraining Nothing
eCourierTrainingRecord = mkMode "Courier Training Record"
iCourierTrainingRecord = OUElementInst eCourierTrainingRecord Nothing

eDonorFollowup = mkRole "Follow-up Donor"
iDonorFollowup = OUElementInst eDonorFollowup Nothing
eFollowup = mkRelator "Follow-up"
iFollowup = OUElementInst eFollowup Nothing
eFollowupResults = mkMode "Follow-up Results"
iFollowupResults = OUElementInst eFollowupResults Nothing

----------------------------------------------------------------------------------------

g0   = mkGeneralization "g0" ePerson [eDonorAspirant, ePatient, eCourier] PlainGT
gi0_1  = OUGeneralizationInst g0 iPerson1 iDonorAspirant
gi0_2  = OUGeneralizationInst g0 iPerson2 iPatient
gi0_3 = OUGeneralizationInst g0 iPerson3 iCourier
g1   = mkGeneralization "g1" eDonorAspirant [eDonor] Disjoint
gi1 = OUGeneralizationInst g1 iDonorAspirant iDonor
g3   = mkGeneralization "g3" eDonor [eDonorAvailable, eDonorTempDeferred, eDonorPermDeferred] DisjointComplete
gi3_1 = OUGeneralizationInst g3 iDonor iDonorAvailable
gi3_2 = OUGeneralizationInst g3 iDonor iDonorTempDeferred
gi3_3 = OUGeneralizationInst g3 iDonor iDonorPermDeferred
g4   = mkGeneralization "g4" eExamination [eInitialExamination, eExtendedExamination, eVerificationExamination, eWorkup] DisjointComplete
gi4_1 = OUGeneralizationInst g4 iExamination iInitialExamination
gi4_2 = OUGeneralizationInst g4 iExamination iExtendedExamination
gi4_3 = OUGeneralizationInst g4 iExamination iVerificationExamination
gi4_4_1 = OUGeneralizationInst g4 iExamination iWorkup1
gi4_4_2 = OUGeneralizationInst g4 iExamination iWorkup2
g5   = mkGeneralization "g5" eGenotype [eGenotypeUnknown, eGenotypeEvaluated] DisjointComplete
gi5_1_1 = OUGeneralizationInst g5 iGenotype1 iGenotypeUnknown1
gi5_1_2 = OUGeneralizationInst g5 iGenotype1 iGenotypeEvaluated1
gi5_2_1 = OUGeneralizationInst g5 iGenotype2 iGenotypeUnknown2
gi5_2_2 = OUGeneralizationInst g5 iGenotype2 iGenotypeEvaluated2
g6   = mkGeneralization "g6" eDonorAvailable [eDonorPotential] PlainGT
gi6 = OUGeneralizationInst g6 iDonorAvailable iDonorPotential
g7  = mkGeneralization "g7" eSelectedV [eDonorVerified] PlainGT
gi7 = OUGeneralizationInst g7 iSelectedV iDonorVerified
g8a  = mkGeneralization "g8a" eDonorVerified [eDonorReserved] PlainGT
gi8a = OUGeneralizationInst g8a iDonorVerified iDonorReserved
g8b  = mkGeneralization "g8b" eDonorReserved [eDonorChosen] PlainGT
gi8b = OUGeneralizationInst g8b iDonorReserved iDonorChosen
g8c  = mkGeneralization "g8c" eDonorAvailableForWorkup [eDonorCleared] PlainGT
gi8c = OUGeneralizationInst g8c iDonorAvailableForWorkup iDonorCleared
g8d  = mkGeneralization "g8d" eDonorCleared [eDonorAccepted] PlainGT
gi8d = OUGeneralizationInst g8d iDonorCleared iDonorAccepted
g9a  = mkGeneralization "g9a" ePatient [ePBSCPatient, eBMPatient] DisjointComplete
gi9a_1 = OUGeneralizationInst g9a iPatient iPBSCPatient
gi9a_2 = OUGeneralizationInst g9a iPatient iBMPatient
g10  = mkGeneralization "g10" eDonation [ePBSCDonation, eBMDonation, eDLIDonation] DisjointComplete
gi10_1 = OUGeneralizationInst g10 iDonation iPBSCDonation
gi10_2 = OUGeneralizationInst g10 iDonation iBMDonation
gi10_3 = OUGeneralizationInst g10 iDonation iDLIDonation
g11  = mkGeneralization "g11" eDonorAccepted [eDonorPBSCPrepared, eDonorBM] DisjointComplete
gi11_1 = OUGeneralizationInst g11 iDonorAccepted iDonorPBSCPrepared
gi11_2 = OUGeneralizationInst g11 iDonorAccepted iDonorBM
g11b = mkGeneralization "g11b" eDonorPBSCPrepared [eDonorPBSC] PlainGT
gi11b = OUGeneralizationInst g11b iDonorPBSCPrepared iDonorPBSC
g12  = mkGeneralization "g12" eCollection [ePBSCCollection, eBMCollection, eDLICollection] DisjointComplete
gi12_1 = OUGeneralizationInst g12 iCollection iPBSCCollection
gi12_2 = OUGeneralizationInst g12 iCollection iBMCollection
gi12_3 = OUGeneralizationInst g12 iCollection iDLICollection
g13  = mkGeneralization "g13" eTransplantation [ePBSCTransplantation, eBMTransplantation] DisjointComplete
gi13_1 = OUGeneralizationInst g13 iTransplantation iPBSCTransplantation
gi13_2 = OUGeneralizationInst g13 iTransplantation iBMTransplantation

g28a = mkGeneralization "g28a" ePBSC [eCollectedPBSC] PlainGT
gi28a = OUGeneralizationInst g28a iPBSC iCollectedPBSC
g29a = mkGeneralization "g29a" eCollectedPBSC [eDeliveredPBSC] PlainGT
gi29a = OUGeneralizationInst g29a iCollectedPBSC iDeliveredPBSC
g30a = mkGeneralization "g30a" eDeliveredPBSC [eTransplantedPBSC] PlainGT
gi30a = OUGeneralizationInst g30a iDeliveredPBSC iTransplantedPBSC

g28b = mkGeneralization "g28b" eBM [eCollectedBM] PlainGT
gi28b = OUGeneralizationInst g28b iBM iCollectedBM
g29b = mkGeneralization "g29b" eCollectedBM [eDeliveredBM] PlainGT
gi29b = OUGeneralizationInst g29b iCollectedBM iDeliveredBM
g30b = mkGeneralization "g30b" eDeliveredBM [eTransplantedBM] PlainGT
gi30b = OUGeneralizationInst g30b iDeliveredBM iTransplantedBM

g14b_2 = mkGeneralization "g14b_2" eBM [eCollectedBM] PlainGT
gi14b_2 = OUGeneralizationInst g14b_2 iBM iCollectedBM
g15b_2 = mkGeneralization "g15b_2" eCollectedBM [eTransplantedBM] PlainGT
gi15b_2 = OUGeneralizationInst g15b_2 iCollectedBM iTransplantedBM

g16_1 = mkGeneralization "g16_1" eDonorPBSC [eDonorSubsequent] PlainGT
gi16_1 = OUGeneralizationInst g16_1 iDonorPBSC iDonorSubsequent
g16_2 = mkGeneralization "g16_2" eDonorBM [eDonorSubsequent] PlainGT
gi16_2 = OUGeneralizationInst g16_2 iDonorBM iDonorSubsequent

g17 = mkGeneralization "g17" eDonorAvailableForWorkupSub [eDonorClearedSub] PlainGT
gi17 = OUGeneralizationInst g17 iDonorAvailableForWorkupSub iDonorClearedSub
g18 = mkGeneralization "g18" eDonorClearedSub [eDonorAcceptedSub] PlainGT
gi18 = OUGeneralizationInst g18 iDonorClearedSub iDonorAcceptedSub

g19 = mkGeneralization "g19" eInfectionTesting [eInfectionTestingV, eInfectionTestingW] DisjointComplete
gi19_1 = OUGeneralizationInst g19 iInfectionTesting iInfectionTestingV
gi19_2_1 = OUGeneralizationInst g19 iInfectionTesting iInfectionTestingW1
gi19_2_2 = OUGeneralizationInst g19 iInfectionTesting iInfectionTestingW2

g20 = mkGeneralization "g20" eMedicalAssessment [eMedicalAssessment1, eMedicalAssessment2, eMedicalAssessment3] DisjointComplete
gi20_1 = OUGeneralizationInst g20 iMedicalAssessment iMedicalAssessment1
gi20_2 = OUGeneralizationInst g20 iMedicalAssessment iMedicalAssessment2
gi20_3_1 = OUGeneralizationInst g20 iMedicalAssessment iMedicalAssessment3_1
gi20_3_2 = OUGeneralizationInst g20 iMedicalAssessment iMedicalAssessment3_2

g21 = mkGeneralization "g21" eMedicalAssessmentResults [eMedicalAssessmentResults1, eMedicalAssessmentResults2, eMedicalAssessmentResults3] DisjointComplete
gi21_1 = OUGeneralizationInst g21 iMedicalAssessmentResults iMedicalAssessmentResults1
gi21_2 = OUGeneralizationInst g21 iMedicalAssessmentResults iMedicalAssessmentResults2
gi21_3_1 = OUGeneralizationInst g21 iMedicalAssessmentResults iMedicalAssessmentResults3_1
gi21_3_2 = OUGeneralizationInst g21 iMedicalAssessmentResults iMedicalAssessmentResults3_1

g22 = mkGeneralization "g22" eBloodSample [eBSFresh, eBSStored, eBSDisposed] DisjointComplete
gi22_1_1 = OUGeneralizationInst g22 iBloodSample1 iBSFresh1
gi22_2_1 = OUGeneralizationInst g22 iBloodSample1 iBSStored1
gi22_3_1 = OUGeneralizationInst g22 iBloodSample1 iBSDisposed1
gi22_1_2 = OUGeneralizationInst g22 iBloodSample2 iBSFresh2
gi22_2_2 = OUGeneralizationInst g22 iBloodSample2 iBSStored2
gi22_3_2 = OUGeneralizationInst g22 iBloodSample2 iBSDisposed2
gi22_1_3 = OUGeneralizationInst g22 iBloodSample3 iBSFresh3
gi22_2_3 = OUGeneralizationInst g22 iBloodSample3 iBSStored3
gi22_3_3 = OUGeneralizationInst g22 iBloodSample3 iBSDisposed3
gi22_3_4 = OUGeneralizationInst g22 iBloodSample4 iBSDisposed4
gi22_3_5 = OUGeneralizationInst g22 iBloodSample5 iBSDisposed5
g23 = mkGeneralization "g23" eDNASample [eDSFresh, eDSStored, eDSDisposed] DisjointComplete
gi23_1_1 = OUGeneralizationInst g23 iDNASample1 iDSFresh1
gi23_2_1 = OUGeneralizationInst g23 iDNASample1 iDSStored1
gi23_3_1 = OUGeneralizationInst g23 iDNASample1 iDSDisposed1
gi23_1_2 = OUGeneralizationInst g23 iDNASample2 iDSFresh2
gi23_2_2 = OUGeneralizationInst g23 iDNASample2 iDSStored2
gi23_3_2 = OUGeneralizationInst g23 iDNASample2 iDSDisposed2
gi23_1_3 = OUGeneralizationInst g23 iDNASample3 iDSFresh3
gi23_2_3 = OUGeneralizationInst g23 iDNASample3 iDSStored3
gi23_3_3 = OUGeneralizationInst g23 iDNASample3 iDSDisposed3

g24 = mkGeneralization "g24" eDonorPotential [eSelectedV, eSelectedET, eNotSelected, eExaminationUnavailable] DisjointComplete
gi24_1 = OUGeneralizationInst g24 iDonorPotential iSelectedV
gi24_2 = OUGeneralizationInst g24 iDonorPotential iSelectedET
gi24_3 = OUGeneralizationInst g24 iDonorPotential iNotSelected
gi24_4 = OUGeneralizationInst g24 iDonorPotential iExaminationUnavailable

g25 = mkGeneralization "g25" eDonorAcceptedSub [eDonorSubsequentHSC] PlainGT
gi25_1 = OUGeneralizationInst g25 iDonorAcceptedSub iDonorSubsequentHSC


g26 = mkGeneralization "g26" ePBSCPatient [eDLIPatient] PlainGT
gi26_1 = OUGeneralizationInst g26 iPBSCPatient iDLIPatient
g27 = mkGeneralization "g27" eBMPatient [eDLIPatient] PlainGT
gi27_1 = OUGeneralizationInst g27 iBMPatient iDLIPatient

g31 = mkGeneralization "g31" eHSC [ePBSC, eBM, eDLI] Disjoint
gi31_1 = OUGeneralizationInst g31 iHSC iPBSC
gi31_2 = OUGeneralizationInst g31 iHSC iBM
gi31_3 = OUGeneralizationInst g31 iHSC iDLI

g32 = mkGeneralization "g32"  eHSC [eCollectedHSC] PlainGT
gi32 = OUGeneralizationInst g32 iHSC iCollectedHSC
g33 = mkGeneralization "g33"  eCollectedHSC [eDeliveredHSC] PlainGT
gi33 = OUGeneralizationInst g33 iCollectedHSC iDeliveredHSC
g34 = mkGeneralization "g34"  eDeliveredHSC [eTransplantedHSC] PlainGT
gi34 = OUGeneralizationInst g34 iDeliveredHSC iTransplantedHSC

g35 = mkGeneralization "g35" eInfectionMarkersV [eInfectionMarkersW] PlainGT
gi35_1 = OUGeneralizationInst g35 iInfectionMarkersV iInfectionMarkersW1
gi35_2 = OUGeneralizationInst g35 iInfectionMarkersV iInfectionMarkersW2

g36 = mkGeneralization "g36" eDonorChosen [eDonorActivated, eDonorAvailableForWorkup, eDonorUnavailableForWorkup] DisjointComplete
gi36_1 = OUGeneralizationInst g36 iDonorChosen iDonorActivated
gi36_2 = OUGeneralizationInst g36 iDonorChosen iDonorAvailableForWorkup
gi36_3 = OUGeneralizationInst g36 iDonorChosen iDonorUnavailableForWorkup

g37 = mkGeneralization "g37" eDonorSubsequent [eDonorActivatedForWorkupSub, eDonorAvailableForWorkupSub, eDonorUnavailableForWorkupSub] DisjointComplete
gi37_1 = OUGeneralizationInst g37 iDonorSubsequent iDonorActivatedForWokupSub
gi37_2 = OUGeneralizationInst g37 iDonorSubsequent iDonorAvailableForWorkupSub
gi37_3 = OUGeneralizationInst g37 iDonorSubsequent iDonorUnavailableForWorkupSub

g38 = mkGeneralization "g38" eDonorPBSC [eDonorFollowup] PlainGT
gi38 = OUGeneralizationInst g38 iDonorPBSC iDonorFollowup
g39 = mkGeneralization "g39" eDonorBM [eDonorFollowup] PlainGT
gi39 = OUGeneralizationInst g39 iDonorBM iDonorFollowup
g40 = mkGeneralization "g40" eDonorSubsequentHSC [eDonorFollowup] PlainGT
gi40 = OUGeneralizationInst g40 iDonorSubsequentHSC iDonorFollowup

------------------------------------------------------------------------------------------

a1   = mkMediation "a1" (eGeneAssignment, "1") (eDonor, "1")
ai1  = OUAssocInst a1 iGeneAssignment1 iDonor
a3   = mkMediation "a3" (eRecruitment, "1..*") (eDonorCentre, "1")
ai3  = OUAssocInst a3 iRecruitment iDonorCentre

a6   = mkMediation "a6" (eBloodSampleDraw, "1") (eBloodSample, "1")
ai6_1  = OUAssocInst a6 iBloodSampleDraw1 iBloodSample1
ai6_2  = OUAssocInst a6 iBloodSampleDraw2 iBloodSample2
ai6_3  = OUAssocInst a6 iBloodSampleDraw3 iBloodSample3
ai6_4  = OUAssocInst a6 iBloodSampleDraw4 iBloodSample4
ai6_5  = OUAssocInst a6 iBloodSampleDraw5 iBloodSample5
a7   = mkMediation "a7" (eBloodSample, "1") (eDNAIsolation, "0..1")
ai7_1  = OUAssocInst a7 iBloodSample1 iDNAIsolation1
ai7_2  = OUAssocInst a7 iBloodSample2 iDNAIsolation2
ai7_3  = OUAssocInst a7 iBloodSample2 iDNAIsolation3
a8   = mkMediation "a8" (eHLALaboratory, "1") (eDNAIsolation, "1..*")
ai8_1  = OUAssocInst a8 iHLALaboratory iDNAIsolation1
ai8_2  = OUAssocInst a8 iHLALaboratory iDNAIsolation2
ai8_3  = OUAssocInst a8 iHLALaboratory iDNAIsolation3
a9   = mkMediation "a9" (eDNAIsolation, "1") (eDNASample, "1")
ai9_1  = OUAssocInst a9 iDNAIsolation1 iDNASample1
ai9_2  = OUAssocInst a9 iDNAIsolation2 iDNASample2
ai9_3  = OUAssocInst a9 iDNAIsolation3 iDNASample3
a10  = mkMediation "a10" (eHLALaboratory, "1") (eGeneAssignment, "1..*")
ai10_1 = OUAssocInst a10 iHLALaboratory iGeneAssignment1
ai10_2 = OUAssocInst a10 iHLALaboratory iGeneAssignment2
ai10_3 = OUAssocInst a10 iHLALaboratory iGeneAssignment3
a11  = mkMediation "a11" (eDonorsTypingResults, "1") (eGeneAssignment, "1")
ai11_1 = OUAssocInst a11 iDonorsTypingResults1 iGeneAssignment1
ai11_2 = OUAssocInst a11 iDonorsTypingResults2 iGeneAssignment2
ai11_3 = OUAssocInst a11 iDonorsTypingResults3 iGeneAssignment3
a12  = mkMediation "a12" (eGenotypeValue, "1") (eGeneAssignment, "1")
ai12_1 = OUAssocInst a12 iGenotypeValue1 iGeneAssignment1
ai12_2 = OUAssocInst a12 iGenotypeValue1 iGeneAssignment2
ai12_3 = OUAssocInst a12 iGenotypeValue1 iGeneAssignment3
a13  = mkCharacterization "a13" (eDonor, "1") (eGenotypeValue, "1")
ai13 = OUAssocInst a13 iDonor iGenotypeValue1
a14  = mkCharacterization "a14" (ePerson, "1") (eGenotype, "1")
ai14_1 = OUAssocInst a14 iPerson1 iGenotype1
ai14_2 = OUAssocInst a14 iPerson2 iGenotype2
a15  = mkCharacterization "a15" (eGenotypeEvaluated, "1") (eGenotypeValue, "1")
ai15_1_1 = OUAssocInst a15 iGenotypeEvaluated1 iGenotypeValue1
ai15_2_1 = OUAssocInst a15 iGenotypeEvaluated2 iGenotypeValue2
a16a_2a  = mkMediation "a16a_2a" (eSelectedET, "1") (eExtendedExamination, "0..*")
ai16a_2a = OUAssocInst a16a_2a iSelectedET iExtendedExamination
a16a_3  = mkMediation "a16a_3" (eDonorVerified, "1") (eVerificationExamination, "1..*")
ai16a_3 = OUAssocInst a16a_3 iDonorVerified iVerificationExamination

a17a  = mkMediation "a17a" (ePatient, "1") (ePatientRegistration, "1")
ai17a = OUAssocInst a17a iPatient iPatientRegistration
a17b  = mkMediation "a17b" (ePatientRegistration, "1..*") (eTransplantCentre, "1")
ai17b = OUAssocInst a17b iPatientRegistration iTransplantCentre

a18a = mkMediation "a18a" (ePatient, "1") (eSearch, "0..1")
ai18a = OUAssocInst a18a iPatient iSearch
a18b  = mkMediation "a18b" (eFoundDonors, "0..1") (eSearch, "1")
ai18b = OUAssocInst a18b iFoundDonors iSearch
a19  = mkMediation "a19" (eSearch, "1..*") (eDonorRegistry, "1")
ai19 = OUAssocInst a19 iSearch iDonorRegistry

a21  = mkMediation "a21" (eDonorCentre, "1") (eExamination, "1..*")
ai21 = OUAssocInst a21 iDonorCentre iExamination
a22  = mkMediation "a22" (eHLATyping, "1..*") (eDNASample, "1")
ai22_1 = OUAssocInst a22 iHLATyping1 iDNASample1
ai22_2 = OUAssocInst a22 iHLATyping2 iDNASample2
ai22_3 = OUAssocInst a22 iHLATyping3 iDNASample3
a23  = mkCharacterization "a23" (eHLATyping, "1..*") (eDonorsTypingResults, "1")
ai23_1 = OUAssocInst a23 iHLATyping1 iDonorsTypingResults1
ai23_2 = OUAssocInst a23 iHLATyping2 iDonorsTypingResults2
ai23_3 = OUAssocInst a23 iHLATyping3 iDonorsTypingResults3
a24  = mkMediation "a24" (eDonorAspirant, "1") (eRecruitment, "1")
ai24 = OUAssocInst a24 iDonorAspirant iRecruitment
a25  = mkAssoc (Just "<<material>>\n/belongs") "a25" (eDonorAspirant, "1") (eBloodSample, "1..*")
ai25 = OUAssocInst a25 iDonorAspirant iBloodSample1
a26  = mkAssoc (Just "<<material>>\n/belongs") "a25" (eDonor, "1") (eDNASample, "1..*")
ai26 = OUAssocInst a26 iDonor iDNASample1
a27  = mkMediation "a27" (eHLATyping, "0..*") (eHLALaboratory, "1")
ai27_1 = OUAssocInst a27 iHLATyping1 iHLALaboratory
ai27_2 = OUAssocInst a27 iHLATyping2 iHLALaboratory
ai27_3 = OUAssocInst a27 iHLATyping3 iHLALaboratory
a28  = mkAssoc (Just "<<material>>\nregisters") "a28" (eDonorCentre, "1") (eBloodSample, "1..*")
ai28 = OUAssocInst a28 iDonorCentre iBloodSample1
a29 = mkMediation "a29" (eInfectionTesting, "1") (eBloodSample, "1")
ai29 = OUAssocInst a29 iInfectionTestingV iBloodSample3
a30_1 = mkMediation "a30_1" (eInfectionTestingV, "1") (eInfectionMarkersV, "1")
ai30_1 = OUAssocInst a30_1 iInfectionTestingV iInfectionMarkersV
a30_2 = mkMediation "a30_2" (eInfectionTestingW, "1") (eInfectionMarkersW, "1")
ai30_2_1 = OUAssocInst a30_2 iInfectionTestingW1 iInfectionMarkersW1
ai30_2_2 = OUAssocInst a30_2 iInfectionTestingW2 iInfectionMarkersW2

a32 = mkCharacterization "a32" (eMedicalAssessment, "1") (eMedicalAssessmentResults, "1")
ai32 = OUAssocInst a32 iMedicalAssessment iMedicalAssessmentResults
a32_1 = mkCharacterization "a32_1" (eMedicalAssessment1, "1") (eMedicalAssessmentResults1, "1")
ai32_1 = OUAssocInst a32_1 iMedicalAssessment1 iMedicalAssessmentResults1
a32_2 = mkCharacterization "a32_2" (eMedicalAssessment2, "1") (eMedicalAssessmentResults2, "1")
ai32_2 = OUAssocInst a32_2 iMedicalAssessment2 iMedicalAssessmentResults2
a32_3 = mkCharacterization "a32_3" (eMedicalAssessment3, "1") (eMedicalAssessmentResults3, "1")
ai32_3_1 = OUAssocInst a32_3 iMedicalAssessment3_1 iMedicalAssessmentResults3_1
ai32_3_2 = OUAssocInst a32_3 iMedicalAssessment3_2 iMedicalAssessmentResults3_2

a33_3a = mkMediation "a33_3a" (eDonorChosen, "1") (eCollectionRequest, "1")
ai33_3a = OUAssocInst a33_3a iDonorChosen iCollectionRequest
a33_3b = mkMediation "a33_3b" (eCollectionRequest, "0..*") (eTransplantCentre, "1")
ai33_3b = OUAssocInst a33_3b iCollectionRequest iTransplantCentre

a34_1a = mkMediation "a34_1a" (eDonorReservation, "1") (eDonorReserved, "1")
ai34_1a = OUAssocInst a34_1a iDonorReservation iDonorReserved
a34_1b = mkMediation "a34_1b" (eDonorReservation, "0..*") (eTransplantCentre, "1")
ai34_1b = OUAssocInst a34_1b iDonorReservation iTransplantCentre

a34_2aa = mkMediation "a34_2aa" (eDonorCleared, "1") (eWorkup, "1")
ai34_2aa = OUAssocInst a34_2aa iDonorCleared iWorkup1
a34_2ab_1 = mkCharacterization "a34_2ab_1" (eDonorCleared, "1") (eDonorClearance, "1")
ai34_2ab_1 = OUAssocInst a34_2ab_1 iDonorCleared iDonorClearance
a34_2ab_2 = mkCharacterization "a34_2ab_2" (eDonorClearedSub, "1") (eDonorClearanceSub, "1")
ai34_2ab_2 = OUAssocInst a34_2ab_2 iDonorClearedSub iDonorClearanceSub
a34_2b = mkMediation "a34_2b" (eDonorAccepted, "1") (eDonorClearanceAcc, "1")
ai34_2b = OUAssocInst a34_2b iDonorAccepted iDonorClearanceAcc
a34_2c = mkMediation "a34_2c" (eDonorClearanceAcc, "0..*") (eTransplantCentre, "1")
ai34_2c = OUAssocInst a34_2c iDonorClearanceAcc iTransplantCentre
a34_2d = mkCharacterization "a34_2d" (eDonorClearanceAcc, "1") (eCollectionReasoning, "0..1")
ai34_2d = OUAssocInst a34_2d iDonorClearanceAcc iCollectionReasoning

a36a = mkMediation "a36a" (ePBSCCollection, "1") (eCollectedPBSC, "1")
ai36a = OUAssocInst a36a iPBSCCollection iCollectedPBSC
a36aa = mkMediation "a36aa" (ePBSCCollection, "1") (eDonorPBSC, "1")
ai36aa = OUAssocInst a36aa iPBSCCollection iDonorPBSC
a36b = mkMediation "a36b" (eBMCollection, "1") (eCollectedBM, "1")
ai36b = OUAssocInst a36b iBMCollection iCollectedBM
a36ba = mkMediation "a36ba" (eBMCollection, "1") (eDonorBM, "1")
ai36ba = OUAssocInst a36ba iBMCollection iDonorBM

a37a = mkMediation "a37a" (ePBSCPreparation, "1") (eDonorPBSCPrepared, "1")
ai37a = OUAssocInst a37a iPBSCPreparation iDonorPBSCPrepared
a37b = mkMediation "a37b" (ePBSCPreparation, "0..*") (eDonorCentre, "1")
ai37b = OUAssocInst a37b iPBSCPreparation iDonorCentre

a39 = mkMediation "a39" (eCollection, "1..*") (eCollectionCentre, "1")
ai39 = OUAssocInst a39 iCollection iCollectionCentre

a40   = mkMediation "a40" (eTransplantation, "0..*") (eTransplantCentre, "1")
ai40  = OUAssocInst a40 iTransplantation iTransplantCentre

a42_1a = mkMediation "a42_1a" (ePBSCTransplantation, "1") (eTransplantedPBSC, "1")
ai42_1a = OUAssocInst a42_1a iPBSCTransplantation iTransplantedPBSC
a42_1b = mkMediation "a42_1b" (ePBSCTransplantation, "1") (ePBSCPatient, "1")
ai42_1b = OUAssocInst a42_1b iPBSCTransplantation iPBSCPatient
a42_2a = mkMediation "a42_2a" (eBMTransplantation, "1") (eTransplantedBM, "1")
ai42_2a = OUAssocInst a42_2a iBMTransplantation iTransplantedBM
a42_2c = mkMediation "a42_2c" (eBMTransplantation, "1") (eBMPatient, "1")
ai42_2c = OUAssocInst a42_2c iBMTransplantation iBMPatient
a42_3c = mkMediation "a42_3c" (eTransplantation, "1") (eDLIPatient, "1")
ai42_3c = OUAssocInst a42_3c iTransplantation iDLIPatient

a44 = mkCharacterization "a44" (eDonorChosen, "1") (eSchedule, "1")
ai44 = OUAssocInst a44 iDonorChosen iSchedule
a45 = mkCharacterization "a45" (eDonorChosen, "1") (eSpecification, "1")
ai45 = OUAssocInst a45 iDonorChosen iSpecification

a46 = mkMediation "a46" (eDonorSubsequent, "1") (eDonorSubsequentRel, "1")
ai46 = OUAssocInst a46 iDonorSubsequent iDonorSubsequentRel
a47a = mkMediation "a47a" (eDonorSubsequentRel, "0..*") (eTransplantCentre, "1")
ai47a = OUAssocInst a47a iDonorSubsequentRel iTransplantCentre
a47b = mkMediation "a47b" (eDonorSubsequentRel, "0..*") (eDonorRegistry, "1")
ai47b = OUAssocInst a47b iDonorSubsequentRel iDonorRegistry

a48 = mkCharacterization "a48" (eDonorSubsequent, "1") (eScheduleSub, "1")
ai48 = OUAssocInst a48 iDonorSubsequent iScheduleSub
a49 = mkCharacterization "a49" (eDonorSubsequent, "1") (eSpecificationSub, "1")
ai49 = OUAssocInst a49 iDonorSubsequent iSpecificationSub

a50 = mkMediation "a50" (eDonorClearedSub, "1..*") (eWorkup, "1")
ai50 = OUAssocInst a50 iDonorClearedSub iWorkup2

a52= mkMediation "a52" (eDonorAcceptedSub, "1") (eDonorClearanceSubAcc, "1")
ai52 = OUAssocInst a52 iDonorAcceptedSub iDonorClearanceSubAcc
a53= mkMediation "a53" (eDonorClearanceSubAcc, "0..*") (eTransplantCentre, "1")
ai53 = OUAssocInst a53 iDonorClearanceSubAcc iTransplantCentre

a54 = mkMediation "a54" (eDonorSubsequentHSC, "1") (eCollection, "1")
ai54 = OUAssocInst a54 iDonorSubsequentHSC iCollection

a55 = mkMediation "a55" (eCourier, "1") (eCourierTraining, "1")
ai55 = OUAssocInst a55 iCourier iCourierTraining
a56 = mkCharacterization "a56" (eCourier, "1") (eCourierTrainingRecord, "1")
ai56 = OUAssocInst a56 iCourier iCourierTrainingRecord
a57 = mkCharacterization "a57" (eCourierTraining, "0..*") (eDonorRegistry, "1")
ai57 = OUAssocInst a57 iCourierTraining iDonorRegistry

a58 = mkMediation "a58" (eCourier, "1") (eTransport, "0..*")
ai58_1 = OUAssocInst a58 iCourier iTransportPBSC
ai58_2 = OUAssocInst a58 iCourier iTransportBM
ai58_3 = OUAssocInst a58 iCourier iTransportHSC

a59_1 = mkMediation "a59_1" (eTransport, "1") (eDeliveredPBSC, "0..1")
ai59_1 = OUAssocInst a59_1 iTransportPBSC iDeliveredPBSC
a59_2 = mkMediation "a59_2" (eTransport, "1") (eDeliveredBM, "0..1")
ai59_2 = OUAssocInst a59_2 iTransportBM iDeliveredBM
a59_3 = mkMediation "a59_3" (eTransport, "1") (eDeliveredHSC, "0..1")
ai59_3 = OUAssocInst a59_3 iTransportHSC iDeliveredHSC

a60 = mkMediation "a60" (eCollectedHSC, "1") (eCollection, "1")
ai60 = OUAssocInst a60 iCollectedHSC iCollection
a61 = mkMediation "a61" (eDeliveredHSC, "1") (eTransport, "1")
ai61 = OUAssocInst a61 iDeliveredHSC iTransportBM
a62 = mkMediation "a62" (eTransplantedHSC, "1") (eTransplantation, "1")
ai62 = OUAssocInst a62 iTransplantedHSC iTransplantation

a63 = mkMediation "a63" (eHSC, "1") (eDonation, "1")
ai63 = OUAssocInst a63 iHSC iDonation

a64 = mkContainment "a64" (ePerson, "1") (eHSC, "1")
ai64 = OUAssocInst a64 iPerson1 iHSC

a65 = mkCharacterization "a65" (eDonorSubsequentRel, "1") (eExpertStatement, "2..3")
ai65_1 = OUAssocInst a65 iDonorSubsequentRel iExpertStatement1
ai65_2 = OUAssocInst a65 iDonorSubsequentRel iExpertStatement2
ai65_3 = OUAssocInst a65 iDonorSubsequentRel iExpertStatement3

a66 = mkCharacterization "a66" (eFollowup, "1") (eFollowupResults, "1..*")
ai66 = OUAssocInst a66 iFollowup iFollowupResults
a67 = mkMediation "a67" (eDonorCentre, "1") (eFollowup, "0..*")
ai67 = OUAssocInst a67 iDonorCentre iFollowup
a68 = mkMediation "a68" (eFollowup, "1") (eDonorFollowup, "1")
ai68 = OUAssocInst a68 iFollowup iDonorFollowup

-----------------------------------------------------------------------------------------

m1 = mkMemberOf "m1" (eDonorRegistry, "1")      (eDonorCentre, "10")           PlainPH
mi1 = OUAssocPHInst m1 iDonorRegistry iDonorCentre
m2 = mkMemberOf "m2" (eDonorRegistry, "1")      (eHLALaboratory, "1")          PlainPH
mi2 = OUAssocPHInst m2 iDonorRegistry iHLALaboratory

m3 = mkMemberOf "m3"  (eRecruitment, "1") (eRegistration, "1") EssentialInseparable
mi3 = OUAssocPHInst m3 iRecruitment iRegistration
m4b = mkMemberOf "m4b"  (eRecruitment, "1") (eInitialExamination, "1") EssentialInseparable
mi4b = OUAssocPHInst m4b iRecruitment iInitialExamination

m5 = mkMemberOf "m5" (eInitialExamination, "1") (eMedicalAssessment1, "1")       EssentialInseparable
mi5 = OUAssocPHInst m5 iInitialExamination iMedicalAssessment1
m6_1 = mkMemberOf "m6_1" (eInitialExamination, "1") (eBloodSampleDraw, "1") EssentialInseparable
mi6_1 = OUAssocPHInst m6_1 iInitialExamination iBloodSampleDraw1
m6_2 = mkMemberOf "m6_2" (eExtendedExamination, "1") (eBloodSampleDraw, "0..1") Inseparable
mi6_2 = OUAssocPHInst m6_2 iExtendedExamination iBloodSampleDraw2
m6_3 = mkMemberOf "m6_3" (eVerificationExamination, "1") (eBloodSampleDraw, "1") EssentialInseparable
mi6_3 = OUAssocPHInst m6_3 iVerificationExamination iBloodSampleDraw3
m7 = mkMemberOf "m7" (eBloodSample, "1") (eDNASample, "0..1") EssentialInseparable
mi7_1 = OUAssocPHInst m7 iBloodSample1 iDNASample1
mi7_2 = OUAssocPHInst m7 iBloodSample2 iDNASample2
mi7_3 = OUAssocPHInst m7 iBloodSample3 iDNASample3
mi7_4 = OUAssocPHInst m7 iBloodSample1 iDNASample2

m9 = mkMemberOf "m9" (eFoundDonors, "1") (eDonorPotential, "0..*")          PlainPH
mi9 = OUAssocPHInst m9 iFoundDonors iDonorPotential
m10a = mkMemberOf "m10a" (eVerificationExamination, "1") (eInfectionTestingV, "1") EssentialInseparable
mi10a = OUAssocPHInst m10a iVerificationExamination iInfectionTestingV
m10b = mkMemberOf "m10b" (eWorkup, "1") (eInfectionTestingW, "1") EssentialInseparable
mi10b_1 = OUAssocPHInst m10b iWorkup1 iInfectionTestingW1
mi10b_2 = OUAssocPHInst m10b iWorkup2 iInfectionTestingW2
m10c = mkMemberOf "m10c" (eWorkup, "1") (eMedicalAssessment3, "1") EssentialInseparable
mi10c_1 = OUAssocPHInst m10c iWorkup1 iMedicalAssessment3_1
mi10c_2 = OUAssocPHInst m10c iWorkup2 iMedicalAssessment3_2
m10d = mkMemberOf "m10d" (eWorkup, "1") (eBloodSampleDraw, "1") EssentialInseparable
mi10d_1 = OUAssocPHInst m10d iWorkup1 iBloodSampleDraw4
mi10d_2 = OUAssocPHInst m10d iWorkup2 iBloodSampleDraw5
m11 = mkMemberOf "m11" (eVerificationExamination, "1") (eMedicalAssessment2, "1") PlainPH
mi11 = OUAssocPHInst m11 iVerificationExamination iMedicalAssessment2
m12a = mkMemberOf "m12a" (ePBSCDonation, "1") (ePBSCPreparation, "1") EssentialInseparable
mi12a = OUAssocPHInst m12a iPBSCDonation iPBSCPreparation

m13_1  = mkMemberOf "m13_1" (eDonation, "1") (eCollection, "1") EssentialInseparable
mi13_1 = OUAssocPHInst m13_1 iDonation iCollection
m13_1a  = mkMemberOf "m13_1a" (ePBSCDonation, "1") (ePBSCCollection, "1") EssentialInseparable
mi13_1a = OUAssocPHInst m13_1a iPBSCDonation iPBSCCollection
m13_1b  = mkMemberOf "m13_1b" (eBMDonation, "1") (eBMCollection, "1") EssentialInseparable
mi13_1b = OUAssocPHInst m13_1b iBMDonation iBMCollection
m13_1c  = mkMemberOf "m13_1c" (eDLIDonation, "1") (eDLICollection, "1") EssentialInseparable
mi13_1c = OUAssocPHInst m13_1c iDLIDonation iDLICollection

m13_2  = mkMemberOf "m13_2" (eDonation, "1") (eTransport, "1") EssentialInseparable
mi13_2a = OUAssocPHInst m13_2 iPBSCDonation iTransportPBSC
mi13_2b = OUAssocPHInst m13_2 iBMDonation iTransportBM
mi13_2c = OUAssocPHInst m13_2 iDLIDonation iTransportHSC

m13_3  = mkMemberOf "m13_3" (eDonation, "1") (eTransplantation, "1") EssentialInseparable
mi13_3 = OUAssocPHInst m13_3 iDonation iTransplantation
m13_3a  = mkMemberOf "m13_3a" (ePBSCDonation, "1") (ePBSCTransplantation, "1") EssentialInseparable
mi13_3a = OUAssocPHInst m13_3a iPBSCDonation iPBSCTransplantation
m13_3b  = mkMemberOf "m13_3b" (eBMDonation, "1") (eBMTransplantation, "1") EssentialInseparable
mi13_3b = OUAssocPHInst m13_3b iBMDonation iBMTransplantation

m15 = mkMemberOf "m15" (eDonorRegistry, "1") (eCollectionCentre, "1") PlainPH
mi15 = OUAssocPHInst m15 iDonorRegistry iCollectionCentre
