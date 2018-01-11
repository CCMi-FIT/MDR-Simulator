{-# LANGUAGE OverloadedStrings #-}

module Model.Models where

import Metamodel.UfoA
import Metamodel.UfoAInst
import Metamodel.UfoB
import Model.Elements
import Texts

ouModel :: OUModel
ouModel = OUModel
  { oumGeneralizationClusters = [[g0, g1, g3, g6, g24, g7, g8a, g8b, g8c, g8d, g11, g11b, g16_1, g16_2, g17, g18, g25], [g5], [g4], [g9a, g26, g27], [g10], [g12], [g13], [g19], [g20], [g21], [g22], [g23], [g28a, g28b, g28c, g29a, g29b, g29c, g30a, g30b, g30c], [g31, g32, g33, g34, g35, g36, g37]]
  , oumAssocs = [a1, a3, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16a_2a, a16a_3, a17a, a17b, a18a, a18b, a19, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30_1, a30_2, a31_1, a31_2, a32_1, a32_2, a32_3, a33_1, a33_2, a33_3a, a33_3b, a34a, a34b, a34_1a, a34_1b, a34_2aa, a34_2ab, a34_2b, a34_2c, a34_2d, a35_1, a36a, a36aa, a36ba, a36b, a36c, a37a, a37b, a39, a40, a42_1a, a42_1b, a42_2a, a42_2c, a42_3a, a42_3c, a44, a45, a46, a47a, a47b, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59_1, a59_2, a59_3, a60, a61, a62, a63, a64, a65]
  , oumAssocsPH = [m1, m2, m3, m4b, m5, m6_1, m6_2, m6_3, m7, m9, m10a, m10b, m10c, m10d, m11, m12a, m13_1, m13_1a, m13_1b, m13_1c, m13_2, m13_3, m13_3a, m13_3b, m13_3c, m15]
  }

-- Instance --

ouModelInst :: OUModelInst
ouModelInst = OUModelInst
  { oumiGeneralizationClusters = [[gi0_1, gi0_2, gi0_3, gi1, gi3_1, gi3_2, gi3_3, gi6, gi24_1, gi24_2, gi24_3, gi24_4, gi7, gi8a, gi8b, gi8c, gi8d, gi11_1, gi11_2, gi11b, gi16_1, gi16_2, gi17, gi18, gi25_1], [gi5_1_1, gi5_1_2], [gi5_2_1, gi5_2_2], [gi4_1, gi4_2, gi4_3, gi4_4], [gi9a_1, gi9a_2], [gi26_1, gi27_1], [gi10_1, gi10_2, gi10_3], [gi12_1, gi12_2, gi12_3], [gi13_1, gi13_2, gi13_3], [gi19_1, gi19_2], [gi20_1, gi20_2, gi20_3], [gi22_1_1, gi22_2_1, gi22_3_1],  [gi22_1_2, gi22_2_2, gi22_3_2], [gi22_1_3, gi22_2_3, gi22_3_3], [gi23_1_1, gi23_2_1, gi23_3_1], [gi23_1_2, gi23_2_2, gi23_3_2], [gi23_1_3, gi23_2_3, gi23_3_3], [gi28a, gi28b, gi28c, gi29a, gi29b, gi29c, gi30a, gi30b, gi30c], [gi31_1, gi31_2, gi31_3], [gi32, gi33, gi34, gi35], [gi36_1, gi36_2], [gi37_1, gi37_2]]
   , oumiAssocs = [ai1, ai3, ai6_1, ai6_2, ai6_3, ai6_4, ai7_1, ai7_2, ai7_3, ai8_1, ai8_2, ai8_3, ai9_1, ai9_2, ai9_3, ai10_1, ai10_2, ai10_3, ai11_1, ai11_2, ai11_3, ai12_1, ai12_2, ai12_3, ai13, ai14_1, ai14_2, ai15_1_1, ai16a_2a, ai16a_3, ai17a, ai17b, ai18a, ai18b, ai19, ai21, ai22_1, ai22_2, ai22_3, ai23_1, ai23_2, ai23_3, ai24, ai25, ai26, ai27_1, ai27_2, ai27_3, ai28, ai29, ai30_1, ai30_2, ai31_1, ai31_2, ai32, ai32_1, ai32_2, ai32_3, ai33_1, ai33_2, ai33_3a, ai33_3b, ai34a, ai34b, ai34_1a, ai34_1b, ai34_2aa, ai34_2ab, ai34_2b, ai34_2c, ai34_2d, ai35_1, ai36a, ai36aa, ai36ba, ai36b, ai36c, ai37a, ai37b,  ai39, ai40, ai42_1a, ai42_1b, ai42_2a, ai42_2c, ai42_3a, ai42_3c, ai44, ai45, ai46, ai47a, ai47b, ai48, ai49, ai50, ai51, ai52, ai53, ai54, ai55, ai56, ai57, ai58_1, ai58_2, ai58_3, ai59_1, ai59_2, ai59_3, ai60, ai61, ai62, ai63, ai64, ai65]
   , oumiAssocsPH = [mi1, mi2, mi3, mi4b, mi5, mi6_1, mi6_2, mi6_3, mi7_1, mi7_2, mi7_3, mi9, mi10a, mi10b, mi10c, mi10d, mi11, mi12a, mi13_1, mi13_1a, mi13_1b, mi13_1c, mi13_2a, mi13_2b, mi13_2c, mi13_3, mi13_3a, mi13_3b, mi13_3c, mi15]
  }

-- UFO-B --

switchPhase' = switchPhase ouModelInst

sInitial :: OUSituation
sInitial =
  mkSituation  "A Person\nwants to\nbecome a Donor"
    [ addElements
      [ iPerson1
      , iGenotype1
      , iGenotypeUnknown1
      , iHSC
      , iBM
      , iPBSC
      , iDLI
      , iDonorRegistry
      , iDonorCentre
      , iHLALaboratory
      , iCollectionCentre
      , iTransplantCentre
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donor\nRecruitment" sAspirantDonor
      ]
    ]
    sInitialT

sNeeds :: OUSituation
sNeeds =
  mkSituation "A Person Needs\nUnrelated\nDonor Transplanation"
  [ addElements
    [ iPerson2
    , iPatient
    , iGenotype2
    , iGenotypeEvaluated2
    , iPatientRegistration
    ]
  ]
  [ OUDisposition Nothing
    [ OUEventB "Search Initiating" sSearchRequested]
  ]
  sNeedsT

sAspirantDonor :: OUSituation
sAspirantDonor =
  mkSituation "A Person becomes\nAspirant Donor"
    [ addElements
      [ iDonorAspirant
      , iRecruitment
      , iRegistration
      , iExamination
      , iInitialExamination
      , iBloodSampleDraw1
      , iBloodSample1
      , iBSFresh1
      , iMedicalAssessment1
      , iMedicalAssessmentResults1
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Initial Histocompatibility Testing" sAvailable
      ]
    ]
    sIssuedRegistrationT

sAvailable :: OUSituation
sAvailable =
  mkSituation "Donor Available"
   -- [ removeElements
   --   [ iBSFresh1
   --   , iDSFresh1 ]
    [ addElements
      [ iDonor
      , iDNAIsolation1
      , iDNASample1
      , iDSStored1
      , iDSDisposed1
      , iBSStored1
      , iBSDisposed1
      , iHLATyping1
      , iGeneAssignment1
      , iDonorsTypingResults1
      , iGenotypeValue1

      ]
    , switchPhase' [iGenotypeEvaluated1]
    , switchPhase' [iDonorAvailable]
    ]
    [ OUDisposition Nothing
      [ mEventB "Donors Search" sFound ]
    , OUDisposition (Just "Unavailability\nReasons\nOccured")
      [ OUEventB "Donor\nBecomes\nTemporary\nUnavailable" sTempDeferred ]
    , OUDisposition (Just "Permament\nUnavailability\nReasons\nOccured")
      [ OUEventB "Donor\nBecomes\nPermanently\nUnavailable" sPermDeferred ]
    , OUDisposition (Just "Subsequent\nDonation Required")
      [ OUEventB "Collection Dates Assessment\nfor Subsequent Donation" sScheduled2 ]
    ]
    sIssuedRegistrationT

sSearchRequested :: OUSituation
sSearchRequested =
  mkSituation "Donor Search Initiated"
    [ addElements
      [ iPatient
      , iSearch
      ]
    ]
    [ OUDisposition Nothing
      [ mEventB "Donors Search" sFound ]
    ]
    ""

sTempDeferred :: OUSituation
sTempDeferred =
  mkSituation "Donor\nTemporary\nDeferred"
    [ switchPhase' [iDonorTempDeferred] ]
    [ OUDisposition (Just "Reasons\nGone")
      [ OUEventB "Donor\nBecomes\nAvailable" sAvailable ]
    , OUDisposition (Just "Reasons\nBecome\nPermanent")
      [ OUEventB "nDonor\nBecomes\nPermanently\nUnavailable" sPermDeferred ]
    , OUDisposition (Just "Subsequent\nDonation Required")
      [ OUEventB "Collection Dates Assessment\nfor Subsequent Donation" sScheduled2 ]
    ]
    "Reasons for making an Available Donor temporary unavailable comprise a performed transplantation, or a travel, medical reasons, gravidity and others."

sPermDeferred :: OUSituation
sPermDeferred =
  mkSituation "Donor\nPermanently\nDeferred"
    [ switchPhase' [iDonorPermDeferred] ]
    []
    "Reasons for making a Donor Permanently Deferred are a.o. Donor's request, age, medical reasons, death or realizing two transplantations."

sFound :: OUSituation
sFound =
  mkSituation "Potential\nDonor(s)\nFound"
    [ addElements
      [ iDonorPotential
      , iNotSelected
      , iFoundDonors
      ]
    ]
    [ OUDisposition (Just "Extended HLA\nTyping not Required")
      [OUEventB "Donors Selection\nfor Verification" sSelection]
    , OUDisposition (Just "Extended HLA\nTyping Required\nand\nBlood Sample\nAvailable")
      [OUEventB "Extended\nHLA\nTyping\nfrom Stored\nBlood Sample" sHLATyped2_1 ]
    , OUDisposition (Just "Extended HLA\nTyping Required\nand\nDNA Sample\nAvailable")
      [OUEventB "Extended\nHLA\nTyping\nfrom Stored\nDNA Sample" sHLATyped2_2 ]
    , OUDisposition (Just "Extended HLA\nTyping Required\nand\nBlood Sample/DNA\nnot Available")
      [OUEventB "Donor Activation\nand Blood Sample Draw\nfor Extended HLA Typing" sDraw2 ]
    ]
    sFoundT

sDraw2 :: OUSituation
sDraw2 =
  mkSituation "Donor Blood Sample\nDrawn for\nExtended HLA Typing"
   -- [ removeElements
   --   [ iBSStored1
   --   , iDSStored1
   --   ]
    [ addElements
      [ iExtendedExamination
      , iBloodSampleDraw2
      , iBloodSample2
      , iBSFresh2
      ]
      , switchPhase' [iSelectedET]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Extended Histocompatibility\nTesting" sHLATyped2_3 ]
    ]
    sBloodSampleDrawnT

sHLATyped2_1 :: OUSituation
sHLATyped2_1 =
  mkSituation "Donor\nExtended HLA Typed\n from a Stored\nBlood Sample"
   -- [ removeElements
   --   [ iDSStored1
   --   , iBSStored1]
   --   ]
    [ addElements
      [ iBSDisposed1
      , iDNASample2
      , iDSStored2
      , iDSDisposed2
      , iDNAIsolation2
      , iHLATyping2
      , iDonorsTypingResults2
      , iGeneAssignment2
      , iGenotypeValue1
      ]
      , switchPhase' [iSelectedET]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donors Selection\nfor Verification" sSelection ]
    ]
    sHLATyped2T

sHLATyped2_2 :: OUSituation
sHLATyped2_2 =
  mkSituation "Donor\nExtended HLA Typed\nfrom a Stored\nDNA Sample"
    [ addElements
      [ iDSStored1
      , iHLATyping2
      , iDonorsTypingResults2
      , iGeneAssignment2
      , iGenotypeValue1
      , iDSDisposed1
      ]
      , switchPhase' [iSelectedET]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donors Selection\nfor Verification" sSelection ]
    ]
    sHLATyped2T

sHLATyped2_3 :: OUSituation
sHLATyped2_3 =
  mkSituation "Donor\nExtended\nHLA Typed\nfrom a Fresh\nBlood Sample"
    [ removeElements
      [ iBSFresh2
      , iDSFresh2 -- pridal?!
      ]
    , addElements
      [ iBSStored2
      , iBSDisposed2
      , iDNASample2
      , iDSStored2
      , iDSDisposed2
      , iDNAIsolation2
      , iHLATyping2
      , iDonorsTypingResults2
      , iGeneAssignment2
      , iGenotypeValue1
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donors Selection\nfor Verification" sSelection ]
    ]
    sHLATyped2T


sSelection :: OUSituation
sSelection =
  mkSituation "Donors Selected\nfor Verification"
    [ switchPhase' [iSelectedV] ]
    [ OUDisposition Nothing
      [ OUEventB "Donor Activation\nand Blood Sample Draw\nfor Verification Examination" sDraw3 ]
    ]
    sSelectionT

sDraw3 :: OUSituation
sDraw3 =
  mkSituation "Donor Blood Sample\nDrawn for\nVerification Examination"
    [ addElements
      [ iBloodSampleDraw3
      , iBloodSample3
      , iBSFresh3
      , iVerificationExamination
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Histocompatibility Confirmation" sHLAVerified
      , OUEventB "Medical Assessment" sScreened
      ]
    ]
    sBloodSampleDrawnT


sScreened :: OUSituation
sScreened =
  mkSituation "Donor\nHealth Screening\nCompleted"
  [ addElements
    [ iMedicalAssessment2
    , iMedicalAssessmentResults2
    , iInfectionTestingV
    , iInfectionMarkersV
    ]
  ]
  [OUDisposition Nothing
    [ mEventB "Making Donor Verified" sVerified ]
  ]
  sScreenedT

sHLAVerified :: OUSituation
sHLAVerified =
  mkSituation "Donor HLA Verified"
  --[ removeElements
  --  [ iBSFresh3 ]
  [ addElements
    [ iBloodSample3
    , iDNASample3
    , iDNAIsolation3
    , iHLATyping3
    , iDonorsTypingResults3
    , iGeneAssignment3
    , iGenotypeValue1
    , iBSStored3
    , iBSDisposed3
    , iDSStored3
    , iDSDisposed3
     ]
  ]
  [OUDisposition Nothing
    [ mEventB "Making Donor Verified" sVerified ]
  ]
  sHLAVerifiedT

sVerified :: OUSituation
sVerified =
  mkSituation "Donor Verified"
  [ addElements [iDonorVerified]]
  [OUDisposition Nothing
    [ OUEventB "Donor Reservation" sDonorReserved ]
  ]
  ""

sDonorReserved :: OUSituation
sDonorReserved =
  mkSituation "Donor Reserved"
  [ addElements
    [ iDonorReserved
    , iDonorReservation
    ]
  ]
  [OUDisposition Nothing
    [ OUEventB "Donor Selection\nfor Transplantation" sChosen ]
  ]
  ""

sChosen :: OUSituation
sChosen =
  mkSituation "Donor Chosen\nfor Donation"
  [ addElements
    [ iDonorChosen
    , iDonorSelection
    ]
  ]
  [ OUDisposition Nothing
    [ OUEventB "Collection Dates Assessment" sScheduled ]
  ]
  sChosenT

sScheduled :: OUSituation
sScheduled =
  mkSituation "Schedule and Specification\nof Donor Clearance Assigned"
  [ addElements
    [ iSchedule
    , iSpecification
    ]
  ]
  [ OUDisposition Nothing
    [ OUEventB "Work-up" sWorkup ]
  ]
  sScheduledT

sWorkup :: OUSituation
sWorkup =
  mkSituation "Donor Clearance\nProvided"
    [ addElements
      [ iWorkup
      , iFitnessDetermination
      , iDonorCleared
      , iDonorClearance
      , iMedicalAssessment3
      , iMedicalAssessmentResults3
      , iInfectionTestingW
      , iInfectionMarkersW
      , iBloodSampleDraw4
      , iBloodSample1
      ]
    ]
    [ OUDisposition (Just "Donor\nEligible")
      [ OUEventB "Collection Approval\nby the Transplant Centre" sApproved ]
    , OUDisposition (Just "Donor Noneligible\nand another Donor not Available\nand Urgent Medical Need")
      [ OUEventB "Reasoning\nof Collection of Noneligible\nDonor" sApproved ]
    , OUDisposition (Just "Donor\nNoneligible (Temporary)\nand another Donor\nAvailable")
      [ OUEventB "Donor\nBecomes\nTemporary\nUnavailable" sTempDeferred
      , OUEventB "Donor Selection\nfor Transplantation" sChosen ]
    , OUDisposition (Just "Donor\nNoneligible (Permanently)\nand Another Donors\nAvailable")
      [ OUEventB "nDonor\nBecomes\nPermanently\nUnavailable" sPermDeferred
      , OUEventB "Donor Selection\nfor Transplantation" sChosen ]
    ]
    sWorkupT

sApproved :: OUSituation
sApproved =
  mkSituation "Collection of HSC\nApproved"
  [ addElements
    [ iDonorAccepted
    , iDonorClearanceAcc
    , iCollectionReasoning
    , iSchedule
    ]
  ]
  [ OUDisposition (Just "The Donor\nWants to Donate\nBM")
    [ OUEventB "BM\nDonation" sBMCollected ]
  , OUDisposition (Just "The Donor\nWants to Donate\nBlood Cells")
      [ OUEventB "PBSC\nDonation\nPreparation" sPBSCPrep ]
  , OUDisposition (Just "Collection\nnot Performed")
      [ OUEventB "Donor Selection\nfor Transplantation" sChosen ]
  ]
  sApprovedT

sPBSCPrep :: OUSituation
sPBSCPrep =
  mkSituation "PBSC\nDonation\nPrepared"
    [ addElements
      [ iDonation
      , iPBSCDonation
      , iDonorPBSC
      , iPBSCPreparation
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "PBSC\nCollection" sPBSCCollected ]
    ]
    "The PBSC Donation preparation consists of several hormonal injections."

sPBSCCollected :: OUSituation
sPBSCCollected =
  mkSituation "PBSC\nCollected"
    [ addElements
      [ iCollection
      , iPBSCCollection
      , iCollectedPBSC
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "PBSC\nTransportaion" sPBSCDelivered ]
    , OUDisposition Nothing
      [ OUEventB "Donor\nMonitoring" sMonitored ]
    ]
    ""

sPBSCDelivered :: OUSituation
sPBSCDelivered =
  mkSituation "PBSC\nDelivered"
    [ addElements
      [ iTransportPBSC
      , iDeliveredPBSC
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "PBSC\nTransplantation" sPBSC]
    ]
    ""

sPBSC :: OUSituation
sPBSC =
  mkSituation "PBSC\nTransplantation\nCompleted"
    [ addElements
      [ iPBSCPatient
      , iTransplantation
      , iPBSCTransplantation
      , iTransplantedPBSC
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donor\nBecomes\nTemporary\nUnavailable" sTempDeferred
      ]
    ]
    "PBSC is a donation of blood cells, which is performed ambulant. After the donation, the Donor is made Temporary Deferred for the period of one year and is monitored for a period of at least 10 years."

sBMCollected :: OUSituation
sBMCollected =
  mkSituation "BM\nCollected"
    [ addElements
      [ iDonation
      , iDonorBM
      , iBMDonation
      , iCollection
      , iBMCollection
      , iCollectedBM
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "BM\nTransportation" sBMDelivered]
    , OUDisposition Nothing
      [ OUEventB "Donor\nMonitoring" sMonitored ]
    ]
    ""

sBMDelivered :: OUSituation
sBMDelivered =
  mkSituation "BM\nDelivered"
    [ addElements
      [ iTransportBM
      , iDeliveredBM
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "BM\nTransplantation" sBM]
    ]
    ""

sBM :: OUSituation
sBM =
  mkSituation "BM\nTransplantation\nCompleted"
    [ addElements
      [ iBMPatient
      , iTransplantation
      , iBMTransplantation
      , iTransplantedBM
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donor\nBecomes\nTemporary\nUnavailable" sTempDeferred ]
    ]
    "BM Donation is performed in an anesthetic state in a hospital. After the donation, the Donor is made Temporary Deferred for the period of one year and is monitored for a period of at least 10 years."

sScheduled2 :: OUSituation
sScheduled2 =
  mkSituation "Schedule and Specification\nof Donor Clearance\nfor Subsequent Donation\nAssigned"
  [ addElements
    [ iSchedule
    , iSpecification
    ]
  ]
  [ OUDisposition Nothing
    [ OUEventB "Schedule\nand Specification\nof Donor Clearance" sWorkup2 ]
  ]
  ""

sWorkup2 :: OUSituation
sWorkup2 =
  mkSituation "Donor Clearance\nfor Subsequent Donation\nProvided"
    [ addElements
      [ iWorkup
      , iFitnessDetermination
      , iDonorCleared
      , iDonorClearance
      , iMedicalAssessment3
      , iMedicalAssessmentResults3
      , iInfectionTestingW
      , iInfectionMarkersW
      , iBloodSampleDraw4
      , iBloodSample1
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Subsequent\nCollection of HSC\nApproval" sApproved2 ]
    ]
    ""

sApproved2 :: OUSituation
sApproved2 =
  mkSituation "Subsequent Collection\nof HSC Approved"
    [ addElements
      [ iDonorSubsequent
      , iDLIDonation
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Subsequent HSC\nCollection" sDLICollected ]
    ]
    ""

sDLICollected :: OUSituation
sDLICollected =
  mkSituation "Subseqent HSC\nCollected"
    [ addElements
      [ iDonation
      , iDonorSubsequent
      , iDLIDonation
      , iCollection
      , iDLICollection
      , iCollectedDLI
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Subseqent HSC\nTransportation" sDLIDelivered]
    , OUDisposition Nothing
      [ OUEventB "Donor\nMonitoring" sMonitored ]
    ]
    ""

sDLIDelivered :: OUSituation
sDLIDelivered =
  mkSituation "Subseqent HSC\nDelivered"
    [ addElements
      [ iTransportDLI
      , iDeliveredDLI
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Subseqent HSC\nTransplantation" sDLITransplanted ]
    ]
    ""

sDLITransplanted :: OUSituation
sDLITransplanted =
  mkSituation "Subsequent HSC\nTransplantation\nCompleted"
    [ addElements
      [ iDLIPatient
      , iDLITransplantation
      , iTransplantedDLI
      ]
    ]
    [ OUDisposition Nothing
      [ OUEventB "Donor\nBecomes\nTemporary\nUnavailable" sTempDeferred ]
    ]
    ""

sMonitored :: OUSituation
sMonitored =
  mkSituation "Donor\nFollow-up"
    []
    []
    ""

ouModelB = OUModelB
  { oumbSituations =
    [ sInitial
    , sNeeds
      , sAspirantDonor
      , sAvailable
      , sSearchRequested
      , sFound
      , sSelection
      , sDraw2
      , sHLATyped2_1
      , sHLATyped2_2
      , sHLATyped2_3
      , sDraw3
      , sScreened
      , sHLAVerified
      , sVerified
      , sDonorReserved
      , sChosen
      , sScheduled
      , sTempDeferred
      , sPermDeferred
      , sWorkup
      , sApproved
      , sPBSCPrep
      , sPBSCCollected
      , sPBSCDelivered
      , sPBSC
      , sBMCollected
      , sBMDelivered
      , sBM
      , sScheduled2
      , sWorkup2
      , sApproved2
      , sDLICollected
      , sDLIDelivered
      , sDLITransplanted
      , sMonitored
    ]
  }
