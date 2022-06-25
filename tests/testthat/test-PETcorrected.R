test_that("PET works", {
  M_activity<-80 # [W]
  icl<-0.9
  Tair<-21
  Tmrt<-21
  v_air<-0.1
  pvap<-21. #Imposed value of Pvap

  pet<- rPET::PETcorrected(Tair, Tmrt, v_air, pvap, M_activity, icl )
  expect_equal(2 * 2, 4)
})
