context("Page count")

test_that("page count is correct", {

  # expect_equal(polish_pages("v.7-9,plates"), 4)
  expect_true(is.na(harmonize_pages("2⁰")))

  expect_equal(polish_pages("[50+] leaves"), 100)

  expect_equal(polish_pages("v. 1-6,plates"), 4)
  expect_equal(polish_pages("3v.([2],x,[16],207,[1],207-862,[6]p.),plates"), 901) # 2+10+16+1+862+6+4
  expect_equal(polish_pages("[1] sheet ([1]+ p.)"), 2) # 1 sheet
  expect_equal(polish_pages("1 sheet (2, [2] p.)"), 2) # = 1 sheet
  expect_equal(polish_pages("2 v. ([26], 400, [24], 192, [2], 167, [1], 203, [13], 144, [4], 152 p.)"), 470) # 26+400+24+2+1+13+4=470
  expect_equal(polish_pages("2v.([4],402p.)"), 406)
  expect_equal(polish_pages("4v.,plates"), 4)  
  # Pitais olla 2 p. [koska 2 p. ilmaistu suluissa]
  expect_equal(polish_pages("1leaf ([2]p.)"), 2)
  # 10+788+4=802
  expect_equal(polish_pages("[10],v-792,4p.,plates"), 806) 
  # 3+560+5+4=572
  expect_equal(polish_pages("[3],xx-579,[5]p.,plates"), 572)
  expect_equal(polish_pages("3"), 3)
  expect_equal(polish_pages("[3]"), 3)
  expect_equal(polish_pages("iii"), 3)
  expect_equal(polish_pages("lxxiip."), 72)
  expect_equal(polish_pages("8,[28],37-88p."), 116) # or 88? 
  expect_equal(polish_pages("[2],1107-1217,[1]p."), 114)
  expect_equal(polish_pages("505-508"), 4)
  expect_equal(polish_pages("1 sheet"), 2)
  expect_equal(polish_pages("1 sheet ([1] page)"), 2)
  expect_equal(polish_pages("1 sheet (2 pages)"), 2)
  expect_equal(polish_pages("1 sheet"), 2)
  expect_equal(polish_pages("2 sheets (versos blank)"), 4)
  expect_equal(polish_pages("2 sheets"), 4)
  expect_equal(polish_pages("1/2 sheet"), 2)
  expect_equal(polish_pages("1 broadside"), 2)
  expect_equal(polish_pages("[2] leaves."), 4)
  expect_equal(polish_pages("48 leaves"), 96)
  expect_equal(polish_pages("vi,7-72p."), 72)
  expect_equal(polish_pages("[2], 58 p."), 60)
  expect_equal(polish_pages("vi,[1],8-67,[1]p."), 68)
  expect_equal(polish_pages("[2], 6 p."), 8)
  expect_equal(polish_pages("[2], 5, [1] p."), 8)
  expect_equal(polish_pages("23,[1]p."), 24)
  expect_equal(polish_pages("[16] p."), 16)
  expect_equal(polish_pages("6p."), 6)
  expect_equal(polish_pages("[2]p."), 2)
  expect_equal(polish_pages("[3],vi-vii,[2],10-70,[2]p"), 70) # ???
  expect_equal(polish_pages("[2],VII,[1],90p."), 100)
  expect_equal(polish_pages("xviii,456,[2]p."), 476)
  expect_equal(polish_pages("2v.([14],242,[6],265-452,[12]p."), 484)
  expect_equal(polish_pages("262p.,plate"), 264)
  expect_equal(polish_pages("127,[1]p.,plates"), 132)
  expect_equal(polish_pages("vi,[1],iv-vii,[27],680,[24]p."), 739)
  expect_equal(polish_pages("Pp.1-190p.,table"), 192)
  expect_equal(polish_pages("24,24p."), 24)
  expect_equal(polish_pages("110,[1],4-60p."), 111)
  expect_equal(polish_pages("72p.,ill."), 72)
  expect_equal(polish_pages("32,[16],33-56p."), 72)

  expect_equal(sum(polish_pages("[4],56,65-176;45,[1]p.")), 226)
  
  expect_equal(polish_pages("[3],vi-vii,[3],98p."), 106)
  expect_equal(polish_pages("vi,9-74p."), 72)
  expect_equal(polish_pages("[4],53,[13]p.,fold.tables"), 74)
  expect_equal(polish_pages("pp. 145-148"), 4)
  expect_equal(polish_pages("pp. [45]-48"), 4)
  expect_equal(polish_pages("p. [2], 129-142"), 16)
  expect_equal(polish_pages("[12], 155-923, [47] p."), 828)
  expect_equal(polish_pages("Ff. [8], 1-209, 209-213, 213-219"), 454) # ???
  expect_equal(polish_pages("122-126 p."), 5)
  expect_equal(polish_pages("p. 66"), 1)
  expect_equal(polish_pages("pp. 49-56"), 8)	       
  expect_equal(polish_pages("v.1(48p.)"), 48)
  expect_equal(polish_pages("Pp.[1],122-169,[1],171*-175*,[2],174-187,[2],190*-195,[1],4,a-m"), 202)
  expect_equal(polish_pages("iii-xxiv, 118, [2] p."), 142)
  expect_equal(polish_pages("[3], 4-8, p. 7, 10-11, 10-13, [1] p."), 17)
  expect_equal(polish_pages("1679 p. in various pagings."), 1679)
  expect_equal(polish_pages("iii-xxiv, 118, [2] p."), 142)
  expect_true(is.na(polish_pages("[fewer than 50 pages]")))
  expect_equal(polish_pages("ca. 298 p."), 298)
  expect_equal(polish_pages("8p. 21cm. (8vo)"), 8)
  expect_equal(polish_pages("[6] pp."), 6)
  expect_equal(polish_pages("1 sheet ([1] p.)"), 2)
  expect_equal(polish_pages("1 sheet ([1]) p."), 2)
  expect_equal(polish_pages("1 sheet ([2] p.)"), 2)
  expect_equal(polish_pages("152,151-573,[1]p.,plates"), 578)
  expect_equal(polish_pages("v.3 (558,[2]p.),plates"), 564)
  expect_equal(polish_pages("v.8 (551,[1]p.),plates"), 556)
  expect_equal(polish_pages("v.2(viii,502p.)"), 510)
  expect_equal(polish_pages("p. 209-[210]"), 2)
  expect_equal(polish_pages("[2],iii,[1],25,[26],50-69,[1];213,[67]p.,plates"), 386)
  expect_equal(polish_pages("[44], 136, 177-296, 313-400, 409-488 p."), 532)
  expect_equal(polish_pages("39, 42-49, [1] p."), 50)
  expect_equal(polish_pages("v.3 (558,[2]p.),plates"), 564)
  expect_equal(polish_pages("v.8 (551,[1]p.),plates"), 556)
  expect_equal(polish_pages("[2],xxi,[9],191,194-205,[5]p.,plate"), 244)
  expect_equal(polish_pages("v.2(viii,502p.)"), 510)
  expect_equal(polish_pages("2v.([2],lx,viii,1650,[50]p.),plates"), 1766)
  expect_equal(polish_pages("[2], 34, 41-48 p."), 50)
  expect_equal(polish_pages("4, 8, 17-48 p."), 48)
  expect_equal(polish_pages("[20], 225, [7], 369-680, 721-1051, [5] p."), 1083)
  expect_equal(polish_pages("[2],16,25-261,[3]p."), 266)
  expect_equal(polish_pages("p. 209-[210]"), 2)
  expect_equal(polish_pages("iv,[1],6-140,[2],40,[2]p."), 149) # or 144 ? if starting romans included in the arab series.   
  expect_equal(polish_pages("[2],45,56-78,[2]p."), 82)
  expect_equal(polish_pages("[10],42,45-88p."), 98)
  expect_equal(polish_pages("[21],vi-xlviii,248,[20]p.,plate,table"), 336)
  expect_equal(polish_pages("2v.([4],x,741,[25],iii,[4],744-853,[1]p.)"), 897)
  expect_equal(polish_pages("v.12 (460,vii,[1],civ p.),plates"), 569)  
  expect_equal(polish_pages("[4],vii-xii,[4],222p.,plates"), 240)
  expect_equal(polish_pages("[14],356,361-408;76,79-216,[14]p.,plates"), 656)
  expect_equal(polish_pages("48,59-66p."), 66)
  expect_equal(polish_pages("[3],vi-xxiv,424p."), 446)
  expect_equal(polish_pages("[2],v-xvii,[1],248p.,plate"), 266)
  expect_equal(polish_pages("[3],vi-vii,[3],110p."), 118)
  expect_equal(polish_pages("[4],231,240-428p.,plate"), 434)
  expect_equal(polish_pages("[2],ii,v-vii,[1],427,[1]p."), 438)
  expect_equal(polish_pages("264,269-723,[3]p.,plates"), 730)
  expect_equal(polish_pages("[2],99,[4],102-110,113-119,[1],119-120,[2];[2],121-246p.,tables"), 261)
  expect_equal(polish_pages("24,[2],25-66p."), 68)
  expect_equal(polish_pages("2v.(xxiv,[1],6-438,[5],446-876p.)"), 906)
  expect_equal(polish_pages("xi,[1],356,[4],357-398,[2],399-466,[2]p.,plates"), 490)
  expect_equal(polish_pages("[2], 34, 41-48 p."), 50)
  expect_equal(polish_pages("[2], 6, 9-12 p."), 14)
  expect_equal(polish_pages("[20], 225, [7], 369-680, 721-1051, [5] p."), 1083)
  expect_equal(polish_pages("4, 253-480 p."), 480)
  expect_equal(polish_pages("v,[3],124,[4],129,[3]p."), 144)
  expect_equal(polish_pages("v,[1],7-18p."), 18)
  expect_equal(polish_pages("1 v. [2 p.]"), 2)
  expect_equal(polish_pages("xxxii [i.e. xxxiii] leaves"), 66) 
  expect_equal(polish_pages("5-49 i.e 5-51]"), 47) 
  expect_equal(polish_pages("12 [i.e. 8 p.]"), 8)
  expect_equal(polish_pages("2 i.e 5"), 5)
  expect_equal(polish_pages("2 p. [i.e. 4 p.]"), 4)
  expect_equal(polish_pages("2 [i.e. 4] leaves"), 8) 
  expect_equal(polish_pages("[16],438[i.e.428],[52]p."), 496)
  expect_equal(polish_pages("[5], 4 [i.e. 6]-8 p."), 8)
  #expect_equal(polish_pages("[2], 169-182 [i.e. 14] p."), 16)
  #expect_true(is.na(polish_pages("v.")))
  expect_equal(polish_pages("v."), 5) 
  expect_true(is.na(polish_pages("1 v")))
  expect_true(is.na(polish_pages("2v")))
  expect_equal(polish_pages("v.8([6],338,[204]),plates"), 552)  
  expect_equal(polish_pages("2-3 i.e 5"), 4) # 2 - 5
  expect_equal(polish_pages("2-3 [ie. 5]"), 4) # 2 - 5
  expect_equal(polish_pages("2-3 [ie 5]"), 4) # 2 - 5
  expect_equal(polish_pages("2-3 [ ie 5]"), 4) # 2 - 5
  expect_equal(polish_pages("2-3 [i.e 5]"), 4) # 2 - 5
  expect_equal(polish_pages("2-3 [i.e. 5]"), 4) # 2 - 5
  expect_equal(polish_pages("xi,[1],309[i.e., 310],[2]p."), 324)
  expect_equal(polish_pages("xvi,3-46,[2]p."), 62)  
  expect_equal(polish_pages("6 p. [i.e.3 leaves] ;"), 6) 
  expect_equal(polish_pages("324[i.e.325]plates"), 650)   
  expect_equal(polish_pages("36 p [1]"), 37)
  expect_equal(polish_pages("25, 27-33, [4] p."), 37)  
  expect_equal(polish_pages("2v.([2],lx,viii,1650,[50]p.),35plates"), 1832)
  expect_equal(polish_pages("2v.([2],xxiv,615,[4],616-1318,[64]p.)"), 1412)
  expect_equal(polish_pages("36p.,fold.plate"), 38)
  expect_equal(polish_pages("4], 18, 130, 149-388, 397-398, 391-394, 403-404, 397-548, 579-580, 551-554, 585-596, 745-979, [1], 980-[981], 981-[982], 982-[983], 983-987, [1], 988-[989], 989-[990], 990-992, 992-1130 p., plates"), 1140)  
  expect_equal(polish_pages("[8],xvi,111,[46],114-314,[6]p.,table"), 392)
  expect_equal(polish_pages("vi,iii-x,[2],346,[2] p."), 360)  
  expect_equal(polish_pages("[8],264,295-342,[4]p."), 354)
  expect_equal(polish_pages("2v.(li,[13],839,[1]p.,tables)"), 908)  
  expect_equal(polish_pages("2 pts in 1 v. (viii, 332, 5, [1] p.)"), 341)  
  expect_equal(polish_pages("36p.,fold.plate"), 38)  
  expect_equal(polish_pages("[10], 554, [5], 556-812, [32] p."), 859)
  expect_equal(polish_pages("6], 104, 109-127, [1] p."), 134)  
  expect_equal(polish_pages("[2] 4 p."), 6)  
  expect_equal(polish_pages("ca. 3108 p. in various pagings"), 3108)
  expect_equal(polish_pages("[4] p. (the last 3 p. blank)"), 4)
  expect_equal(polish_pages("2 Sheets (versos blank)"), 4)  
  expect_equal(polish_pages("[2], 3-58 p."), 58)
  expect_equal(polish_pages("[2], 283-287, [1] p."), 8)
  expect_equal(polish_pages("p. 237 [i.e. 245]-247, [1]"), 4)
  expect_equal(polish_pages("[4], 31, 28-138, [2] p."), 144)
  expect_equal(polish_pages("[2] 6 p."), 8)  
  expect_equal(polish_pages("[3],6-31,[1]p."), 30)
  expect_equal(polish_pages("[2],xxxv,[3],106,[26],286,[30]p.,plates,table"), 388)
  expect_equal(polish_pages("v.4 ([8],418,[30]p.),plate"), 458)
  expect_equal(polish_pages("9,[17]p."), 26)  
  expect_equal(polish_pages("366 p."), 366)
  expect_equal(polish_pages("[4], 31, 28-138, [2] p."), 144)  
  expect_equal(polish_pages("3 sheets (3 pages)"), 6)  
  expect_equal(polish_pages("35,8,9,16p"), 35)
  expect_equal(polish_pages("2v.([6],iii,[1],lxxxviii,994p.)"), 1089)
  expect_equal(polish_pages("[24+} p."), 24)
  expect_equal(polish_pages("2v.(CLI,[1],800p.)"), 952)

  expect_equal(polish_pages("[4] s. (s. [4] tyhjä)"), 4)
  expect_equal(polish_pages("XXIV s."), 24)
  expect_equal(polish_pages("IX + 313 s."), 322)
  expect_equal(polish_pages("9 + 15 s."), 24)
  expect_equal(polish_pages("6 vihkoa (396 s.)"), 396)
  expect_equal(polish_pages("4 nid. (464 s.)"), 464)
  expect_equal(polish_pages("2 osaa (229 s.)"), 229)
  expect_equal(polish_pages("[XXIII], 161 s."), 184)
  
  # expect_equal(polish_pages(""), )
  # expect_equal(polish_pages(""), )

  expect_equal(polish_pages("2 kuvalehteä"), 4)
  expect_equal(polish_pages("2 kuvaliitettä"), 4)
  expect_equal(polish_pages("2 kuvasivua"), 4)
  expect_equal(polish_pages("2 malliliitettä"), 4)
  expect_equal(polish_pages("2 kartblad"), 4)
  expect_equal(polish_pages("2 kuvaa"), 4)
  expect_equal(polish_pages("2 karttaa"), 4)
  expect_equal(polish_pages("2 blad"), 4)
  expect_equal(polish_pages("2 muotokuvalehteä"), 4)
  expect_equal(polish_pages("2 pl."), 4)
  expect_equal(polish_pages("2 blad."), 4)
  expect_equal(polish_pages("2 irtokuval."), 4)
  expect_equal(polish_pages("2 irtokuvalehteä"), 4)
  expect_equal(polish_pages("2 kartor"), 4)
  expect_equal(polish_pages("2 valokuvaa"), 4)
  expect_equal(polish_pages("2 liitelehteä"), 4)
  expect_equal(polish_pages("2 numeroimatonta lehteä"), 4)
  expect_equal(polish_pages("2 silhuetter"), 4)
  expect_equal(polish_pages("2 dubbelsidor"), 4)
  expect_equal(polish_pages("2 taulua"), 4)

  # expect_equal(polish_pages(""), 4)      

})




# Summary of unit testing with testthat
# http://r-pkgs.had.co.nz/tests.html

# Create testthat directory and scripts:
# devtools::use_testthat()

# Test your package with 
# Ctrl/Cmd + Shift + T or devtools::test().

# Test options
#    is_true: Does the expression evaluate to TRUE?
#    is_false: Does the expression evaluate to FALSE?
#    is_a: Did the object inherit from a specified class?
#    equals: Is the expression equal within numerical tolerance to your expected value?
#    is_equivalent_to: Is the object equal up to attributes to your expected value?
#    is_identical_to: Is the object exactly equal to your expected value?
#    matches: Does a string match the specified regular expression?
#    prints_text: Does the text that’s printed match the specified regular expression?
#    throws_error: Does the expression raise an error?
#    takes_less_than: Does the expression take less than a specified number of seconds to run?

#test_that("floor_date works for different units", {
#  base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
#  expect_equal(floor_date(base, "second"), 
#    as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))
#  expect_equal(floor_date(base, "minute"), 
#}})
