
<!-- README.md is generated from README.Rmd. Please edit that file -->

# congress\_tweets

Collecting tweets posted by members of the United States Congress.

## Data

Data available in shared `congress_tweets` Dropbox folder

``` r
##
link <- rdrop2::drop_share("congress_tweets")
link$url
```

Read and preview the data

``` r
## read data
data <- readRDS("~/Dropbox/congress_tweets/tml.rds")

## preview data
data %>%
  arrange(desc(created_at)) %>%
  select(created_at:text) %>%
  mutate(text = gsub("\n", " ", text)) %>%
  head(10)
```

<div class="kable-table">

| created\_at         | screen\_name    | text                                                                                                                                                                                                                                                                                                               |
| :------------------ | :-------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 2018-07-17 20:11:46 | CongressmanRaja | I’m calling for an investigation into FBI leaks to Rudy Giuliani during the 2016 election cycle and in the year and a half since. We need answers on the leaks to Giuliani in 2016 and whether they’ve continued as he represents the President as his lawyer. <https://t.co/5hsADwcHEO> <https://t.co/yWoO74pgFn> |
| 2018-07-17 20:07:03 | RepKihuen       | I was honored to address and take questions from members of the Las Vegas Chamber of Commerce who are visiting Washington D.C. We discussed Yucca Mountain, I-11 corridor, marijuana banking, immigration reform and other topics affecting Nevadans. Thanks for having me, @LVChamber\! <https://t.co/KcxWWpmJAE> |
| 2018-07-17 20:05:10 | RepJackyRosen   | Tourism is the foundation of Nevada’s economy. Organizations like the @LVCVA ensure that our area continues to attract millions of visitors per year. Thank you for all you do\! <https://t.co/S9wRsHuIZf>                                                                                                         |
| 2018-07-17 20:01:13 | RepValDemings   | Today, I met with the second place congressional art competition winner, Cameron Lombardi and her family. Her beautiful artwork hangs in my D.C. office. <https://t.co/ZyTEsyUXRF>                                                                                                                                 |
| 2018-07-17 20:00:44 | RepJayapal      | Rep. Jayapal is not letting up re: conflicts of interest. \#techhearings                                                                                                                                                                                                                                           |
| 2018-07-17 19:59:14 | SenatorHassan   | There is no doubt that @realDonaldTrump meant it when he said “I hold both countries responsible” for Russia attacking our democracy. If you think otherwise, I have a bridge to sell you in Moscow.                                                                                                               |
| 2018-07-17 19:58:58 | jontester       | Did you know it’s \#WorldEmojiDay? I’m sure you could all guess my favorite one: 🤙 \#mtpol                                                                                                                                                                                                                         |
| 2018-07-17 19:56:17 | RepDonBeyer     | .@SenateDems asked for a public Judiciary Committee hearing on Trump’s cruel and immoral family separations with Administration witnesses testifying under oath. Instead, today we received a short private briefing that was closed to the press and public.                                                      |
| 2018-07-17 19:51:00 | RepMiaLove      | ICYMI: “Bipartisan collaboration is needed to both protect Utah’s natural resources, ensuring the viability of these sports, and promote the state’s most valuable assets beyond its borders.” \#utpol @DougOwensUtah <https://t.co/8Xhs9pohLc>                                                                    |
| 2018-07-17 19:50:42 | RepCarbajal     | Today, I joined @RepLujanGrisham and 61 of my colleagues in introducing the \#AntiquitiesActof2018, which enhances protections for our national monuments, ensuring they will remain a source of recreation and good jobs for generations to come.                                                                 |

</div>

View number of tweets collected per account

``` r
## number of tweets collected per user
data %>% 
  group_by(screen_name) %>%
  summarise(n = n())
```

<div class="kable-table">

| screen\_name     |    n |
| :--------------- | ---: |
| aguilarpete      |  726 |
| amasharchive     | 3049 |
| auctnr1          | 3199 |
| AustinScottGA08  | 2562 |
| BennieGThompson  | 1430 |
| BettyMcCollum04  | 3200 |
| BillPascrell     | 3072 |
| BlumenauerMedia  |  352 |
| BobbyScott       | 3197 |
| boblatta         | 3188 |
| BradSherman      | 1562 |
| bradwenstrup     | 1477 |
| Call\_Me\_Dutch  | 3116 |
| cathymcmorris    | 3198 |
| chelliepingree   | 3198 |
| ChrisVanHollen   | 3200 |
| ChuckGrassley    | 3200 |
| chuckschumer     |  375 |
| Clyburn          | 2061 |
| coffmanforco     |  515 |
| ConawayTX11      |  880 |
| congbillposey    | 1260 |
| CongBoyle        | 2695 |
| CongMikeSimpson  |  714 |
| CongPalazzo      | 2743 |
| CongressmanGT    | 3194 |
| CongressmanHice  | 2168 |
| CongressmanRaja  | 2790 |
| CongressmanRuiz  | 1019 |
| dandonovan\_ny   | 1079 |
| DarrellIssa      | 3194 |
| daveloebsack     | 1008 |
| davereichert     | 3197 |
| DesJarlaisTN04   | 3200 |
| dinatitus        | 3190 |
| DonaldNorcross   | 3181 |
| DorisMatsui      | 2600 |
| DrNealDunnFL2    |  895 |
| DrPhilRoe        | 2941 |
| EleanorNorton    | 3199 |
| FrankPallone     | 3200 |
| GerryConnolly    | 3195 |
| GKButterfield    | 3193 |
| gracenapolitano  | 2171 |
| GrahamBlog       | 3197 |
| GreggHarper      | 3190 |
| HerreraBeutler   |  836 |
| HurdOnTheHill    | 3197 |
| InhofePress      | 3194 |
| JacksonLeeTX18   | 3199 |
| jahimes          | 3186 |
| janschakowsky    | 3200 |
| jaredpolis       | 3193 |
| JeffFlake        | 2612 |
| JeffFortenberry  | 3197 |
| JerryMoran       | 3194 |
| Jim\_Jordan      | 2569 |
| JimLangevin      | 3200 |
| JimPressOffice   | 1287 |
| JoaquinCastrotx  | 3184 |
| JohnBoozman      | 3200 |
| JohnCornyn       | 3195 |
| johnculberson    | 3199 |
| jontester        | 2712 |
| JudgeCarter      | 3196 |
| JudgeTedPoe      | 3199 |
| JuliaBrownley26  | 3200 |
| keithellison     | 3196 |
| KeithRothfus     | 3192 |
| KenCalvert       | 3198 |
| kevinomccarthy   |  886 |
| kyrstensinema    | 3195 |
| LamarSmithTX21   | 3197 |
| larsenrick       | 2553 |
| lisamurkowski    | 3197 |
| louforsenate     | 1280 |
| MacTXPress       | 1109 |
| MarioDB          | 3196 |
| MarkAmodeiNV2    | 1186 |
| MarkWarner       | 3200 |
| MarshaBlackburn  | 3194 |
| MartinHeinrich   | 3200 |
| maziehirono      | 3198 |
| McCaskillOffice  | 3200 |
| McConnellPress   | 3200 |
| michaelcburgess  | 3199 |
| MikeCrapo        | 3199 |
| MikeKellyPA      | 2290 |
| NancyPelosi      | 3200 |
| nikiinthehouse   | 3192 |
| NitaLowey        | 3199 |
| NormaJTorres     | 3186 |
| NydiaVelazquez   | 3200 |
| PatrickMcHenry   | 3199 |
| PattyMurray      | 3197 |
| PeterRoskam      | 3188 |
| PeterWelch       | 2014 |
| PeteSessions     | 3198 |
| Raul\_Labrador   | 2173 |
| Rep\_Hunter      |  554 |
| rep\_stevewomack | 3196 |
| RepAbraham       |  671 |
| RepAdams         | 3200 |
| RepAdamSchiff    | 3199 |
| RepAdamSmith     | 3065 |
| RepAdrianSmith   | 3123 |
| RepAlexMooney    | 1540 |
| RepAlGreen       | 1594 |
| RepAlLawsonJr    |  710 |
| RepAmata         |  346 |
| RepAndreCarson   | 3196 |
| RepAndyBarr      | 3114 |
| RepAndyBiggsAZ   | 3197 |
| RepAndyHarrisMD  | 2950 |
| RepAnnaEshoo     | 2175 |
| RepAnnieKuster   | 3198 |
| RepAnnWagner     | 3163 |
| RepAnthonyBrown  | 1721 |
| RepArrington     | 1086 |
| RepBarbaraLee    | 3200 |
| RepBarragan      | 2396 |
| RepBeatty        | 3200 |
| repbenraylujan   | 1840 |
| RepBera          | 3188 |
| RepBetoORourke   | 3182 |
| RepBillFlores    | 3193 |
| RepBillFoster    | 2752 |
| RepBillJohnson   | 3196 |
| RepBillShuster   | 3195 |
| RepBlaine        | 1770 |
| RepBobbyRush     | 1835 |
| RepBobGibbs      | 3011 |
| RepBonamici      | 3050 |
| RepBonnie        | 3195 |
| RepBost          | 1406 |
| RepBrady         |  740 |
| RepBrianBabin    | 2461 |
| RepBrianFitz     |  777 |
| RepBrianHiggins  | 3199 |
| RepBrianMast     | 1092 |
| RepBuddyCarter   | 1533 |
| RepByrne         | 3198 |
| RepCarbajal      | 1101 |
| RepCardenas      | 3191 |
| RepCartwright    | 1936 |
| RepCharlieCrist  | 1672 |
| RepCheri         | 3197 |
| RepChrisCollins  | 3200 |
| RepChrisSmith    | 1062 |
| RepChrisStewart  | 2898 |
| RepChuck         | 1957 |
| RepCicilline     | 3197 |
| RepClayHiggins   |  706 |
| repcleaver       | 3199 |
| RepCohen         | 3192 |
| RepComstock      | 3200 |
| RepCuellar       | 3192 |
| RepCummings      | 3199 |
| RepCurbelo       | 3200 |
| RepDanKildee     | 3195 |
| RepDannyDavis    | 1487 |
| RepDarrenSoto    | 1909 |
| RepDaveBrat      | 1665 |
| RepDaveJoyce     | 3196 |
| repdavetrott     |  559 |
| RepDavid         | 3189 |
| RepDavidEPrice   | 3190 |
| RepDavidKustoff  |  685 |
| RepDavidRouzer   |  705 |
| repdavidscott    | 2703 |
| RepDavidValadao  | 1018 |
| RepDavidYoung    | 3199 |
| RepDebDingell    | 2328 |
| RepDelBene       | 3200 |
| RepDennisRoss    | 3196 |
| RepDennyHeck     | 3198 |
| RepDerekKilmer   | 2065 |
| RepDeSantis      | 2422 |
| RepDeSaulnier    | 2484 |
| RepDevinNunes    |  153 |
| RepDianaDeGette  | 3198 |
| RepDianeBlack    | 3195 |
| RepDLamborn      | 3199 |
| RepDonaldPayne   | 3197 |
| RepDonBacon      | 3191 |
| RepDonBeyer      | 3198 |
| repdonyoung      | 1598 |
| RepDougCollins   | 3192 |
| RepDrewFerguson  | 1211 |
| RepDwightEvans   | 3195 |
| RepDWStweets     | 3197 |
| RepEBJ           | 3185 |
| RepEdRoyce       | 3189 |
| RepEliotEngel    | 3193 |
| RepErikPaulsen   | 3198 |
| RepEspaillat     | 3200 |
| RepEsty          | 3199 |
| RepEvanJenkins   | 2774 |
| RepFilemonVela   | 1327 |
| RepFrankLucas    |  665 |
| RepFredUpton     | 3192 |
| RepFrenchHill    | 1751 |
| RepGallagher     | 1533 |
| RepGaramendi     | 3196 |
| RepGarretGraves  | 3023 |
| RepGeneGreen     | 2764 |
| RepGonzalez      |  697 |
| RepGoodlatte     | 3199 |
| RepGosar         | 3188 |
| RepGraceMeng     | 3200 |
| RepGregoryMeeks  | 3196 |
| repgregwalden    | 2263 |
| RepGrothman      | 1328 |
| RepGusBilirakis  | 3190 |
| RepGuthrie       |  764 |
| RepGutierrez     | 3199 |
| RepGwenMoore     | 3199 |
| RepHalRogers     | 3197 |
| RepHanabusa      | 2232 |
| RepHankJohnson   | 3188 |
| RepHartzler      | 3197 |
| RepHastingsFL    | 1162 |
| RepHensarling    | 1067 |
| RepHolding       |  698 |
| RepHuffman       | 2150 |
| RepHuizenga      | 3192 |
| RepHultgren      | 3200 |
| RepJackBergman   |  753 |
| RepJackyRosen    | 2249 |
| RepJasonLewis    |  890 |
| RepJasonSmith    | 2906 |
| RepJayapal       | 3193 |
| RepJeffDenham    | 2832 |
| RepJeffDuncan    | 3191 |
| RepJeffries      | 2906 |
| RepJenniffer     | 3163 |
| RepJerryNadler   | 3200 |
| RepJimBanks      | 2414 |
| repjimcooper     | 2711 |
| RepJimCosta      | 1862 |
| RepJimmyGomez    | 1417 |
| RepJimmyPanetta  | 1355 |
| RepJimRenacci    | 3198 |
| RepJoeBarton     | 2078 |
| RepJoeCourtney   | 3200 |
| repjoecrowley    | 3199 |
| RepJoeKennedy    | 3195 |
| RepJoeWilson     | 1993 |
| RepJohnDelaney   | 3195 |
| RepJohnDuncanJr  |  701 |
| RepJohnFaso      |  866 |
| RepJohnKatko     |  710 |
| RepJohnLarson    | 2976 |
| repjohnlewis     | 1875 |
| RepJohnYarmuth   | 3192 |
| RepJoseSerrano   | 3197 |
| RepJoshG         | 1844 |
| RepJuanVargas    | 1884 |
| RepJudyChu       | 3195 |
| RepKarenBass     | 3188 |
| RepKathleenRice  | 2345 |
| RepKayGranger    | 1658 |
| RepKClark        | 3200 |
| RepKenBuck       | 1300 |
| RepKenMarchant   | 3200 |
| RepKevinBrady    | 3199 |
| RepKevinCramer   | 2946 |
| RepKevinYoder    | 3200 |
| RepKihuen        | 2995 |
| RepKinzinger     | 3200 |
| RepKristiNoem    | 3198 |
| RepLaHood        | 1067 |
| RepLaMalfa       |  543 |
| RepLanceNJ7      | 1798 |
| RepLarryBucshon  | 3191 |
| RepLawrence      | 3200 |
| RepLBR           |  560 |
| RepLeeZeldin     | 1595 |
| RepLindaSanchez  | 2847 |
| RepLipinski      | 2320 |
| RepLizCheney     |  241 |
| RepLloydDoggett  | 3198 |
| RepLoBiondo      | 3199 |
| RepLoisFrankel   | 3200 |
| RepLouCorrea     |  389 |
| RepLoudermilk    | 1856 |
| replouiegohmert  | 3199 |
| RepLowenthal     | 3197 |
| RepLujanGrisham  | 3197 |
| RepLukeMesser    | 2113 |
| RepLynnJenkins   | 3196 |
| RepMaloney       | 3200 |
| RepMarciaFudge   | 3196 |
| RepMarcyKaptur   | 3199 |
| RepMarkMeadows   | 1129 |
| repmarkpocan     | 3186 |
| RepMarkTakano    | 2605 |
| RepMarkWalker    | 3188 |
| RepMarthaRoby    | 3200 |
| RepMattGaetz     | 2051 |
| RepMaxineWaters  | 2662 |
| RepMcCaul        | 3196 |
| RepMcClintock    |  383 |
| RepMcEachin      | 2399 |
| RepMcGovern      | 3200 |
| RepMcKinley      | 3197 |
| RepMcNerney      | 2034 |
| RepMcSally       | 2796 |
| RepMGriffith     | 1204 |
| RepMiaLove       | 1477 |
| RepMikeBishop    | 2012 |
| RepMikeCapuano   |  510 |
| RepMikeJohnson   | 1110 |
| RepMikeQuigley   | 3200 |
| RepMikeRogersAL  | 3199 |
| RepMikeTurner    | 2565 |
| RepMimiWalters   | 3196 |
| RepMoBrooks      | 1716 |
| RepMoolenaar     |  789 |
| RepMullin        | 2601 |
| RepNewhouse      | 1200 |
| RepOHalleran     | 2148 |
| RepPaulCook      | 1531 |
| RepPaulMitchell  |  852 |
| RepPaulTonko     | 3199 |
| RepPerlmutter    | 3193 |
| RepPeteKing      | 2551 |
| RepPeteOlson     | 3200 |
| RepPeterDeFazio  | 1171 |
| RepPittenger     | 3200 |
| RepPoliquin      | 1540 |
| RepRalphNorman   |  817 |
| RepRaskin        | 3195 |
| RepRatcliffe     | 2187 |
| RepRaulGrijalva  | 3200 |
| RepRichardNeal   | 1491 |
| RepRichHudson    | 3199 |
| RepRichmond      | 2298 |
| RepRickAllen     | 1338 |
| RepRickCrawford  | 3196 |
| RepRobinKelly    | 3194 |
| RepRobWoodall    |  720 |
| RepRodBlum       | 2408 |
| RepRohrabacher   |  237 |
| RepRoKhanna      | 3200 |
| RepRonEstes      |  395 |
| RepRonKind       | 1260 |
| RepRooney        |  779 |
| RepRoybalAllard  | 3200 |
| RepRubenGallego  | 3195 |
| RepRussell       |  489 |
| RepRutherfordFL  |  638 |
| RepRWilliams     | 3198 |
| RepRyanCostello  | 1551 |
| RepSamGraves     | 3196 |
| repsandylevin    | 3181 |
| RepSanfordSC     | 2380 |
| RepSarbanes      | 1287 |
| RepSchneider     | 3017 |
| RepSchrader      | 2153 |
| RepScottPerry    | 3089 |
| RepScottPeters   | 3200 |
| RepScottTaylor   |  157 |
| RepSeanDuffy     | 3200 |
| RepSeanMaloney   | 3193 |
| RepSheaPorter    | 1283 |
| RepShimkus       | 3195 |
| RepSires         | 2668 |
| RepSmucker       | 3184 |
| RepSpeier        | 3199 |
| RepStefanik      | 3197 |
| RepStephenLynch  | 3198 |
| RepStephMurphy   | 1692 |
| RepSteveChabot   | 3190 |
| RepStevePearce   | 3200 |
| RepSteveStivers  | 3007 |
| RepSusanDavis    | 1989 |
| RepSwalwell      | 3199 |
| RepTedBudd       |  927 |
| RepTedDeutch     | 3199 |
| RepTedLieu       | 3184 |
| RepTedYoho       | 1059 |
| RepTenney        | 1518 |
| RepTerriSewell   | 3195 |
| RepThomasMassie  | 3040 |
| RepThompson      | 3197 |
| RepTimRyan       | 3197 |
| RepTimWalz       | 3200 |
| RepTipton        | 2789 |
| RepTomEmmer      | 3198 |
| RepTomGarrett    | 1318 |
| RepTomGraves     | 3196 |
| RepTomMacArthur  | 1987 |
| RepTomMarino     | 2914 |
| RepTomReed       | 2724 |
| RepTomRice       | 1307 |
| RepTomSuozzi     |  572 |
| RepTrentKelly    | 1037 |
| RepTrey          |  338 |
| RepValDemings    | 2485 |
| RepVeasey        | 3087 |
| RepVisclosky     | 1835 |
| RepWalberg       | 3200 |
| RepWalorski      | 3198 |
| RepWalterJones   | 2424 |
| RepWebster       | 3197 |
| RepWesterman     | 3192 |
| RepWilson        | 3197 |
| RepYvetteClarke  | 3192 |
| RepZoeLofgren    | 2154 |
| Robert\_Aderholt | 1487 |
| RobWittman       | 3164 |
| RodneyDavis      | 3199 |
| RonWyden         | 3200 |
| rosadelauro      | 3198 |
| RosLehtinen      | 3198 |
| RoyBlunt         | 3200 |
| SamsPressShop    | 1198 |
| SanfordBishop    | 1805 |
| Sen\_JoeManchin  | 3198 |
| SenAlexander     | 3199 |
| SenAngusKing     | 3198 |
| SenatorBurr      | 3198 |
| SenatorCantwell  | 3200 |
| SenatorCardin    | 3197 |
| SenatorCarper    | 3199 |
| SenatorCollins   | 2774 |
| SenatorDurbin    | 3200 |
| SenatorEnzi      | 3199 |
| SenatorFischer   | 3200 |
| SenatorHassan    | 3199 |
| SenatorHeitkamp  | 3199 |
| SenatorIsakson   | 3196 |
| SenatorLankford  | 3200 |
| SenatorLeahy     | 3195 |
| SenatorMenendez  | 3199 |
| SenatorRisch     | 1269 |
| SenatorRounds    | 2486 |
| SenatorShaheen   | 3198 |
| SenatorTimScott  | 3196 |
| SenatorTomUdall  | 3199 |
| SenatorWicker    | 3200 |
| SenBennetCO      | 3040 |
| SenBillNelson    | 1351 |
| SenBlumenthal    | 3200 |
| SenBobCasey      | 3200 |
| SenBobCorker     | 3199 |
| SenBooker        | 3196 |
| SenBrianSchatz   | 1452 |
| SenCapito        | 3200 |
| SenCoonsOffice   | 2192 |
| SenCortezMasto   | 3200 |
| SenCoryGardner   | 3200 |
| SenDanSullivan   | 2000 |
| sendavidperdue   | 3199 |
| SenDeanHeller    | 3196 |
| SenDonnelly      | 3200 |
| SenDuckworth     | 3199 |
| SenFeinstein     | 3200 |
| SenGaryPeters    | 3199 |
| SenGillibrand    | 3195 |
| SenJackReed      | 3198 |
| SenJeffMerkley   | 3200 |
| SenJohnBarrasso  | 3200 |
| SenJohnHoeven    | 2775 |
| SenJohnKennedy   | 1003 |
| SenJohnMcCain    | 3200 |
| SenJohnThune     | 3198 |
| SenJoniErnst     | 1983 |
| SenKamalaHarris  | 2170 |
| SenMarkey        | 3200 |
| SenMikeLee       | 3198 |
| SenMurphyOffice  |  995 |
| senorrinhatch    | 3200 |
| SenPatRoberts    | 3197 |
| senrobportman    | 3200 |
| SenRonJohnson    | 3197 |
| SenRubioPress    | 3200 |
| SenSanders       | 3199 |
| SenSasse         |  885 |
| SenShelby        | 1795 |
| SenSherrodBrown  | 3200 |
| SenThomTillis    | 3196 |
| SenToddYoung     | 3195 |
| SenTomCotton     | 3200 |
| SenToomey        | 3197 |
| SenWarren        | 3200 |
| SenWhitehouse    | 3200 |
| SpeakerRyan      | 3198 |
| stabenow         | 1868 |
| StaceyPlaskett   |  922 |
| SteveDaines      | 3199 |
| SteveKingIA      | 2345 |
| SteveKnight25    | 1031 |
| SteveScalise     | 3198 |
| SusanWBrooks     | 3198 |
| tammybaldwin     | 3195 |
| teammoulton      | 3186 |
| tedcruz          | 3186 |
| TGowdySC         | 1886 |
| ToddRokita       | 3194 |
| TomColeOK04      | 1248 |
| TomRooney        | 2961 |
| TulsiPress       | 3198 |
| TXRandy14        | 2418 |
| USRepGaryPalmer  | 1846 |
| USRepKCastor     | 3194 |
| USRepKeating     | 1298 |
| USRepMikeDoyle   | 1207 |
| USRepRickNolan   | 2804 |
| USRepRodney      | 1138 |
| VernBuchanan     | 2430 |
| virginiafoxx     | 3199 |
| WarrenDavidson   | 1800 |
| WhipHoyer        | 3200 |

</div>
