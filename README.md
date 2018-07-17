
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
data <- drop_read_rds("congress_tweets/tml1.rds")

## preview data
tibble::as_tibble(data) %>%
  select(user_id:source)
#> # A tibble: 329,089 x 6
#>    user_id  status_id    created_at          screen_name text                             source   
#>    <chr>    <chr>        <dttm>              <chr>       <chr>                            <chr>    
#>  1 3026622… 10123388153… 2018-06-28 14:15:56 RepAmata    Talofa! Here's a quick YouTube … Twitter …
#>  2 3026622… 10123369602… 2018-06-28 14:08:34 RepAmata    Talofa! This week is a busy one… Twitter …
#>  3 3026622… 10123347151… 2018-06-28 13:59:39 RepAmata    Talofa! This week, I spoke on t… Twitter …
#>  4 3026622… 10123338351… 2018-06-28 13:56:09 RepAmata    Talofa! A bill led by Chairman … Twitter …
#>  5 3026622… 10084876886… 2018-06-17 23:12:56 RepAmata    Talofa! Congratulations to @ton… Twitter …
#>  6 3026622… 10000495143… 2018-05-25 16:22:38 RepAmata    Talofa! This week, a bill passe… Twitter …
#>  7 3026622… 99970193558… 2018-05-24 17:21:29 RepAmata    Talofa! My office had the honor… TweetDeck
#>  8 3026622… 99213436009… 2018-05-03 20:10:39 RepAmata    This Asian Pacific American Her… Twitter …
#>  9 3026622… 98844551140… 2018-04-23 15:52:29 RepAmata    Happy birthday to @USArmyReserv… Twitter …
#> 10 3026622… 98667264455… 2018-04-18 18:27:44 RepAmata    I am deeply saddened by the pas… TweetDeck
#> # ... with 329,079 more rows

## number of tweets collected per user
data %>% 
  filter(!is.na(screen_name)) %>%
  group_by(screen_name) %>%
  summarise(n = n()) %>% 
  print(n = 500)
#> # A tibble: 115 x 2
#>     screen_name         n
#>     <chr>           <int>
#>   1 AustinScottGA08  2562
#>   2 BennieGThompson  1430
#>   3 BillPascrell     3072
#>   4 BobbyScott       3197
#>   5 BradSherman      1562
#>   6 Call_Me_Dutch    3116
#>   7 chelliepingree   3198
#>   8 ChrisVanHollen   3200
#>   9 congbillposey    1260
#>  10 CongMikeSimpson   714
#>  11 CongPalazzo      2743
#>  12 CongressmanGT    3194
#>  13 davereichert     3197
#>  14 DorisMatsui      2600
#>  15 DrPhilRoe        2941
#>  16 EleanorNorton    3199
#>  17 FrankPallone     3200
#>  18 gracenapolitano  2171
#>  19 janschakowsky    3200
#>  20 JimPressOffice   1287
#>  21 JudgeTedPoe      3199
#>  22 LamarSmithTX21   3197
#>  23 MacTXPress       1109
#>  24 MarkWarner       3200
#>  25 McCaskillOffice  3200
#>  26 NancyPelosi      3200
#>  27 nikiinthehouse   3192
#>  28 NydiaVelazquez   3200
#>  29 PeterRoskam      3188
#>  30 PeterWelch       2014
#>  31 PeteSessions     3198
#>  32 rep_stevewomack  3196
#>  33 RepAdamSchiff    3199
#>  34 RepAdamSmith     3065
#>  35 RepAdrianSmith   3123
#>  36 RepAmata          346
#>  37 RepBillShuster   3195
#>  38 RepBobbyRush     1835
#>  39 RepDavid         3189
#>  40 RepDavidEPrice   3190
#>  41 repdavidscott    2703
#>  42 RepDennisRoss    3196
#>  43 RepDevinNunes     153
#>  44 repdonyoung      1598
#>  45 RepDWStweets     3197
#>  46 RepEdRoyce       3189
#>  47 RepErikPaulsen   3198
#>  48 RepFredUpton     3192
#>  49 repgregwalden    2263
#>  50 RepHalRogers     3197
#>  51 RepJerryNadler   3200
#>  52 RepJimRenacci    3198
#>  53 RepJoeWilson     1993
#>  54 RepJohnYarmuth   3192
#>  55 RepJoseSerrano   3197
#>  56 RepKevinYoder    3200
#>  57 RepKristiNoem    3198
#>  58 RepLindaSanchez  2847
#>  59 RepMarthaRoby    3200
#>  60 RepMaxineWaters  2662
#>  61 RepMcClintock     383
#>  62 RepMcKinley      3197
#>  63 RepMcNerney      2034
#>  64 RepMikeQuigley   3200
#>  65 RepMikeRogersAL  3199
#>  66 RepMikeTurner    2565
#>  67 RepPaulTonko     3199
#>  68 RepPerlmutter    3193
#>  69 RepPeteOlson     3200
#>  70 RepRichardNeal   1491
#>  71 RepRichmond      2298
#>  72 RepRoybalAllard  3200
#>  73 RepSchrader      2153
#>  74 RepShimkus       3195
#>  75 RepSires         2668
#>  76 RepSpeier        3199
#>  77 RepStevePearce   3200
#>  78 RepSteveStivers  3007
#>  79 RepTerriSewell   3195
#>  80 RepThompson      3197
#>  81 RepTimRyan       3197
#>  82 RepTipton        2789
#>  83 RepTomMarino     2914
#>  84 RepTomReed       2724
#>  85 RepVisclosky     1835
#>  86 RepWalberg       3200
#>  87 RepWebster       3197
#>  88 RepWilson        3197
#>  89 RobWittman       3164
#>  90 RonWyden         3200
#>  91 RosLehtinen      3198
#>  92 Sen_JoeManchin   3198
#>  93 SenatorShaheen   3198
#>  94 SenatorTimScott  3196
#>  95 SenatorTomUdall  3199
#>  96 SenatorWicker    3200
#>  97 SenBillNelson    1351
#>  98 SenGaryPeters    3199
#>  99 SenJackReed      3198
#> 100 SenJeffMerkley   3200
#> 101 SenJohnThune     3198
#> 102 SenJoniErnst     1983
#> 103 SenPatRoberts    3197
#> 104 senrobportman    3200
#> 105 SenRubioPress    3200
#> 106 SenSanders       3199
#> 107 SenShelby        1795
#> 108 SenThomTillis    3196
#> 109 SenToddYoung     3195
#> 110 SenToomey        3197
#> 111 SenWhitehouse    3200
#> 112 SpeakerRyan      3198
#> 113 SteveScalise     3198
#> 114 ToddRokita       3194
#> 115 TomRooney        2961
```
