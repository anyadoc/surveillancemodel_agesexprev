extensions [gis
  ]
globals
[
  forest-map
  hr                                             ;; home-range
  hrng                                           ;; home-range number generator
  tnhr                                           ;; total number of home-ranges in the model landscape
  cds                                            ;; counter for deer setup
  d                                              ;; counter that resets every 12 months
  cps                                            ;; counter for patch setup
  afd                                            ;; adult female deer on the patch counter
  xx                                             ;; counter for displaced female deer during initial setup
  ;pp
  mad                                            ;; male adult deer
  fad                                            ;; female adult deer
  myd                                            ;; male yearling deer
  fyd                                            ;; female yearling deer
  mfd                                            ;; male fawn deer
  ffd                                            ;; female fawn deer
  ;mf6nhm                                        ;; male fawn non-hunting mortality upto 6 months
  ;ff6nhm                                        ;; female fawn non-hunting mortality upto 6 months
  ;mf12nhm                                       ;; male fawn non-hunting mortality 6-12 months
  ;ff12nhm                                       ;; female fawn non-hunting mortality 6-12 months
  ;mynhm                                         ;; male yearling non-hunting mortality
  ;fynhm                                         ;; female yearling non-hunting mortality
  ;manhm                                         ;; male adult non-hunting mortality
  ;fanhm                                         ;; female adult non-hunting mortality
 ;mf6hm,ff6hm                                    ;; male/female fawn upto 6 mo huntinh mortality =00000
  ;mf12hm                                        ;; male fawn hunting mortality 6-12 months
  ;ff12hm                                        ;; female fawn hunting mortality 6-12 months
  ;myhm                                          ;; male yearling humting mortality
  ;fyhm                                          ;; female yearling hunting mortality
  ;mahm                                          ;; male adult mortality hunting
  ;fahm                                          ;; female adult mortality hunting

  mf
  my
  ma
  ff
  fy
  fa
  fhr
  dc
  dxp                                           ;; xcord prior to disp movement
  dxn                                           ;; ycord after disp movement
  dyp
  dyn
  counter
  fhp                                            ;; finalize home-patch after dispersal
  ddohrn                                         ;; dispersing deer original hrn
  mdd                                            ;; mean dispersal distance
  sddd                                           ;; std dev dispersal distance
  dd                                             ;; predicted dispersal distance from log-normal pdf
  lv
  lm
  ls                                             ;; parameters for calculating log-normal pdf of dd
  ndd                                            ;; number of deer dispersing out of the landscape
  nhrn                                           ;; new home range number
  vv
  year
  td
  phn
  ncwd                                           ;; to set no of cwd deer every year
  mcwd
  mycwd                                         ;;22Jan16
  mfcwd                                         ;;22Jan16
  fcwd
  fycwd                                         ;;22Jan16
  ffcwd                                         ;;22Jan16
  dcwdm
  dcwdmy                                        ;;22Jan16
  dcwdmf                                        ;;22Jan16
  dcwdf
  dcwdfy                                        ;;22Jan16
  dcwdff                                        ;;22Jan16
  tcwdm
  tcwdf                                         ;;tested cwd male/female
  hcwd
  tcwd
  totcwdd
  pdcwd                                         ;; probability of detecting CWD
  vals1                                          ;; for excel output
  vals2
  vals3
  vals
  op                                            ;;observed prevalence
  tamh                                          ;; tot adult male deer hunted
  tymh                                          ;;22Jan16
  tafh
  tyfh                                          ;;22Jan16
  tamt                                          ;; tot adult male deer tested
  taft
  peramht                                       ;; percent adult male harvest tested
  perafht
  dcp                                           ;; to calculate number of patches where CWD is clustered
  ndcp
  nadh                                          ;; number of adult deer hunted-------------------------------------new global for male-biased sampling model
  nadt                                          ;; number of adult deer tested-------------------------------------
  pertestm
  pertestf
  pidc
  piss1
  piss2
  piss
  sse                                            ;; sampling strategy efficiency
  ano                                            ;; for inf process---11mar15
  ano2
  ;;=================================================================================added on 23July15
  mom                                            ;; mom's who
  cdf                                            ;; counter for female deer in the landscape
  cdl                                            ;; counter for deer leaders
  mgs                                            ;; mean group size
  ;;=================================================================================added on 27July15
  ;c3
  tgs                                            ;;transfer post-harvest group size
  ndag                                           ;; number of deer available for grouping
  tgroid
  gs
  zz
  zz1      ;; temporary counter to be deleted
  zz2      ;; temp counter to count deers with sex = 2 and aim > 18 < 29 (while transferring group leadership) 16 Sep 15
  ttgroid  ;; temp counter for transfer of leadership process when leader dies/ is hunted 7 Sep 15
  ;;===============================
  twho     ;; transfer who for new code in to die 20 Sep 15
  counter1
  tmomid
  tgr
  ;;===============================22Nov15
  oldm                ;;%old males (above 229 when d = 1) of total adult males
  oldf                ;;old females (above 229 when d = 1) of total adult females
  ;;================================2Dec15
  osg                 ;;oversized group
  osg1                ;;counter
  sd                  ;;solitary females
  sd1                 ;;counter
  ;;==============================21Dec15
  ttmomid             ;;for identifying adult siblings
  ttaim               ;; same
  ;;===============================29Dec15
  tmbg                ;; total male bachelor groups at d=1
  mtgs                 ;; male bachelor group size
  tmgroid             ;;
  cgs                 ;;
  cgs1                ;;24May16
  potcand             ;;
  ;;==============================14Jan16
  tnm                 ;; to transfer the nm value
  ter                 ;; mating area (more for young deers)
  infm
  inff
  avmates
  tinff
  ;===============================11Feb16
  cwdpd
  grfis               ;; group fission 10Jun16
  tttgroid            ;;16Jun16 for testing
  ngr                 ;;30Jul16
  posthpop
  tot_harvest
  tmfh
  tffh
  tmyh
  tfyh
  ;------------9Nov16
  tdd
  ;-----------21Nov16
  hpp               ;;hunter preferred patches-proportion
  dhhhpp             ;;counter for deer hunted on high hunting prob patch
  dhlhpp            ;;counter for deer hunted on low hunting prob patch
  ;global variables from MOOvPOP
  Region
  rregion
  Recommended_parameter_values
  PostHarvestDensity
  sexratio
  adultprop
  yearlingprop
  breeding-prop-female-fawns
  doe-group-size-regulator
  min%ForestCover
  max%ForestCover
  mean-bachelor-group-size
  yearling-male-dispersal-rate
  yearling-female-dispersal-rate
  mean-female-dispersal-distance
  stddev-dispersal-distance
  tp                                   ;3Aug17 removed from interface and added here
  npd
  %CWDLandscape
  pertest
  permah
  perfah
  ;----3 Aaug 17

  ini-inf-male
  ini-inf-female
  ;4 August 2017
  fawn-deers
  yearling-deers
  adult-deers
  n-inf-fawns
  n-inf-yearlings
  n-inf-adults
  ]

patches-own
[
  forest-percent
  dh                                            ;; home-range number
  do                                             ;; deer occupancy 0/1male/2female)
  border                                         ;; identifying border patches
  dfp
  dcl       ;21Jan16 inactivated                 ;; patches where CWD is clustered:::PATCH STATUS 1: CWDclusterpatch;0: noncluster patch; 3 patch with 1 or more cwd+ deer; 4 patch where deer/s tested and found +; 2 buffer patch
  ;-------------9Nov16
  add
]
breed [deers deer]
deers-own
[
  sex
  aim                                            ;;age in months
  cwd
  cwdm                                           ;; when a deer will die post cwd infection
  cwdi                                           ;; when a deer will become infectious post cwd inf
  cwdc                                           ;; when a deer will show clinical signs post cwd inf
  cwdpr                                          ;; cwd progression in months
  momid                                          ;; mother's who number
  gl                                             ;; 1 group leader; 0 follower
  ;;========================added on 23July15
  groid
  pgr                                            ;; 12Jan16 potential group size
  gr                                             ;; basically a leader attribute-group size excluding the leader (0 to n); if -1 the deer (gl = 0) is in a group; -2 if the deer is not in a group
  ;============================29Dec15
  ml                                             ;; male bachelor group leader ml = 1 otherwise 0
  mgroid                                         ;; male bachelor group -2 at birth, -1 after dispersal and = who of leader when join group as adults
  ;============================14Jan16
  nm                                             ;; max number of matings allowed during rut (between 1 to 3 for female deer)
  anm                                            ;; counter for actual number of matings
  mgr
  ]

;*************************************************************************************************************************

to setup
  clear-all
  setup-landscape
  let tot_deer count deers
  let noshow_deer round (.9 * tot_deer)
  ask n-of noshow_deer deers [ht]
  if (file-exists? (word "c:/Users/" machine "/Desktop/CWDsurveillanceMO.csv") = FALSE)[;SUBSTITUTE APPROPRIATE PATH (C:/Documents and Settings/userName/Desktop/CWDsurveillanceMO.csv)
    file-open (word "c:/Users/" machine "/Desktop/CWDsurveillanceMO.csv");SUBSTITUTE APPROPRIATE PATH (C:/Documents and Settings/userName/Desktop/CWDsurveillanceMO.csv)
    file-print "AdultMale,YearlingMale,FawnMale,AdultFemale,YearlingFemale,FawnFemale,AdultMaleCWD,YearlingMaleCWD,FawnMaleCWD,AdultFemaleCWD,YearlingFemaleCWD,FawnFemaleCWD,TotalCWD,CWDhuntedDeer,CWDtestedDeer,DetProb,ObsPrev,TotalAdultMaleHunted,TotalYearlingMaleHunted,TotalAdultFemaleHunted,TotalYearlingFemaleHunted,TotalAdultMaleTested,TotalAdultFemaleTested,CWD_distribution,Sampling,Sampling_Region";moved TruePre
    file-close
  ]
  reset-ticks
end

to setup-landscape
  if (Sampling_Region = "Boone County")[    ;County_prehpop
    import-world "BooneCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Callaway County")[
    import-world "CallawayCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Carroll County")[
    import-world "CarrollCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Chariton County")[
    import-world "CharitonCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Cole County")[
    import-world "ColeCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Cooper County")[
    import-world "CooperCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Franklin County")[
    import-world "FranklinCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Gasconade County")[
    import-world "GasconadeCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Knox County")[
    import-world "KnoxCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Linn County")[
    import-world "LinnCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Livingston County")[
    import-world "LivingstonCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Miller County")[
    import-world "MillerCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Moniteau County")[
    import-world "MoniteauCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Morgan County")[
    import-world "MorganCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Osage County")[
    import-world "OsageCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Putnam County")[
    import-world "PutnamCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Randolph County")[
    import-world "RandolphCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Schuyler County")[
    import-world "SchuylerCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Scotland County")[
    import-world "ScotlandCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Shelby County")[
    import-world "ShelbyCounty_prehpop.csv"
  ]
  if (Sampling_Region = "St. Charles County")[
    import-world "StCharlesCounty_prehpop.csv"
  ]
  if (Sampling_Region = "St. Louis County")[
    import-world "StLouisCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Sullivan County")[
    import-world "SullivanCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Warren County")[
    import-world "WarrenCounty_prehpop.csv"
  ]
  if (Sampling_Region = "Washington County")[
    import-world "WashingtonCounty_prehpop.csv"
  ]
;  if (Sampling_Region = "7 Counties")[
;    import-world "prehpop7counties.csv"
;  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;------------
to go
  random-seed new-seed
  tick

  if (ticks = 2) [
    stop
  ]

  ;set tp CWD_prevalence  ; ######################    SET TRUE PREVALENCE (1% is 0.01/ 0.1% is 0.001)   4TH aUGUST 17 REMOVED GLOBAL cwd_PREVALENCE FROM INTERFACE
  set permah SampleSizeMaleHarvest ; ############    SET PERCENTAGE OF ADULT MALE HARVEST TESTED (10% IS 0.1, 20% IS 0.2, ETC.)
  set perfah SampleSizeFemaleHarvest ; ##########    SET PERCENTAGE OF ADULT FEMALE HARVEST TESTED (10% IS 0.1, 20% IS 0.2, ETC.)

  set d ((remainder (ticks) 12) + 1)
  set adult-deers deers with [aim > 24]                           ;4 August 17 changed from 30 to 24
  set yearling-deers deers with [aim > 12 and aim < 25]
  set fawn-deers deers with [aim < 13]
  set n-inf-fawns round (Fawn-prevalence * count fawn-deers)
  set n-inf-yearlings round (Yearling-prevalence * count yearling-deers)
  set n-inf-adults round (Adult-prevalence * count adult-deers)
  if (CWD_distribution = "clustered_dist")[
    let xdcp patches with [do = 1]
    set %CWDlandscape 10
    set dcp (count xdcp / (100 / %CWDlandscape))
    ask one-of xdcp [
      set dcl 1
      set ndcp 1
      set pcolor yellow
      while [ndcp < dcp]
      [let dcl-patches patches with [dcl = 1]
        ask dcl-patches [
          set dxp pxcor
          set dyp pycor
          if any? neighbors with [do = 1 and dcl = 0][
            ask one-of neighbors with [do = 1 and dcl = 0][
              set dxn pxcor
              set dyn pycor
              if (abs(dxn - dxp) < 2 and abs(dyn - dyp) < 2)[
                set dcl 1
                set ndcp (ndcp + 1)
                set pcolor yellow
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  distribute-CWD
  set ini-inf-male count deers with [cwd = 1 and sex = 1]
  set ini-inf-female count deers with [cwd = 1 and sex = 2]
  ;---------------- non-random sampling
  if (Sampling = "non-random_sampling")[
    let nb-patches patches with [do = 1 and border = 0]
    ask nb-patches [
      let tdh count deers-here                       ;total deer here
      let tdns sum [count deers-here] of neighbors   ;total deer on neighboring patches
      set add round ((tdh + tdns) / 9)               ;average deer density for this patch
      if (add = 0) [set add -9999]
      ]
    let b-patches patches with [do = 1 and border >= 1]
    ask b-patches [
      ifelse (any? neighbors with [border = 0 and add > 0])
      [ask one-of neighbors with [border = 0 and add > 0][
        set tdd add
        ]
        set add tdd
        ]
      [set add -9999]
      ]
    let num-nohunt-patches round (count patches with [do = 1] * .25)
    ask n-of num-nohunt-patches patches with [do = 1][
      set add 0
      ask deers-here [
        if (aim > 30 and sex = 1) [set color brown + 2]
        if (aim > 30 and sex = 2) [set color brown + 3]
        ]
      ]
    let denom (count patches with [do = 1] - num-nohunt-patches)
    set hpp precision(count patches with [add >= 25] / count patches with [add > 0])2
    ask patches with [add >= 25][
      ask deers-here [
        if (aim > 30 and sex = 1) [set color brown]
        if (aim > 30 and sex = 2) [set color brown + 1]
        ]
      ]
  ]
  ;---------------------  nonrandom sampling end
  let cwddeer deers with [cwd = 1]
  let ndeers deers with [cwd = 0]
  ask cwddeer [
    st
    set color blue
    ]
  ask ndeers [
    ht
    ]
  ;--------------------------------------------
  let male-fawns deers with [sex = 1 and aim < 12.5]
  set mf (count male-fawns);deers with [sex = 1 and aim < 12.5])
  let female-fawns deers with [sex = 2 and aim < 12.5]
  set ff (count female-fawns);deers with [sex = 2 and aim < 12.5])
  let male-yearlings deers with [sex = 1 and aim = 19]
  set my (count male-yearlings);deers with [sex = 1 and aim = 19])
  let female-yearlings deers with [sex = 2 and aim = 19]
  set fy (count female-yearlings);deers with [sex = 2 and aim = 19])
  let adult-males deers with [sex = 1 and aim > 30]
  set ma (count adult-males);deers with [sex = 1 and aim > 30])
  let adult-females deers with [sex = 2 and aim > 30]
  set fa (count adult-females);deers with [sex = 2 and aim > 30])
  set phn (mf + my + ma + ff + fy + fa)
  let cwd-deers deers with [cwd = 1]
  ask cwd-deers [
    ifelse (sex = 1)
    [ifelse (aim > 21)
      [set mcwd (mcwd + 1)]
      [ifelse (aim > 17)
        [set mycwd (mycwd + 1)]
        [set mfcwd (mfcwd + 1)]
        ]
      ]
    [ifelse (aim > 21)
      [set fcwd (fcwd + 1)]
      [ifelse (aim > 17)
        [set fycwd (fycwd + 1)]
        [set ffcwd (ffcwd + 1)]
        ]
      ]
    ]
  set totcwdd (mcwd + mycwd + mfcwd + fcwd + fycwd + ffcwd)
  set vals1 (list (ma) (my) (mf) (fa) (fy) (ff) (mcwd) (mycwd) (mfcwd) (fcwd) (fycwd) (ffcwd) (totcwdd))

  ;----------------------------for nonrandom sampling
  let highhhpm deers with [color = brown]
  let highhhpf deers with [color = brown + 1]
  let lowhhpm deers with [color != brown and aim > 30 and sex = 1]
  let lowhhpf deers with [color != brown and aim > 30 and sex = 2]
  ;----------------------------------------------
  ask deers [                                                         ;18Jun16 "deer-grow"
    set aim (aim + 1)
    deer-die                                                          ;27July16 added deer-die in this loop
    if (aim < 10)[
      ifelse (sex = 1)
      [if (random-float 1 < mf12hm)[     ;11April17 mfmo12h
        set tgroid groid
        hunting-mortality-mf12
        ]
        ]
      [if (random-float 1 < ff12hm)[    ;11April17 ffmo12h
        set tgroid groid
        set twho who
        hunting-mortality-ff12
        ]
        ]
      ]
    if (aim = 20)[
      ifelse (sex = 1)
      [if (random-float 1 < myhm)[  ;11April17 mymoh
        set tgroid groid
        hunting-mortality-my
        ]
        ]
      [if (random-float 1 < fyhm)[ ;;11April17 fymoh
        set tgroid groid
        set twho who
        hunting-mortality-fy
        ]
        ]
      ]
    if (aim > 30)[
      ifelse (sex = 1)
      [if (random-float 1 < mahm)[  ;11April17 mamoh
        if (Sampling = "non-random_sampling")[
          ifelse (color = brown)
          [ifelse (random-float 1 <= .75)
            [set tgroid groid
              set dhhhpp dhhhpp + 1
              hunting-mortality-ma]
            [ask one-of lowhhpm [
              set tgroid groid
              set dhlhpp dhlhpp + 1
              hunting-mortality-ma
              ]
              ]
            ]
          [ifelse (random-float 1 <= .25)
            [set tgroid groid
              set dhlhpp dhlhpp + 1
              hunting-mortality-ma]
            [ifelse (count highhhpm > 0)
              [ask one-of highhhpm [
                set tgroid groid
                set dhhhpp dhhhpp + 1
                hunting-mortality-ma
                ]
                ]
              [ask one-of lowhhpm [
                set tgroid groid
                set dhlhpp dhlhpp + 1
                hunting-mortality-ma
                ]
                ]
              ]
            ]
          ]
        if (Sampling = "random_sampling")[
          set tgroid groid
          hunting-mortality-ma
          ]
        ]
        ]
      [if (random-float 1 < fahm)[    ;11April17 famoh
        if (Sampling = "non-random_sampling")[
          ifelse (color = brown + 1)
          [ifelse (random-float 1 <= .75)
            [set tgroid groid
              set dhhhpp dhhhpp + 1
              set twho who
              hunting-mortality-fa]
            [ask one-of lowhhpf[
              set tgroid groid
              set dhlhpp dhlhpp + 1
              set twho who
              hunting-mortality-fa
              ]
              ]
            ]
          [ifelse (random-float 1 <= .25)
            [set tgroid groid
              set dhlhpp dhlhpp + 1
              set twho who
              hunting-mortality-fa]
            [ifelse (count highhhpf > 0)          ;21Nov16 added
              [ask one-of highhhpf[
                set tgroid groid
                set dhhhpp dhhhpp + 1
                set twho who
                hunting-mortality-fa
                ]
                ]
              [ask one-of lowhhpf[
                set tgroid groid
                set dhlhpp dhlhpp + 1
                set twho who
                hunting-mortality-fa
                ]
                ]
              ]
            ]
          ]
        ;random sampling
        if (Sampling = "random_sampling")[
          set tgroid groid
          set twho who
          hunting-mortality-fa
          ]
        ]
        ]
      ]
    ]
  ;--------------------------------------
  set hcwd (dcwdm + dcwdmy + dcwdmf + dcwdf + dcwdfy + dcwdff)
  set tcwd (tcwdm + tcwdf)
  if (tcwd > 0) [
    set pdcwd 1
    ]
  set peramht tamt / tamh
  set perafht taft / tafh
  set op precision (tcwd / (tamt + taft)) 3
  set vals2 (list (hcwd) (tcwd) (pdcwd) (op) (tamh) (tymh) (tafh) (tyfh) (tamt) (taft) (CWD_distribution) (Sampling) (Sampling_Region))
  set vals (sentence vals1 vals2)
  file-open (word "c:/Users/" machine "/Desktop/CWDsurveillanceMO.csv"); STEP 3: SUBSTITUTE WITH APPROPRIATE PATH (C:/Documents and Settings/userName/Desktop/CWDsurveillanceMO.csv)
  file-type first vals
  foreach but-first vals [ [?1] ->
    file-type "," file-type ?1
    ]
  file-print""
  file-close
  set ndd 0
end

  ;-------------------------------------
to distribute-CWD
  if (n-inf-fawns > 0)[
    let m-n-inf-fawns round (m-f-prev-ratio * n-inf-fawns)
    let f-n-inf-fawns (n-inf-fawns - m-n-inf-fawns)
    ;--
    if (CWD_distribution = "clustered_dist")[
      let yellow-deers deers-on patches with [pcolor = yellow]
      let nonyellow-deers deers-on patches with [pcolor != yellow]
      let m-f-cluster-cases round (m-n-inf-fawns * 0.75)
      let m-f-remaining-cases (m-n-inf-fawns - m-f-cluster-cases)
      if (m-f-cluster-cases > 0)[
        ask n-of m-f-cluster-cases yellow-deers with [aim < 13 and sex = 1][
          set cwd 1
        ]
      ]
      if (m-f-remaining-cases > 0)[
        ask n-of m-f-remaining-cases nonyellow-deers with [aim < 13 and sex = 1][
          set cwd 1
        ]
        ]
      let f-f-cluster-cases round (f-n-inf-fawns * 0.75)
      let f-f-remaining-cases (f-n-inf-fawns - f-f-cluster-cases)
      if (f-f-cluster-cases > 0)[
        ask n-of f-f-cluster-cases yellow-deers with [aim < 13 and sex = 2][
          set cwd 1
        ]
      ]
      if (f-f-remaining-cases > 0)[
        ask n-of f-f-remaining-cases nonyellow-deers with [aim < 13 and sex = 2][
          set cwd 1
        ]
      ]
    ]
    if (CWD_distribution = "random_dist")[
      if (m-n-inf-fawns > 0)[
        ask n-of m-n-inf-fawns fawn-deers with [sex = 1][
          set cwd 1
          ]
        ]
      if (f-n-inf-fawns > 0)[
        ask n-of f-n-inf-fawns fawn-deers with [sex = 2][
          set cwd 1
          ]
        ]
      ]
  ]
  ;-----
  if (n-inf-yearlings > 0)[
    let m-n-inf-yearlings round (m-f-prev-ratio * n-inf-yearlings)
    let f-n-inf-yearlings (n-inf-yearlings - m-n-inf-yearlings)
    if (CWD_distribution = "clustered_dist")[
      let yellow-deers deers-on patches with [pcolor = yellow]
      let nonyellow-deers deers-on patches with [pcolor != yellow]
      let m-y-cluster-cases round (m-n-inf-yearlings * 0.75)
      let m-y-remaining-cases (m-n-inf-yearlings - m-y-cluster-cases)
      if (m-y-cluster-cases > 0)[
        ask n-of m-y-cluster-cases yellow-deers with [aim > 12 and aim < 25 and sex = 1][
          set cwd 1
          ]
        ]
      if (m-y-remaining-cases > 0)[
        ask n-of m-y-remaining-cases nonyellow-deers with [aim > 12 and aim < 25 and sex = 1][
          set cwd 1
          ]
        ]
      let f-y-cluster-cases round (f-n-inf-yearlings * 0.75)
      let f-y-remaining-cases (f-n-inf-yearlings - f-y-cluster-cases)
      if (f-y-cluster-cases > 0)[
        ask n-of f-y-cluster-cases yellow-deers with [aim > 12 and aim < 25 and sex = 2][
          set cwd 1
          ]
        ]
      if (f-y-remaining-cases > 0)[
        ask n-of f-y-remaining-cases nonyellow-deers with [aim > 12 and aim < 25 and sex = 2][
          set cwd 1
          ]
        ]
      ]
    if (CWD_distribution = "random_dist")[
      if (m-n-inf-yearlings > 0)[
        ask n-of m-n-inf-yearlings yearling-deers with [sex = 1][
          set cwd 1
          ]
        ]
      if (f-n-inf-yearlings > 0)[
        ask n-of f-n-inf-yearlings yearling-deers with [sex = 2][
          set cwd 1
          ]
        ]
      ]
  ]
  ;---adults
  if (n-inf-adults > 0)[
    let m-n-inf-adults round (m-f-prev-ratio * n-inf-adults)
    let f-n-inf-adults (n-inf-adults - m-n-inf-adults)
    if (CWD_distribution = "clustered_dist")[
      let yellow-deers deers-on patches with [pcolor = yellow]
      let nonyellow-deers deers-on patches with [pcolor != yellow]
      let m-a-cluster-cases round (m-n-inf-adults * 0.75)
      let m-a-remaining-cases (m-n-inf-adults - m-a-cluster-cases)
      if (m-a-cluster-cases > 0)[
        ask n-of m-a-cluster-cases yellow-deers with [aim > 24 and sex = 1][
          set cwd 1
          ]
        ]
      if (m-a-remaining-cases > 0)[
        ask n-of m-a-remaining-cases nonyellow-deers with [aim > 24 and sex = 1][
          set cwd 1
          ]
        ]
      let f-a-cluster-cases round (f-n-inf-adults * 0.75)
      let f-a-remaining-cases (f-n-inf-adults - f-a-cluster-cases)
      if (f-a-cluster-cases > 0)[
        ask n-of f-a-cluster-cases yellow-deers with [aim > 24 and sex = 2][
          set cwd 1
          ]
        ]
      if (f-a-remaining-cases > 0)[
        ask n-of f-a-remaining-cases nonyellow-deers with [aim > 24 and sex = 2][
          set cwd 1
          ]
        ]
      ]
    if (CWD_distribution = "random_dist")[
      if (m-n-inf-adults > 0)[
        ask n-of m-n-inf-adults adult-deers with [sex = 1][
          set cwd 1
          ]
        ]
      if (f-n-inf-adults > 0)[
        ask n-of f-n-inf-adults adult-deers with [sex = 2][
          set cwd 1
          ]
        ]
      ]
    ]

end


to deer-die
  set tgroid groid
  if (sex = 2)[
    set twho who
  ]
  ;============================================================== fawns upto 6 months
  ifelse (aim < 6.5)
  [ifelse (sex = 1)
    [if precision (random-float 1) 3 < mf6nhm [    ;11April17 mfmo6 replaced with mf6nhm
      die
      ]
    ]
    [if precision (random-float 1) 3 < ff6nhm [     ;11April17 ffmo6 replaced with ff6nhm
      die
      ]
    ]
  ]
  ;================================================================ 7 to 12 months
  [ifelse (aim < 12.5)
    [ifelse (sex = 1)
      [if random-float 1 < mf12nhm [   ;11April17 mfmo12 replaced with mf12nhm
        die
        ]
      ]
      [if random-float 1 < ff12nhm [    ;11April17 ffmo12 replaced with ff12nhm
        die
        ]
      ]
    ]
    ;=============================================================== 13 to 24
    [ifelse (aim < 24.5)
      [ifelse (sex = 1)
        [if random-float 1 < mynhm [    ;11April17 mymonh replaced with mynhm
          die
          ]
        ]
        [if random-float 1 < fynhm [ ;11April17 fymonh replaced with fynhm
            if any? deers with [momid = twho and aim < 2.5][
              let my-fawns deers with [momid = twho and aim < 2.5]
              ask my-fawns [
                set counter1 counter1 + 1
                die
              ]
            ]
            set counter1 0
            die
            ]
        ]
      ]
      ;=================================================================male 25 to 240 and more than 240
      [ifelse (sex = 1)
        [ifelse (aim < 240)
          [if random-float 1 < precision (manhm - oldm) 3 [ ;11April17 mamonh replaced with manhm
            die
            ]
          ]
          [if random-float 1 < .8 [
            die
            ]
          ]
        ]
      ;===============================================================female 25 to 240
      [ifelse (aim < 240)
        [if random-float 1 < precision (fanhm - oldf) 3 [ ;11April17 famonh replaced with fanhm
          let my-fawns deers with [momid = twho and aim < 2.5]
          if (count my-fawns > 0)[
            ask my-fawns[
              set counter1 counter1 + 1
              die
              ]
            ]
          set counter1 0
          die
          ]
        ]
      ;============================================================female 240 and more
      [if random-float 1 < .8 [
        if any? deers with [momid = twho and aim < 2.5][
          let my-fawns deers with [momid = twho and aim < 2.5]
          ask my-fawns [
            set counter1 counter1 + 1
            die
            ]
          ]
        set counter1 0
        die
        ]
      ]
      ]
      ]
    ]
  ]

end

;===================================================

to hunting-mortality-mf12
if (cwd = 1)[
  set dcwdmf (dcwdmf + 1)
  ]
die
end
;=====================================
to hunting-mortality-ff12
if (cwd = 1)[
  set dcwdff (dcwdff + 1)
  ]
die
end
;====================================
to hunting-mortality-my
  if (cwd = 1)[
    set dcwdmy (dcwdmy + 1)
    ]
set tymh tymh + 1
  die
end

to hunting-mortality-fy
  if (cwd = 1)[
    set dcwdfy (dcwdfy + 1)
    ]
  set tyfh tyfh + 1
  die
end
;=====================================
to hunting-mortality-ma
ifelse (random-float 1 < permah)                         ;;percent of male adult harvest tested set by the slider
[if (cwd = 1) [
  set dcwdm (dcwdm + 1)
  set tcwdm (tcwdm + 1)
  ]
set tamh tamh + 1
set tamt tamt + 1
die
]
[if (cwd = 1) [
  set dcwdm (dcwdm + 1)
  ]
set tamh tamh + 1
die
]
end
;=====================================
to hunting-mortality-fa
  ifelse (random-float 1 < perfah)
  [if (cwd = 1)[
      set dcwdf (dcwdf + 1)
      set tcwdf (tcwdf + 1)
      ]
  set tafh tafh + 1
  set taft taft + 1
  die
  ]
  [if (cwd = 1)[
    set dcwdf (dcwdf + 1)
    ]
  set tafh tafh + 1
  die
  ]
end

to deer-count
  ifelse (sex = 1)
  [ifelse (aim < 12.5)
    [set mf (mf + 1)]
    [ifelse (aim < 24.5)
      [set my (my + 1)]
      [set ma (ma + 1)]
    ]
  ]
  [ifelse (aim < 12.5)
    [set ff (ff + 1)]
    [ifelse (aim < 24.5)
      [set fy (fy + 1)]
      [set fa (fa + 1)]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
642
10
950
523
-1
-1
12.0
1
10
1
1
1
0
1
1
1
0
24
0
41
1
1
1
ticks
30.0

BUTTON
448
18
503
51
SETUP
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1317
10
1398
55
MALE DEER
count deers with [sex = 1]
17
1
11

MONITOR
1408
11
1498
56
FEMALE DEER
count deers with [sex = 2]
17
1
11

SLIDER
133
142
248
175
mf6nhm
mf6nhm
0
.1
0.055
.001
1
NIL
HORIZONTAL

SLIDER
133
182
249
215
ff6nhm
ff6nhm
0
.1
0.055
.001
1
NIL
HORIZONTAL

SLIDER
134
220
249
253
mf12nhm
mf12nhm
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
134
259
249
292
ff12nhm
ff12nhm
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
262
296
376
329
myhm
myhm
0
1
0.25
.01
1
NIL
HORIZONTAL

SLIDER
262
332
376
365
fyhm
fyhm
0
1
0.15
.01
1
NIL
HORIZONTAL

SLIDER
134
296
248
329
mynhm
mynhm
0
1
0.01
.01
1
NIL
HORIZONTAL

SLIDER
134
334
249
367
fynhm
fynhm
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
262
369
376
402
mahm
mahm
0
1
0.4
.01
1
NIL
HORIZONTAL

SLIDER
262
410
376
443
fahm
fahm
0
1
0.2
.001
1
NIL
HORIZONTAL

SLIDER
134
372
249
405
manhm
manhm
0
1
0.01
.01
1
NIL
HORIZONTAL

SLIDER
135
409
248
442
fanhm
fanhm
0
1
0.02
.01
1
NIL
HORIZONTAL

BUTTON
507
18
563
51
GO
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1317
63
1499
202
deer population
months
deer
0.0
10.0
0.0
15000.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -2674135 true "" ""

SLIDER
262
142
377
175
mf6hm
mf6hm
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
263
182
377
215
ff6hm
ff6hm
0
1
0.0
.01
1
NIL
HORIZONTAL

SLIDER
263
220
376
253
mf12hm
mf12hm
0
1
0.05
.01
1
NIL
HORIZONTAL

SLIDER
263
259
376
292
ff12hm
ff12hm
0
1
0.02
.01
1
NIL
HORIZONTAL

PLOT
1317
347
1503
487
Doe group size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [gr] of deers with [gl = 1]"

PLOT
1317
209
1501
342
Bachelor group size
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [gr] of deers with [ml > 0]"

CHOOSER
166
11
283
56
CWD_distribution
CWD_distribution
"random_dist" "clustered_dist"
1

CHOOSER
289
11
406
56
Sampling
Sampling
"random_sampling" "non-random_sampling"
0

TEXTBOX
120
122
252
140
NON-HUNTING MORTALITY
11
15.0
1

TEXTBOX
264
121
370
139
HUNTING MORTALITY
11
15.0
1

TEXTBOX
20
151
118
169
Young fawns: male
11
0.0
1

TEXTBOX
21
190
125
208
Young fawns: female
11
0.0
1

TEXTBOX
23
229
112
247
Older fawns: male
11
0.0
1

TEXTBOX
23
269
125
287
Older fawns: female
11
0.0
1

TEXTBOX
25
305
108
323
Yearlings: male
11
0.0
1

TEXTBOX
24
342
108
360
Yearlings: female
11
0.0
1

TEXTBOX
25
381
94
399
Adults: male
11
0.0
1

TEXTBOX
26
419
98
437
Adults: female
11
0.0
1

CHOOSER
3
10
161
55
Sampling_Region
Sampling_Region
"Boone County" "Callaway County" "Carroll County" "Chariton County" "Cole County" "Cooper County" "Franklin County" "Gasconade County" "Knox County" "Linn County" "Livingston County" "Miller County" "Moniteau County" "Morgan County" "Osage County" "Putnam County" "Randolph County" "Schuyler County" "Scotland County" "Shelby County" "St. Charles County" "St. Louis County" "Sullivan County" "Warren County" "Washington County"
0

MONITOR
1162
295
1315
340
CWD+ DEER IN POPULATION
totcwdd
17
1
11

MONITOR
1164
348
1315
393
CWD+ DEER IN HARVEST
hcwd
0
1
11

MONITOR
1164
402
1315
447
CWD+ DEER TESTED
tcwd
0
1
11

SLIDER
3
74
162
107
SampleSizeMaleHarvest
SampleSizeMaleHarvest
0
1
0.3
.1
1
NIL
HORIZONTAL

SLIDER
169
74
336
107
SampleSizeFemaleHarvest
SampleSizeFemaleHarvest
0
1
0.3
.1
1
NIL
HORIZONTAL

CHOOSER
29
477
167
522
machine
machine
"PC" "abelsare"
1

SLIDER
441
183
585
216
m-f-prev-ratio
m-f-prev-ratio
0
1
0.5
.1
1
NIL
HORIZONTAL

MONITOR
414
228
509
273
CWD+ male deer
ini-inf-male
17
1
11

MONITOR
511
228
614
273
CWD+ female deer
ini-inf-female
17
1
11

SLIDER
425
63
602
96
Fawn-prevalence
Fawn-prevalence
0
1
0.0
.005
1
NIL
HORIZONTAL

SLIDER
424
101
602
134
Yearling-prevalence
Yearling-prevalence
0
1
0.0
.005
1
NIL
HORIZONTAL

SLIDER
423
141
603
174
Adult-prevalence
Adult-prevalence
0
1
0.01
.005
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

deer
false
0
Polygon -7500403 true true 195 210 210 255 195 240 180 195 165 165 135 165 105 165 75 165 72 211 60 210 60 180 45 150 45 120 30 90 45 105 180 105 225 45 225 60 270 90 255 90 225 90 180 150
Polygon -7500403 true true 73 210 86 251 75 240 60 210
Polygon -7500403 true true 45 105 30 75 30 90 45 105 60 120 45 120
Line -7500403 true 210 60 165 15
Line -7500403 true 225 60 255 45
Line -7500403 true 195 45 210 15
Line -7500403 true 255 45 255 30
Line -7500403 true 255 45 270 30
Line -7500403 true 195 15 180 30

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="7county_baseline_100_.2" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.23"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="FranklincountyAL_CLNRS_100_.5" repetitions="56" runMetricsEveryStep="true">
    <setup>ca
setup</setup>
    <go>go</go>
    <timeLimit steps="1"/>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="tp">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Boone_tp1_ss.5" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Boone_bl_tp.2_.5" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="BOONE_CL_NRS" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="Sampling">
      <value value="&quot;non-random_sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sampling_Region">
      <value value="&quot;Boone County&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CWD_distribution">
      <value value="&quot;clustered_dist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="boone_rnd_nrnd" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="famonh">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CWD_prevalence">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SampleSizeMaleHarvest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymonh">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SampleSizeFemaleHarvest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo12h">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CWD_distribution">
      <value value="&quot;random_dist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sampling">
      <value value="&quot;non-random_sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo6h">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disp_rate">
      <value value="0.45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymoh">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mymoh">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="famoh">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sampling_Region">
      <value value="&quot;Boone County&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mamoh">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fymonh">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mfmo6h">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ffmo12h">
      <value value="0.02"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment1" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>pdcwd</metric>
    <enumeratedValueSet variable="manhm">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mf6nhm">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ff12hm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CWD_prevalence">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SampleSizeMaleHarvest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perfah">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SampleSizeFemaleHarvest">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fahm">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fynhm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="myhm">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fanhm">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CWD_distribution">
      <value value="&quot;clustered_dist&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mf12hm">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fyhm">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mf6hm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pertest">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sampling">
      <value value="&quot;non-random_sampling&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="permah">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ff6nhm">
      <value value="0.055"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ff12nhm">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mf12nhm">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%CWDlandscape">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="machine">
      <value value="&quot;abelsare&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mahm">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Sampling_Region">
      <value value="&quot;Carroll County&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ff6hm">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tp">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mynhm">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="npd">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
