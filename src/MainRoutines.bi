Declare Sub ALIGN ()
Declare Sub AdjustGames (selectedIndex)
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub BLANK ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CAREER ()
Declare Sub CARVIEW ()
Declare Sub COMPARE ()
Declare Sub CREATE ()
Declare Sub CareerToPrinter (A$(), Z$)
Declare Sub CareerToScreen (A$(), Z$)
Declare Sub ChangeGlobalSettings ()
Declare Sub CheckStatsPro (TN$, totalGames)
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)
Declare Sub Comparisons_Printer (teamName$)
Declare Sub Comparisons_Screen (teamName$, backToMenu, posn$())
Declare Sub CompileLeague ()
Declare Sub ConfigureDrives ()
Declare Sub ContinueTrade ()
Declare Sub CreateCareerFile ()
Declare Sub DRAFT ()
Declare Sub DefineLeaguePro (LG$, div1$, div2$, div3$, div4$)
Declare Sub DeleteBoxscores (fileChoice%, fileSpec$)
Declare Sub DisplayLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (printOrView%, TVN, maxRecords, categoryId)
Declare Sub DoTheInput (action%)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DraftChoices (draftNew, doneDrafting, teamIdx, tmTotal_DRAFT())
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditParameters ()
Declare Sub EditPlayer (J, action)
Declare Sub EditStats ()
Declare Sub ExpandedLeaders_Menu  (printOrView%, JB, TS, TTS)
Declare Sub ExtendedStatSummary (statFile$, teamName$, totalGames%)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub GPMIN ()
Declare Sub GameRecords (menuChoice)
Declare Sub GetListLength (printOrView%, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub HD2HDPRO ()
Declare Sub Hd2HdPro_Printer (teamName$)
Declare Sub Hd2HdPro_Screen (teamName$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, teamIdx, haveSteals, haveBlocks)
Declare Sub LOOKY ()
Declare Sub LeaderStats_Screen ()
Declare Sub Leaders_Menu ()
Declare Sub LoadLeaderFiles_Pro (targetFile$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (tradeTeam)
Declare Sub LoadTeamStats (statFile$, totalGames%)
Declare Sub MERGE ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub MergeAddYear (selectedFile$, statFile$)
Declare Sub MergeCareerFiles ()
Declare Sub MergeStats ()
Declare Sub NEWLDR ()
Declare Sub NewGame (visitor$, home$, YN$, NTMS, gameIndex%)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandingOptions ()
Declare Sub NewTeamParameters (teamYear$, teamName$, teamIdx)
Declare Sub NewTeamParameters_OffDef ()
Declare Sub PlayerComparisons_Screen (idx, position$)
Declare Sub PostLeadersPro (leaderFile$)
Declare Sub PrintGameRecords ()
Declare Sub PrintLeaders (TVN, maxRecords, categoryId)
Declare Sub PrintNewStandings (LG$)
Declare Sub PrintRoster (YN$)
Declare Sub PrintSeasonRecords ()
Declare Sub PrintStatsSchedule (teamName$, totalGames%)
Declare Sub PrintStatsSummary (teamName$, totalGames%)
Declare Sub PrintTradeSummary (p1Idx, p2Idx)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub ReadDraftTeam (teamYear$, targetTeam$, teamIdx, draftIdx)
Declare Sub ReadStatsPro (statFile$, TN$)
Declare Sub ReadTradeTeam (teamYear$, targetTeam$, teamIdx, tradeIdx)
Declare Sub RecordsMenu ()
Declare Sub Replay_Menu ()
Declare Sub ReportFilePrompt_League (reportFile$)
Declare Sub ResetStatPrompt (statFile$)
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SAVELEADER (statFile$, JB, TS, TTS)
Declare Sub SCHEDULE ()
Declare Sub SEELEAG ()
Declare Sub SaveLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SavePrintLeagueLeaders (statFile$, destination)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)
Declare Sub SaveTeamFile (yearNumber$, teamIdx, action%)
Declare Sub SaveTrades ()
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)
Declare Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, tradeIdx)
Declare Sub SortLeadersPro ()
Declare Sub SortOptions (JB, ZZ, printOrView%, sortType%)
Declare Sub SortPlayer (PPP, stpl, sortType%)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortStats (CP%)
Declare Sub SortTeam (sortMax, stpl, sortType%)
Declare Sub StatOptions ()
Declare Sub StatsByCategoryPro (TN$, finalUpdate$, fullWins, fullLosses)
Declare Sub TINPUT (menuShortcut%)
Declare Sub TRADE ()
Declare Sub TeamComparisons_Screen (T$)
Declare Sub TeamMgt_Menu ()
Declare Sub UpdateCareerLeaders (careerFile$)
Declare Sub UpdateExpandedLeaders (statFile$, JB, TS, TTS)
Declare Sub UpdateLeagueStandings (LG$)
Declare Sub ViewExpandedStats ()
Declare Sub ViewGameRecords ()
Declare Sub ViewIndividualLeaders (JB, printOrView%)
Declare Sub ViewNewStandings (LG$)
Declare Sub ViewPlayer (A$(), Z$)
Declare Sub ViewPlayerStats (statFile$, teamName$, CP%, totalGames%, menuExit)
Declare Sub ViewPrintCareerRecords ()
Declare Sub ViewRoster (A$, T1)
Declare Sub ViewRosterSequence (A$(), Z$)
Declare Sub ViewSeasonRecords ()
Declare Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ViewTeamLeaders (printOrView%, TS, TTS)
Declare Sub ViewTeamStats (statFile$, teamName$, CP%, totalGames%, menuExit)

Declare Function FindStatsLeague% (statFile$, totalGames, fullWins, fullLosses)
