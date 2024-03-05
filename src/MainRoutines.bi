Declare Sub ALIGN ()
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub AllComparisons_Printer (teamName$, posn$())
Declare Sub BLANK ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CAREER ()
Declare Sub CARVIEW ()
Declare Sub COMPARE ()
Declare Sub CREATLEAG ()
Declare Sub CalcStatLeadersPro (teamName$, totalGames)
Declare Sub CalcStatTotalsPro (numberGames, teamName$, finalUpdate$)
Declare Sub CareerToPrinter (A$(), Z$)
Declare Sub CareerToScreen (A$(), Z$)
Declare Sub ChangeGlobalSettings ()
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)
Declare Sub Comparisons_Screen (teamName$, backToMenu, posn$())
Declare Sub CompileLeague ()
Declare Sub ConfigureDrives ()
Declare Sub ContinueTrade ()
Declare Sub CreateCareerFile ()
Declare Sub DRAFT ()
Declare Sub DefineLeaguePro (league$, div1$, div2$, div3$, div4$)
Declare Sub DeleteBoxscores (fileChoice%, fileSpec$)
Declare Sub DisplayLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (printOrView, TVN, maxRecords, categoryId)
Declare Sub DoTheInput (action%)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DraftChoices (draftNew, doneDrafting, teamIdx, tmTotal_DRAFT(), normYrIdx)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditParameters ()
Declare Sub EditPlayer (J, action)
Declare Sub EditStats ()
Declare Sub ExpandedLeaders_Menu  (printOrView, JB, plyrTotalStats, teamTotalStats)
Declare Sub ExtendedStatSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub GPMIN ()
Declare Sub GameRecords (menuChoice)
Declare Sub GetListLength (printOrView, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub HD2HDLEAG ()
Declare Sub Hd2HdPro_Printer (teamName$)
Declare Sub Hd2HdPro_Screen (teamName$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, teamIdx, haveSteals, haveBlocks)
Declare Sub LOOKY ()
Declare Sub LeaderStats_Screen ()
Declare Sub Leaders_Menu ()
Declare Sub LoadLeaderFiles_Pro (targetFile$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (tradeTeam)
Declare Sub LoadTeamStats (statFile$, team$, totalGames%)
Declare Sub MERGE ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub MergeAddYear (selectedFile$, statFile$)
Declare Sub MergeCareerFiles ()
Declare Sub MergeFirstYear ()
Declare Sub MergeStats ()
Declare Sub NEWLDR ()
Declare Sub NewGame (visitor$, home$, YN$, NTMS, gameIndex%)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandingOptions ()
Declare Sub NewTeamParameters (teamYear$, teamName$, normYrIdx)
Declare Sub NewTeamParameters_OffDef ()
Declare Sub PlayerComparisons_Screen (idx, teamName$, position$)
Declare Sub PostLeadersPro (leaderFile$)
Declare Sub PrintGameRecords ()
Declare Sub PrintLeaders (TVN, maxRecords, categoryId)
Declare Sub PrintNewStandings (league$)
Declare Sub PrintRoster (YN$)
Declare Sub PrintSeasonRecords ()
Declare Sub PrintStatsSchedule (teamName$, totalGames%)
Declare Sub PrintStatsSummary (teamName$, totalGames%)
Declare Sub PrintTradeSummary (p1Idx, p2Idx)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub READLEADER (statFile$, JB, plyrTotalStats, teamTotalStats)
Declare Sub ReadDraftTeam (teamYear$, teamIdx, draftIdx)
Declare Sub ReadStatsPro (statFile$, teamName$)
Declare Sub ReadTradeTeam (teamYear$, teamIdx, tradeIdx)
Declare Sub Records_Menu ()
Declare Sub Replay_Menu ()
Declare Sub ReportFilePrompt_League (reportFile$)
Declare Sub ResetStatPrompt (statFile$)
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SAVELEADER (statFile$, JB, plyrTotalStats, teamTotalStats)
Declare Sub SCHEDULE ()
Declare Sub SEELEAG ()
Declare Sub SaveLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SavePrintLeagueLeaders (statFile$, destination)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)
Declare Sub SaveTeamFile (yearNumber$, teamIdx, action%)
Declare Sub SaveTrades ()
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, draftIdx)
Declare Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, tradeIdx)
Declare Sub SortStandingsLeague ()
Declare Sub SortOptions (printOrView, sortType, newCategory)
Declare Sub SortPlayer (PPP, stpl, sortType)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortStats (categoryPick)
Declare Sub SortTeam (sortMax, stpl, sortType)
Declare Sub StatOptions ()
Declare Sub TINPUT (menuShortcut%)
Declare Sub TRADE ()
Declare Sub TeamComparisons_Printer (teamName$)
Declare Sub TeamComparisons_Screen (teamName$)
Declare Sub TeamMgt_Menu ()
Declare Sub UpdateCareerLeaders (careerFile$)
Declare Sub UpdateExpandedLeaders (JB, plyrTotalStats, teamTotalStats)
Declare Sub UpdateLeagueStandings (league$)
Declare Sub ViewExpandedStats ()
Declare Sub ViewGameRecords
Declare Sub ViewIndividualLeaders (printOrView, JB)
Declare Sub ViewNewStandings (league$)
Declare Sub ViewPlayer (A$(), teamName$)
Declare Sub ViewPlayerStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)
Declare Sub ViewPrintCareerRecords ()
Declare Sub ViewRoster ()
Declare Sub ViewRosterSequence (A$(), teamName$)
Declare Sub ViewSeasonRecords ()
Declare Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ViewTeamLeaders (printOrView, plyrTotalStats, teamTotalStats)
Declare Sub ViewTeamStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)

Declare Function FindStatsLeague% (statFile$, totalGames, fullWins, fullLosses)
