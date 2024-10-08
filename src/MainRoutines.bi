Declare Sub AddCareerYear (finalUpdate, careerFile$, statFile$)
Declare Sub ALIGN ()
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub BLANKPRO ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CalcStatLeadersPro (teamName$, numberGames)
Declare Sub CalcStatTotalsPro (numberGames, teamName$, finalUpdate$)
Declare Sub CAREER ()
Declare Sub CareerRecords_Report (printDest, destFile$)
Declare Sub CareerStats_Report (printDest, destFile$, careerFile$)
Declare Sub CareerStats_Screen (careerFile$)
Declare Sub CARVIEW ()
Declare Sub ChangeGlobalSettings ()
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)
Declare Sub COMPARE ()
Declare Sub Comparisons_Report (printDest, destFile$, teamName$, posn$())
Declare Sub Comparisons_Screen (teamName$, backToMenu, posn$())
Declare Sub CompileLeague ()
Declare Sub ContinueTrade ()
Declare Sub CreateCareerFile ()
Declare Sub CREATLEAG ()
Declare Sub DefineLeaguePro (league$, div1$, div2$, div3$, div4$)
Declare Sub DeleteBoxscores (fileChoice%, fileSpec$)
Declare Sub DisplayLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (printOrView, TVN, maxRecords, categoryId)
Declare Sub DoTheInput (action%)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$, nbrGamesCurr, nbrGamesRoad)
Declare Sub DRAFT ()
Declare Sub DraftChoices (draftNew, doneDrafting, teamIdx, tmTotal_DRAFT(), normYrIdx)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame (DT$, TM$)
Declare Sub EditParameters ()
Declare Sub EditPlayer (playerIdx, action)
Declare Sub EditStats ()
Declare Sub ExpandedLeaders_Menu  (printOrView, JB, plyrTotalStats, teamTotalStats)
Declare Sub ExtendedStatSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub GameRecords (menuChoice)
Declare Sub GetListLength (printOrView, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub GPMIN ()
Declare Sub HD2HDLEAG ()
Declare Sub Hd2HdPro_Report (teamName$, printDest)
Declare Sub Hd2HdPro_Screen (teamName$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, teamIdx, haveSteals, haveBlocks)
Declare Sub InputTeam (menuShortcut%)
Declare Sub LeaderStats_Screen ()
Declare Sub Leaders_Menu ()
Declare Sub LoadCareerFile (loadFile$)
Declare Sub LoadLeaderFiles_Pro (targetFile$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (tradeTeam)
Declare Sub LoadTeamStats (statFile$, team$, totalGames%)
Declare Sub LOOKY ()
Declare Sub Main_Menu (menuSelection)
Declare Sub MERGE ()
Declare Sub MergeAddYear (selectedFile$, statFile$)
Declare Sub MergeCareerFiles ()
Declare Sub MergeFirstYear ()
Declare Sub MergeStats ()
Declare Sub NewGame (visitor$, home$, YN$, DT$, TM$, NTMS, gameIndex%)
Declare Sub NEWLDR ()
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandings_Report (printDest, destFile$, league$)
Declare Sub NewStandings_Screen (league$)
Declare Sub NewTeamParameters_Base (teamYear$, teamName$, normYrIdx)
Declare Sub NewTeamParameters_OffDef ()
Declare Sub PlayerComparisons_Screen (idx, position$, teamName$)
Declare Sub PostLeadersPro (leaderFile$)
Declare Sub PostRecordsPro()
Declare Sub GameRecords_Report (printDest, destFile$)
Declare Sub Leaders_Report (printDest, destFile$, TVN, maxRecords, categoryId)
Declare Sub PrintRoster (printDest, destFile$, YN$)
Declare Sub SeasonRecords_Report (printDest, destFile$)
Declare Sub PrintTradeSummary (p1Idx, p2Idx)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub PROHOOP ()
Declare Sub ReadDraftTeam (teamYear$, teamIdx, draftIdx, validData)
Declare Sub READLEADER (statFile$, JB, plyrTotalStats, teamTotalStats)
Declare Sub ReadStatsPro (statFile$, teamName$)
Declare Sub ReadTradeTeam (teamYear$, teamIdx, tradeIdx, validData)
Declare Sub Records_Menu ()
Declare Sub Replay_Menu ()
Declare Sub ResetStatPrompt (statFile$)
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewCareerRecords ()
Declare Sub ReviewEditLeague (leagueFile$, league$, div1$, div2$, div3$, div4$)
Declare Sub SAVELEADER (statFile$, JB, plyrTotalStats, teamTotalStats)
Declare Sub SaveLeague (leagueFile$, league$, div1$, div2$, div3$, div4$)
Declare Sub SavePrintLeagueLeaders (printDest, destFile$)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)
Declare Sub SaveTeamFile (yearNumber$, teamIdx, action%)
Declare Sub SaveTrades ()
Declare Sub SCHEDULE ()
Declare Sub SEELEAG ()
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, draftIdx)
Declare Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, tradeIdx)
Declare Sub SortOptions (printOrView, sortType, reselect)
Declare Sub SortPlayer (PPP, stpl, sortType)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortStandingsLeague ()
Declare Sub SortStats (categoryPick)
Declare Sub SortTeam (sortMax, stpl, sortType)
Declare Sub StatOptions_Pro ()
Declare Sub StatsSchedule_Report (printDest, destFile$, teamName$, totalGames%)
Declare Sub StatsSummary_Report (printDest, destFile$, teamName$, totalGames%)
Declare Sub TeamComparisons_Screen (teamName$)
Declare Sub TeamMgt_Menu ()
Declare Sub TRADE ()
Declare Sub SaveCareerFile (saveFile$)
Declare Sub UpdateCareerRecords (recordFile$)
Declare Sub UpdateExpandedLeaders (JB, plyrTotalStats, teamTotalStats)
Declare Sub UpdateExpandedStandings (league$)
Declare Sub ViewExpandedStats ()
Declare Sub ViewGameRecords
Declare Sub ViewIndividualleaders (printOrView, JB)
Declare Sub ViewPlayer (careerFile$)
Declare Sub ViewOffenseStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)
Declare Sub ViewRoster ()
Declare Sub ViewRosterSequence (careerFile$)
Declare Sub ViewSeasonRecords ()
Declare Sub ViewScheduleSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ViewTeamLeaders (printOrView, plyrTotalStats, teamTotalStats)
Declare Sub ViewDefenseStats (statFile$, teamName$, categoryPick, totalGames%, menuExit)

Declare Function FindStatsLeague% (statFile$, totalGames, fullWins, fullLosses)
