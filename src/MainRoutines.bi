Declare Sub AddYear (selectedFile$, statFile$)
Declare Sub AdjustGames (selectedIndex)
Declare Sub ALIGN ()
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub BLANK ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CAREER ()
Declare Sub CareerToPrinter (A$(), Z$)
Declare Sub CareerToScreen (A$(), Z$)
Declare Sub CARVIEW ()
Declare Sub ChangeGlobalSettings ()
Declare Sub CheckStatsPro (TN$)
Declare Sub CheckTeam_Schedule (teamInfo, yearNumber$)
Declare Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseLeaguePro (league$)
Declare Sub COMPARE ()
Declare Sub Comparisons_Printer (teamName$)
Declare Sub Comparisons_Screen (teamName$, backToMenu, posn$())
Declare Sub CompileLeague ()
Declare Sub ConfigureDrives ()
Declare Sub CREATE ()
Declare Sub CreateCareerFile ()
Declare Sub DefineLeaguePro (LG$, div1$, div2$, div3$, div4$)
Declare Sub DeleteSchedules (fileChoice%, fileSpec$)
Declare Sub DisplayLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (printOrView%, TVN, maxRecords, categoryId)
Declare Sub DoTheInput (action%)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DRAFT ()
Declare Sub DraftChoices (draftNew, doneDrafting, teamIdx, TTL%())
Declare Sub DraftNewTeam (teamYear$, teamName$, teamIdx)
Declare Sub DraftNewTeam_OffDef ()
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditParameters ()
Declare Sub EditPlayer (J, action)
Declare Sub ExtendedStatSummary (statFile$, teamName$, totalGames%)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub GameRecords (menuChoice)
Declare Sub GetDiskID_ALIGN (A$)
Declare Sub GetDiskID_MERGE (A$)
Declare Sub GetListLength (printOrView%, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub GetTeam_CAREER (teamIDX, A$)
Declare Sub GetTeam_CNGTM (A$, yearNumber$, teamIdx)
Declare Sub GetTeam_CREATE (T1$, YN$, D)
Declare Sub GetTeam_HD2HD (TM$)
Declare Sub GetTeam_Schedule (numberTeams, yearNumber$, teamIdx)
Declare Sub GetTeam_EditStats (T1$, YN$, D)
Declare Sub ChooseTradeTeam (draftYear$, tradeTeam)
Declare Sub GPMIN ()
Declare Sub HD2HDPRO ()
Declare Sub Hd2HdPro_Printer (teamName$)
Declare Sub Hd2HdPro_Screen (teamName$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, teamIdx)
Declare Sub LeaderStats_Screen ()
Declare Sub Leaders_Menu ()
Declare Sub LoadLeaderFiles_Pro (targetFile$)
Declare Sub LoadSchedule (targetFile$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (tradeTeam)
Declare Sub LoadTeamStats (statFile$, totalGames%)
Declare Sub LOOKY ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub MERGE ()
Declare Sub MergeCareerFiles ()
Declare Sub MergeStats ()
Declare Sub NewGame (visitor$, home$, YN$, NTMS, gameIndex%)
Declare Sub NEWLDR ()
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandingOptions ()
Declare Sub PlayerComparisons_Screen (idx, position$)
Declare Sub PlayerStats (statFile$, teamName$, CP%, totalGames%, menuExit)
Declare Sub PostLeaders (leaderFile$)
Declare Sub PrintGameRecords ()
Declare Sub PrintLeaders (TVN, maxRecords, categoryId)
Declare Sub PrintNewStandings (LG$)
Declare Sub PrintRoster (YN$)
Declare Sub PrintSeasonRecords ()
Declare Sub PrintStatsSchedule (teamName$, totalGames%)
Declare Sub PrintStatsSummary (teamName$, totalGames%)
Declare Sub PrintTradeSummary (p1Idx, p2Idx)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub ReadStatsPro (statFile$, TN$)
Declare Sub ReadTeamData_COMPARE (yearNumber$, numberTeams)
Declare Sub ReadTeamData_HD2HD (yearNumber$, numberTeams)
Declare Sub ReadDraftTeam (teamYear$, targetTeam$, teamIdx, draftIdx)
Declare Sub RecordsMenu ()
Declare Sub Replay_Menu ()
Declare Sub ReportFilePrompt_League (reportFile$)
Declare Sub ResetStatPrompt (statFile$)
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SAVELEADER (statFile$, JB, TS, TTS)
Declare Sub SaveLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SavePrintLeagueLeaders (statFile$, destination)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)
Declare Sub SaveTeamFile (yearNumber$, teamIdx, action%)
Declare Sub SaveTrades ()
Declare Sub SCHEDULE ()
Declare Sub SEELEAG ()
Declare Sub SelectLeague_HD2HD (targetFile$)
Declare Sub SelectScheduleFile (targetFile$, scheduleFiles$(), FileCount%, ExitCode%)
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)
Declare Sub SelectTeam_ALIGN (yearNumber$, numberTeams)
Declare Sub SelectTeam_CAREER (yearNumber$, numberTeams, teamNumber)
Declare Sub SelectTeam_CNGTM (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_COMPARE (yearNumber$, numberTeams, teamName$)
Declare Sub SelectTeam_CREATE (yearNumber$, numberTeams, selectedIndex)
Declare Sub SelectTeam_GPMIN (yearNumber$, numberTeams, selectedIndex)
Declare Sub SelectTeam_HD2HD (yearNumber$, numberTeams, TM$)
Declare Sub SelectTeam_LOOKY (yearNumber$, numberTeams)
Declare Sub SelectTeam_MERGE (yearNumber$, numberTeams)
Declare Sub SelectTeam_Schedule (yearNumber$, numberTeams)
Declare Sub SelectTeam_EditStats (yearNumber$, numberTeams, selectedIndex)
Declare Sub SelectTradeTeam (tradeTeam, numberTeams, yearNumber$)
Declare Sub EditStats ()
Declare Sub SortLeaders ()
Declare Sub SortOptions (JB, ZZ, printOrView%, sortType%)
Declare Sub SortPlayer (PPP, stpl, sortType%)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortStats (CP%)
Declare Sub SortTeam (sortMax, stpl, sortType%)
Declare Sub StatOptions ()
Declare Sub StatsByCategoryPro (numberGames%, TN$, finalUpdate$, fullWins, fullLosses)
Declare Sub TeamComparisons_Screen (T$)
Declare Sub TeamMgt_Menu ()
Declare Sub TeamStats (statFile$, teamName$, CP%, totalGames%, menuExit)
Declare Sub TINPUT (menuShortcut%)
Declare Sub TRADE ()
Declare Sub ContinueTrade ()
Declare Sub UpdateCareerLeaders (careerFile$)
Declare Sub UpdateExpandedLeaders (JB, statFile$, TS, TTS)
Declare Sub UpdateLeagueStandings (LG$)
Declare Sub ViewExpandedStats
Declare Sub ViewGameRecords
Declare Sub ViewIndividualLeaders (JB, printOrView%)
Declare Sub ViewNewStandings (LG$)
Declare Sub ViewPlayer (A$(), Z$)
Declare Sub ViewPrintCareerRecords ()
Declare Sub ViewRoster (A$, T1)
Declare Sub ViewRosterSequence (A$(), Z$)
Declare Sub ViewSeasonRecords ()
Declare Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ViewTeamLeaders (printOrView%, TS, TTS)

Declare Function FindStatsLeague% (statFile$, numberGames%, fullWins, fullLosses)

