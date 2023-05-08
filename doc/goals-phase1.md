
# PHASE 1 - PRELIMINARY WORK

## GOAL: ADDRESS MISSING / EXTRANEOUS SOURCE FILES

- [X] Pro-Basketball
	- [X] BLANK.BAS 
		used to 'DELETE ALL STATS FOR A LEAGUE'
		was in /src, but not referenced in menus
		added to menus based on V4.2 compiled exe menu layout

	- [X] COMPBOOT.EXE
		file is part of game install, but is not referenced by source
		ran EXE to determine it's identity
			"Standings and League Leaders" DISK v3.3
			Is used to review / create league stats
			file is dated from 1991
		latest version (4.2) has these features "included"
	
	- [X] NEWDRAFT.EXE 
		file is part of game install, but is not referenced by source
		ran EXE to determine it's identity
			"Draft / Trade / Change Ratings Disk" v3.3
			used to "normalize" drafted players
			file is dated from 1991
		latest version (4.2) has these features "included" (DRAFT.EXE)


## GOAL: COMPILE FULL GAMES FROM SOURCES

- [X] Re-create dependent functions with no QB64 equivalent in native BASIC
	- [X] ClearEOL
	- [X] FCopy
	- [X] MAMenu
	- [X] MMenuVert
	- [-] MPaintBox
	- [X] MScrnSave/MScrnRest
	- [X] QPrint/MQPrint	
	- [X] ReadFile + FCount
	- [X] Text In
	- [X] YesNoB
	
- [X] Replace dependent functions with QB64 equivalents

- [X] Remove obsolete code
	Remove checks for disk drives
	Remove references to SortStr (not needed)
		They are all used to sort file lists
		My function for getting file lists does a sort

- [X] Make code compatible with QB64

- [X] Attempt first compilation of each file
