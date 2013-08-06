Freenet updater for some exe files on Windows plateform.		
See: [Issue 0005883](https://bugs.freenetproject.org/view.php?id=5883)	

### How to compile and test	
1. Download and install [Lazarus](http://www.lazarus.freepascal.org/) for your plateform.					
2. Download the [last win-freenetupdater master](https://github.com/romnGit/win-freenetupdater/archive/master.zip)				
3. From Lazarus, Project >> Open project >> Choose "FreenetUpdater.lpi"			
4. From Lazarus, Run >> Run (or F9)		
5. Result is visible in the file freenetupdater.log	(in the project directory)		

Note:		
As most of the code is cross plateform, this project can be tested (compiled and executed) on linux.		
The only specific part for the Windows plateform is the function **FindProcessByID** (which, under linux will do nothing and return false)		