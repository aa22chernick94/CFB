@echo off
rem ===========================================================================
rem  run_cfb.bat -- runs the FBS football pipeline (build_cfb.R)
rem
rem  Put this file in the same folder as build_cfb.R and double-click it.
rem  It finds Rscript, runs the build, syncs index.html, and opens index.html.
rem
rem  For scheduled runs (Task Scheduler), add  --log  as the argument: output
rem  goes to logs\build_<date>.log and nothing opens or waits for a key.
rem ===========================================================================
setlocal EnableDelayedExpansion
cd /d "%~dp0"

set "LOGMODE="
if /i "%~1"=="--log" set "LOGMODE=1"

if not exist "build_cfb.R" (
  echo build_cfb.R isn't in %CD%. Put run_cfb.bat in the pipeline folder.
  if not defined LOGMODE pause
  exit /b 1
)

rem ---- find Rscript: on PATH first, then the standard install folders (newest wins)
set "RSCRIPT="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT for /d %%d in ("%ProgramFiles%\R\R-*") do if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
if not defined RSCRIPT for /d %%d in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%d\bin\Rscript.exe" set "RSCRIPT=%%d\bin\Rscript.exe"
if not defined RSCRIPT (
  echo Couldn't find Rscript.exe. Install R from https://cran.r-project.org, or edit
  echo this file and set RSCRIPT to the full path of Rscript.exe.
  if not defined LOGMODE pause
  exit /b 1
)

echo Using %RSCRIPT%
echo Started %DATE% %TIME%

if defined LOGMODE (
  if not exist "logs" mkdir "logs"
  for /f %%t in ('powershell -NoProfile -Command "Get-Date -Format yyyy-MM-dd_HHmm"') do set "TS=%%t"
  "%RSCRIPT%" build_cfb.R > "logs\build_!TS!.log" 2>&1
) else (
  "%RSCRIPT%" build_cfb.R
)
set "RC=%ERRORLEVEL%"

if not "%RC%"=="0" (
  echo.
  echo The build stopped with an error ^(code %RC%^). See the messages above
  if defined LOGMODE echo or logs\build_!TS!.log
  echo The last good cfb_dashboard.html/index.html, if any, is unchanged.
  if not defined LOGMODE pause
  exit /b %RC%
)

rem ---- Copy cfb_dashboard.html to index.html for GitHub Pages
if exist "cfb_dashboard.html" (
  copy /y "cfb_dashboard.html" "index.html" >nul
  echo Generated index.html from cfb_dashboard.html
)

echo Finished %DATE% %TIME%
if not defined LOGMODE (
  if exist "index.html" start "" "index.html"
  timeout /t 5 >nul
)
exit /b 0