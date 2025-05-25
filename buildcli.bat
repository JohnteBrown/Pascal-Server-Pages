@echo off
setlocal enabledelayedexpansion

:: === Set Delphi Env ===
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"

:: === Configuration ===
set PROJECT_PATH=.\PSP.dproj
set BUILD_CONFIG=Release
set BUILD_PLATFORM=Win32
set LOG_FILE=log\build_output.logs
set MSBUILD=msbuild

:: === Ensure log folder exists
if not exist log (
    mkdir log
)

:: === Timestamp and intro ===
echo ========================================================
echo 🛠️  PSP Build Started: %DATE% %TIME%
echo Project: %PROJECT_PATH%
echo Target: %BUILD_CONFIG% ^| Platform: %BUILD_PLATFORM%
echo ========================================================
echo Logging to %LOG_FILE%
echo Build Log - %DATE% %TIME% > %LOG_FILE%

:: === Call PowerShell script to build and stream logs
powershell -ExecutionPolicy Bypass -NoProfile -File buildcli.ps1 ^
    -MSBuildPath "%MSBUILD%" -Project "%PROJECT_PATH%" -Platform "%BUILD_PLATFORM%" -Config "%BUILD_CONFIG%" -Log "%LOG_FILE%"

:: === Result check
findstr /C:"Build succeeded." %LOG_FILE% >nul
if %errorlevel% neq 0 (
    echo ❌ Build FAILED — see %LOG_FILE%
    echo ❌ FAILED at %TIME% >> %LOG_FILE%
    pause
    exit /b 1
) else (
    echo ✅ Build SUCCEEDED — logged in %LOG_FILE%
    echo ✅ SUCCESS at %TIME% >> %LOG_FILE%
)

echo ========================================================
echo ✅ PSP Build Complete: %TIME%
echo ========================================================
pause
endlocal
