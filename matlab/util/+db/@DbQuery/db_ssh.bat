@echo off
setlocal

:: This script is set up to accept command-line arguments in this order: 
:: Host, Port, RemoteHost, RemotePort, User, Password. 
:: If any argument is not provided, it will use the default value.
:: Please replace the 'Cmd' command with the real one. The current command is a placeholder
:: since providing passwords directly in command line is generally insecure. 
:: Consider using key-based authentication or sshpass if you must automate the password entry. 
:: However, note that these approaches have their own security implications.
:: Also note that Ctrl+C will stop the script and close the SSH connection.
::
:: Usage: my_script.bat [Host] [Port] [RemoteHost] [RemotePort] [User] [Password]

:: Check number of arguments and set defaults
if "%~1"=="" (set "Host=localhost") else (set "Host=%~1")
if "%~2"=="" (set "Port=63331") else (set "Port=%~2")
if "%~3"=="" (set "RemoteHost=10.23.1.25") else (set "RemoteHost=%~3")
if "%~4"=="" (set "RemotePort=5432") else (set "RemotePort=%~4")
if "%~5"=="" (set "User=ocs") else (set "User=%~5")
if "%~6"=="" (set "Password=physics") else (set "Password=%~6")

:Loop
echo Establishing SSH connection...

:: Here we build the SSH command
set "Cmd=ssh -L %Port%:%Host%:%RemotePort% %User%@%RemoteHost%"

:: Print and execute the command
echo %Cmd%
%Cmd%

:: If the SSH connection closed, we go back to Loop
echo SSH connection closed, retrying in 5 seconds...
timeout /t 5 /nobreak > nul
goto Loop

endlocal
