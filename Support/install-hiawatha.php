<?php

print "Downloading Hiawatha web server component...\n";

file_put_contents("Hiawatha.zip", 
	file_get_contents("https://github.com/mindfulvector/StoneNotes2023_Support/raw/main/Hiawatha.zip"));

print "Installing...\n";

if ($output = system("tar -xf .\Hiawatha.zip -C .\\")) {
    print "Error extracting Hiawatha archive!\n";
    system("pause");
    cleanup();
    exit(1);
}

print "Create config...\n";

file_put_contents(".\Hiawatha\config\hiawatha.conf", <<<END
# Hiawatha main configuration file
#
set INSTALL_DIR = CONFIG_DIR\\..


# GENERAL SETTINGS
#
ConnectionsTotal = 150
ConnectionsPerIP = 10
SystemLogfile = C:\\ProgramData\\StoneNotes\\Hiawatha\\system.log
GarbageLogfile = C:\\ProgramData\\StoneNotes\\Hiawatha\\garbage.log
ExploitLogfile = C:\\ProgramData\\StoneNotes\\Hiawatha\\exploit.log
PIDfile = C:\\ProgramData\\StoneNotes\\Hiawatha\\hiawatha.pid
WorkDirectory = C:\\ProgramData\\StoneNotes\\Hiawatha\\work
FollowSymlinks = yes

# BINDING SETTINGS
# A binding is where a client can connect to.
#
Binding {
	Port = 64769
#	Interface = 127.0.0.1
#	MaxKeepAlive = 30
#	TimeForRequest = 3,20
}
#
#Binding {
#	Port = 443
#	Interface = ::1
#	MaxKeepAlive = 30
#	TimeForRequest = 3,20
#	TLScertFile = INSTALL_DIR\\config\\hiawatha.pem
#}


# BANNING SETTINGS
# Deny service to clients who misbehave.
#
#BanOnGarbage = 300
#BanOnMaxPerIP = 60
#BanOnMaxReqSize = 300
#KickOnBan = yes
#RebanDuringBan = yes


# COMMON GATEWAY INTERFACE (CGI) SETTINGS
# These settings can be used to run CGI applications.
#
#CGIhandler = C:\\Program Files\\PHP8\\php-cgi.exe:php
#CGIhandler = INSTALL_DIR\\program\\ssi-cgi.exe:shtml
#CGIextension = exe
#
#FastCGIserver {
#	FastCGIid = PHP8
#	ConnectTo = 127.0.0.1:2005
#	Extension = php
#}


# URL TOOLKIT
# This URL toolkit rule was made for the Banshee PHP framework, which
# can be downloaded from https://www.hiawatha-webserver.org/banshee
#
#UrlToolkit {
#	ToolkitID = banshee
#	RequestURI isfile Return
#	Match ^/(css|files|images|js)/ Return
#	Match ^/(favicon.ico|robots.txt|sitemap.xml)$ Return
#	Match .*\\?(.*) Rewrite /index.php?$1
#	Match .* Rewrite /index.php
#}


# DEFAULT WEBSITE
# It is wise to use your IP address as the hostname of the default website
# and give it a blank webpage. By doing so, automated webscanners won't find
# your possible vulnerable website.
#
Hostname = 127.0.0.1
WebsiteRoot = INSTALL_DIR\\default_site
StartFile = index.html
AccessLogfile = INSTALL_DIR\\logfiles\\access.log
ErrorLogfile = INSTALL_DIR\\logfiles\\error.log
END);

print "Initial component startup...\n";

file_exists("C:\ProgramData\StoneNotes\Hiawatha\work") || mkdir("C:\ProgramData\StoneNotes\Hiawatha\work", 0777, true);

system('mshta javascript:alert("StoneNotes component installation\n\nNote: You are about to be prompted to allow a Command Processor script to make changes to your system.\n\nYou must allow this for the Hiawatha component to be installed properly. Without this step, some of your StoneNotes plugins will not function properly.");close();');

file_put_contents("service-hiawatha.bat", <<<END
@ECHO OFF

SET INSTALL_DIR=%~dp0\Hiawatha\
SET CYGWIN=nodosfilewarning

WHOAMI /groups | FINDSTR Administrators | FINDSTR /c:"Enabled group" > NUL && GOTO MAIN
ECHO You need to have Administrator rights to do this.
GOTO END

:MAIN
IF /i "%1" == "i" GOTO INSTALL
IF /i "%1" == "u" GOTO UNINSTALL
ECHO Invalid option, please specify either i or u when calling service-hiawatha.bat
PAUSE
GOTO END

:INSTALL
ECHO Installing Hiawatha as a Windows service...
"%INSTALL_DIR%program\cygrunsrv.exe" -I hiawathaStoneNotes  -d "Hiawatha webserver for StoneNotes" -f "Secure and advanced webserver, optional component for StoneNotes" -p "%INSTALL_DIR%program\hiawatha.exe" -a "-d -c '%INSTALL_DIR%config'"
net start hiawathaStoneNotes

ECHO Creating resource links...
MKLINK /J %INSTALL_DIR%\\default_site\\Assets %INSTALL_DIR%\\..\\..\\Assets
MKLINK /J %INSTALL_DIR%\\default_site\\Plugins %INSTALL_DIR%\\..\\..\\Plugins

GOTO END

:UNINSTALL
ECHO Uninstalling Hiawatha as a Windows service...
net stop hiawathaStoneNotes
"%INSTALL_DIR%program\cygrunsrv.exe" -R hiawathaStoneNotes

:END
ECHO.
END);

file_put_contents("service-hiawatha.vbs", <<<END
set shell=CreateObject("Shell.Application")
shell.ShellExecute "service-hiawatha.bat","i",".\", "runas", 0 
set shell=nothing
END);

if(false === system("cscript service-hiawatha.vbs")) {
	print "Error installing Hiawatha service!";
    system("pause");
    cleanup();
    exit(1);
}

cleanup();

function cleanup() {
	print "Cleaning up...\n";
	//file_exists("service-hiawatha.bat") && unlink("service-hiawatha.bat");
	//file_exists("service-hiawatha.vbs") && unlink("service-hiawatha.vbs");
	//file_exists("Hiawatha.zip") && unlink("Hiawatha.zip");
}