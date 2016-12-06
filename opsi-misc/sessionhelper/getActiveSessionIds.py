"""
   = = = = = = = = = = = = = = = = = = =
   =   ocdlib.Localization             =
   = = = = = = = = = = = = = = = = = = =
   
   opsiclientd is part of the desktop management solution opsi
   (open pc server integration) http://www.opsi.org
   
   Copyright (C) 2010 uib GmbH
   
   http://www.uib.de/
   
   All rights reserved.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 as
   published by the Free Software Foundation.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
   
   @copyright:	uib GmbH <info@uib.de>
   @author: Erol Ueluekmen <e.ueluekmen@uib.de>
   @license: GNU General Public License version 2
"""

import win32security
import win32ts
import sys


sessionIds = []
result = {}

wtsUserName = None


#print int(windll.kernel32.WTSGetActiveConsoleSessionId())
if len(sys.argv) > 1:
    sessionId = int(sys.argv[1])
    try:
        wtsUserName = win32ts.WTSQuerySessionInformation(None, sessionId, win32ts.WTSUserName)
    except:
        pass
    for s in win32security.LsaEnumerateLogonSessions():
        sessionData = win32security.LsaGetLogonSessionData(s)
        if (int(sessionData.get("Session")) == sessionId):
            if wtsUserName and sessionData.get("UserName").lower() != wtsUserName.lower():
                continue
            sessionData["Sid"] = str(sessionData["Sid"])
            sessionData["LogonTime"] = str(sessionData["LogonTime"])
            result = sessionData
    print result       
else:
    for s in win32security.LsaEnumerateLogonSessions():
        sessionData = win32security.LsaGetLogonSessionData(s)
        if not int(sessionData.get("LogonType")) in (2,10):
            continue
        sessionId = int(sessionData.get("Session"))
        if not sessionId in sessionIds:
            sessionIds.append(sessionId)
    print sessionIds
