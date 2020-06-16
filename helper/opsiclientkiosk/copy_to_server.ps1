#Function used to decrypt password
function Decrypt([string]$exportfile)
{
$securepassword = ConvertTo-SecureString $exportfile
$helper = new-object -typename System.Management.Automation.PSCredential -argumentlist $username, $password
$global:pass = $helper.GetNetworkCredential().Password
}
 
#Set some variables
# Secure file stores the password for the DOMAIN\USER Account the file was generated using this command
#       read-host -assecurestring | convertfrom-securestring | out-file C:\scripts\SecureFile.txt
$username = "DOMAIN\USERNAME"
$sourcePath = "\\someserver\x$\somefile.ext"
$destpath = "x:\some path\"
$securefile = "C:\scripts\SecureFile.txt"
 
#Open up the secure file and decrypt it
$exportfile = get-content $securefile
Decrypt $exportfile
 
#Map the Drive
#using old school NET USE command to map the drive. This will cache the credentials so the Copy-Item command will work.
net use \\server\x$ $pass /USER:$username
 
#copy the File and delete the drive
# Clean up the NET USE command by deleting the connection.
Copy-Item $sourcePath -Destination $destPath
net use \\server\x$ /DELETE