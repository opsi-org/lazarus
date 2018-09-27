// MSIInfo.js
// Written by Bill Stewart (bstewart@iname.com)
// Outputs a selected set of properties from an MSI database.

var SCRIPT_NAME = "MSIInfo.js";

// This array specifies the property names the script will handle.
var VALID_PROPERTIES = ["Manufacturer", "ProductName", "ProductVersion",
                        "ProductCode", "UpgradeCode"];

// These are dialog box constants.
var DLG_CRITICAL    = 16,
    DLG_EXCLAMATION = 48,
    DLG_INFORMATION = 64;

// Returns the script host's name (cscript.exe or wscript.exe).
function scriptHost() {
  return WScript.FullName.substr(WScript.Path.length + 1).toLowerCase();
}

// Outputs the specified string depending on which script host is active.
// If the script host is wscript.exe, the type parameter specifies the type
// of dialog box to use. The type parameter is ignored if the script host
// is cscript.exe.
function echo(string, type) {
  if (scriptHost() == "cscript.exe")
    WScript.Echo(string);
  else {
    var wshShell = new ActiveXObject("WScript.Shell");
    wshShell.Popup(string, 0, "MSI Information", type);
  }
}

// Returns the specified array as a space-delimited string.
function arrayToString(theArray) {
  var result = "";
  for (var n = 0; n < theArray.length; n++)
    result += result == "" ? theArray[n] : " " + theArray[n];
  return result;
}

// Outputs a usage message for the user.
function usage() {
  echo("Displays information about an MSI database.\r\n" +
    "\r\n" +
    "Usage: " + SCRIPT_NAME + " <msifile> [<propertyname>]\r\n" +
    "\r\n" +
    "If specified, <propertyname> must be one of the following values:\r\n" +
    "\r\n" +
    arrayToString(VALID_PROPERTIES) + "\r\n" +
    "\r\n" +
    "Property names must be spelled with the correct case. Without a\r\n" +
    "property name, all property values are displayed.",
    DLG_INFORMATION);

  WScript.Quit(0);
}

// Returns true if the specified item is an element in the array, or false
// otherwise.
function itemInArray(theItem, theArray) {
  for (var n = 0; n < theArray.length; n++)
    if (theItem == theArray[n]) return true;
  return false;
}

// Returns the specified number as a hex string.
function hex(n) {
  return n < 0 ? (n + Math.pow(2, 32)).toString(0x10).toUpperCase() :
    n.toString(0x10).toUpperCase();
}

// Returns a property's value from an MSI database. If the property doesn't
// exist, returns a blank string.
function getProperty(database, property) {
  var view = database.OpenView("select `Value` from `Property` where " +
    "`Property` = '" + property + "'");
  view.Execute();
  var record = view.Fetch();
  return record == null ? "" : record.StringData(1);
}

function main() {
  var args = WScript.Arguments;

  if ((args.Unnamed.length == 0) || (args.Named.Exists("?")))
    usage();

  var msiFile = args.Unnamed.Item(0);

  // Return an error if the named MSI file doesn't exist.
  var fso = new ActiveXObject("Scripting.FileSystemObject");
  if (! fso.FileExists(msiFile)) {
    echo("File not found - " + msiFile, DLG_EXCLAMATION);
    return 2;  // ERROR_FILE_NOT_FOUND
  }

  // Open the MSI database, or fail gracefully if an error occurs.
  try {
    var installer = new ActiveXObject("WindowsInstaller.Installer");
    var database = installer.OpenDatabase(msiFile, 0); // read-only
  }
  catch(err) {
    echo("Error " + hex(err.number) + " opening msi file", DLG_CRITICAL);
    return err.number;
  }

  // If a property name is not specified, output all properties.
  if (args.Unnamed.length == 1) {
    var properties = VALID_PROPERTIES;
    var output = "MSI file: " + msiFile;
    for (var n = 0; n < properties.length; n++) {
      var property = getProperty(database, properties[n]);
      output += "\r\n" + properties[n] + ": " + property;
    }
    echo(output, 0);
    return 0;
  }

  // Get the property name from the command line.
  var property = args.Unnamed.Item(1);

  // If the property name is not valid, output an error and exit.
  if (! itemInArray(property, VALID_PROPERTIES)) {
    echo("Property name must be one of the following (case-sensitive):\r\n" +
      "\r\n" +
      arrayToString(VALID_PROPERTIES),
      DLG_EXCLAMATION);
    return 87;  // ERROR_INVALID_PARAMETER
  }

  // Output the property.
  echo(getProperty(database, property), 0);
  return 0;
}

WScript.Quit(main());