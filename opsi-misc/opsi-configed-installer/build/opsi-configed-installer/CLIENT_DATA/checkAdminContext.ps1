$user=[Security.Principal.WindowsIdentity]::GetCurrent();
return (New-Object Security.Principal.WindowsPrincipal $user).IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator) 